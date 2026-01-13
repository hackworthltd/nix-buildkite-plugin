{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main (main) where

import Protolude hiding (empty, isPrefixOf)

import Algebra.Graph.AdjacencyMap (AdjacencyMap, edge, empty, hasVertex, overlay, overlays, postSet, vertexSet)
import Data.Aeson (Value (..), encode, object, (.=))
import Data.Attoparsec.Text (Parser, char, parseOnly, sepBy, takeWhile1)
import Data.ByteString.Lazy qualified
import Data.Containers.ListUtils (nubOrd)
import Data.List (partition)
import Data.Map qualified as Map
import Data.Set qualified as S
import Data.Text (isPrefixOf)
import Data.Text qualified as T
import Data.Vector qualified as V
import Nix.Derivation (
  Derivation (Derivation, env, inputDrvs),
  parseDerivationWith,
  textParser,
 )
import System.Environment (lookupEnv)
import System.FilePath (takeBaseName, takeFileName)

-- Minimize exposure to annoying 'FilePath'
readFile' :: Text -> IO Text
readFile' = readFile . toS

takeFileName' :: Text -> Text
takeFileName' = toS . takeFileName . toS

takeBaseName' :: Text -> Text
takeBaseName' = toS . takeBaseName . toS

parseDerivation :: Parser (Derivation Text Text)
parseDerivation = parseDerivationWith textParser textParser

data CacheStatus = Local | Cached | NotBuilt
  deriving (Eq, Show)

parseCacheStatus :: Text -> CacheStatus
parseCacheStatus "local" = Local
parseCacheStatus "cached" = Cached
parseCacheStatus "notBuilt" = NotBuilt
parseCacheStatus unknown = panic $ "Unknown cache status: " <> unknown

parseAgentTags :: Text -> Either [Char] (Map Text Text)
parseAgentTags input = do
  pairs' <- parseOnly pairs input
  return $ Map.fromList pairs'
  where
    pairs = pair `sepBy` ","
    pair = do
      key <- takeWhile1 (/= '=')
      _ <- char '='
      value <- takeWhile1 (/= ',')
      return (key, value)

main :: IO ()
main = do
  nixStoreOpts <- lookupEnv "NIX_STORE_OPTS" <&> maybe mempty (words . toS)

  nixBuildOpts <- lookupEnv "NIX_BUILD_OPTS" <&> maybe mempty (words . toS)

  kubernetesPodTemplate <- lookupEnv "KUBERNETES_POD_TEMPLATE" <&> fmap toS

  agentTags <- do
    tags <- lookupEnv "AGENT_TAGS"
    case tags of
      Nothing -> return mempty
      Just tags' -> do
        case parseAgentTags (toS tags') of
          Left err -> panic $ "Failed to parse AGENT_TAGS: " <> toS err
          Right pairs' -> return pairs'

  darwinStepAgentTags <- do
    tags <- lookupEnv "DARWIN_STEP_AGENT_TAGS"
    case tags of
      Nothing -> return mempty
      Just tags' -> do
        case parseAgentTags (toS tags') of
          Left err -> panic $ "Failed to parse DARWIN_STEP_AGENT_TAGS: " <> toS err
          Right pairs' -> return pairs'

  -- TODO: this should be made into an option
  -- (and probably should add options for prefixing at all, using emoji, and sorting also)
  let skipPrefix = ["required"]

  useNixBuild <- isJust <$> lookupEnv "USE_NIX_BUILD"
  ignoreCacheStatus <- isJust <$> lookupEnv "IGNORE_CACHE_STATUS"
  attrPrefix <- lookupEnv "ATTR_PREFIX" <&> maybe mempty toS

  -- Parse input.
  (inputDrvPaths, drvAttrMap, drvStatusMap) <- do
    inputLines <- map words . lines <$> getContents
    if useNixBuild
      then do
        -- In `nix build` mode, each input line is "drvPath attribute status".
        let tuples = [(drv, attr, status) | [drv, attr, status] <- inputLines]
        let drvs' = nubOrd $ map (\(drv, _, _) -> drv) tuples
        let drvAttrMap' = Map.fromList [(drv, attr) | (drv, attr, _) <- tuples]
        let drvStatusMap' = Map.fromList [(drv, parseCacheStatus status) | (drv, _, status) <- tuples]
        return (drvs', drvAttrMap', drvStatusMap')
      else do
        -- Otherwise, each line is "drvPath status".
        let pairs = [(drv, status) | [drv, status] <- inputLines]
        let drvs' = nubOrd $ map fst pairs
        let drvStatusMap' = Map.fromList [(drv, parseCacheStatus status) | (drv, status) <- pairs]
        return (drvs', Map.empty, drvStatusMap')

  -- Build an association list of a job name and the derivation that should be
  -- realised for that job, and also track the system for each derivation.
  (drvs, drvSystemMap) <- do
    results <- for inputDrvPaths \drvPath ->
      readFile' drvPath
        >>= ( \case
                Left _ ->
                  -- We couldn't parse the derivation to get a name, so we'll just use the
                  -- derivation name.
                  return (takeFileName' drvPath, drvPath, Nothing)
                Right drv ->
                  let name = case Map.lookup "name" (env drv) of
                        -- There was no 'name' environment variable, so we'll just use the
                        -- derivation name.
                        Nothing -> takeFileName' drvPath
                        Just n -> n
                      system = Map.lookup "system" (env drv)
                      emoji =
                        if any (`isPrefixOf` name) skipPrefix
                          then ""
                          else case system of
                            Nothing -> ""
                            Just s -> emojify s <> ":"
                   in return (emoji <> name, drvPath, system)
            )
        . parseOnly parseDerivation

    let drvs' = map (\(name, path, _) -> (name, path)) results
    let drvSystemMap' = Map.fromList [(path, sys) | (_, path, Just sys) <- results]
    return (drvs', drvSystemMap')

  g <- foldr (\(_, drv) m -> m >>= \g -> add g drv) (pure empty) drvs

  let jobSet = S.fromList $ map snd drvs

  -- Calculate the dependency graph
  -- For each vertex, we calculate its direct dependencies from the job set.
  -- This is:
  -- - any direct dependencies that are in the job set (base case)
  -- - the transitive job set dependencies of non-job set dependencies (recursive case)
  let closureG =
        Map.fromList
          [ (v, us)
          | v <- S.toList (vertexSet g)
          , let nexts = S.toList $ postSet v g
          , let (ins, outs) = partition (`S.member` jobSet) nexts
          , let us = S.unions $ S.fromList ins : map (\i -> fromMaybe S.empty $ Map.lookup i closureG) outs
          ]

  let steps = map (uncurry step) drvs
        where
          step :: Text -> Text -> (Text, Value)
          step label drvPath =
            ( label
            , object $
                [ "label" .= label
                , "command" .= stepCommand drvPath
                , "key" .= stepify drvPath
                , "depends_on" .= dependencies
                ]
                  ++ agentFields
                  ++ kubernetesPluginFields
            )
            where
              dependencies = map stepify $ maybe [] S.toList $ Map.lookup drvPath closureG

              -- Possibly override the agent tags if this is a Darwin
              -- derivation
              agentFields =
                [ "agents" .= darwinStepAgentTags
                | isDarwinSystem (Map.lookup drvPath drvSystemMap)
                , not (Map.null darwinStepAgentTags)
                ]

              -- Optionally add the agent-stack-k8s kubernetes plugin
              -- for Linux steps
              kubernetesPluginFields =
                [ "plugins"
                    .= Array
                      ( V.fromList
                          [ object
                              [ "kubernetes"
                                  .= object
                                    [ "podTemplate" .= String tpl
                                    ]
                              ]
                          ]
                      )
                | isLinuxSystem (Map.lookup drvPath drvSystemMap)
                , Just tpl' <- [kubernetesPodTemplate]
                , let tpl = T.strip tpl'
                , not (T.null tpl)
                ]

              isDarwinSystem :: Maybe Text -> Bool
              isDarwinSystem (Just system) = "darwin" `T.isSuffixOf` system
              isDarwinSystem Nothing = False

              isLinuxSystem :: Maybe Text -> Bool
              isLinuxSystem (Just system) = "linux" `T.isSuffixOf` system
              isLinuxSystem Nothing = False

          buildCommand :: Text -> Text
          buildCommand drvPath =
            if useNixBuild
              then case Map.lookup drvPath drvAttrMap of
                Nothing -> panic $ "Internal error: could not find attribute for derivation " <> drvPath
                Just attr -> unwords $ ["nix", "build"] <> nixBuildOpts <> [attrPrefix <> attr]
              else unwords $ ["nix-store"] <> nixStoreOpts <> ["-r", drvPath]

          stepCommand :: Text -> Value
          stepCommand drvPath =
            String $
              if ignoreCacheStatus
                then buildCommand drvPath
                else case Map.lookup drvPath drvStatusMap of
                  Just Local ->
                    if useNixBuild
                      then case Map.lookup drvPath drvAttrMap of
                        Nothing -> panic $ "Internal error: could not find attribute for derivation " <> drvPath
                        Just attr -> "echo 'Attribute " <> attrPrefix <> attr <> " already built locally'"
                      else "echo 'Derivation " <> takeFileName' drvPath <> " already built locally'"
                  Just Cached ->
                    if useNixBuild
                      then case Map.lookup drvPath drvAttrMap of
                        Nothing -> panic $ "Internal error: could not find attribute for derivation " <> drvPath
                        Just attr -> "echo 'Attribute " <> attrPrefix <> attr <> " already cached'"
                      else "echo 'Derivation " <> takeFileName' drvPath <> " already cached'"
                  _ ->
                    buildCommand drvPath

  Data.ByteString.Lazy.putStr $
    encode $
      object
        [ "agents" .= agentTags
        , "steps" .= map snd (sortOn fst steps)
        ]

-- Transform nix platforms into buildkite emoji
-- See https://github.com/buildkite/emojis
emojify :: Text -> Text
emojify system =
  let (_, os) = T.breakOnEnd "-" system
   in toEmoji os <> system
  where
    toEmoji "darwin" = ":darwin: "
    toEmoji "freebsd" = ":freebsd: "
    toEmoji "linux" = ":linux: "
    toEmoji "netbsd" = ":netbsd: "
    toEmoji "openbsd" = ":openbsd: "
    toEmoji "windows" = ":windows: "
    toEmoji _ = ""

stepify :: Text -> Text
stepify = T.take 99 . T.map repl . takeBaseName'
  where
    repl x | isAlphaNum x = x
    repl '/' = '/'
    repl '-' = '-'
    repl _ = '_'

add :: AdjacencyMap Text -> Text -> IO (AdjacencyMap Text)
add g drvPath =
  if hasVertex drvPath g
    then return g
    else
      readFile' drvPath
        >>= ( \case
                Left _ ->
                  return g
                Right Derivation{inputDrvs} -> do
                  deps <- foldr (\dep m -> m >>= \g' -> add g' dep) (pure g) (Map.keys inputDrvs)

                  let g' = overlays (edge drvPath <$> Map.keys inputDrvs)

                  return $ overlay deps g'
            )
        . parseOnly parseDerivation

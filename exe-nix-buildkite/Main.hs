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

main :: IO ()
main = do
  nixStoreOpts <- lookupEnv "NIX_STORE_OPTS" <&> maybe mempty (words . toS)

  nixBuildOpts <- lookupEnv "NIX_BUILD_OPTS" <&> maybe mempty (words . toS)

  agentTags <- do
    tags <- lookupEnv "AGENT_TAGS"
    case tags of
      Nothing -> return mempty
      Just tags' -> do
        case parseOnly pairs $ toS tags' of
          Left err -> panic $ "Failed to parse AGENT_TAGS: " <> toS err
          Right pairs' -> return $ Map.fromList pairs'
        where
          pairs = pair `sepBy` ","
          pair = do
            key <- takeWhile1 (/= '=')
            _ <- char '='
            value <- takeWhile1 (/= ',')
            return (key, value)

  -- TODO: this should be made into an option
  -- (and probably should add options for prefixing at all, using emoji, and sorting also)
  let skipPrefix = ["required"]

  useNixBuild <- isJust <$> lookupEnv "USE_NIX_BUILD"
  attrPrefix <- lookupEnv "ATTR_PREFIX" <&> maybe mempty toS

  -- Read derivations (or a space-delimited pair of derivation and
  -- attribute, depending on the mode) from stdin.
  (inputDrvPaths, drvAttrMap) <-
    if useNixBuild
      then do
        -- In attribute mode, each line is "drvPath attribute".
        pairs' <- map words . lines <$> getContents
        let validPairs = [(drv, attr) | [drv, attr] <- pairs']
        let drvs' = nubOrd $ map fst validPairs
        let drvAttrMap' = Map.fromList validPairs
        return (drvs', drvAttrMap')
      else do
        -- Otherwise, each line is just a drvPath.
        drvs' <- nubOrd . lines <$> getContents
        return (drvs', Map.empty)

  -- Build an association list of a job name and the derivation that should be
  -- realised for that job.
  drvs <- for inputDrvPaths \drvPath ->
    readFile' drvPath
      >>= ( \case
              Left _ ->
                -- We couldn't parse the derivation to get a name, so we'll just use the
                -- derivation name.
                return (takeFileName' drvPath, drvPath)
              Right drv ->
                let name = case Map.lookup "name" (env drv) of
                      -- There was no 'name' environment variable, so we'll just use the
                      -- derivation name.
                      Nothing -> takeFileName' drvPath
                      Just n -> n
                    system =
                      if any (`isPrefixOf` name) skipPrefix
                        then ""
                        else case Map.lookup "system" (env drv) of
                          Nothing -> ""
                          Just s -> emojify s <> ":"
                 in return (system <> name, drvPath)
          )
      . parseOnly parseDerivation

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
            , object
                [ "label" .= label
                , "command" .= buildCommand drvPath
                , "key" .= stepify drvPath
                , "depends_on" .= dependencies
                ]
            )
            where
              dependencies = map stepify $ maybe [] S.toList $ Map.lookup drvPath closureG

          buildCommand :: Text -> Value
          buildCommand drvPath =
            String $
              if useNixBuild
                then case Map.lookup drvPath drvAttrMap of
                  Nothing -> panic $ "Internal error: could not find attribute for derivation " <> drvPath
                  Just attr -> unwords $ ["nix", "build"] <> nixBuildOpts <> [attrPrefix <> attr]
                else
                  unwords $ ["nix-store"] <> nixStoreOpts <> ["-r", drvPath]

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

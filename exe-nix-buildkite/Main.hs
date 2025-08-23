{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

-- algebraic-graphs
import Algebra.Graph.AdjacencyMap (AdjacencyMap, edge, empty, hasVertex, overlay, overlays, postSet, vertexSet)

-- aeson
import Data.Aeson (Value (..), encode, object, (.=))

-- attoparsec
import Data.Attoparsec.Text (char, parseOnly, sepBy, takeWhile1)

-- base
import Data.Char
import Data.List (partition, sortOn)
import Data.List qualified as List
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Traversable (for)
import System.Environment (getArgs, lookupEnv)
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.IO (hGetContents, hPutStrLn, stderr)
import Prelude hiding (getContents, lines, readFile, words)
import Prelude qualified

-- bytestring
import Data.ByteString.Lazy qualified

-- containers
import Data.Containers.ListUtils (nubOrd)
import Data.Map qualified as Map
import Data.Set qualified as S

-- filepath
import System.FilePath

-- nix-derivation
import Nix.Derivation

-- process
import System.Process hiding (env, system)

-- text
import Data.Text (Text, isPrefixOf, pack, unpack)
import Data.Text qualified as T
import Data.Text.IO (readFile)

nixInstantiate :: String -> IO [String]
nixInstantiate jobsExpr = Prelude.lines <$> readProcess "nix-instantiate" [jobsExpr] ""

nixBuildDryRun :: [String] -> IO [String]
nixBuildDryRun jobsExpr =
  withCreateProcess ((proc "nix-build" ("--dry-run" : jobsExpr)){std_err = CreatePipe}) $ \_stdin _stdout stderrHndl prchndl -> do
    inputLines <-
      Prelude.lines <$> case stderrHndl of
        Just hndl -> hGetContents hndl
        Nothing -> pure []
    -- See Note: [nix-build --dry-run output]
    let stripLeadingWhitespace = dropWhile (== ' ')
    let theseLine line = List.isPrefixOf "these" line || List.isPrefixOf "this" line
    let buildLine line = theseLine line && List.isSubsequenceOf "built" line
    let fetchLine line = theseLine line && List.isSubsequenceOf "fetched" line

    -- dump the output to stderr
    mapM_ (hPutStrLn stderr) inputLines

    let res = map stripLeadingWhitespace . takeWhile (not . fetchLine) . drop 1 $ dropWhile (not . buildLine) inputLines
    exitCode <- waitForProcess prchndl
    case exitCode of
      ExitSuccess -> pure res
      ExitFailure err -> error $ "nix-build --dry run failed with exit code: " ++ show err

main :: IO ()
main = do
  jobsExpr <- fromMaybe "./jobs.nix" . listToMaybe <$> getArgs

  postBuildHook <- do
    cmd <- lookupEnv "POST_BUILD_HOOK"
    case cmd of
      Nothing -> return []
      Just path -> return ["--post-build-hook", path]

  skipAlreadyBuilt <- do
    e <- lookupEnv "SKIP_ALREADY_BUILT"
    pure $ case e of
      Just "true" -> True
      Just "false" -> False
      Just _ -> error "SKIP_ALREADY_BUILT only accepts 'true' or 'false'."
      Nothing -> False

  agentTags <- do
    tags <- lookupEnv "AGENT_TAGS"
    case tags of
      Nothing -> return mempty
      Just tags' -> do
        case parseOnly pairs $ pack tags' of
          Left err -> error $ "Failed to parse AGENT_TAGS: " ++ err
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

  -- Run nix-instantiate on the jobs expression to instantiate .drvs for all
  -- things that may need to be built.
  inputDrvPaths <- nubOrd <$> nixInstantiate jobsExpr

  -- Get the list of derivations that will be built, which may include drvs not in inputDrvPaths
  pathsToBuild <- if skipAlreadyBuilt then nixBuildDryRun inputDrvPaths else pure inputDrvPaths

  -- Filter our inputDrvs down to just those that will be built (if the skip already built flag is set)
  let inputDrvPathsToBuild = S.toList $ S.fromList inputDrvPaths `S.intersection` S.fromList pathsToBuild

  -- Build an association list of a job name and the derivation that should be
  -- realised for that job.
  drvs <- for inputDrvPathsToBuild \drvPath ->
    readFile drvPath
      >>= ( \case
              Left _ ->
                -- We couldn't parse the derivation to get a name, so we'll just use the
                -- derivation name.
                return (pack (takeFileName drvPath), drvPath)
              Right drv ->
                let name = case Map.lookup "name" (env drv) of
                      -- There was no 'name' environment variable, so we'll just use the
                      -- derivation name.
                      Nothing -> pack (takeFileName drvPath)
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
          step :: Text -> FilePath -> (Text, Value)
          step label drvPath =
            ( label
            , object
                [ "label" .= unpack label
                , "command" .= String (pack $ unwords $ ["nix-store"] <> postBuildHook <> ["-r", drvPath])
                , "key" .= stepify drvPath
                , "depends_on" .= dependencies
                ]
            )
            where
              dependencies = map stepify $ maybe [] S.toList $ Map.lookup drvPath closureG

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

stepify :: String -> String
stepify = take 99 . map replace . takeBaseName
  where
    replace x | isAlphaNum x = x
    replace '/' = '/'
    replace '-' = '-'
    replace _ = '_'

add :: AdjacencyMap FilePath -> FilePath -> IO (AdjacencyMap FilePath)
add g drvPath =
  if hasVertex drvPath g
    then return g
    else
      readFile drvPath
        >>= ( \case
                Left _ ->
                  return g
                Right Derivation{inputDrvs} -> do
                  deps <- foldr (\dep m -> m >>= \g' -> add g' dep) (pure g) (Map.keys inputDrvs)

                  let g' = overlays (edge drvPath <$> Map.keys inputDrvs)

                  return $ overlay deps g'
            )
          . parseOnly parseDerivation

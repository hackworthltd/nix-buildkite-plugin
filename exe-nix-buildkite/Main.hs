{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

-- algebraic-graphs
import Algebra.Graph.AdjacencyMap (AdjacencyMap, edge, empty, hasVertex, overlay, overlays)
import Algebra.Graph.AdjacencyMap.Algorithm (reachable)

-- aeson
import Data.Aeson (Value (..), encode, object, (.=))

-- attoparsec
import Data.Attoparsec.Text (char, parseOnly, sepBy, takeWhile1)

-- base
import Data.Char
import Data.List (sortOn)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Traversable (for)
import System.Environment (getArgs, lookupEnv)
import Prelude hiding (getContents, lines, readFile, words)
import qualified Prelude

-- bytestring
import qualified Data.ByteString.Lazy

-- containers
import Data.Containers.ListUtils (nubOrd)
import qualified Data.Map as Map

-- filepath
import System.FilePath

-- nix-derivation
import Nix.Derivation

-- process
import System.Process hiding (env, system)

-- text
import Data.Text (Text, isPrefixOf, pack, unpack)
import qualified Data.Text as T
import Data.Text.IO (readFile)

main :: IO ()
main = do
  jobsExpr <- fromMaybe "./jobs.nix" . listToMaybe <$> getArgs

  postBuildHook <- do
    cmd <- lookupEnv "POST_BUILD_HOOK"
    case cmd of
      Nothing -> return []
      Just path -> return ["--post-build-hook", path]

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
  inputDrvPaths <- nubOrd . Prelude.lines <$> readProcess "nix-instantiate" [jobsExpr] ""

  -- Build an association list of a job name and the derivation that should be
  -- realised for that job.
  drvs <- for inputDrvPaths \drvPath -> do
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

  let steps = map (uncurry step) drvs
        where
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
              dependencies = map stepify $ filter (`elem` map snd drvs) $ drop 1 $ reachable drvPath g

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

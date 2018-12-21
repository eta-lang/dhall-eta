module Main where

import qualified Dhall.Eta.Test.Parser as Parser
import qualified Dhall.Eta.Test.Import as Import
import qualified Dhall.Eta.Test.Normalization as Normalization
import qualified Dhall.Eta.Test.TypeCheck as TypeCheck

import qualified GHC.IO.Encoding
import qualified System.IO

import qualified Data.Text.IO as Text

import Control.Monad (forM, when)
import Data.List (partition)
import Data.Text (Text)
import Dhall.Eta.Test.Common (dhallCasesBasePath, selfCasesBasePath, skipTest)
import System.Directory (listDirectory, doesFileExist, doesDirectoryExist)
import System.FilePath (takeExtension, takeBaseName, (</>))
import Test.Tasty


allTests :: ([(FilePath, Text)],[(FilePath, Text)])
         -> (([(FilePath, Text)], [(FilePath,Text)]), [(FilePath, Text)])
         -> [(FilePath, Text)]
         -> ([(FilePath, Text)],[(FilePath, Text)])
         -> TestTree
allTests parseCases importCases normalizationCases typeCheckCases =
  testGroup "dhall-eta tests"
      [ Parser.tests parseCases
      , Import.tests importCases
      , Normalization.tests normalizationCases
      , TypeCheck.tests typeCheckCases
      ]

main :: IO ()
main = do
  GHC.IO.Encoding.setLocaleEncoding System.IO.utf8
  parseCases <- readParseCases
  importCases <- readImportCases
  normalCases <- readNormalizationCases
  typeCheckCases <- readTypeCheckCases
  defaultMain $ allTests parseCases importCases
                         normalCases typeCheckCases

readParseCases :: IO ([(FilePath, Text)], [(FilePath, Text)])
readParseCases = do
  oks <- readDhallCases $ "parser" </> "success"
  kos <- readDhallCases $ "parser" </> "failure"
  let isCaseA = (== 'A') . last . takeBaseName . fst
      oksA = filter isCaseA oks
      kos' = filter (not . skipTest . fst) kos
  return (oksA, kos')

readImportCases :: IO (([(FilePath, Text)], [(FilePath,Text)])
                           , [(FilePath, Text)])
readImportCases = do
  ok <- readDhallCases $ "import" </> "success"
  ko <- readDhallCases $ "import" </> "failure"
  ok' <- readSelfCases $ "import" </> "success"
  let (rel,abs) = partition isRelative ( ok ++ ok' )
  return ((abs,rel),ko)
  where isRelative (path,_) = takeBaseName path  `elem` relativeCases
        relativeCases = ["relative", "fieldOrderA", "issue553B" ]

readNormalizationCases :: IO [(FilePath, Text)]
readNormalizationCases =
  readDhallCases $ "normalization" </> "success"

readTypeCheckCases :: IO ([(FilePath, Text)], [(FilePath, Text)])
readTypeCheckCases = do
  oks <- readDhallCases $ "typecheck" </> "success"
  kos <- readDhallCases $ "typecheck" </> "failure"
  return (oks, kos)

readDhallCases :: FilePath -> IO [(FilePath, Text)]
readDhallCases relDir = readCases $ dhallCasesBasePath </> relDir

readSelfCases :: FilePath -> IO [(FilePath, Text)]
readSelfCases relDir = readCases $ selfCasesBasePath </> relDir

readCases :: FilePath -> IO [(FilePath, Text)]
readCases dir = do
  exists <- doesDirectoryExist dir
  when (not exists) $
    error $ "Dhall haskell tests cases dir not exist: " ++ dir
  dhallFiles <- traverseDir dir ((== ".dhall") . takeExtension)
  when (null dhallFiles) $
    error $ "Dhall haskell tests cases not found in " ++ dir
  forM dhallFiles $ \ path -> do
    content <- Text.readFile path
    return (path, content)

traverseDir :: FilePath -> (FilePath -> Bool) -> IO [FilePath]
traverseDir top include = do
  ds <- listDirectory top
  paths <- forM ds $ \d -> do
    let path = top </> d
    isDir <- doesDirectoryExist path
    isFile <- doesFileExist path
    if isDir
      then traverseDir path include
      else return ( if isFile && include path
                    then [path] else [] )        
  return (concat paths)


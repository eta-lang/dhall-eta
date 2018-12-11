module Main where

import qualified Dhall.Eta.Test.Parser as Parser
import qualified Dhall.Eta.Test.Import as Import
import qualified Dhall.Eta.Test.Normalization as Normalization

import qualified GHC.IO.Encoding
import qualified System.IO

import qualified Data.Text.IO as Text

import Control.Monad (forM, when)
import Data.List (partition)
import Data.Text (Text)
import Dhall.Eta.Test.Common (dhallCasesBasePath)
import System.Directory (listDirectory, doesFileExist, doesDirectoryExist)
import System.FilePath (takeExtension, takeBaseName, (</>))
import Test.Tasty


allTests :: ([(FilePath, Text)],[(FilePath, Text)])
         -> (([(FilePath, Text)], [(FilePath,Text)]), [(FilePath, Text)])
         -> [(FilePath, Text)]
         -> TestTree
allTests parseCases importCases normalizationCases =
  testGroup "dhall-eta tests"
      [ Parser.tests parseCases
      , Import.tests importCases
      , Normalization.tests normalizationCases
      ]

main :: IO ()
main = do
  GHC.IO.Encoding.setLocaleEncoding System.IO.utf8
  dhallParseCases <- readDhallParseCases
  dhallImportCases <- readDhallImportCases
  dhallNormalCases <- readDhallNormalizationCases
  defaultMain $ allTests dhallParseCases dhallImportCases dhallNormalCases

readDhallParseCases :: IO ([(FilePath, Text)], [(FilePath, Text)])
readDhallParseCases = do
  oks <- readDhallCases $ "parser" </> "success"
  kos <- readDhallCases $ "parser" </> "failure"
  let isCaseA = (== 'A') . last . takeBaseName . fst
      oksA = filter isCaseA oks 
  return (oksA, kos)

readDhallImportCases :: IO (([(FilePath, Text)], [(FilePath,Text)])
                           , [(FilePath, Text)])
readDhallImportCases = do
  ok <- readDhallCases $ "import" </> "success"
  ko <- readDhallCases $ "import" </> "failure"
  let (rel,abs) = partition isRelative ok
  return ((abs,rel),ko)
  where isRelative (path,_) = takeBaseName path  `elem` relativeCases
        relativeCases = ["relative", "fieldOrderA", "issue553B" ]

readDhallNormalizationCases :: IO [(FilePath, Text)]
readDhallNormalizationCases =
  readDhallCases $ "normalization" </> "success"

readDhallCases :: FilePath -> IO [(FilePath, Text)]
readDhallCases relDir = do
  let dir = dhallCasesBasePath </> relDir
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

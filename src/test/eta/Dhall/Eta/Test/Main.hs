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
import Dhall.Eta.Test.Common
  (dhallCasesBasePath, selfCasesBasePath, skipTest, isTestCaseA)
import System.Directory
  (listDirectory, doesFileExist, doesDirectoryExist)
import System.FilePath
  (takeExtension, takeBaseName, (</>))
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
  oks <- readDhallCasesPred isTestCaseA ( "parser" </> "success" )
  kos <- readDhallCases ( "parser" </> "failure" )
  return (oks, kos)

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
  readDhallCasesPred isTestCaseA ( "normalization" </> "success" )

readTypeCheckCases :: IO ([(FilePath, Text)], [(FilePath, Text)])
readTypeCheckCases = do
  oks <- readDhallCasesPred isTestCaseA $ "typecheck" </> "success"
  kos <- readDhallCases $ "typecheck" </> "failure"
  return (oks, kos)

readDhallCases :: FilePath -> IO [(FilePath, Text)]
readDhallCases relDir =
  readDhallCasesPred ( const True ) relDir

readDhallCasesPred :: ( FilePath -> Bool ) -> FilePath -> IO [(FilePath, Text)]
readDhallCasesPred pred relDir =
  readCases pred ( dhallCasesBasePath </> relDir )

readSelfCases :: FilePath -> IO [(FilePath, Text)]
readSelfCases relDir =
  readCases ( const True ) ( selfCasesBasePath </> relDir )

readCases :: ( FilePath -> Bool ) -> FilePath -> IO [(FilePath, Text)]
readCases pred dir = do
  let pred' path = takeExtension path == ".dhall"
                   && pred path && not ( skipTest path )
  exists <- doesDirectoryExist dir
  when (not exists) $
    error $ "Dhall haskell tests cases dir not exist: " ++ dir
  dhallFiles <- traverseDir dir pred'
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


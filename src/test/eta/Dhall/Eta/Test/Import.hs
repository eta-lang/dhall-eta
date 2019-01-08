module Dhall.Eta.Test.Import where

import Data.Text ( Text )

import Dhall.Eta.Test.Common

import System.FilePath (takeBaseName, takeDirectory, (</>))

import Test.Tasty
import Test.Tasty.HUnit

import Java

tests :: (([(FilePath, Text)], [(FilePath,Text)])
         , [(FilePath, Text)])
      -> TestTree
tests ((shouldImportCases, shouldImportRelCases), shouldNotImportCases) =
  testGroup "Import tests"
    [ testGroup "Should import"
      ( map shouldImportAndBeEqual shouldImportCases )
    , testGroup "Should import relative"
      ( map shouldImportAndBeEqualRelative shouldImportRelCases )
    , testGroup "Should NOT import"
      ( map shouldNotImport shouldNotImportCases )
    ]

shouldNotImport :: (FilePath, Text) -> TestTree
shouldNotImport = importShouldBeEqual succeded failed
  where succeded = fail "The import succeded but it had to fail"
        failed = return ()
          
shouldImportAndBeEqual :: (FilePath, Text) -> TestTree
shouldImportAndBeEqual = importShouldBeEqual succeded failed
  where succeded = return ()
        failed = fail "The import failed but it had to success."
        
importShouldBeEqual :: Assertion -> Assertion -> (FilePath, Text) -> TestTree
importShouldBeEqual successAssert errorAssert ( path, txt ) =
  testCase ( "Importing " ++ takeBaseName path )
  ( do
      (expr, jexpr) <- parseOrThrow txt
      -- To save the cache file and make both resolutions exactly equal
      _ <- resolve (expr, jexpr)
      (rexpr, rjexpr) <- resolve (expr, jexpr) 
      case (rexpr, rjexpr) of
        (Left  _,    Left _)      -> errorAssert
        (Left  _,    Right _)     -> fail "Dhall import failed but Dhall.Eta one didn't."
        (Right _,    Left _)      -> fail "Dhall import succeded but Dhall.Eta one didn't."
        (Right expr, Right jexpr) -> successAssert >>
          if expr == fromJava jexpr then return ()
          else fail $ "Importing is not equal between Dhall and Dhall.Eta."
          
  )

shouldImportAndBeEqualRelative :: (FilePath, Text) -> TestTree
shouldImportAndBeEqualRelative ( path, txt ) =
  testCase ( "Importing " ++ takeBaseName path )
  ( do (expr, jexpr) <- parseOrThrow txt
       (rexpr, rjexpr) <- resolveRelative (relativeDir path) (expr, jexpr)
       case (rexpr, rjexpr) of
         (Left  ex,    Left _)      ->
           fail $ "The import failed but it had to success. Dhall exception: " ++ show ex
         (Left  _,    Right _)     -> fail "Dhall import failed but Dhall.Eta one didn't."
         (Right _,    Left jex)      -> 
           fail $ "Dhall import succeded but Dhall.Eta one didn't. Dhall.Eta exception: "
               ++ show jex
         (Right expr, Right jexpr) ->
           assertEqual
            ( "Importing is not equal between Dhall and Dhall.Eta." )
            expr ( fromJava jexpr )
  )

relativeDir :: FilePath -> FilePath
relativeDir origPath = 
  if takeBaseName origPath == "relative"
     then origDir </> ".." </> "data" </> "foo" </> "bar"
     else origDir
  where origDir = takeDirectory origPath

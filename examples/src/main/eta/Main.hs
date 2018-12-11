{-# LANGUAGE OverloadedStrings #-}
module Main where
import Dhall.Eta.Parser

main :: IO ()
main = putStrLn $ show $ exprFromText "main" "1" 

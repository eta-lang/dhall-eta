{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}

module Dhall.Eta.Parser.Java where

import qualified Dhall.Parser as Dhall 
import qualified Text.Megaparsec  as Megaparsec
  (SourcePos(..),Pos,unPos,mkPos)
import qualified Text.Megaparsec.Error as Megaparsec (errorBundlePretty)

import Java

data JPos = JPos @org.dhall.parser.Pos
   deriving (Class,Show,Eq)

foreign import java unsafe "@new" newJPos  :: Int -> JPos

foreign import java unsafe "getValue" jposVal :: JPos -> Int

instance JavaConverter Megaparsec.Pos JPos where
  toJava = newJPos . Megaparsec.unPos
  fromJava = Megaparsec.mkPos . jposVal

data JSourcePos = JSourcePos @org.dhall.parser.SourcePos
   deriving (Class,Show,Eq)

foreign import java unsafe "@new" newJSourcePos  :: JString -> JPos -> JPos -> JSourcePos

foreign import java unsafe "getFileName" jsourcePosName :: JSourcePos -> JString

foreign import java unsafe "getLine" jsourcePosLine :: JSourcePos -> JPos

foreign import java unsafe "getColumn" jsourcePosColumn :: JSourcePos -> JPos

instance JavaConverter Megaparsec.SourcePos JSourcePos where
  toJava (Megaparsec.SourcePos {..}) =
    newJSourcePos (toJava sourceName) (toJava sourceLine) (toJava sourceColumn)
  fromJava jSrcPos = Megaparsec.SourcePos
                     { sourceName = fromJava $ jsourcePosName jSrcPos
                     , sourceLine = fromJava $ jsourcePosLine jSrcPos
                     , sourceColumn = fromJava $ jsourcePosColumn jSrcPos
                     } 

data JSrc = JSrc @org.dhall.parser.Src
   deriving (Class,Show,Eq)

foreign import java unsafe "@new" newJSrc  :: JSourcePos -> JSourcePos -> JString -> JSrc

foreign import java unsafe "getBegin" jsrcBegin :: JSrc -> JSourcePos

foreign import java unsafe "getEnd" jsrcEnd :: JSrc -> JSourcePos

foreign import java unsafe "getText" jsrcText :: JSrc -> JString

instance JavaConverter Dhall.Src JSrc where
  toJava (Dhall.Src line col txt) =
    newJSrc (toJava line) (toJava col) (toJava txt)
  {-# INLINE toJava #-}
  fromJava jsrc =
    Dhall.Src (fromJava $ jsrcBegin jsrc)
              (fromJava $ jsrcEnd jsrc)
              (fromJava $ jsrcText jsrc)
  {-# INLINE fromJava #-}

-- TODO Convert internal megaparsec errors and fromJava
data JParseError = JParseError @org.dhall.parser.error.ParseError
   deriving (Class,Show,Eq)

foreign import java unsafe "@new" newJParseError  :: JString -> JString -> JParseError

foreign import java unsafe "getMessage" parseErrorMsg :: JParseError -> JString

foreign import java unsafe "getInput" parseErrorInput :: JParseError -> JString

instance JavaConverter Dhall.ParseError JParseError where
  toJava Dhall.ParseError {..} =
    newJParseError (toJava $ Megaparsec.errorBundlePretty unwrap)
                   (toJava $ input)
  fromJava  = error "Not implemented"

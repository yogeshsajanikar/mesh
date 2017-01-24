{-# LANGUAGE MagicHash #-}
module Data.Object where

import Data.Vector.Unboxed hiding ((++))
import qualified Data.Vector.Unboxed.Mutable as MV
import qualified Data.ByteString.Char8 as B8
import Data.Attoparsec.ByteString.Char8
import qualified Data.Attoparsec.ByteString as B


data Vertex a# = Vertex a# a# a#
               | HVertex a# a# a# a#

data Texture a# = Texture a# a#
                | HTexture a# a# a#

data Normal a# = Normal a# a# a#

data Face i# = Face [i#] [i#] [i#]

type Group i# = [Face i#]

data NamedObject i# = NamedObject String [Face i#] [Group i#]

data Object a# i# = Object { _vertices :: Vector (Vertex a#)
                           , _textures :: Vector (Texture a#)
                           , _normals :: Vector (Normal a#)
                           , _root :: NamedObject i#
                           , _namedobjects :: [NamedObject i#]
                           }
             

  
data MObject a# i# = MObject { _mvertices :: Vector (Vertex a#)
                             , _mtextures :: Vector (Texture a#)
                             , _mnormals :: Vector (Normal a#)
                             , _mroot :: NamedObject i#
                             , _mnamedobjects :: [NamedObject i#]
                             }


skipWS = skipWhile isSpace

-- comment
commentP = do
  skipWhile isSpace
  char '#'
  many' (B.satisfy isEndOfLine)
  endOfLine

vertexP = do
  skipWS
  char 'v'
  xs <- fmap realToFrac <$> count 3 (skipWS >> scientific)
  h  <- option Nothing (Just <$> (skipWS >> scientific))
  skipWS >> endOfLine
  return (vertexFrom xs h) 

  where
    vertexFrom (x:y:z:_) Nothing  = Vertex x y z
    vertexFrom (x:y:z:_) (Just h) = HVertex x y z h
    vertexFrom _       _        = error "Incomplete vertex information"

faceIndexP = signed decimal

faceP = do
  fs <- count 3 (skipWS >> faceIndexP)
  fs' <- many faceIndexP
  return fs ++ fs'

--vertexP = 

-- faceP 

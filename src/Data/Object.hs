{-# LANGUAGE MagicHash #-}
module Data.Object where

import           Control.Applicative
import           Control.Monad.State.Strict
import qualified Data.Attoparsec.ByteString as B
import           Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as B8
import           Data.Vector.Unboxed hiding ((++))
import qualified Data.Vector.Unboxed.Mutable as MV

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

blankLine = skipWS >> endOfLine

-- Comment
-- Assume that we are starting off with a white space and skipping
-- till the end of the line
commentP :: Parser ()
commentP = do
  skipWhile isSpace
  char '#'
  many' (B.satisfy (not . isEndOfLine))
  endOfLine

commentSP :: StateT (MObject a i) Parser ()
commentSP = lift $ commentP

-- A vertex can have three or four floats, representing a 3
-- dimensional point or homogeneous point respectively
-- <ws>*v(<ws>+<num>).3[<ws>+<num>]<ws>*<eol>
vertexP :: Fractional a => Parser (Vertex a)
vertexP = do
  skipWS >> char 'v'
  xs <- fmap realToFrac <$> count 3 (skipWS >> scientific)
  h  <- fmap realToFrac <$> option Nothing (Just <$> (skipWS >> scientific))
  skipWS >> endOfLine
  return (vertexFrom xs h)

  where
    vertexFrom (x:y:z:_) Nothing  = Vertex x y z
    vertexFrom (x:y:z:_) (Just h) = HVertex x y z h
    vertexFrom _       _        = error "Incomplete vertex information"

vertexSP :: (Fractional a, Integral i) => StateT (MObject a i) Parser ()
vertexSP = do
  s <- get
  v <- lift $ vertexP
  put $ addVertex s v

  where
    addVertex (

-- Fae may contains at least 3 indices (more would make more facets
-- with three pairs each)
faceP :: Integral i => Parser (Face i)
faceP = do
  fs <- count 3 (skipWS >> faceIndexP)
  fs' <- many' faceIndexP
  return $ Face (fs ++ fs') [] []

  where
    faceIndexP = signed decimal



--vertexP =

-- faceP

module Utils

where

import Data.Text as T (Text, pack, unpack)
import qualified Data.Text as T


data Tree a = Node a | Leaf (Tree a) (Tree a) deriving (Show)

-- 例: あたり あたり あたる 動詞 * 子音動詞ラ行 基本連用形
data Morpheme = Morpheme Text Text Text Text Text Text Text deriving (Show)
getPOS :: Morpheme -> Text
getPOS (Morpheme _ _ _ s3 _ _ _) = s3

getMrph :: Morpheme -> Text
getMrph (Morpheme s0 _ _ _ _ _ _) = s0

data Chunk = Chunk ChunkID ModifieeID [Morpheme] deriving (Show)


type ChunkID = Int
type ModifieeID = Int

data ChunkTree = ChunkTree ChunkID ModifieeID (Tree Morpheme) deriving (Show)
getModifieeID (ChunkTree cID mID t) = mID

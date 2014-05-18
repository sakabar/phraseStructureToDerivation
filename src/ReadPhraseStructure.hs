import Control.Applicative
import Data.List
import Data.List.Split
import Data.Char(isDigit)
import Data.Text as T (Text, pack, unpack)
import qualified Data.Text as T
import qualified Data.Text.IO as Tio

type ChunkID = Int
type ModifieeID = Int
-- 例: あたり あたり あたる 動詞 * 子音動詞ラ行 基本連用形
data Morpheme = Morpheme Text Text Text Text Text Text Text deriving (Show)
getPOS :: Morpheme -> Text
getPOS (Morpheme s0 s1 s2 s3 s4 s5 s6) = s3

data Chunk = Chunk ChunkID ModifieeID [Morpheme] deriving (Show)
type KyotoCorpusLine = Text

-- 京大コーパスの形態素の行をMorphemeに変換する
makeMorpheme :: KyotoCorpusLine -> Morpheme
makeMorpheme l = Morpheme (lst !! 0) (lst !! 1) (lst !! 2) (lst !! 3) (lst !! 4) (lst !! 5) (lst !! 6)
  where lst = T.words l

-- 1つの文についての京大コーパスの行をチャンクのリストに変換する
-- EOSは引数から除外してある必要がある
makeChunkList :: [KyotoCorpusLine] -> [Chunk]
makeChunkList [] = []
makeChunkList (x:xs) | T.isPrefixOf (pack "*") x = (Chunk chunkID modifieeID (map makeMorpheme mrphs)) : (makeChunkList others)
                     | otherwise          = undefined -- 起こりえない
  where wordLst = T.words x
        chunkID :: Int
        chunkID = read $ unpack $ (wordLst !! 1) :: Int
        modifieeID :: Int
        modifieeID = read $ takeWhile (\ch -> ch /= 'D' && ch /= 'P' && ch /= 'I' && ch /= 'A') (unpack (wordLst !! 2)) :: Int
        mrphs  = takeWhile (\l -> not $ T.isPrefixOf (pack "*") l) xs
        others = dropWhile (\l -> not $ T.isPrefixOf (pack "*") l) xs


data Tree a = Node a | Leaf (Tree a) (Tree a) deriving (Show)
data ChunkTree = ChunkTree ChunkID ModifieeID (Tree Morpheme) deriving (Show)
getModifieeID (ChunkTree cID mID t) = mID

convertChunkToTree :: Chunk -> ChunkTree
convertChunkToTree (Chunk chunkID modifieeID []) = undefined -- 起こりえない
convertChunkToTree (Chunk chunkID modifieeID mrphs@(x:xs))
  | any (\mrph -> getPOS mrph == (pack "動詞")) mrphs = ChunkTree chunkID modifieeID morphemeTree
  | otherwise = ChunkTree chunkID modifieeID ansTree
    where morphemeTree = foldl' (\ans m -> Leaf ans (Node m)) (Node x) xs

          pre         = takeWhile (\mrph -> getPOS mrph /= (pack "特殊")) mrphs
          pre_tail    = dropWhile (\mrph -> getPOS mrph /= (pack "特殊")) mrphs
          nounPhrases = takeWhile (\mrph -> getPOS mrph /= (pack "助詞") && getPOS mrph /= (pack "特殊")) pre_tail
          suf         = dropWhile (\mrph -> getPOS mrph /= (pack "助詞") && getPOS mrph /= (pack "特殊")) pre_tail

          npTreeList = if (null nounPhrases) then [] else [foldr (\np ans -> Leaf (Node np) ans) (Node (last nounPhrases)) (init nounPhrases)]
          ansTree = concatTreeListL $ (map Node pre) ++ npTreeList ++ (map Node suf)

-- 木のリストをfold-leftでたたむ
concatTreeListL :: [Tree a] -> Tree a
concatTreeListL [] = undefined -- 想定してない
concatTreeListL (x:xs) = foldl' (\ans tr -> Leaf ans tr) x xs


-- 末尾の形態素にかかっていない形態素をグループにまとめる
-- [3D 2D 3D -1D] -> [[3D], [2D, 3D], [-1D]]
groupTree :: [ChunkTree] -> [ChunkTree] -> [[ChunkTree]]
groupTree treeLst [] = []
groupTree treeLst [x] = [[x]]
groupTree treeLst lst@(x@(ChunkTree chunkID modifieeID tree):xs)
  | (modifieeID == (-1) && modifieeID == (length treeLst -1)) = [x] : groupTree treeLst xs
  | otherwise = s1 : groupTree treeLst s2
  where hoge = takeWhile (\chTree -> (getModifieeID chTree) /= (length lst)-1) lst
        bar  = dropWhile (\chTree -> (getModifieeID chTree) /= (length lst)-1) lst
        s1   = if (null bar) then hoge else hoge ++ [head bar]
        s2   = if (null bar) then []   else tail bar

makeTreeR :: [a] -> Tree a
makeTreeR [] = undefined -- 想定してない
makeTreeR lst = foldr (\n ans -> Leaf (Node n) ans) (Node $ last lst) (init lst)

makeTreeL :: [a] -> Tree a
makeTreeL [] = undefined -- 想定してない
makeTreeL (x:xs) = foldl' (\ans n -> Leaf ans (Node n)) (Node x) xs


main = do
  f' <- splitOn "EOS\n" <$> getContents
  let f = map lines f'
      stringChunkList = map (\cpslines -> makeChunkList $ map pack cpslines) f
      chunkList = stringChunkList !! 0
      chunkTree = (map convertChunkToTree chunkList)
      ans = makeTreeR $ map makeTreeL $ (groupTree chunkTree chunkTree)
  print ans



-- outputMTree :: Tree Morpheme -> IO()
-- outputMTree

-- outputCTree :: Tree ChunkTree -> IO()
-- outputCTree (Node (ChunkTree a b mrph)) = mapM
-- outputCTree (Leaf a b) = (outputTree a) >> (outputTree b)

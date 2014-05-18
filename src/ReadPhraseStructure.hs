import Control.Applicative
import Data.List
import Data.List.Split
import Data.Char(isDigit)

type ChunkID = Int
type ModifieeID = Int
-- 例: あたり あたり あたる 動詞 * 子音動詞ラ行 基本連用形
data Morpheme = Morpheme String String String String String String String deriving (Show)
getPOS :: Morpheme -> String
getPOS (Morpheme s0 s1 s2 s3 s4 s5 s6) = s3

data Chunk = Chunk ChunkID ModifieeID [Morpheme] deriving (Show)
type KyotoCorpusLine = String

-- 京大コーパスの形態素の行をMorphemeに変換する
makeMorpheme :: KyotoCorpusLine -> Morpheme
makeMorpheme l = Morpheme (lst !! 0) (lst !! 1) (lst !! 2) (lst !! 3) (lst !! 4) (lst !! 5) (lst !! 6)
  where lst = words l

-- 1つの文についての京大コーパスの行をチャンクのリストに変換する
-- EOSは引数から除外してある必要がある
makeChunkList :: [KyotoCorpusLine] -> [Chunk]
makeChunkList [] = []
makeChunkList (x:xs) | "*" `isPrefixOf` x = (Chunk chunkID modifieeID (map makeMorpheme mrphs)) : (makeChunkList others)
                     | otherwise          = undefined -- 起こりえない
  where wordLst = words x
        chunkID :: Int
        chunkID = read (wordLst !! 1) :: Int
        modifieeID :: Int
        modifieeID = read $ takeWhile (\ch -> ch /= 'D' && ch /= 'P' && ch /= 'I' && ch /= 'A') (wordLst !! 2) :: Int
        mrphs  = takeWhile (\l -> not $ "*" `isPrefixOf` l) xs
        others = dropWhile (\l -> not $ "*" `isPrefixOf` l) xs


data Tree a = Node a | Leaf (Tree a) (Tree a) deriving (Show)
data ChunkTree = ChunkTree ChunkID ModifieeID (Tree Morpheme) deriving (Show)

convertChunkToTree :: Chunk -> ChunkTree
convertChunkToTree (Chunk chunkID modifieeID []) = undefined -- 起こりえない
convertChunkToTree (Chunk chunkID modifieeID mrphs@(x:xs))
  | any (\mrph -> getPOS mrph == "動詞") mrphs = ChunkTree chunkID modifieeID morphemeTree
  | otherwise = ChunkTree chunkID modifieeID ansTree
    where morphemeTree = foldl' (\ans m -> Leaf ans (Node m)) (Node x) xs

          pre         = takeWhile (\mrph -> getPOS mrph /= "特殊") mrphs
          pre_tail    = dropWhile (\mrph -> getPOS mrph /= "特殊") mrphs
          nounPhrases = takeWhile (\mrph -> getPOS mrph /= "助詞" && getPOS mrph /= "特殊") pre_tail
          suf         = dropWhile (\mrph -> getPOS mrph /= "助詞" && getPOS mrph /= "特殊") pre_tail

          npTreeList = if (null nounPhrases) then [] else [foldr (\np ans -> Leaf (Node np) ans) (Node (last nounPhrases)) (init nounPhrases)]
          ansTree = concatTreeListL $ (map Node pre) ++ npTreeList ++ (map Node suf)

-- 木のリストをfold-leftでたたむ
concatTreeListL :: [Tree a] -> Tree a
concatTreeListL [] = undefined -- 想定してない
concatTreeListL (x:xs) = foldl' (\ans tr -> Leaf ans tr) x xs

main = do
  f' <- splitOn "EOS\n" <$> getContents
  let f = map lines f'
      stringChunkList = map makeChunkList f
      chunkList = stringChunkList !! 0
      tree = (map convertChunkToTree chunkList)
  mapM_ print tree

import Control.Applicative
import Data.List
import Data.List.Split
import Data.Text as T (Text, pack, unpack)
import qualified Data.Text as T
import qualified Data.Text.IO as Tio

type ChunkID = Int
type ModifieeID = Int
-- 例: あたり あたり あたる 動詞 * 子音動詞ラ行 基本連用形
data Morpheme = Morpheme Text Text Text Text Text Text Text deriving (Show)
getPOS :: Morpheme -> Text
getPOS (Morpheme _ _ _ s3 _ _ _) = s3

getMrph :: Morpheme -> Text
getMrph (Morpheme s0 _ _ _ _ _ _) = s0

data Chunk = Chunk ChunkID ModifieeID [Morpheme] deriving (Show)
type KyotoCorpusLine = Text

-- 京大コーパスの形態素の行をMorphemeに変換する
makeMorpheme :: KyotoCorpusLine -> Morpheme
makeMorpheme l | (length lst) == 7 = Morpheme (lst !! 0) (lst !! 1) (lst !! 2) (lst !! 3) (lst !! 4) (lst !! 5) (lst !! 6)
               | otherwise = undefined -- 想定していない
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
groupTree :: [ChunkTree] -> [[ChunkTree]]
groupTree lst | length lst >= 1 = groupTree' (length lst) lst
              | otherwise = undefined -- 想定してない
  where
    groupTree' :: Int -> [ChunkTree] -> [[ChunkTree]]
    groupTree' _ [] = []
    groupTree' _ [x] = [[x]]
    groupTree' len lst@(x@(ChunkTree chunkID modifieeID tree):xs)
      | (modifieeID == (-1) && modifieeID == (len-1)) = [x] : groupTree' len xs
      | otherwise = s1 : groupTree' len s2
        where
          hoge = takeWhile (\chTree -> (getModifieeID chTree) /= (len-1)) lst
          bar  = dropWhile (\chTree -> (getModifieeID chTree) /= (len-1)) lst
          s1   = if (null bar) then hoge else hoge ++ [head bar]
          s2   = if (null bar) then []   else tail bar

makeTreeL :: [a] -> Tree a
makeTreeL [] = undefined -- 想定してない
makeTreeL (x:xs) = foldl' (\ans n -> Leaf ans (Node n)) (Node x) xs

main = do
  f' <- splitOn "EOS\n" <$> getContents
  let f = map lines f'
      stringChunkList = map (\cpslines -> makeChunkList $ map pack cpslines) f
      chunkList = stringChunkList !! 0
      chunkTree = (map convertChunkToTree chunkList)
      ans = foldr1 (\t ans -> Leaf t ans) $ map makeTreeL $ (groupTree chunkTree)
  outputCTreeInd ans
  putStrLn ""

  -- outputAsDerivation ans

-- チャンクツリーを出力する
outputCTree :: Tree ChunkTree -> IO()
outputCTree (Node (ChunkTree _ _ mrphTree)) = do
  putStr "(" >> Tio.putStr (showMTree mrphTree) >> putStr ")"
outputCTree (Leaf a b) = do
  putStr "(L " >> (outputCTree a) >> (outputCTree b) >> putStr ")"


-- レベルを考慮してインデントして出力
outputCTreeInd :: Tree ChunkTree -> IO()
outputCTreeInd t = outputCTreeInd' 0 t
  where
    outputCTreeInd' :: Int -> Tree ChunkTree -> IO()
    outputCTreeInd' d (Node (ChunkTree _ _ mrphTree)) = do
      putStr $ take d (repeat ' ')
      Tio.putStr (showMTree mrphTree) >> putStrLn ""
    outputCTreeInd' d (Leaf a b) = do
      (putStr $ take d (repeat ' ')) >> putStrLn "(L"
      outputCTreeInd' (d+1) a
      outputCTreeInd' (d+1) b
      (putStr $ take d (repeat ' ')) >> putStrLn ")"


showMTree :: Tree Morpheme -> Text
showMTree (Node mrph) = (getMrph mrph)
showMTree (Leaf t1 t2) = foldl1 T.append [(showMTree t1), (pack " "), (showMTree t2)]


-- f :: Tree ChunkTree -> IO()
-- f tr = do
--   let lvCt = (lvCTree tr)
--   let maxDepth = maximum $ map (\(t,d) -> d) lvCt
--       change d [] = pack ""
--       change d (x@(t',d'):xs) | d' == d   = T.append t' (change d xs)
--                               | d' == d+1 = T.append (pack $ take (getMultiByteLength t') (repeat '-')) (change d xs)
--                               | otherwise = T.append (pack $ take (getMultiByteLength t') (repeat ' ')) (change d xs)

--   mapM_ Tio.putStrLn $ map (\d -> change d lvCt) (reverse [0..maxDepth])

-- getMultiByteLength :: Text -> Int
-- getMultiByteLength t | T.length t == 0 = 0
--             | T.head t == ' ' = 1 + getMultiByteLength (T.tail t)
--             | otherwise = 2 + getMultiByteLength (T.tail t)

-- -- レベルがdと一致している文字列だけ残して他は空白に
-- changeToSpace :: Int -> [(Text, Int)] -> [Text]
-- changeToSpace d lst = map (\(t',d') -> if d == d' then t' else pack (take (getMultiByteLength t') (repeat ' '))) lst

-- -- レベルがd以上の文字列を----にして残して他は空白に
-- changeToHyphen :: Int -> [(Text, Int)] -> [Text]
-- changeToHyphen d lst = map (\(t',d') -> if d' >= d then pack (take (getMultiByteLength t') (repeat '-')) else pack (take (getMultiByteLength t') (repeat ' '))) lst

-- -- レベルdとレベルe(なんでもよい)の間は空ける
-- -- そのレベルのチャンクの後に空白を入れるのかどうかのboolのリストを返す
-- getBoolArray :: Int -> [(Text, Int)] -> [Bool]
-- getBoolArray d lst = map (\(t', d') -> d == d') lst

-- output_LvDchunks :: Int -> [(Text, Int)] -> IO()
-- output_LvDchunks d lvCt = do
--   Tio.putStrLn $ T.concat $ changeToSpace d lvCt
--   Tio.putStrLn $ T.concat $ zipWith (\bool text -> if bool then T.append text (pack " ") else text) (getBoolArray d lvCt) $ changeToHyphen d lvCt

-- -- CCGの導出木のような形式で出力する
-- outputAsDerivation :: Tree ChunkTree -> IO()
-- outputAsDerivation tr = do
--   let lvCt = lvCTree tr
--       maxDepth = maximum $ map (\(t,d) -> d) lvCt

--   mapM_ (\d -> output_LvDchunks d lvCt) (reverse [0..maxDepth])

-- lvCTree :: Tree ChunkTree -> [(Text, Int)]
-- lvCTree t = lvCTree' 0 t
--   where
--     lvCTree' :: Int -> Tree ChunkTree -> [(Text, Int)]
--     lvCTree' d (Node (ChunkTree _ _ mrphTree)) = [(showMTree mrphTree, d)]
--     lvCTree' d (Leaf a b) = (lvCTree' (d+1) a) ++ (lvCTree' (d+1) b)

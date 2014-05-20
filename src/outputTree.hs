module OutputTree
(
  outputCTree,
  outputCTreeInd,
  -- outputAsDerivation
)
where
import Data.Text as T (Text, pack, unpack)
import qualified Data.Text as T
import qualified Data.Text.IO as Tio

import Utils


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





-- -- CCGの導出木のような形式で出力する
-- outputAsDerivation :: Tree ChunkTree -> IO()
-- outputAsDerivation tr = do
--   let lvCt = getLVandTextPair tr
--       maxDepth = maximum $ map (\(t,d) -> d) lvCt
--   mapM_ (\d -> output_LvDchunks d lvCt) (reverse [0..maxDepth])

-- getLVandTextPair :: Tree ChunkTree -> [(Text, Int)]
-- getLVandTextPair t = getLVandTextPair' 0 t
--   where
--     getLVandTextPair' :: Int -> Tree ChunkTree -> [(Text, Int)]
--     getLVandTextPair' d (Node (ChunkTree _ _ mrphTree)) = [(showMTree mrphTree, d)]
--     getLVandTextPair' d (Leaf a b) = (getLVandTextPair' (d+1) a) ++ (getLVandTextPair' (d+1) b)

-- -- レベルがdのチャンクだけ出力する?
-- output_LvDchunks :: Int -> [(Text, Int)] -> IO()
-- output_LvDchunks d lvCt = do
--   Tio.putStrLn $ T.concat $ changeToSpace d lvCt
--   Tio.putStrLn $ T.concat $ zipWith (\bool text -> if bool then T.append text (pack " ") else text) (getBoolArray d lvCt) $ changeToHyphen d lvCt

-- -- レベルがdと一致している文字列だけ残して他は空白に
-- changeToSpace :: Int -> [(Text, Int)] -> [Text]
-- changeToSpace d lst = map (\(t',d') -> if d == d' then t' else pack (take (getMultiByteLength t') (repeat ' '))) lst

-- -- レベルdとレベルe(なんでもよい)の間は空ける
-- -- そのレベルのチャンクの後に空白を入れるのかどうかのboolのリストを返す
-- getBoolArray :: Int -> [(Text, Int)] -> [Bool]
-- getBoolArray d lst = map (\(t', d') -> d == d') lst

-- -- レベルがd以上の文字列を----にして残して他は空白に
-- changeToHyphen :: Int -> [(Text, Int)] -> [Text]
-- changeToHyphen d lst = map (\(t',d') -> if d' >= d then pack (take (getMultiByteLength t') (repeat '-')) else pack (take (getMultiByteLength t') (repeat ' '))) lst


import Control.Applicative
import Data.List
import Data.List.Split
import Data.Char(isDigit)

type ChunkID = Int
type ModifieeID = Int
data Morpheme = Morpheme String String String String String String String deriving (Show)
data Chunk = Chunk ChunkID ModifieeID [Morpheme] deriving (Show)

type KyotoCorpusLine = String

makeMorpheme :: KyotoCorpusLine -> Morpheme
makeMorpheme l = Morpheme (lst !! 0) (lst !! 1) (lst !! 2) (lst !! 3) (lst !! 4) (lst !! 5) (lst !! 6)
  where lst = words l

makeChunkTree :: [KyotoCorpusLine] -> [Chunk]
makeChunkTree [] = []
makeChunkTree (x:xs) | "*" `isPrefixOf` x = (Chunk chunkID modifieeID (map makeMorpheme mrphs)) : (makeChunkTree others)
                     | otherwise          = [] -- 起こりえない
  where wordLst = words x
        chunkID :: Int
        chunkID = read (wordLst !! 1) :: Int
        modifieeID :: Int
        modifieeID = read $ takeWhile (\ch -> ch /= 'D' && ch /= 'P' && ch /= 'I' && ch /= 'A') (wordLst !! 2) :: Int
        mrphs  = takeWhile (\l -> not $ "*" `isPrefixOf` l) xs
        others = dropWhile (\l -> not $ "*" `isPrefixOf` l) xs

main = do
  f' <- splitOn "EOS\n" <$> getContents
  let f = map lines f'
      s1 = f !! 0
      chunkTrees = map makeChunkTree f
      chunk = chunkTrees !! 0
  mapM_ print chunk

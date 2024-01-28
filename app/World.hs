module World where
import qualified Dir
import Dir (Direction)
import Data.Functor ((<&>))
import Pipes
import Data.Maybe (fromMaybe)

data Block = Air | Dirt | Stone
type Chunk = [[[Block]]]

genBlock :: Int -> Int -> Int -> Block
genBlock x y z
    | y < x + z  = Stone
    | y == x + z = Dirt
    | otherwise  = Air

genChunk :: Chunk
genChunk =
    [0..15] <&> \x ->
    [0..255] <&> \y ->
    [0..15] <&> \z ->
        genBlock x y z

getBlockSafe :: Int -> Int -> Int -> Chunk -> Maybe Block
getBlockSafe x y z chunk = chunk !? x >>= (!? y) >>= (!? z)

getBlockOrDefault :: Int -> Int -> Int -> Block -> Chunk -> Block
getBlockOrDefault x y z d chunk = fromMaybe d $ getBlockSafe x y z chunk

getBlock :: Int -> Int -> Int -> Chunk -> Block
getBlock x y z = getBlockOrDefault x y z Air

getBlockInDir :: Int -> Int -> Int -> Direction -> Chunk -> Block
getBlockInDir x y z dir = let
    (x',y',z') = Dir.offset (x,y,z) dir
    in getBlock x' y' z'


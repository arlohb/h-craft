module Chunk where

import Data.Functor ((<&>))
import Data.Maybe (fromMaybe)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Raylib.Types (Vector3(..))
import Raylib.Util.Math ((|*))

import Dir (Direction)
import qualified Dir
import Block (Block(..))
import qualified Block

type Chunk = ((Int,Int), Vector (Vector (Vector Block)))

gen :: Int -> Int -> Chunk
gen cx cz = (
    (cx,cz),
    V.fromList [cx*16..cx*16 + 15]  <&> \x ->
    V.fromList [0..255] <&> \y ->
    V.fromList [cz*16..cz*16 + 15]  <&> \z ->
        Block.gen x y z )

getBlockSafe :: Int -> Int -> Int -> Chunk -> Maybe Block
getBlockSafe x y z (_,chunk) = chunk V.!? x >>= (V.!? y) >>= (V.!? z)

getBlockOrDefault :: Int -> Int -> Int -> Block -> Chunk -> Block
getBlockOrDefault x y z d chunk = fromMaybe d $ getBlockSafe x y z chunk

getBlock :: Int -> Int -> Int -> Chunk -> Block
getBlock x y z = getBlockOrDefault x y z Air

getBlockInDir :: Int -> Int -> Int -> Direction -> Chunk -> Block
getBlockInDir x y z dir = let
    (x',y',z') = Dir.offset (x,y,z) dir
    in getBlock x' y' z'

posOffset :: Chunk -> Vector3
posOffset ((x,z),_) = Vector3 (fromIntegral x) 0 (fromIntegral z) |* 16.0


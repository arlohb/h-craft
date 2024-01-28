module Mesh where

import Pipes
import Raylib.Core.Models
import Raylib.Util
import Raylib.Types
import Data.Functor ((<&>))
import Data.Word (Word16)
import Data.Maybe (isJust, fromJust)
import qualified Dir
import Dir (Direction(..))
import World

data Face = Face {
    face'pos :: !Vector3,
    face'dir :: !Direction,
    face'block :: !Block
}

getFaceBlock :: Block -> Block -> Maybe Block
getFaceBlock Air   Air   = Nothing
getFaceBlock Air   block = Just block
getFaceBlock block Air   = Just block
getFaceBlock _     _     = Nothing

facesInChunk :: Chunk -> [Face]
facesInChunk chunk =
    [ Face {
        face'pos = Vector3 (fromIntegral x) (fromIntegral y) (fromIntegral z),
        face'dir = dir,
        face'block = fromJust faceBlock
    }
    | x <- [0..15]
    , y <- [0..255]
    , z <- [0..15]
    , let block = getBlock x y z chunk
    , dir <- [Px, Py, Pz]
    , let block' = getBlockInDir x y z dir chunk
    , let faceBlock = getFaceBlock block block'
    , isJust faceBlock]

faceVerts :: Direction -> [Vector3]
faceVerts Px = [
    Vector3 1 0 0,
    Vector3 1 1 0,
    Vector3 1 1 1,
    Vector3 1 0 1
    ]
faceVerts Py = [
    Vector3 0 1 0,
    Vector3 0 1 1,
    Vector3 1 1 1,
    Vector3 1 1 0
    ]
faceVerts Pz = [
    Vector3 1 0 1,
    Vector3 1 1 1,
    Vector3 0 1 1,
    Vector3 0 0 1
    ]
faceVerts Nx = [
    Vector3 0 0 1,
    Vector3 0 1 1,
    Vector3 0 1 0,
    Vector3 0 0 0
    ]
faceVerts Ny = [
    Vector3 0 0 0,
    Vector3 0 0 1,
    Vector3 1 0 1,
    Vector3 1 0 0
    ]
faceVerts Nz = [
    Vector3 0 0 0,
    Vector3 0 1 0,
    Vector3 1 1 0,
    Vector3 1 0 0
    ]

faceNormal :: Direction -> Vector3
faceNormal Px = Vector3 1 0 0
faceNormal Py = Vector3 0 1 0
faceNormal Pz = Vector3 0 0 1
faceNormal Nx = Vector3 (-1) 0 0
faceNormal Ny = Vector3 0 (-1) 0
faceNormal Nz = Vector3 0 0 (-1)

faceTris :: [Word16]
faceTris = [0,1,2,0,2,3]

emptyMesh :: Mesh
emptyMesh = Mesh {
    mesh'vertexCount = 0,
    mesh'triangleCount = 0,
    mesh'vertices = [],
    mesh'texcoords = [],
    mesh'texcoords2 = Nothing,
    mesh'normals = [],
    mesh'tangents = Nothing,
    mesh'colors = Nothing,
    mesh'indices = Nothing,
    mesh'animVertices = Nothing,
    mesh'animNormals = Nothing,
    mesh'boneIds = Nothing,
    mesh'boneWeights = Nothing,
    mesh'vaoId = 0,
    mesh'vboId = Nothing
}

cubeMesh :: WindowResources -> IO Mesh
cubeMesh = let
    verts = Dir.all >>= faceVerts
    tris = faceTris |> replicate 6 |> zip [0..] >>= \(i,ts) -> ts <&> (+ (i * 4))
    in uploadMesh (emptyMesh {
        mesh'vertexCount = length verts,
        mesh'triangleCount = length tris `div` 3,
        mesh'vertices = verts,
        mesh'texcoords = replicate (length verts) (Vector2 0 0),
        mesh'normals = Dir.all >>= replicate 4 <&> faceNormal,
        mesh'indices = Just tris
    }) False


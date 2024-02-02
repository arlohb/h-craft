module World where

import Raylib.Types
import Data.Map (Map)
import qualified Data.Map as Map
import Raylib.Util
import Raylib.Core.Models
import Raylib.Util.Math

import Pipes
import Mesh (buildChunk)
import Chunk (Chunk)
import qualified Chunk

type World = Map (Int,Int) (Chunk, Model)

setModelPos :: Vector3 -> Model -> Model
setModelPos Vector3 {
    vector3'x = x,
    vector3'y = y,
    vector3'z = z
} model = model { model'transform = matrixTranslate x y z }

createModel :: WindowResources -> Shader -> Chunk -> IO Model
createModel w shader chunk = do
    mesh <- buildChunk chunk
        |> \m -> uploadMesh m False w
    model <- loadModelFromMesh mesh w
    return $ model
        |> \m -> setMaterialShader m 0 shader
        |> setModelPos (Chunk.posOffset chunk)

addChunk :: WindowResources -> Shader -> Int -> Int -> World -> IO World
addChunk w shader x z world = do
    let chunk = Chunk.gen x z
    model <- createModel w shader chunk
    return $ Map.insert (x,z) (chunk,model) world


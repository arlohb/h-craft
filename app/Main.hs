module Main where
import Raylib.Core
import Raylib.Core.Text
import Raylib.Core.Models
import Raylib.Core.Camera
import Raylib.Util
import Raylib.Util.Colors
import Raylib.Types
import Data.Functor ((<&>))
import System.CPUTime (getCPUTime)
import qualified Data.Map as Map

import Pipes
import qualified Mesh
import World (World)
import qualified World

timeIO :: IO a -> IO a
timeIO fx = do
    start <- getCPUTime
    x <- fx
    end <- getCPUTime
    let ms = (end - start) `div` 1_000_000_000
    putStrLn $ "Chunk mesh time: " ++ show ms ++ " ms"
    return x

data State = State {
    frame :: !Int,
    time :: !Float,
    camera :: !Camera3D,
    world :: !World
}

cubeModel :: WindowResources -> IO Model
cubeModel w = Mesh.cubeMesh w >>= (`loadModelFromMesh` w)

update :: State -> IO State
update state = do
    btn_r <- isMouseButtonDown MouseButtonRight
    camera <- if btn_r
        then updateCamera (camera state) CameraModeFree
        else return $ camera state

    time <- getFrameTime <&> (+ time state)
    return state { frame = frame state + 1, time, camera }

uploadShaderTime :: WindowResources -> Float -> Shader -> IO ()
uploadShaderTime w time shader = setShaderValue shader "time" (ShaderUniformFloat time) w

upload :: WindowResources -> State -> IO ()
upload w State {
    time,
    world
} = world
    |> Map.elems
    ||> snd
    ||> model'materials
    ||> (\mats -> mats
        ||> (uploadShaderTime w time . material'shader)
        |> sequence_)
    |> sequence_

draw :: State -> IO ()
draw State {
    frame,
    camera,
    world
} = drawing $ do
    clearBackground black

    world
        |> Map.elems
        ||> snd
        ||> (\model -> drawModel model (Vector3 0 0 0) 1 white)
        |> sequence_
        |> mode3D camera

    drawFPS 10 10
    drawText (show frame) 30 40 18 lightGray

loop :: WindowResources -> State -> IO State
loop w state = do
    newState <- update state
    upload w state
    draw newState
    return newState

path :: String -> String
path = ("assets/" ++)

initialState :: WindowResources -> IO State
initialState w = do
    shader <- loadShader (Just $ path "vert.glsl") (Just $ path "frag.glsl") w

    world <-
        [ (x,z)
        | x <- [-1..1]
        , z <- [-1..1] ]
        |> foldr
            (\(x,z) worldIO -> worldIO >>= \world -> timeIO $ World.addChunk w shader x z world)
            (return Map.empty)

    return State {
        frame = 0,
        time = 0,
        camera = Camera3D
            (Vector3 5 5 0)
            (Vector3 0 0 0)
            (Vector3 0 1 0)
            70
            CameraPerspective,
        world
    }

main :: IO ()
main = do
    withWindow
        600
        450
        "h-craft"
        60
        $ \w -> initialState w >>= whileWindowOpen_ (loop w)


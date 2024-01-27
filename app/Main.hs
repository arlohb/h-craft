module Main where
import Pipes

import Raylib.Core
import Raylib.Core.Text
import Raylib.Core.Models
import Raylib.Core.Camera
import Raylib.Util
import Raylib.Util.Colors
import Raylib.Types
import Data.Functor ((<&>))

data State = State {
    frame :: !Int,
    time :: !Float,
    camera :: !Camera3D,
    model :: !Model
}

cubeModel :: WindowResources -> IO Model
cubeModel w = genMeshCube 1 1 1 w >>= \cube -> loadModelFromMesh cube w

update :: State -> IO State
update state = do
    camera <- updateCamera (camera state) CameraModeOrbital
    time <- getFrameTime <&> (+ time state)
    return state { frame = frame state + 1, time, camera }

upload :: WindowResources -> State -> IO ()
upload w State {
    frame = _,
    time,
    camera = _,
    model
} = model
    |> model'materials
    |> mapM (\mat ->
        setShaderValue (mat |> material'shader) "time" (ShaderUniformFloat time) w
    ) >> return ()

draw :: State -> IO ()
draw State {
    frame,
    time = _,
    camera,
    model
} = drawing $ do
    clearBackground black

    mode3D camera $ do
        drawModel model (Vector3 0 0 0) 1 white

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
    model <- cubeModel w >>= \model -> return $ setMaterialShader model 0 shader
    return State {
        frame = 0,
        time = 0,
        camera = Camera3D
            (Vector3 5 5 0)
            (Vector3 0 0 0)
            (Vector3 0 1 0)
            70
            CameraPerspective,
        model
    }

main :: IO ()
main = do
    withWindow
        600
        450
        "h-craft"
        60
        $ \w -> initialState w >>= \s -> whileWindowOpen_ (loop w) s


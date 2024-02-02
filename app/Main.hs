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
import qualified Mesh
import World (Chunk, genChunk)
import Mesh (buildChunk)
import Raylib.Util.RLGL (rlDisableBackfaceCulling)
import System.CPUTime (getCPUTime)

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
    model :: !Model,
    chunk :: !Chunk,
    chunkModel :: !Model
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

upload :: WindowResources -> State -> IO ()
upload w State {
    time,
    model
} = model
    |> model'materials
    |> mapM (\mat ->
        setShaderValue (mat |> material'shader) "time" (ShaderUniformFloat time) w
    ) >> return ()

draw :: State -> IO ()
draw State {
    frame,
    camera,
    model,
    chunkModel
} = drawing $ do
    clearBackground black

    mode3D camera $ do
        rlDisableBackfaceCulling
        drawModel model (Vector3 0 0 0) 1 white
        drawModel chunkModel (Vector3 0 0 0) 1 white

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
    let chunk = genChunk
    chunkModel <- timeIO $ uploadMesh (buildChunk chunk) False w
        >>= (`loadModelFromMesh` w)
        >>= \chunkModel -> return $ setMaterialShader chunkModel 0 shader
    return State {
        frame = 0,
        time = 0,
        camera = Camera3D
            (Vector3 5 5 0)
            (Vector3 0 0 0)
            (Vector3 0 1 0)
            70
            CameraPerspective,
        model,
        chunk,
        chunkModel
    }

main :: IO ()
main = do
    withWindow
        600
        450
        "h-craft"
        60
        $ \w -> initialState w >>= whileWindowOpen_ (loop w)


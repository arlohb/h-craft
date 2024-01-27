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
import Data.Word (Word16)

data Direction = Px | Py | Pz | Nx | Ny | Nz
directions :: [Direction]
directions = [Px, Py, Pz, Nx, Ny, Nz]

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
    verts = directions >>= faceVerts
    tris = faceTris |> replicate 6 |> zip [0..] >>= \(i,ts) -> ts <&> (+ (i * 4))
    in uploadMesh (emptyMesh {
        mesh'vertexCount = length verts,
        mesh'triangleCount = length tris `div` 3,
        mesh'vertices = verts,
        mesh'texcoords = replicate (length verts) (Vector2 0 0),
        mesh'normals = directions >>= replicate 4 <&> faceNormal,
        mesh'indices = Just tris
    }) False

data State = State {
    frame :: !Int,
    time :: !Float,
    camera :: !Camera3D,
    model :: !Model
}

cubeModel :: WindowResources -> IO Model
cubeModel w = cubeMesh w >>= (`loadModelFromMesh` w)

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
        $ \w -> initialState w >>= whileWindowOpen_ (loop w)


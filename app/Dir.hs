module Dir where

data Direction = Px | Py | Pz | Nx | Ny | Nz
all :: [Direction]
all = [Px, Py, Pz, Nx, Ny, Nz]

offset :: (Int,Int,Int) -> Direction -> (Int,Int,Int)
offset (x,y,z) Px = (x+1,y,z)
offset (x,y,z) Py = (x,y+1,z)
offset (x,y,z) Pz = (x,y,z+1)
offset (x,y,z) Nx = (x-1,y,z)
offset (x,y,z) Ny = (x,y-1,z)
offset (x,y,z) Nz = (x,y,z-1)


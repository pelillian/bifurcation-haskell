import Graphics.UI.GLUT

width, height :: GLint
width = 1920
height = 1080

iters = 1000
points = 20

logisticMap :: GLfloat -> GLfloat -> GLfloat
logisticMap r x = r * x * (1 - x)

iterMap :: Int -> GLfloat -> [GLfloat]
iterMap n r = drop (n - points) $ take n $ iterate (logisticMap r) 1e-4

range :: [GLfloat]
range = [1,1.0001..4]

x = concatMap (replicate points) range
y = concatMap (iterMap iters) range

myPoints :: [(GLfloat,GLfloat,GLfloat)]
myPoints = zip3 (map (* 0.5) $ map (subtract 2.5) x) (map (subtract 0.5) y) (cycle [0])

main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize
  initialWindowSize  $= Size width height
  _window <- createWindow "bifurcation"
  displayCallback $= display
  mainLoop

display :: DisplayCallback
display = do
  clear [ColorBuffer]
  renderPrimitive Points $
     mapM_ (\(x, y, z) -> vertex $ Vertex3 x y z) myPoints
  flush

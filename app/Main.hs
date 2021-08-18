import Graphics.UI.GLUT
import System.Exit

width, height :: GLint
width = 1920
height = 1080

logisticMap :: GLfloat -> GLfloat -> GLfloat
logisticMap r x = r * x * (1 - x)

iterMap :: Int -> GLfloat -> GLfloat
iterMap n r = (iterate (logisticMap r) (1e-5 * r)) !! n

myPoints :: [(GLfloat,GLfloat,GLfloat)]
--myPoints = [ (iterate logisticMap r !! 1000, r, 0) | r <- [0,0.001..4] ]
myPoints = [ ((r - 2.5) / 1.8, (iterMap 1000 r) - 0.5, 0) | r <- [1,1.00001..4] ]

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

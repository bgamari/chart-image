import Data.Accessor                
import Graphics.Rendering.Chart.Plot.Image
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Gtk

import Data.Array.Repa ((:.)(..))
import qualified Data.Array.Repa as A

image :: A.Array A.U A.DIM2 (Double,Double,Double)
image = A.computeS $ A.fromFunction (A.ix2 500 500)
        $ \(A.Z :. x :. y)->(realToFrac y / 500, realToFrac x / 500, realToFrac (x+y)/1000)

chart :: Layout1 Double Double
chart = layout
  where layout = layout1_title ^= "Hello World"
               $ layout1_plots ^= [Left (toPlot hi)]
               $ defaultLayout1
        hi = image_data ^= image
           $ image_extent ^= ((0,0), (2,2))
           $ defaultImage
main = renderableToWindow (toRenderable chart) 640 480
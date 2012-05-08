{-# LANGUAGE FlexibleContexts, FlexibleInstances, TemplateHaskell, RankNTypes #-}

module Graphics.Rendering.Chart.Plot.Image ( Image
                                           , defaultImage
                                           , image_data
                                           , image_extent
                                           ) where

import qualified Graphics.Rendering.Cairo as C

import           Graphics.Rendering.Chart.Types
import           Graphics.Rendering.Chart.Plot.Types
import           Graphics.Rendering.Chart.Axis

import           Data.Word
import           Data.Bits
import           Data.Array.Repa ((:.)(..))
import qualified Data.Array.Repa as A
import           Data.Array.Repa.Eval (Fillable, fromList)
import qualified Data.Array.MArray as DA

import           Data.Accessor.Template

import           Control.Monad

data Image r e x y = Image { image_data_    :: A.Array r A.DIM2 e
                           , image_extent_  :: ((x,y), (x,y))
                           }
     
defaultImage :: (Fillable r e, PlotValue x, PlotValue y) => Image r e x y
defaultImage = Image
  { image_data_    = fromList (A.Z :. 0 :. 0) []
  , image_extent_  = ((fromValue 0, fromValue 0), (fromValue 1, fromValue 1))
  }
  
instance (ImageData e, A.Repr r e) => ToPlot (Image r e) where
  toPlot p = Plot { plot_render_     = renderImage p
                  , plot_legend_     = []
                  , plot_all_points_ = let ((x0,y0), (x1,y1)) = image_extent_ p
                                       in ([x0, x1], [y0, y1])
                  }
                  
class ImageData e where
  copyArrayToSurface :: A.Repr r e => A.Array r A.DIM2 e -> IO C.Surface
  
instance ImageData (Double,Double,Double) where
  copyArrayToSurface a = do
    let A.Z :. w :. h = A.extent a
    surf <- C.createImageSurface C.FormatRGB24 w h
    stride <- C.imageSurfaceGetStride surf
    d <- C.imageSurfaceGetPixels surf :: IO (C.SurfaceData Int Word32)
    forM_ [0..w-1] $ \x->
      forM_ [0..h-1] $ \y->do
        let (r,g,b) = a A.! (A.ix2 x y)
        DA.writeArray d (x+y*(stride `div` 4)) $ round (255*r) `shiftL` 16
                                   .|. round (255*g) `shiftL` 8
                                   .|. round (255*b)
    C.surfaceMarkDirty surf
    return surf

vflip :: A.Repr r e => A.Array r A.DIM2 e -> A.Array A.D A.DIM2 e
vflip a = A.backpermute (A.extent a) (\(A.Z :. x :. y)->A.ix2 x (h-1-y)) a
  where A.Z :. w :. h = A.extent a

renderImage  :: (A.Repr r e, ImageData e) => Image r e x y -> PointMapFn x y -> CRender ()
renderImage p pmap = preserveCState $ c $ do
    surf <- C.liftIO $ copyArrayToSurface $ vflip $ image_data_ p
    C.setSourceSurface surf 0 0
    let ((x0,y0), (x1,y1)) = image_extent_ p
        Point x0' y0' = pmap (LValue x0, LValue y0)
        Point x1' y1' = pmap (LValue x1, LValue y1)
    C.rectangle x0' y0' (x1'-x0') (y1'-y0')
    C.fill
  
$( deriveAccessors ''Image )


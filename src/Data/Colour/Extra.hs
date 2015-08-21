module Data.Colour.Extra where
import Data.Colour.RGBSpace
import Data.Colour.RGBSpace.HSL
import Graphics.GL
import Linear

hslColor :: GLfloat -> GLfloat -> GLfloat -> GLfloat -> V4 GLfloat
hslColor h s l a = (uncurryRGB V4 $ hsl (h * 360) s l) a

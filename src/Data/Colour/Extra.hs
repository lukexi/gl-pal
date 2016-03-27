module Data.Colour.Extra where
import Data.Colour.RGBSpace
import Data.Colour.RGBSpace.HSL
import Graphics.GL
import Linear

hslaColor :: GLfloat -> GLfloat -> GLfloat -> GLfloat -> V4 GLfloat
hslaColor h s l a = (uncurryRGB V4 $ hsl (h * 360) s l) a

hslColor :: GLfloat -> GLfloat -> GLfloat -> V4 GLfloat
hslColor h s l = hslaColor h s l 1 
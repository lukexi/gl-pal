module Data.Colour.Extra where
import Data.Colour.RGBSpace
import Data.Colour.RGBSpace.HSL
import Graphics.GL
import Linear



colorRGBA :: GLfloat -> GLfloat -> GLfloat -> GLFloat -> V4 GLfloat
colorRGBA = V4

colorHSLA :: GLfloat -> GLfloat -> GLfloat -> GLfloat -> V4 GLfloat
colorHSLA h s l = uncurryRGB V4 (hsl (h * 360) s l)

colorHSL :: GLfloat -> GLfloat -> GLfloat -> V4 GLfloat
colorHSL h s l = colorHSLA h s l 1

colorRGB :: GLfloat -> GLfloat -> GLfloat -> V4 GLfloat
colorRGB r g b = colorRGBA r g b 1

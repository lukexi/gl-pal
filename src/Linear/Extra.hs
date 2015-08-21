module Linear.Extra where
import Linear
import Graphics.GL

scaleMatrix :: V3 GLfloat -> M44 GLfloat
scaleMatrix (V3 x y z) = V4 (V4 x 0 0 0)
                            (V4 0 y 0 0)
                            (V4 0 0 z 0)
                            (V4 0 0 0 1)

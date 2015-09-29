{-# LANGUAGE RecordWildCards #-}

module Graphics.GL.Pal.Geometries.Icosahedron where

import Graphics.GL

import Graphics.GL.Pal.Types
import Graphics.GL.Pal.Geometry

import Linear       hiding ( trace   )
import Control.Lens hiding ( indices )
import Data.Foldable
import Debug.Trace

--fI :: ( Integral a , Num b ) => a -> b
--fI = fromIntegral

icosahedronData :: GLfloat  -> Int -> GeometryData
icosahedronData size subdivisions = GeometryData{..}

  where

    t  = ( 1 + sqrt 5  ) / 2

    -- The base Array of vertices
    vertList      = [ V3  (-1)   t   0 
                    , V3   1   t   0
                    , V3  (-1)  (-t)   0
                    , V3   1  (-t)   0
                    , V3   0  (-1)   t
                    , V3   0   1   t
                    , V3   0  (-1)  (-t)
                    , V3   0   1  (-t)
                    , V3   t   0  (-1)
                    , V3   t   0   1
                    , V3  (-t)   0  (-1)
                    , V3  (-t)   0   1
                    ]

    indiceList    = [ V3   0  11   5 
                    , V3   0   5   1
                    , V3   0   1   7
                    , V3   0   7  10
                    , V3   0  10  11
                    , V3   1   5   9
                    , V3   5  11   4
                    , V3  11  10   2
                    , V3  10   7   6
                    , V3   7   1   8
                    , V3   3   9   4
                    , V3   3   4   2
                    , V3   3   2   6
                    , V3   3   6   8
                    , V3   3   8   9
                    , V3   4   9   5
                    , V3   2   4  11
                    , V3   6   2  10
                    , V3   8   6   7
                    , V3   9   8   1
                    ]



    numVerts  = 3 * fromIntegral ( length indiceList ) 
    numPoints = 3 * fromIntegral ( length vertList )

    posArray      = makeIcosahedronPositions size vertList
    uvArray       = makeIcosahedronUVs vertList
    indexArray    = makeIcosahedronIndicies  indiceList 
    normalArray   = makeIcosahedronNormals  vertList
    tanArray      = makeIcosahedronTangents vertList



    positionList  = posArray
    normalList    = normalArray
    
    tangentList   = tanArray
    uvList        = uvArray

    indexList     = indexArray  


makeIcosahedronPositions :: GLfloat -> [V3 GLfloat] -> [ GLfloat ]
makeIcosahedronPositions size vertList  = concatMap toList ( map ( realToFrac size * ) vertList )

makeIcosahedronIndicies indexList       = concatMap toList indexList
makeIcosahedronNormals positionList     = concatMap toList ( map normalize positionList ) 

makeIcosahedronUVs :: [V3 GLfloat] -> [ GLfloat ]
makeIcosahedronUVs     positionList     = uvs

    where 
        uvs = concat[ getUV p | p <- [0 .. (length positionList )]  ]
        getUV p = [ 0 , 0 ] 


makeIcosahedronTangents :: [V3 GLfloat] -> [ GLfloat ]
makeIcosahedronTangents positionList = tangents
    where 
        tangents = concat[ getTangent p | p <- [0 .. (length positionList )] ]
        getTangent p = [ 0 ,  0 , 0 ]


icosahedronGeometry :: GLfloat -> Int -> IO Geometry   
icosahedronGeometry size subdivisions = geometryFromData $ icosahedronData size subdivisions 

{-

makePlaneUVs :: Int -> Int -> [ GLfloat ]
makePlaneUVs subdivisionsX subdivisionsY = uvs
  where

    uvs = concat [ getPoint x  y | x <- [ 0 .. subdivisionsX ] , y <- [ 0 .. subdivisionsY ] ]
    getPoint x y = toList p
      where 
        p = V2 ( fI x  / fI subdivisionsX )  ( fI y / fI subdivisionsY )
-}
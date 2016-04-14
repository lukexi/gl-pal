module Graphics.GL.Pal.Geometries.Shared where
import Graphics.GL

import Graphics.GL.Pal.Types

import Linear


mergeGeometries :: [GeometryData] -> GeometryData
mergeGeometries geoDatas =
    let getIndexCount = fromIntegral . length . gdPositions
        offsets = scanl (+) 0 (map getIndexCount geoDatas)

        shifted = map (uncurry shiftIndices) (zip offsets geoDatas)

    in GeometryData 
        { gdPositions = concatMap gdPositions shifted
        , gdIndices   = concatMap gdIndices   shifted
        , gdUVs       = concatMap gdUVs       shifted
        , gdNormals   = concatMap gdNormals   shifted
        , gdTangents  = concatMap gdTangents  shifted
        }


shiftPoints :: V3 GLfloat -> GeometryData -> GeometryData
shiftPoints shift geoData = geoData { gdPositions = newPositions }
  where
    newPositions = map (+ shift) (gdPositions geoData) 

shiftIndices :: GLsizei -> GeometryData -> GeometryData
shiftIndices startIndex geoData = geoData { gdIndices = map (+ (fromIntegral startIndex)) (gdIndices geoData) }


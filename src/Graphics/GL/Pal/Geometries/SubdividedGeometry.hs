{-# LANGUAGE RecordWildCards #-}

module Graphics.GL.Pal.Geometries.SubdividedGeometry where

import Graphics.GL

import Graphics.GL.Pal.Types
import Graphics.GL.Pal.Geometry

import Linear       hiding ( trace   )
import Control.Lens hiding ( indices )
import Data.Foldable
import Debug.Trace

subdividedData :: GeometryData -> Int -> GeometryData
subdividedData geoData subdivisions = GeometryData{..}
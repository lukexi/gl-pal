{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module Graphics.GL.Pal.Uniforms where
import Graphics.GL

import Graphics.GL.Pal.Types

import Control.Monad.Trans
import Foreign

import Linear
import Data.Foldable

import Control.Lens

uniformI :: MonadIO m => UniformLocation GLint -> GLint -> m () 
uniformI uniform int   = glUniform1i (unUniformLocation uniform) int

uniformF :: MonadIO m => UniformLocation GLfloat -> GLfloat -> m () 
uniformF uniform float = glUniform1f (unUniformLocation uniform) float


uniformV3 :: MonadIO m => UniformLocation (V3 GLfloat) -> V3 GLfloat -> m ()
uniformV3 uniform vec3 = glUniform3f (unUniformLocation uniform)
                                     (vec3 ^. _x)
                                     (vec3 ^. _y)
                                     (vec3 ^. _z)

uniformV4 :: MonadIO m => UniformLocation (V4 GLfloat) -> V4 GLfloat -> m ()
uniformV4 uniform vec4 = glUniform4f (unUniformLocation uniform)
                                     (vec4 ^. _x)
                                     (vec4 ^. _y)
                                     (vec4 ^. _z)
                                     (vec4 ^. _w)

uniformM44 :: MonadIO m => UniformLocation (M44 GLfloat) -> M44 GLfloat -> m ()
uniformM44 uniform matrix = liftIO $ do
  let mvpUniformLoc = unUniformLocation uniform
  withArray (concatMap toList (transpose matrix)) $ \matrixPtr ->
    glUniformMatrix4fv mvpUniformLoc 1 GL_FALSE matrixPtr


uniformM33 :: MonadIO m => UniformLocation (M33 GLfloat) -> M33 GLfloat -> m ()
uniformM33 uniform matrix = liftIO $ do
  let mvpUniformLoc = unUniformLocation uniform
  withArray (concatMap toList (transpose matrix)) $ \matrixPtr ->
    glUniformMatrix3fv mvpUniformLoc 1 GL_FALSE matrixPtr
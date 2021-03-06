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

uniformV2 :: MonadIO m => UniformLocation (V2 GLfloat) -> V2 GLfloat -> m ()
uniformV2 uniform vec2 = glUniform2f (unUniformLocation uniform)
                                     (vec2 ^. _x)
                                     (vec2 ^. _y)

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

uniformV4V :: MonadIO m => UniformLocation [V4 GLfloat] -> [V4 GLfloat] -> m ()
uniformV4V uniform array = liftIO $ do

    let uniformLoc = unUniformLocation uniform
        finalArray = concatMap toList array

    withArray finalArray $ \ptr ->
        glUniform4fv uniformLoc (fromIntegral (length array)) ptr

uniformV3V :: MonadIO m => UniformLocation [V3 GLfloat] -> [V3 GLfloat] -> m ()
uniformV3V uniform array = liftIO $ do

    let uniformLoc = unUniformLocation uniform
        finalArray = concatMap toList array

    withArray finalArray $ \ ptr ->
        glUniform3fv uniformLoc (fromIntegral (length array)) ptr

uniformV2V :: MonadIO m => UniformLocation [V2 GLfloat] -> [V2 GLfloat] -> m ()
uniformV2V uniform array = liftIO $ do

    let uniformLoc = unUniformLocation uniform
        finalArray = concatMap toList array

    withArray finalArray $ \ptr ->
        glUniform2fv uniformLoc (fromIntegral (length array)) ptr


uniformM44 :: MonadIO m => UniformLocation (M44 GLfloat) -> M44 GLfloat -> m ()
uniformM44 uniform matrix = liftIO $ do
    let uniformLoc = unUniformLocation uniform
        doTranspose = GL_TRUE
    with matrix $ \ptr ->
        glUniformMatrix4fv uniformLoc 1 doTranspose (castPtr (ptr :: Ptr (M44 GLfloat)))


uniformM33 :: MonadIO m => UniformLocation (M33 GLfloat) -> M33 GLfloat -> m ()
uniformM33 uniform matrix = liftIO $ do
    let uniformLoc = unUniformLocation uniform
        doTranspose = GL_TRUE
    with matrix $ \ptr ->
        glUniformMatrix3fv uniformLoc 1 doTranspose (castPtr (ptr :: Ptr (M33 GLfloat)))


{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module Graphics.GL.Pal2.Uniforms where
import Graphics.GL

import Graphics.GL.Pal2.Types
import Graphics.GL.Pal2.Shader

import Control.Monad.Trans
import Foreign

import Linear
import Data.Foldable


import Data.Data
import Control.Monad.State
import Control.Monad.Reader
import Unsafe.Coerce

import Control.Lens

fillArgs :: (MonadIO m, MonadState [String] m, MonadReader Program m) => m b
fillArgs = do
    args <- get
    case args of 
      [] -> fail "fillArgs - not enough arguments provided"
      (x:xs) -> do
        put xs

        prog <- ask
        uniformLoc <- getShaderUniform prog x
        
        liftIO . putStrLn $ "Got loc: " ++ show uniformLoc
        -- Would like to get rid of this unsafeCoerce if possible!
        -- But I couldn't get fromConstrM to provide
        -- concrete instances of UniformLocation a
        return (unsafeCoerce uniformLoc)

applyConstr :: (Data b, MonadIO m) => Program -> Constr -> [String] -> m b
applyConstr prog c = evalStateT (runReaderT (fromConstrM fillArgs c) prog)

dataTypeToConstr :: DataType -> Constr
dataTypeToConstr dataType = case dataTypeConstrs dataType of
  [oneConstr] -> oneConstr
  [] -> error "Empty types not supported"
  _ -> error "Sum types not supported"

-- We use ScopedTypeVariables and the inferred return type
-- to figure out what datatype we're trying to create
acquireUniforms :: forall a m. (MonadIO m, Monad m, Data a) => Program -> m a
acquireUniforms prog = do
  -- Get the constructor of the inferred type 
  -- (only types with one constructor are supported)
  let constructor   = dataTypeToConstr (dataTypeOf (undefined :: a))
      fieldNames    = constrFields constructor
  applyConstr prog constructor fieldNames


uniformF :: MonadIO m => UniformLocation GLfloat -> GLfloat -> m () 
uniformF uniform float = glUniform1f  ( unUniformLocation uniform ) float

uniformV3 :: MonadIO m => UniformLocation (V3 GLfloat) -> (V3 GLfloat) -> m ()
uniformV3 uniform vec3 = glUniform3f  ( unUniformLocation uniform )
                                      ( vec3 ^. _x )
                                      ( vec3 ^. _y )
                                      ( vec3 ^. _z )

uniformM44 :: MonadIO m => UniformLocation (M44 GLfloat) -> M44 GLfloat -> m ()
uniformM44 uniform matrix = liftIO $ do
  let mvpUniformLoc = unUniformLocation uniform
  withArray (concatMap toList (transpose matrix)) (\matrixPtr ->
    glUniformMatrix4fv mvpUniformLoc 1 GL_FALSE matrixPtr)

uniformM33 :: MonadIO m => UniformLocation (M33 GLfloat) -> M33 GLfloat -> m ()
uniformM33 uniform matrix = liftIO $ do
  let mvpUniformLoc = unUniformLocation uniform
  withArray (concatMap toList (transpose matrix)) (\matrixPtr ->
    glUniformMatrix3fv mvpUniformLoc 1 GL_FALSE matrixPtr)

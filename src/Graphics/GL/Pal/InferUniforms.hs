{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module Graphics.GL.Pal.InferUniforms where

import Graphics.GL.Pal.Types
import Graphics.GL.Pal.Shader

import Control.Monad.Trans



import Data.Data
import Control.Monad.State
import Control.Monad.Reader
import Unsafe.Coerce


{- |

Given some MyUniforms type that derives Data 
and is full of fields named like:
  data MyUniforms = MyUniforms
    { uMVP   :: UniformLocation (M44 GLfloat)
    , uColor :: UniformLocation (V4 GLfloat)
    ...etc. 
    } deriving (Data)

and a (shader) Program,
acquireUniforms will construct an instance of that type,
filling in each field by calling getShaderUniform with the
name of the field matching the name of the uniform.

-}


fillArgs :: (MonadIO m, MonadState [String] m, MonadReader Program m) => m b
fillArgs = do
    args <- get
    case args of 
      [] -> fail "fillArgs - not enough arguments provided"
      (x:xs) -> do
        put xs

        prog <- ask
        uniformLoc <- getShaderUniform prog x
        
        -- liftIO . putStrLn $ "Got loc: " ++ show uniformLoc
        -- Would like to get rid of this unsafeCoerce if possible!
        -- But I couldn't get fromConstrM to accept
        -- non-concrete instances of UniformLocation a,
        -- nor could I figure out how to make them concrete.
        -- (I'd have expected the return type of fillArgs to fill it in...)
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
-- NOTE: The Uniform type you provide must be a record with named fields, all of type
-- UniformLocation a.
acquireUniforms :: forall a m. (MonadIO m, Monad m, Data a) => Program -> m a
acquireUniforms prog = do
  -- Get the constructor of the inferred type 
  -- (only types with one constructor are supported)
  let constructor   = dataTypeToConstr (dataTypeOf (undefined :: a))
      fieldNames    = constrFields constructor
  applyConstr prog constructor fieldNames

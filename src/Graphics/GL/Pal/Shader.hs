module Graphics.GL.Pal.Shader where

import Graphics.GL.Pal.Types
import Graphics.GL.Pal.Utility

import Graphics.GL
import Control.Monad.Trans
import Foreign
import Foreign.C.String

import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text

import Data.Text (Text)

useProgram :: MonadIO m => Program -> m ()
useProgram (Program prog) = glUseProgram (fromIntegral prog)

---------------
-- Load shaders
---------------

-- | Takes the raw source of a pair of shader programs. 
-- Useful when getting shader source from somewhere other than a file,
-- or for munging shader source before compiling it.
createShaderProgramFromSources :: MonadIO m => String -> Text -> String -> Text -> m Program
createShaderProgramFromSources vertexShaderName vertexShaderSource fragmentShaderName fragmentShaderSource = do 
  
    vertexShader <- glCreateShader GL_VERTEX_SHADER
    _ <- compileShaderSource vertexShaderName vertexShaderSource vertexShader
  
    fragmentShader <- glCreateShader GL_FRAGMENT_SHADER
    _ <- compileShaderSource fragmentShaderName fragmentShaderSource fragmentShader
  
    fst <$> attachProgram vertexShader fragmentShader

createShaderProgram :: MonadIO m => FilePath -> FilePath -> m Program
createShaderProgram vp fp = fst <$> createShaderProgram' vp fp

createShaderProgram' :: MonadIO m => FilePath -> FilePath -> m (Program, String)
createShaderProgram' vertexPath fragmentPath = do 
  
    vertexShader <- glCreateShader GL_VERTEX_SHADER
    vertexResult <- compileShaderAtPath vertexPath vertexShader
  
    fragmentShader <- glCreateShader GL_FRAGMENT_SHADER
    fragmentResult <- compileShaderAtPath fragmentPath fragmentShader
  
    (program, linkResult) <- attachProgram vertexShader fragmentShader
  
    let results = unlines . concat $
            [ if null vertexResult   then [] else [vertexPath, vertexResult]
            , if null fragmentResult then [] else [fragmentPath, fragmentResult]
            , if null linkResult     then [] else [vertexPath ++ " and/or " ++ fragmentPath, linkResult]
            ]
    return (program, results)



attachProgram :: MonadIO m => GLuint -> GLuint -> m (Program, String)
attachProgram vertexShader fragmentShader = do

    prog <- glCreateProgram
  
    glAttachShader prog vertexShader
    glAttachShader prog fragmentShader
  
    glLinkProgram prog
  
    linkResult <- checkLinkStatus prog
    
    return (Program prog, linkResult)




compileShaderAtPath :: MonadIO m => FilePath -> GLuint -> m String
compileShaderAtPath path shader = do

    src <- liftIO $ Text.readFile path
  
    compileShaderSource path src shader 

compileShaderSource :: MonadIO m => String -> Text -> GLuint -> m String
compileShaderSource path src shader = do
  
    liftIO $ BS.useAsCString (Text.encodeUtf8 src) $ \ptr ->
        withArray [ptr] $ \srcs ->
            glShaderSource shader 1 srcs nullPtr
  
    glCompileShader shader
    
    checkCompileStatus path shader




getShaderAttribute :: MonadIO m => Program -> String -> m AttributeLocation
getShaderAttribute (Program prog) attributeName = liftIO $ do

    location <- withCString attributeName $ \attributeNameCString -> 
        glGetAttribLocation prog attributeNameCString
  
    --when (location == -1) $ 
    --  putStrLn $ "Couldn't bind attribute: " ++ attributeName 
    --    ++ " - ignoring since it might have just been optimized out"
  
    return (AttributeLocation location)



getShaderUniform :: MonadIO m => Program -> String -> m (UniformLocation a)
getShaderUniform (Program prog) uniformName = liftIO $ do

    location <- withCString uniformName $ \uniformNameCString -> 
        glGetUniformLocation prog uniformNameCString
  
    --when (location == -1) $ 
    --  putStrLn $ "Couldn't bind uniform: " ++ uniformName 
    --    ++ " - ignoring since it might have just been optimized out"
  
    return (UniformLocation location)

getUniformBlockIndex :: MonadIO m => Program -> String -> m UniformBlockIndex
getUniformBlockIndex shader uniformBlockName = fmap UniformBlockIndex $ liftIO $
    withCString uniformBlockName $
        glGetUniformBlockIndex (unProgram shader)

bindShaderUniformBuffer :: MonadIO m => Program -> String -> UniformBlockBindingPoint -> m ()
bindShaderUniformBuffer shader uniformBlockName bindingPoint = do
    uniformBlockIndex <- getUniformBlockIndex shader uniformBlockName
    glUniformBlockBinding 
        (unProgram shader) 
        (unUniformBlockIndex uniformBlockIndex) 
        (unUniformBlockBindingPoint bindingPoint)


glGetErrors :: MonadIO m => m ()
glGetErrors = do

    code <- glGetError
  
    case code of
    
        GL_NO_ERROR -> return ()
    
        e -> do
    
            liftIO $ case e of
        
                GL_INVALID_ENUM                   -> putStrLn "* Invalid Enum"
                GL_INVALID_VALUE                  -> putStrLn "* Invalid Value"
                GL_INVALID_OPERATION              -> putStrLn "* Invalid Operation"
                GL_INVALID_FRAMEBUFFER_OPERATION  -> putStrLn "* Invalid Framebuffer Operation"
                GL_OUT_OF_MEMORY                  -> putStrLn "* Out of Memory"
                GL_STACK_UNDERFLOW                -> putStrLn "* Stack underflow"
                GL_STACK_OVERFLOW                 -> putStrLn "* Stack overflow"
        
                _ -> return ()
      
            glGetErrors

checkLinkStatus :: MonadIO m => GLuint -> m String
checkLinkStatus prog = do
  
    linked <- overPtr (glGetProgramiv prog GL_LINK_STATUS)
  
    if linked == GL_FALSE
        then do
            maxLength <- overPtr (glGetProgramiv prog GL_INFO_LOG_LENGTH)
      
            logLines <- liftIO $ allocaArray (fromIntegral maxLength) $ \p ->
                        alloca $ \lenP -> do
                            glGetProgramInfoLog prog maxLength lenP p
                            len <- peek lenP
                            peekCStringLen (p, fromIntegral len)
            
            liftIO $ putStrLn logLines
            return logLines
        else return ""


checkCompileStatus :: MonadIO m => String -> GLuint -> m String
checkCompileStatus path shader = do

    compiled <- overPtr (glGetShaderiv shader GL_COMPILE_STATUS)
  
    if compiled == GL_FALSE 
        then do
            maxLength <- overPtr (glGetShaderiv shader GL_INFO_LOG_LENGTH)
      
            logLines <- liftIO $ allocaArray (fromIntegral maxLength) $ \p ->
                        alloca $ \lenP -> do 
      
                            glGetShaderInfoLog shader maxLength lenP p
                            len <- peek lenP
                            peekCStringLen (p, fromIntegral len)
      
            liftIO $ putStrLn ("In " ++ path ++ ":")
            
            liftIO $ putStrLn logLines
            return logLines
        else return ""

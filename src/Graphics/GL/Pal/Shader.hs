{-# LANGUAGE BangPatterns #-}
module Graphics.GL.Pal.Shader (module Graphics.GL.Pal.Shader, getDir) where

import Graphics.GL.Pal.Types
import Graphics.GL.Pal.Utility

import Graphics.GL
import Graphics.GL.Ext.ARB.ShadingLanguageInclude
import Control.Monad.Trans
import Control.Monad
import Foreign
import Foreign.C.String

import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text

import Data.Text (Text)
import Data.FileEmbed (getDir)

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
    let searchPaths = []
    vertexShader <- Shader <$> glCreateShader GL_VERTEX_SHADER
    _ <- compileShaderSource vertexShaderName vertexShaderSource vertexShader searchPaths

    fragmentShader <- Shader <$> glCreateShader GL_FRAGMENT_SHADER
    _ <- compileShaderSource fragmentShaderName fragmentShaderSource fragmentShader searchPaths

    fst <$> attachProgram [vertexShader, fragmentShader]

createShaderProgram :: MonadIO m => FilePath -> FilePath -> m Program
createShaderProgram vp fp = createShaderProgramInclude vp fp []

createShaderProgramInclude :: MonadIO m => FilePath -> FilePath -> [FilePath] -> m Program
createShaderProgramInclude vp fp searchPaths =
    fst <$> createShaderProgram' vp fp searchPaths

createShaderProgram' :: MonadIO m => FilePath -> FilePath -> [FilePath] -> m (Program, String)
createShaderProgram' vertexPath fragmentPath searchPaths = do

    vertexShader <- Shader <$> glCreateShader GL_VERTEX_SHADER
    vertexResult <- compileShaderAtPath vertexPath vertexShader searchPaths

    fragmentShader <- Shader <$> glCreateShader GL_FRAGMENT_SHADER
    fragmentResult <- compileShaderAtPath fragmentPath fragmentShader searchPaths

    (program, linkResult) <- attachProgram [vertexShader, fragmentShader]

    let results = unlines . concat $
            [ if null vertexResult   then [] else [vertexPath, vertexResult]
            , if null fragmentResult then [] else [fragmentPath, fragmentResult]
            , if null linkResult     then [] else [vertexPath ++ " and/or " ++ fragmentPath, linkResult]
            ]
    return (program, results)



attachProgram :: MonadIO m => [Shader] -> m (Program, String)
attachProgram shaders = do

    prog <- glCreateProgram

    mapM_ (glAttachShader prog . unShader) shaders

    glLinkProgram prog

    linkResult <- checkLinkStatus prog

    return (Program prog, linkResult)




compileShaderAtPath :: MonadIO m => FilePath -> Shader -> [FilePath] -> m String
compileShaderAtPath sourcePath shader searchPaths = do

    src <- liftIO $ Text.readFile sourcePath

    compileShaderSource sourcePath src shader searchPaths

compileShaderSource :: MonadIO m => String -> Text -> Shader -> [FilePath] -> m String
compileShaderSource sourcePath src (Shader shader) searchPaths = do

    liftIO $ BS.useAsCString (Text.encodeUtf8 src) $ \ptr ->
        withArray [ptr] $ \srcs ->
            glShaderSource shader 1 srcs nullPtr

    compileShaderInclude (Shader shader) searchPaths

    checkCompileStatus sourcePath (Shader shader)




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


checkCompileStatus :: MonadIO m => String -> Shader -> m String
checkCompileStatus path (Shader shader) = do

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


-- |
--
-- Using statically embedded strings in the executable:
--
-- @
-- buildNamedStrings $(embedDir "foo") ('/':)
-- @
--
-- Dynamically embedding all files in a given directory into the named string set
--
-- @
-- getDir "foo" >>= \ ss -> buildNamedStrings ss ('/':)
-- @
--
-- Falls back to doing nothing if 'gl_ARB_shading_langauge_include' isn't available.
buildNamedStrings :: MonadIO m => [(FilePath, BS.ByteString)] -> (FilePath -> String) -> m ()
buildNamedStrings includes tweak = liftIO $ when gl_ARB_shading_language_include $ do
    forM_ includes $ \(fp',body) -> do
        withCStringLen (tweak fp') $ \ (name, namelen) ->
            BSU.unsafeUseAsCString body $ \string -> do
                glNamedStringARB GL_SHADER_INCLUDE_ARB
                    (fromIntegral namelen)
                    name
                    (fromIntegral $ BS.length body)
                    string

-- | Compile a shader with @#include@ support (if available).
--
-- Remember to use
--
-- @
-- #extension GL_ARB_shading_language_include : <behavior>
-- @
--
-- as appropriate within your shader.
--
-- Falls back to 'compileShader' if 'gl_ARB_shading_language_include' isn't available.
compileShaderInclude :: MonadIO m => Shader -> [FilePath] -> m ()
compileShaderInclude (Shader s) searchPaths
    | gl_ARB_shading_language_include =
        liftIO $ withCStrings searchPaths $ \n cpcs ->
            glCompileShaderIncludeARB s (fromIntegral n) cpcs nullPtr
    | otherwise = glCompileShader s


withCStrings :: [String] -> (Int -> Ptr CString -> IO a) -> IO a
withCStrings all_xs f = go 0 [] all_xs where
  go !n acc (x:xs) = withCString x $ \s -> go (n + 1) (s:acc) xs
  go !n acc [] = allocaArray n $ \p -> do
    pokeArray p (reverse acc)
    f n p

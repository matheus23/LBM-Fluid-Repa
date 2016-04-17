module ShaderUtils where

import qualified Graphics.Rendering.OpenGL as GL
import Data.StateVar

loadShaderProgram :: String -> String -> IO GL.Program
loadShaderProgram vertSource fragSource = do
    vertexShader <- loadShaderObject GL.VertexShader vertSource
    fragmentShader <- loadShaderObject GL.FragmentShader fragSource
    program <- GL.createProgram
    GL.attachShader program vertexShader
    GL.attachShader program fragmentShader
    GL.attribLocation program "position" $= GL.AttribLocation 0
    GL.linkProgram program
    infoLog <- get $ GL.programInfoLog program
    putStrLn $ "Shader Program info log:\n" ++ infoLog
    return program

loadShaderObject :: GL.ShaderType -> String -> IO GL.Shader
loadShaderObject shaderType source = do
    shader <- GL.createShader shaderType
    GL.shaderSourceBS shader $= GL.packUtf8 source
    GL.compileShader shader
    return shader

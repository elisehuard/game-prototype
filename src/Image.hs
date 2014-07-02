module Image 
(compileTexture2DRGBAF)
where

import Data.ByteString.Char8 (ByteString)
import Data.Word
import Data.Bitmap.Pure
import Foreign.Ptr (castPtr)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Storable (peek)
import Control.Monad (when)
import Graphics.Rendering.OpenGL.Raw.Core32 (
    GLuint,
    glPixelStorei,
    glTexParameteri,
    glGenerateMipmap,
    gl_TEXTURE_2D,
    gl_UNSIGNED_BYTE,
    glTexImage2D,
    gl_RGBA,
    gl_RGB,
    gl_TEXTURE_WRAP_T,
    gl_TEXTURE_WRAP_S,
    glGenTextures,
    gl_UNPACK_ALIGNMENT,
    gl_CLAMP_TO_EDGE,
    gl_REPEAT,
    gl_LINEAR,
    gl_LINEAR_MIPMAP_LINEAR,
    gl_TEXTURE_MIN_FILTER,
    gl_TEXTURE_MAG_FILTER,
    gl_TEXTURE_MAX_LEVEL,
    gl_RGBA8,
    gl_TEXTURE_BASE_LEVEL,
    glBindTexture
  )

data TextureData
    = TextureData
    { textureObject :: GLuint
    }

-- we won't need mipmaps for 3d obvs
compileTexture2DRGBAF :: Bool -> Bool -> Bitmap Word8 -> IO TextureData
compileTexture2DRGBAF isMip isClamped bitmap = do
    glPixelStorei gl_UNPACK_ALIGNMENT 1
    to <- alloca $! \pto -> glGenTextures 1 pto >> peek pto
    glBindTexture gl_TEXTURE_2D to
    let (width,height) = bitmapSize bitmap
        wrapMode = case isClamped of
            True    -> gl_CLAMP_TO_EDGE
            False   -> gl_REPEAT
        (minFilter,maxLevel) = case isMip of
            False   -> (gl_LINEAR,0)
            True    -> (gl_LINEAR_MIPMAP_LINEAR, floor $ log (fromIntegral $ max width height) / log 2)
    glTexParameteri gl_TEXTURE_2D gl_TEXTURE_WRAP_S $ fromIntegral wrapMode
    glTexParameteri gl_TEXTURE_2D gl_TEXTURE_WRAP_T $ fromIntegral wrapMode
    glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MIN_FILTER $ fromIntegral minFilter
    glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MAG_FILTER $ fromIntegral gl_LINEAR
    glTexParameteri gl_TEXTURE_2D gl_TEXTURE_BASE_LEVEL 0
    glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MAX_LEVEL $ fromIntegral maxLevel
    withBitmap bitmap $ \(w,h) nchn 0 ptr -> do
        let internalFormat  = fromIntegral gl_RGBA8
            dataFormat      = fromIntegral $ case nchn of
                3   -> gl_RGB
                4   -> gl_RGBA
                _   -> error "unsupported texture format!"
        glTexImage2D gl_TEXTURE_2D 0 internalFormat (fromIntegral w) (fromIntegral h) 0 dataFormat gl_UNSIGNED_BYTE $ castPtr ptr
    when isMip $ glGenerateMipmap gl_TEXTURE_2D
    -- for texture data into texture object
    -- glTexImage2D(m_textureTarget, 0, GL_RGBA, m_pImage->columns(), m_pImage->rows(), 0, GL_RGBA, GL_UNSIGNED_BYTE, m_blob.data());
    return $ TextureData to

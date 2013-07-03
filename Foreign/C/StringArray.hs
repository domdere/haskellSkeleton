module Foreign.C.StringArray ( withCStringArray ) where

import qualified Data.ByteString as BS
import Foreign.C.String (CString, withCString)
import Foreign.Marshal.Array (withArray)
import Foreign.Ptr (nullPtr, plusPtr, Ptr)
import Foreign.Storable (poke)

withCStringArray :: [BS.ByteString] -> (Ptr CString -> IO a) -> IO a
withCStringArray strings action =
    let arrayLength                                 = length strings
        nullPtrs                                    = take arrayLength $ repeat nullPtr
        initialiseAndRun array _ [] action          = action array
        initialiseAndRun array offset (x:xs) action = do
            BS.useAsCString x $ \cstr -> do
                poke (array `plusPtr` offset) cstr
                initialiseAndRun array (offset + 1) xs action
    in do
        withArray nullPtrs $ \array -> do
            initialiseAndRun array 0 strings action

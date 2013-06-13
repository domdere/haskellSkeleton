-- | Some types and helpers to aid in some of the more 
-- | common C/C++ argument passing conventions

module Foreign.C.ArgPassing where

import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import Foreign 

-- TODO: (Dom De Re) Right now I only define these new types so i can start using them in 
-- Function type signatures, i'll write up the helper functions that actually 
-- handle the arg passing later

-- | For C functions that accept a pointer to write the output to,
-- | e.g,
-- | err_t foo(void* output);
newtype OutPtr a = OutPtr { fromOutPtr :: Ptr a } deriving (Show, Eq)

-- | For C functions that accept a pointer to input,
-- | e.g,
-- | err_t foo(void* inputbuffer);
newtype InPtr a = InPtr { fromInPtr :: Ptr a } deriving (Show, Eq)

-- | For C functions that accept a pointer to input and also write to that buffer as output,
-- | e.g,
-- | err_t foo(void* inputoutputbuffer);
newtype InOutPtr a = InOutPtr { fromInOutPtr :: Ptr a } deriving (Show, Eq)

-- | Similar to OutPtr but a special case for CString = Ptr CChar,
-- | as there will be some slightly different semantics due to the additional steps necessary to
-- | handle the NUL terminated strings
-- | e.g:
-- | err_t foo(char* output);
newtype OutCString = OutCString { fromOutCString :: CString } deriving (Show, Eq)

-- | Similar to InPtr but a special case for CString = Ptr CChar,
-- | as there will be some slightly different semantics due to the additional steps necessary to
-- | handle the NUL terminated strings
-- | e.g:
-- | err_t foo(const char* input);
newtype InCString = InCString { fromInCString :: CString } deriving (Show, Eq)

-- | Similar to InOutPtr but a special case for CString = Ptr CChar,
-- | as there will be some slightly different semantics due to the additional steps necessary to
-- | handle the NUL terminated strings
-- | e.g:
-- | err_t foo(char* inputoutput);
newtype InOutCString = InOutCString { fromInOutCString :: CString } deriving (Show, Eq)

-- Storable instances for these 6 types:
-- essentially use the Storableness of the underlying Ptr/CString, 
-- bit of a shmame you cant derive Storable.

instance Storable (InPtr a) where
    sizeOf (InPtr ptr) = sizeOf ptr

    alignment = sizeOf

    peek p = do
        ptr <- peek (p `plusPtr` 0)
        return $ InPtr ptr

    poke p (InPtr ptr) = do
        poke (p `plusPtr` 0) ptr
        return ()

instance Storable (OutPtr a) where
    sizeOf (OutPtr ptr) = sizeOf ptr

    alignment = sizeOf

    peek p = do
        ptr <- peek (p `plusPtr` 0)
        return $ OutPtr ptr

    poke p (OutPtr ptr) = do
        poke (p `plusPtr` 0) ptr
        return ()

instance Storable (InOutPtr a) where
    sizeOf (InOutPtr ptr) = sizeOf ptr

    alignment = sizeOf

    peek p = do
        ptr <- peek (p `plusPtr` 0)
        return $ InOutPtr ptr

    poke p (InOutPtr ptr) = do
        poke (p `plusPtr` 0) ptr
        return ()

instance Storable InCString where
    sizeOf (InCString cstr) = sizeOf cstr

    alignment = sizeOf

    peek p = do
        cstr <- peek (p `plusPtr` 0)
        return $ InCString cstr

    poke p (InCString cstr) = do
        poke (p `plusPtr` 0) cstr
        return ()

instance Storable OutCString where
    sizeOf (OutCString cstr) = sizeOf cstr

    alignment = sizeOf

    peek p = do
        cstr <- peek (p `plusPtr` 0)
        return $ OutCString cstr

    poke p (OutCString cstr) = do
        poke (p `plusPtr` 0) cstr
        return ()

instance Storable InOutCString where
    sizeOf (InOutCString cstr) = sizeOf cstr

    alignment = sizeOf

    peek p = do
        cstr <- peek (p `plusPtr` 0)
        return $ InOutCString cstr

    poke p (InOutCString cstr) = do
        poke (p `plusPtr` 0) cstr
        return ()

-- simple things we want to do with these data types

-- | Marshals a value into a Ptr and wraps that in the InPtr type
withInPtr :: (Storable a) => a -> (InPtr a -> IO b) -> IO b
withInPtr value action = with value (\ptr -> action (InPtr ptr))

wrapOutPtrIO :: (Storable a) => (OutPtr a -> IO b) -> Ptr a -> IO (a, b)
wrapOutPtrIO action ptr = do
    returnVal <- action $ OutPtr ptr
    outVal <- peek ptr
    return (outVal, returnVal)

-- TODO (Dom De Re) : Clean this up, this is probably a hint that these ptr types
-- should be part of a common class, but I'll play around with them a bit more before I
-- decide whether it should be a new type class or an existing one like a Comonad or something.
wrapInOutPtrIO :: (Storable a) => (InOutPtr a -> IO b) -> Ptr a -> IO (a, b)
wrapInOutPtrIO action ptr = do
    returnVal <- action $ InOutPtr ptr
    outVal <- peek ptr
    return (outVal, returnVal)


-- | Marshals a value into a Ptr and wraps that in the OutPtr type, runs the action
-- | and then retrieve the output value with Storable.peek
withOutPtr :: (Storable a) => (OutPtr a -> IO b) -> IO (a, b)
withOutPtr action = alloca (wrapOutPtrIO action) 

-- | Combines In and OutPtr into InOutPtr
withInOutPtr :: (Storable a) => a -> (InOutPtr a -> IO b) -> IO (a, b)
withInOutPtr value action = with value $ wrapInOutPtrIO action

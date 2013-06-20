-- | Some types and helpers to aid in some of the more 
-- | common C/C++ argument passing conventions

module Foreign.C.ArgPassing where

import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import Foreign 

-- Type classes to define the semantics through which inputs and outputs can be passed to C functions.

-- TODO: (Dom De Re) Right now I only define these new types so i can start using them in 
-- Function type signatures, i'll write up the helper functions that actually 
-- handle the arg passing later

-- Type Classes

class ArrayCArgument m where
    fromArrayCArgument :: m a -> Ptr a

    wrapArray :: Ptr a -> m a

-- End Type Classes

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

-- | This is a data type not so much for passing to C functions, but for including in Haskell
-- | representations of C structs, with fixed length char arrays

data FixedLengthString = FixedLengthString {
    fromFixedLengthCString :: String,
    fixedArrayLength :: Int
} deriving (Show, Eq)

-- | Input and output buffers:

-- | void* buffer
newtype VoidBuffer = VoidBuffer { fromVoidBuffer :: Ptr () } deriving (Show, Eq)

type InBuffer = VoidBuffer
type OutBuffer = VoidBuffer
type InOutBuffer = VoidBuffer

-- | End Buffers

-- | Input and output arrays

newtype InArray a = InArray (Ptr a) deriving (Show, Eq)
newtype OutArray a = OutArray (Ptr a)  deriving (Show, Eq)
newtype InOutArray a = InOutArray (Ptr a) deriving (Show, Eq)

-- | End Input and output arrays

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


instance Storable (InArray a) where
    sizeOf (InArray ptr) = sizeOf ptr

    alignment = sizeOf

    peek p = do
        ptr <- peek (p `plusPtr` 0)
        return $ InArray ptr

    poke p (InArray ptr) = do
        poke (p `plusPtr` 0) ptr
        return ()

instance ArrayCArgument InArray where
    fromArrayCArgument (InArray ptr) = ptr

    wrapArray = InArray

instance ArrayCArgument OutArray where
    fromArrayCArgument (OutArray ptr) = ptr

    wrapArray = OutArray

instance ArrayCArgument InOutArray where
    fromArrayCArgument (InOutArray ptr) = ptr

    wrapArray = InOutArray

instance Storable (OutArray a) where
    sizeOf (OutArray ptr) = sizeOf ptr

    alignment = sizeOf

    peek p = do
        ptr <- peek (p `plusPtr` 0)
        return $ OutArray ptr

    poke p (OutArray ptr) = do
        poke (p `plusPtr` 0) ptr
        return ()

instance Storable (InOutArray a) where
    sizeOf (InOutArray ptr) = sizeOf ptr

    alignment = sizeOf

    peek p = do
        ptr <- peek (p `plusPtr` 0)
        return $ InOutArray ptr

    poke p (InOutArray ptr) = do
        poke (p `plusPtr` 0) ptr
        return ()

-- simple things we want to do with these data types

-- Plain Ptr functions:

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

-- End Plain Ptr functions:

-- CString Ptr Functions:

-- | peeks at the value of a Cstring and then caps the length of the result
-- | at the given length bound and wraps the whole thing up in a FixedLengthString with the given max length
-- | Note that 'maxlength' includes the NUL character)
peekFixedLengthString :: Int -> CString -> IO FixedLengthString
peekFixedLengthString maxlength p = do
    str <- peekCString $ p `plusPtr` 0
    -- Only take maxlength - 1 chars to allow for the NUL
    -- character when 'poke' is used to store it
    return $ FixedLengthString (take (maxlength - 1) str) maxlength

pokeFixedLengthString :: CString -> FixedLengthString -> IO ()
pokeFixedLengthString p (FixedLengthString str maxlength)= do
    withCString str (\cstr -> do
        copyBytes p cstr (min maxlength ((length str) + 1)))
    return ()

withInCString :: String -> (InCString -> IO a) -> IO a
withInCString value action = withCString value (\cstr -> action (InCString cstr))

withOutCString :: Int -> (OutCString -> IO a) -> IO (String, a)
withOutCString bufferSize action = do
    allocaArray bufferSize (\buffer -> do
        retval <- action (OutCString buffer)
        str <- peekCString buffer
        return (str, retval))

-- | creates a CString with the initial value given,
-- | with the max length given (in case it doesnt 
-- | match the length of the given initial value)
withInOutCString :: Int -> String -> (InOutCString -> IO a) -> IO (String, a)
withInOutCString bufferlength initialValue action = do
    withCString initialValue (\cstr -> do
        allocaArray bufferlength (\buffer -> do
            copyBytes buffer cstr (min bufferlength ((length initialValue) + 1))
            retval <- action (InOutCString buffer)
            str <- peekCString buffer
            return (str, retval))) 

-- End CString Ptr Functions

-- Buffer Ptr Functions:
withVoidBuffer :: Int -> (VoidBuffer -> IO a) -> IO a
withVoidBuffer numBytes action = allocaBytes numBytes (\buffer -> action (VoidBuffer buffer))
    

-- End Buffer Ptr Functions

-- Array Functions

peekWrappedArray :: (Integral a, Storable b, ArrayCArgument m) => a -> m b -> IO [b]
peekWrappedArray length wrappedPtr = peekArray intLength (fromArrayCArgument wrappedPtr)
    where 
        intLength = (fromInteger . toInteger) length

-- | takes a list of arguments to write to a given array bound by the given length,
-- | returns a list containing the 
pokeWrappedArray :: (Integral a, Storable b, ArrayCArgument m) => a -> m b -> [b] -> IO [b]
pokeWrappedArray arraySize wrappedPtr list = 
    let intArraySize    = (fromInteger . toInteger) arraySize
        (xs, remainder) = (splitAt intArraySize list) 
    in do
        pokeArray (fromArrayCArgument wrappedPtr) xs
        return $ remainder

withInArray :: (Integral a, Storable b) => a -> [b] -> (InArray b -> IO c) -> IO ([b], c)
withInArray arraySize list action = 
    let intArraySize = (fromInteger . toInteger) arraySize
        (xs, remainder) = (splitAt intArraySize list) 
    in do
        retval <- withArray xs (\array -> (action (InArray array)))
        return (remainder, retval)

withOutArray :: (Integral a, Storable b) => a -> (OutArray b -> IO c) -> IO ([b], c)
withOutArray arraySize action =
    let intArraySize        = (fromInteger . toInteger) arraySize
        arrayAction array   = do
            retval <- action (wrapArray array)
            list <- peekArray intArraySize array
            return (list, retval)
    in do
        allocaArray intArraySize arrayAction

-- End Array Functions

-- | Some types and helpers to aid in some of the more 
-- | common C/C++ argument passing conventions

module Foreign.C.ArgPassing where

-- TODO: (Dom De Re) Right now I only define these new types so i can start using them in 
-- Function type signatures, i'll write up the helper functions that actually 
-- handle the arg passing later

-- | For C functions that accept a pointer to write the output to,
-- | e.g,
-- | err_t foo(void* output);
newtype OutPtr a = OutPtr { fromOutPtr :: Ptr a } deriving (Show, Eq)

-- | Similar to OutPtr but a special case for CString = Ptr CChar,
-- | as there will be some slightly different semantics due to the additional seps necesarry to
-- | handle the NUL terminated strings

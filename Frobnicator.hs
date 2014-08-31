{-# LANGUAGE ForeignFunctionInterface #-}

module Frobnicator
  ( Context
  , Measurement
  , withContext
  , measure
  ) where

import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable

-- The context is an opaque type allocated by the C library.

data ContextT = ContextT
type Context = Ptr ContextT

-- A Measurement represents the struct that the C library uses to pass it
-- outputs.

data Measurement = Measurement { time :: Integer, value :: Double }
                   deriving (Eq, Show)

instance Storable Measurement where
  sizeOf _ = sizeOf (undefined :: CUInt) + sizeOf (undefined :: CDouble)
  alignment _ = alignment (undefined :: CDouble)
  peek ptr =
    do time <- peek ptrTime
       value <- peek ptrValue
       return $ Measurement (fromIntegral time) (realToFrac value)
    where
      -- These pointer calculations are tedious. In real life you would want
      -- to use `hsc2hs` to help you out with these. However it is good to
      -- have some understanding about issues that can arise during the
      -- packing.
      ptrTime = castPtr ptr :: Ptr CUInt
      ptrValue' :: Ptr CDouble
      ptrValue' = castPtr $ ptrTime `plusPtr` sizeOf (undefined :: CUInt)
      -- The C standard does not require that the structs are packed. In
      -- practice the elements are individually aligned unless there is an
      -- explicit compiler directive requesting for tight packing.
      ptrValue = alignPtr ptrValue' $ alignment ptrValue'
  poke ptr (Measurement time value) =
    do poke ptrTime $ fromIntegral time
       poke ptrValue $ realToFrac value
    where
      ptrTime = castPtr ptr :: Ptr CUInt
      ptrValue' :: Ptr CDouble
      ptrValue' = castPtr $ ptrTime `plusPtr` sizeOf (undefined :: CUInt)
      ptrValue = alignPtr ptrValue' $ alignment ptrValue'

foreign import ccall unsafe "cbits.h alloc_context"
  c_alloc_context :: IO Context

foreign import ccall unsafe "cbits.h release_context"
  c_release_context :: Context -> IO CInt

foreign import ccall unsafe "cbits.h measure"
  c_measure :: Context -> Ptr Measurement -> IO CInt

withContext :: (Context -> IO a) -> IO (Maybe a)
withContext action =
  do context <- c_alloc_context
     if context /= nullPtr
     then
       -- TODO: I should treat the context here like any other resource
       -- and guard against exceptions and whatnot.
       do result <- action context
          retVal <- c_release_context context
          -- TODO: Check the return value
          return $ Just result
     else
       return Nothing

measure :: Context -> IO (Maybe Measurement)
measure context =
  do alloca $ \ptrResult ->
       do retVal <- c_measure context ptrResult
          if retVal == 0
          then
            do result <- peek ptrResult
               return $ Just result
          else
            return Nothing

-- Local variables:
-- haskell-indent-offset: 2
-- End:

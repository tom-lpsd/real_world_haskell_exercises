{-# INCLUDE <pcre.h> #-}
{-# LINE 1 "Regex.hsc" #-}
{-# LANGUAGE CPP, ForeignFunctionInterface #-}
{-# LINE 2 "Regex.hsc" #-}

module Regex where

import Foreign
import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import Foreign.ForeignPtr

import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Unsafe as S
import qualified Data.ByteString.Internal as S


{-# LINE 16 "Regex.hsc" #-}

newtype PCREOption = PCREOption { unPCREOption :: CInt }
    deriving (Eq, Show)

caseless        :: PCREOption
caseless        = PCREOption 1
dollar_endonly  :: PCREOption
dollar_endonly  = PCREOption 32
dotall          :: PCREOption
dotall          = PCREOption 4

{-# LINE 25 "Regex.hsc" #-}

foreign import ccall unsafe "pcre.h pcre_compile"
    c_pcre_compile :: CString
                   -> PCREOption
                   -> Ptr CString
                   -> Ptr CInt
                   -> Ptr Word8
                   -> IO (Ptr PCRE)

newtype PCRE = PCRE (Ptr PCRE)

data Regex = Regex !(ForeignPtr PCRE)
                   !S.ByteString
             deriving (Eq, Ord, Show)

combineOptions :: [PCREOption] -> PCREOption
combineOptions = PCREOption . foldr ((.|.) . unPCREOption) 0

compile :: S.ByteString -> [PCREOption] -> Either String Regex
compile str flags = unsafePerformIO $
    S.useAsCString str $ \pattern -> do
      alloca $ \errptr -> do
      alloca $ \erroffset -> do
        pcre_ptr <- c_pcre_compile pattern (combineOptions flags)
                    errptr erroffset nullPtr
        if pcre_ptr == nullPtr
            then do
                err <- peekCString =<< peek errptr
                return (Left err)
            else do
                reg <- newForeignPtr finalizerFree pcre_ptr
                return (Right (Regex reg str))

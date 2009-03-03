{-# LANGUAGE CPP, ForeignFunctionInterface #-}

module Regex where

import Foreign
import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import Foreign.ForeignPtr

import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Unsafe as S
import qualified Data.ByteString.Internal as S

#include <pcre.h>

newtype PCREOption = PCREOption { unPCREOption :: CInt }
    deriving (Eq, Show)

#{enum PCREOption, PCREOption
 , caseless       = PCRE_CASELESS
 , dollar_endonly = PCRE_DOLLAR_ENDONLY
 , dotall         = PCRE_DOTALL
}

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

foreign import ccall "pcre.h pcre_exec"
    c_pcre_exec :: Ptr PCRE
                -> Ptr PCREExtra
                -> Ptr Word8
                -> CInt
                -> CInt
                -> PCREException
                -> Ptr CInt
                -> CInt
                -> IO CInt

foreign import ccall "pcre.h pcre_fullinfo"
    c_pcre_fullinfo :: Ptr PCRE
                    -> Ptr PCREExtra
                    -> PCREInfo
                    -> Ptr a
                    -> IO CInt

capturedCount :: Ptr PCRE -> IO Int
capturedCount regex_ptr =
    alloca $ \n_ptr -> do
      c_pcre_fullinfo regex_ptr nullPtr info_capturecount n_ptr
      return . fromIntegral =<< peek (n_ptr :: Ptr CInt)

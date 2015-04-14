module ByteString ( regexec ) where

import qualified Regex as Reg

import Data.ByteString(ByteString)
import qualified Data.ByteString as B(empty,useAsCString,last,take,drop,null)
import qualified Data.ByteString.Unsafe as B(unsafeUseAsCString)

import Foreign.C.String

-- ---------------------------------------------------------------------
-- | Matches a regular expression against a buffer, returning the buffer
-- indicies of the match, and any submatches
--
regexec :: Reg.Regex              -- ^ Compiled regular expression
        -> ByteString         -- ^ The buffer to match against
        -> Int                -- ^ Offset in buffer to start searching from
        -> IO (Maybe ((Int,Int), [(Int,Int)]))
        -- ^ Returns: 'Nothing' if the regex did not match the
        -- or Just the start and end indicies of the match and submatches
regexec regx bs flag = asCString bs (\cstr -> Reg.regexec regx cstr flag)
        
asCString :: ByteString -> (CString -> IO a) -> IO a
asCString bs = if (not (B.null bs)) && (0==B.last bs)
                  then B.unsafeUseAsCString bs
                  else B.useAsCString bs

module System.Posix.Directory.Foreign where

import Data.Bits
import Data.List (foldl')
import Foreign.C.Types

#include <stdlib.h>
#include <dirent.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

newtype DirType = DirType Int deriving (Eq, Show)
newtype Flags = Flags { unFlags :: Int } deriving (Eq, Show)

#{enum DirType, DirType, DT_BLK, DT_CHR, DT_DIR, DT_FIFO, DT_LNK, DT_REG, DT_SOCK, DT_UNKNOWN}

#{enum Flags, Flags, O_APPEND, O_ASYNC, O_CLOEXEC, O_CREAT, O_DIRECTORY, O_EXCL, O_NOCTTY, O_NOFOLLOW, O_NONBLOCK, O_RDONLY, O_SYNC, O_TRUNC}

unionFlags :: [Flags] -> CInt
unionFlags = fromIntegral . foldl' ((. unFlags) . (.|.)) 0

#{def unsigned int
    __posixdir_d_type(struct dirent* d)
    {
      return(d -> d_type);
    }
}

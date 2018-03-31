/* ----------------------------------------------------------------------------
   (c) The University of Glasgow 2006

   Useful Win32 bits
   ------------------------------------------------------------------------- */

#if defined(_WIN32)

#include "HsBase.h"
#include <stdbool.h>
#include <stdint.h>
/* Using Secure APIs */
#define MINGW_HAS_SECURE_API 1
#include <wchar.h>
#include <windows.h>
#include <io.h>

/* This is the error table that defines the mapping between OS error
   codes and errno values */

struct errentry {
        unsigned long oscode;           /* OS return value */
        int errnocode;  /* System V error code */
};

static struct errentry errtable[] = {
        {  ERROR_INVALID_FUNCTION,       EINVAL    },  /* 1 */
        {  ERROR_FILE_NOT_FOUND,         ENOENT    },  /* 2 */
        {  ERROR_PATH_NOT_FOUND,         ENOENT    },  /* 3 */
        {  ERROR_TOO_MANY_OPEN_FILES,    EMFILE    },  /* 4 */
        {  ERROR_ACCESS_DENIED,          EACCES    },  /* 5 */
        {  ERROR_INVALID_HANDLE,         EBADF     },  /* 6 */
        {  ERROR_ARENA_TRASHED,          ENOMEM    },  /* 7 */
        {  ERROR_NOT_ENOUGH_MEMORY,      ENOMEM    },  /* 8 */
        {  ERROR_INVALID_BLOCK,          ENOMEM    },  /* 9 */
        {  ERROR_BAD_ENVIRONMENT,        E2BIG     },  /* 10 */
        {  ERROR_BAD_FORMAT,             ENOEXEC   },  /* 11 */
        {  ERROR_INVALID_ACCESS,         EINVAL    },  /* 12 */
        {  ERROR_INVALID_DATA,           EINVAL    },  /* 13 */
        {  ERROR_INVALID_DRIVE,          ENOENT    },  /* 15 */
        {  ERROR_CURRENT_DIRECTORY,      EACCES    },  /* 16 */
        {  ERROR_NOT_SAME_DEVICE,        EXDEV     },  /* 17 */
        {  ERROR_NO_MORE_FILES,          ENOENT    },  /* 18 */
        {  ERROR_LOCK_VIOLATION,         EACCES    },  /* 33 */
        {  ERROR_BAD_NETPATH,            ENOENT    },  /* 53 */
        {  ERROR_NETWORK_ACCESS_DENIED,  EACCES    },  /* 65 */
        {  ERROR_BAD_NET_NAME,           ENOENT    },  /* 67 */
        {  ERROR_FILE_EXISTS,            EEXIST    },  /* 80 */
        {  ERROR_CANNOT_MAKE,            EACCES    },  /* 82 */
        {  ERROR_FAIL_I24,               EACCES    },  /* 83 */
        {  ERROR_INVALID_PARAMETER,      EINVAL    },  /* 87 */
        {  ERROR_NO_PROC_SLOTS,          EAGAIN    },  /* 89 */
        {  ERROR_DRIVE_LOCKED,           EACCES    },  /* 108 */
        {  ERROR_BROKEN_PIPE,            EPIPE     },  /* 109 */
        {  ERROR_DISK_FULL,              ENOSPC    },  /* 112 */
        {  ERROR_INVALID_TARGET_HANDLE,  EBADF     },  /* 114 */
        {  ERROR_INVALID_HANDLE,         EINVAL    },  /* 124 */
        {  ERROR_WAIT_NO_CHILDREN,       ECHILD    },  /* 128 */
        {  ERROR_CHILD_NOT_COMPLETE,     ECHILD    },  /* 129 */
        {  ERROR_DIRECT_ACCESS_HANDLE,   EBADF     },  /* 130 */
        {  ERROR_NEGATIVE_SEEK,          EINVAL    },  /* 131 */
        {  ERROR_SEEK_ON_DEVICE,         EACCES    },  /* 132 */
        {  ERROR_DIR_NOT_EMPTY,          ENOTEMPTY },  /* 145 */
        {  ERROR_NOT_LOCKED,             EACCES    },  /* 158 */
        {  ERROR_BAD_PATHNAME,           ENOENT    },  /* 161 */
        {  ERROR_MAX_THRDS_REACHED,      EAGAIN    },  /* 164 */
        {  ERROR_LOCK_FAILED,            EACCES    },  /* 167 */
        {  ERROR_ALREADY_EXISTS,         EEXIST    },  /* 183 */
        {  ERROR_FILENAME_EXCED_RANGE,   ENOENT    },  /* 206 */
        {  ERROR_NESTING_NOT_ALLOWED,    EAGAIN    },  /* 215 */
           /* Windows returns this when the read end of a pipe is
            * closed (or closing) and we write to it. */
        {  ERROR_NO_DATA,                EPIPE     },  /* 232 */
        {  ERROR_NOT_ENOUGH_QUOTA,       ENOMEM    }  /* 1816 */
};

/* size of the table */
#define ERRTABLESIZE (sizeof(errtable)/sizeof(errtable[0]))

/* The following two constants must be the minimum and maximum
   values in the (contiguous) range of Exec Failure errors. */
#define MIN_EXEC_ERROR ERROR_INVALID_STARTING_CODESEG
#define MAX_EXEC_ERROR ERROR_INFLOOP_IN_RELOC_CHAIN

/* These are the low and high value in the range of errors that are
   access violations */
#define MIN_EACCES_RANGE ERROR_WRITE_PROTECT
#define MAX_EACCES_RANGE ERROR_SHARING_BUFFER_EXCEEDED

void maperrno(void)
{
    errno = maperrno_func(GetLastError());
}

int maperrno_func(DWORD dwErrorCode)
{
    int i;

    /* check the table for the OS error code */
    for (i = 0; i < ERRTABLESIZE; ++i)
        if (dwErrorCode == errtable[i].oscode)
            return errtable[i].errnocode;

    /* The error code wasn't in the table.  We check for a range of */
    /* EACCES errors or exec failure errors (ENOEXEC).  Otherwise   */
    /* EINVAL is returned.                                          */

    if (dwErrorCode >= MIN_EACCES_RANGE && dwErrorCode <= MAX_EACCES_RANGE)
        return EACCES;
    else if (dwErrorCode >= MIN_EXEC_ERROR && dwErrorCode <= MAX_EXEC_ERROR)
        return ENOEXEC;
    else
        return EINVAL;
}

LPWSTR base_getErrorMessage(DWORD err)
{
    LPWSTR what;
    DWORD res;

    res = FormatMessageW(
              (FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_ALLOCATE_BUFFER),
              NULL,
              err,
              MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), /* Default language */
              (LPWSTR) &what,
              0,
              NULL
          );
    if (res == 0)
        return NULL;
    return what;
}

int get_unique_file_info_hwnd(HANDLE h, HsWord64 *dev, HsWord64 *ino)
{
    BY_HANDLE_FILE_INFORMATION info;

    if (GetFileInformationByHandle(h, &info))
    {
        *dev = info.dwVolumeSerialNumber;
        *ino = info.nFileIndexLow
             | ((HsWord64)info.nFileIndexHigh << 32);

        return 0;
    }

    return -1;
}

int get_unique_file_info(int fd, HsWord64 *dev, HsWord64 *ino)
{
    HANDLE h = (HANDLE)_get_osfhandle(fd);
    return get_unique_file_info_hwnd (h, dev, ino);
}

BOOL file_exists(LPCTSTR path)
{
    DWORD r = GetFileAttributes(path);
    return r != INVALID_FILE_ATTRIBUTES;
}

bool getTempFileNameErrorNo (wchar_t* pathName, wchar_t* prefix,
                             wchar_t* suffix, uint32_t uUnique,
                             wchar_t* tempFileName)
{
  if (!GetTempFileNameW(pathName, prefix, uUnique, tempFileName))
    {
      maperrno();
      return false;
    }

  wchar_t* drive = malloc (sizeof(wchar_t) * _MAX_DRIVE);
  wchar_t* dir   = malloc (sizeof(wchar_t) * _MAX_DIR);
  wchar_t* fname = malloc (sizeof(wchar_t) * _MAX_FNAME);
  bool success = true;
  if (_wsplitpath_s (tempFileName, drive, _MAX_DRIVE, dir, _MAX_DIR,
                     fname, _MAX_FNAME, NULL, 0) != 0)
    {
      success = false;
      maperrno ();
    }
  else
    {
      wchar_t* temp = _wcsdup (tempFileName);
      if (wcsnlen(drive, _MAX_DRIVE) == 0)
        swprintf_s(tempFileName, MAX_PATH, L"%s\%s%s",
                   dir, fname, suffix);
      else
        swprintf_s(tempFileName, MAX_PATH, L"%s\%s\%s%s",
                   drive, dir, fname, suffix);
      MoveFileW(temp, tempFileName);
      free(temp);
    }

  free(drive);
  free(dir);
  free(fname);

  return success;
}

/* This function converts Windows paths between namespaces. More specifically
   It converts an explorer style path into a NT or Win32 namespace.
   This has several caveats but they are caviats that are native to Windows and
   not POSIX. See
   https://msdn.microsoft.com/en-us/library/windows/desktop/aa365247.aspx.
   Anything else such as raw device paths we leave untouched.  The main benefit
   of doing any of this is that we can break the MAX_PATH restriction and also
   access raw handles that we couldn't before.  */
wchar_t* __base_create_device_name (const wchar_t* filename) {
  const wchar_t* win32_dev_namespace  = L"\\\\.\\";
  const wchar_t* win32_file_namespace = L"\\\\?\\";
  const wchar_t* nt_device_namespace  = L"\\Device\\";
  const wchar_t* unc_prefix           = L"UNC\\";
  const wchar_t* network_share        = L"\\\\";

  wchar_t* result = _wcsdup (filename);
  wchar_t ns[10] = {0};

  /* If the file is already in a native namespace don't change it.  */
  if (   wcsncmp (win32_dev_namespace , filename, 4) == 0
      || wcsncmp (win32_file_namespace, filename, 4) == 0
      || wcsncmp (nt_device_namespace , filename, 8) == 0)
    return result;

  /* Since we're using the lower level APIs we must normalize slashes now.  The
     Win32 API layer will no longer convert '/' into '\\' for us.  */
  for (size_t i = 0; i < wcslen (result); i++)
    {
      if (result[i] == L'/')
        result[i] = L'\\';
    }

  /* Now resolve any . and .. in the path or subsequent API calls may fail since
     Win32 will no longer resolve them.  */
  DWORD nResult = GetFullPathNameW (result, 0, NULL, NULL) + 1;
  wchar_t *temp = _wcsdup (result);
  result = malloc (nResult * sizeof (wchar_t));
  if (GetFullPathNameW (temp, nResult, result, NULL) == 0)
    {
      goto cleanup;
    }

  free (temp);

  if (wcsncmp (network_share, result, 2) == 0)
    {
      if (swprintf (ns, 10, L"%ls%ls", win32_file_namespace, unc_prefix) <= 0)
        {
          goto cleanup;
        }
    }
  else if (swprintf (ns, 10, L"%ls", win32_file_namespace) <= 0)
    {
      goto cleanup;
    }

  /* Create new string.  */
  int bLen = wcslen (result) + wcslen (ns) + 1;
  temp = _wcsdup (result);
  result = malloc (bLen * sizeof (wchar_t));
  if (swprintf (result, bLen, L"%ls%ls", ns, temp) <= 0)
    {
      goto cleanup;
    }

  free (temp);

  return result;

cleanup:
  free (temp);
  free (result);
  return NULL;
}
#endif

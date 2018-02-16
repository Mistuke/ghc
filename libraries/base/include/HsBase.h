/* -----------------------------------------------------------------------------
 *
 * (c) The University of Glasgow 2001-2004
 *
 * Definitions for package `base' which are visible in Haskell land.
 *
 * ---------------------------------------------------------------------------*/

#pragma once

#include "HsBaseConfig.h"

/* ultra-evil... */
#undef PACKAGE_BUGREPORT
#undef PACKAGE_NAME
#undef PACKAGE_STRING
#undef PACKAGE_TARNAME
#undef PACKAGE_VERSION

/* Needed to get the macro version of errno on some OSs (eg. Solaris).
   We must do this, because these libs are only compiled once, but
   must work in both single-threaded and multi-threaded programs. */
#define _REENTRANT 1

#include "HsFFI.h"

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#if HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#if HAVE_UNISTD_H
#include <unistd.h>
#endif
#if HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif
#if HAVE_FCNTL_H
# include <fcntl.h>
#endif
#if HAVE_TERMIOS_H
#include <termios.h>
#endif
#if HAVE_SIGNAL_H
#include <signal.h>
/* Ultra-ugly: OpenBSD uses broken macros for sigemptyset and sigfillset (missing casts) */
#if __OpenBSD__
#undef sigemptyset
#undef sigfillset
#endif
#endif
#if HAVE_ERRNO_H
#include <errno.h>
#endif
#if HAVE_STRING_H
#include <string.h>
#endif
#if HAVE_UTIME_H
#include <utime.h>
#endif
#if HAVE_SYS_UTSNAME_H
#include <sys/utsname.h>
#endif
#if HAVE_GETTIMEOFDAY
#  if HAVE_SYS_TIME_H
#   include <sys/time.h>
#  endif
#elif HAVE_GETCLOCK
# if HAVE_SYS_TIMERS_H
#  define POSIX_4D9 1
#  include <sys/timers.h>
# endif
#endif
#if HAVE_TIME_H
#include <time.h>
#endif
#if HAVE_SYS_TIMEB_H && !defined(__FreeBSD__)
#include <sys/timeb.h>
#endif
#if HAVE_WINDOWS_H
#include <windows.h>
#endif
#if HAVE_SYS_TIMES_H
#include <sys/times.h>
#endif
#if HAVE_WINSOCK_H && defined(_WIN32)
#include <winsock.h>
#endif
#if HAVE_LIMITS_H
#include <limits.h>
#endif
#if HAVE_WCTYPE_H
#include <wctype.h>
#endif
#if HAVE_INTTYPES_H
# include <inttypes.h>
#elif HAVE_STDINT_H
# include <stdint.h>
#endif
#if defined(HAVE_CLOCK_GETTIME)
# if defined(_POSIX_MONOTONIC_CLOCK)
#  define CLOCK_ID CLOCK_MONOTONIC
# else
#  define CLOCK_ID CLOCK_REALTIME
# endif
#elif defined(darwin_HOST_OS)
# include <mach/mach.h>
# include <mach/mach_time.h>
#endif

#if !defined(_WIN32)
# if HAVE_SYS_RESOURCE_H
#  include <sys/resource.h>
# endif
#endif

#if !HAVE_GETRUSAGE && HAVE_SYS_SYSCALL_H
# include <sys/syscall.h>
# if defined(SYS_GETRUSAGE)	/* hpux_HOST_OS */
#  define getrusage(a, b)  syscall(SYS_GETRUSAGE, a, b)
#  define HAVE_GETRUSAGE 1
# endif
#endif

/* For System */
#if HAVE_SYS_WAIT_H
#include <sys/wait.h>
#endif
#if HAVE_VFORK_H
#include <vfork.h>
#endif
#include "WCsubst.h"

#if defined(_WIN32)
/* in Win32Utils.c */
extern void maperrno (void);
extern int maperrno_func(DWORD dwErrorCode);
extern HsWord64 getMonotonicUSec(void);
#endif

#if defined(_WIN32)
#include <io.h>
#include <fcntl.h>
#include <shlobj.h>
#include <share.h>
#endif

#if HAVE_SYS_SELECT_H
#include <sys/select.h>
#endif

/* in inputReady.c */
extern int fdReady(int fd, bool write, int64_t msecs, bool isSock);

/* -----------------------------------------------------------------------------
   INLINE functions.

   These functions are given as inlines here for when compiling via C,
   but we also generate static versions into the cbits library for
   when compiling to native code.
   -------------------------------------------------------------------------- */

#if !defined(INLINE)
# if defined(_MSC_VER)
#  define INLINE extern __inline
# else
#  define INLINE static inline
# endif
#endif

INLINE int __hscore_get_errno(void) { return errno; }
INLINE void __hscore_set_errno(int e) { errno = e; }

INLINE HsInt
__hscore_bufsiz(void)
{
  return BUFSIZ;
}

INLINE int
__hscore_o_binary(void)
{
#if defined(_MSC_VER)
  return O_BINARY;
#else
  return CONST_O_BINARY;
#endif
}

INLINE int
__hscore_o_rdonly(void)
{
#if defined(O_RDONLY)
  return O_RDONLY;
#else
  return 0;
#endif
}

INLINE int
__hscore_o_wronly( void )
{
#if defined(O_WRONLY)
  return O_WRONLY;
#else
  return 0;
#endif
}

INLINE int
__hscore_o_rdwr( void )
{
#if defined(O_RDWR)
  return O_RDWR;
#else
  return 0;
#endif
}

INLINE int
__hscore_o_append( void )
{
#if defined(O_APPEND)
  return O_APPEND;
#else
  return 0;
#endif
}

INLINE int
__hscore_o_creat( void )
{
#if defined(O_CREAT)
  return O_CREAT;
#else
  return 0;
#endif
}

INLINE int
__hscore_o_excl( void )
{
#if defined(O_EXCL)
  return O_EXCL;
#else
  return 0;
#endif
}

INLINE int
__hscore_o_trunc( void )
{
#if defined(O_TRUNC)
  return O_TRUNC;
#else
  return 0;
#endif
}

INLINE int
__hscore_o_noctty( void )
{
#if defined(O_NOCTTY)
  return O_NOCTTY;
#else
  return 0;
#endif
}

INLINE int
__hscore_o_nonblock( void )
{
#if defined(O_NONBLOCK)
  return O_NONBLOCK;
#else
  return 0;
#endif
}

INLINE int
__hscore_ftruncate( int fd, off_t where )
{
#if defined(HAVE_FTRUNCATE)
  return ftruncate(fd,where);
#elif defined(HAVE__CHSIZE)
  return _chsize(fd,where);
#else
// ToDo: we should use _chsize_s() on Windows which allows a 64-bit
// offset, but it doesn't seem to be available from mingw at this time
// --SDM (01/2008)
#error at least ftruncate or _chsize functions are required to build
#endif
}

INLINE int
__hscore_setmode( int fd, HsBool toBin )
{
#if defined(_WIN32)
  return _setmode(fd,(toBin == HS_BOOL_TRUE) ? _O_BINARY : _O_TEXT);
#else
  return 0;
#endif
}

#if defined(_WIN32)
// We want the versions of stat/fstat/lseek that use 64-bit offsets,
// and you have to ask for those explicitly.  Unfortunately there
// doesn't seem to be a 64-bit version of truncate/ftruncate, so while
// hFileSize and hSeek will work with large files, hSetFileSize will not.
typedef struct _stati64 struct_stat;
typedef off64_t stsize_t;
#else
typedef struct stat struct_stat;
typedef off_t stsize_t;
#endif

INLINE HsInt
__hscore_sizeof_stat( void )
{
  return sizeof(struct_stat);
}

INLINE time_t __hscore_st_mtime ( struct_stat* st ) { return st->st_mtime; }
INLINE stsize_t __hscore_st_size  ( struct_stat* st ) { return st->st_size; }
#if !defined(_MSC_VER)
INLINE mode_t __hscore_st_mode  ( struct_stat* st ) { return st->st_mode; }
INLINE dev_t  __hscore_st_dev  ( struct_stat* st ) { return st->st_dev; }
INLINE ino_t  __hscore_st_ino  ( struct_stat* st ) { return st->st_ino; }
#endif

#if defined(_WIN32)
INLINE int __hscore_stat(wchar_t *file, struct_stat *buf) {
	return _wstati64(file,buf);
}

INLINE int __hscore_fstat(int fd, struct_stat *buf) {
	return _fstati64(fd,buf);
}
INLINE int __hscore_lstat(wchar_t *fname, struct_stat *buf )
{
	return _wstati64(fname,buf);
}
#else
INLINE int __hscore_stat(char *file, struct_stat *buf) {
	return stat(file,buf);
}

INLINE int __hscore_fstat(int fd, struct_stat *buf) {
	return fstat(fd,buf);
}

INLINE int __hscore_lstat( const char *fname, struct stat *buf )
{
#if HAVE_LSTAT
  return lstat(fname, buf);
#else
  return stat(fname, buf);
#endif
}
#endif

#if HAVE_TERMIOS_H
INLINE tcflag_t __hscore_lflag( struct termios* ts ) { return ts->c_lflag; }

INLINE void
__hscore_poke_lflag( struct termios* ts, tcflag_t t ) { ts->c_lflag = t; }

INLINE unsigned char*
__hscore_ptr_c_cc( struct termios* ts )
{ return (unsigned char*) &ts->c_cc; }

INLINE HsInt
__hscore_sizeof_termios( void )
{
#if !defined(_WIN32)
  return sizeof(struct termios);
#else
  return 0;
#endif
}
#endif

#if !defined(_WIN32)
INLINE HsInt
__hscore_sizeof_sigset_t( void )
{
  return sizeof(sigset_t);
}
#endif

INLINE int
__hscore_echo( void )
{
#if defined(ECHO)
  return ECHO;
#else
  return 0;
#endif

}

INLINE int
__hscore_tcsanow( void )
{
#if defined(TCSANOW)
  return TCSANOW;
#else
  return 0;
#endif

}

INLINE int
__hscore_icanon( void )
{
#if defined(ICANON)
  return ICANON;
#else
  return 0;
#endif
}

INLINE int __hscore_vmin( void )
{
#if defined(VMIN)
  return VMIN;
#else
  return 0;
#endif
}

INLINE int __hscore_vtime( void )
{
#if defined(VTIME)
  return VTIME;
#else
  return 0;
#endif
}

INLINE int __hscore_sigttou( void )
{
#if defined(SIGTTOU)
  return SIGTTOU;
#else
  return 0;
#endif
}

INLINE int __hscore_sig_block( void )
{
#if defined(SIG_BLOCK)
  return SIG_BLOCK;
#else
  return 0;
#endif
}

INLINE int __hscore_sig_setmask( void )
{
#if defined(SIG_SETMASK)
  return SIG_SETMASK;
#else
  return 0;
#endif
}

#if !defined(_WIN32)
INLINE size_t __hscore_sizeof_siginfo_t (void)
{
    return sizeof(siginfo_t);
}
#endif

INLINE int
__hscore_f_getfl( void )
{
#if defined(F_GETFL)
  return F_GETFL;
#else
  return 0;
#endif
}

INLINE int
__hscore_f_setfl( void )
{
#if defined(F_SETFL)
  return F_SETFL;
#else
  return 0;
#endif
}

INLINE int
__hscore_f_setfd( void )
{
#if defined(F_SETFD)
  return F_SETFD;
#else
  return 0;
#endif
}

INLINE long
__hscore_fd_cloexec( void )
{
#if defined(FD_CLOEXEC)
  return FD_CLOEXEC;
#else
  return 0;
#endif
}

// defined in rts/RtsStartup.c.
extern void* __hscore_get_saved_termios(int fd);
extern void __hscore_set_saved_termios(int fd, void* ts);

#if defined(_WIN32)
/* This function converts Windows paths between namespaces. More specifically
   It converts an explorer style path into a NT or Win32 namespace.
   This has several caveats but they are caviats that are native to Windows and
   not POSIX. See
   https://msdn.microsoft.com/en-us/library/windows/desktop/aa365247.aspx.
   Anything else such as raw device paths we leave untouched.  The main benefit
   of doing any of this is that we can break the MAX_PATH restriction and also
   access raw handles that we couldn't before.  */
INLINE wchar_t* __hs_create_device_name (const wchar_t* filename) {
  const wchar_t* nt_namespace     = L"\\\\.\\";
  const wchar_t* win32_namespace  = L"\\\\?\\";
  const wchar_t* device_namespace = L"\\Device\\";
  const wchar_t* unc_prefix       = L"UNC\\";
  const wchar_t* network_share    = L"\\\\";

  wchar_t* result = _wcsdup (filename);
  wchar_t ns[10] = {0};

  /* If the file is already in a native namespace don't change it.  */
  if (   wcsncmp (nt_namespace, filename, 4) == 0
      || wcsncmp (win32_namespace, filename, 4) == 0
      || wcsncmp (device_namespace, filename, 8) == 0)
    return result;

    /* Since we're using the lower level APIs we must normalize slashes now.  The
     Win32 API layer will no longer convert '/' into '\\' for us.  */
  for (int i = 0; i < wcslen (result); i++)
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
      if (swprintf (ns, 10, L"%ls%ls", win32_namespace, unc_prefix) <= 0)
        {
          goto cleanup;
        }
    }
  else if (swprintf (ns, 10, L"%ls", win32_namespace) <= 0)
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
  maperrno ();
  free (temp);
  free (result);
  return NULL;
}

#define HAS_FLAG(a,b) ((a & b) == b)

INLINE int __hs_sopen (const wchar_t* filename, int oflag,
                       int shflag, int pmode) {
  /* Construct access mode.  */
  DWORD dwDesiredAccess = 0;
  if (HAS_FLAG (oflag, _O_RDONLY))
    dwDesiredAccess |= GENERIC_READ | FILE_READ_DATA | FILE_READ_ATTRIBUTES |
                       FILE_WRITE_ATTRIBUTES;;
  if (HAS_FLAG (oflag, _O_RDWR))
    dwDesiredAccess |= GENERIC_WRITE | GENERIC_READ | FILE_READ_DATA |
                       FILE_WRITE_DATA | FILE_READ_ATTRIBUTES |
                       FILE_WRITE_ATTRIBUTES;
  if (HAS_FLAG (oflag,  _O_WRONLY))
    dwDesiredAccess|= GENERIC_WRITE | FILE_WRITE_DATA |
                      FILE_READ_ATTRIBUTES | FILE_WRITE_ATTRIBUTES;

  /* Construct shared mode.  */
  DWORD dwShareMode = FILE_SHARE_DELETE | FILE_SHARE_READ | FILE_SHARE_WRITE;
  if (HAS_FLAG (shflag, _SH_DENYRW))
    dwShareMode &= ~(FILE_SHARE_READ | FILE_SHARE_WRITE);
  if (HAS_FLAG (shflag, _SH_DENYWR))
    dwShareMode &= ~FILE_SHARE_WRITE;
  if (HAS_FLAG (shflag, _SH_DENYRD))
    dwShareMode &= ~FILE_SHARE_READ;
  if (HAS_FLAG (pmode, _S_IWRITE))
    dwShareMode |= FILE_SHARE_READ | FILE_SHARE_WRITE;
  if (HAS_FLAG (pmode, _S_IREAD))
    dwShareMode |= FILE_SHARE_READ;

  /* Override access mode with pmode if creating file.  */
  if (HAS_FLAG (oflag, _O_CREAT))
    {
      if (HAS_FLAG (pmode, _S_IWRITE))
        dwDesiredAccess |= FILE_GENERIC_WRITE;
      if (HAS_FLAG (pmode, _S_IREAD))
        dwDesiredAccess |= FILE_GENERIC_READ;
    }

  /* Create file disposition.  */
  DWORD dwCreationDisposition = OPEN_EXISTING;
  if (HAS_FLAG (oflag, _O_CREAT))
    dwCreationDisposition = OPEN_ALWAYS;
  if (HAS_FLAG (oflag, (_O_CREAT | _O_EXCL)))
    dwCreationDisposition = CREATE_NEW;
  if (HAS_FLAG (oflag, _O_TRUNC) && !HAS_FLAG (oflag, _O_CREAT))
    dwCreationDisposition = TRUNCATE_EXISTING;

  /* Set file access attributes.  */
  DWORD dwFlagsAndAttributes = FILE_ATTRIBUTE_NORMAL;
  if (HAS_FLAG (oflag, _O_RDONLY))
    dwFlagsAndAttributes |= 0; /* No special attribute.  */
  if (HAS_FLAG (oflag, (_O_CREAT | _O_TEMPORARY)))
    dwFlagsAndAttributes |= FILE_FLAG_DELETE_ON_CLOSE;
  if (HAS_FLAG (oflag, (_O_CREAT | _O_SHORT_LIVED)))
    dwFlagsAndAttributes |= FILE_ATTRIBUTE_TEMPORARY;
  if (HAS_FLAG (oflag, _O_RANDOM))
    dwFlagsAndAttributes |= FILE_FLAG_RANDOM_ACCESS;
  if (HAS_FLAG (oflag, _O_SEQUENTIAL))
    dwFlagsAndAttributes |= FILE_FLAG_SEQUENTIAL_SCAN;
  /* Flag is only valid on it's own.  */
  if (dwFlagsAndAttributes != FILE_ATTRIBUTE_NORMAL)
    dwFlagsAndAttributes &= ~FILE_ATTRIBUTE_NORMAL;

  /* Set security attributes.  */
  SECURITY_ATTRIBUTES securityAttributes;
  ZeroMemory (&securityAttributes, sizeof(SECURITY_ATTRIBUTES));
  securityAttributes.bInheritHandle       = !(oflag & _O_NOINHERIT);
  securityAttributes.lpSecurityDescriptor = NULL;
  securityAttributes.nLength              = sizeof(SECURITY_ATTRIBUTES);

  wchar_t* _filename = __hs_create_device_name (filename);
  if (!_filename)
    return -1;

  HANDLE hResult
    = CreateFileW (_filename, dwDesiredAccess, dwShareMode, &securityAttributes,
                   dwCreationDisposition, dwFlagsAndAttributes, NULL);
  free (_filename);
  if (INVALID_HANDLE_VALUE == hResult)
    {
      maperrno ();
      return -1;
    }

  /* Now we have a Windows handle, we have to convert it to an FD and apply
     the remaining flags.  */
  const int flag_mask = _O_APPEND | _O_RDONLY | _O_TEXT | _O_WTEXT;
  int fd = _open_osfhandle ((intptr_t)hResult, oflag & flag_mask);
  if (-1 == fd)
    {
      maperrno();
      return -1;
    }

  /* Finally we can change the mode to the requested one.  */
  const int mode_mask = _O_TEXT | _O_BINARY | _O_U16TEXT | _O_U8TEXT | _O_WTEXT;
  if ((oflag & mode_mask) && (-1 == _setmode (fd, oflag & mode_mask)))
    {
      maperrno();
      return -1;
    }

  return fd;
}

INLINE int __hscore_open(wchar_t *file, int how, mode_t mode) {
	if ((how & O_WRONLY) || (how & O_RDWR) || (how & O_APPEND))
	  return __hs_sopen(file,how | _O_NOINHERIT,_SH_DENYNO,mode);
          // _O_NOINHERIT: see #2650
	else
	  return __hs_sopen(file,how | _O_NOINHERIT,_SH_DENYNO,mode);
          // _O_NOINHERIT: see #2650
}
#else
INLINE int __hscore_open(char *file, int how, mode_t mode) {
	return open(file,how,mode);
}
#endif

#if darwin_HOST_OS
// You should not access _environ directly on Darwin in a bundle/shared library.
// See #2458 and http://developer.apple.com/library/mac/#documentation/Darwin/Reference/ManPages/man7/environ.7.html
#include <crt_externs.h>
INLINE char **__hscore_environ(void) { return *(_NSGetEnviron()); }
#else
/* ToDo: write a feature test that doesn't assume 'environ' to
 *    be in scope at link-time. */
extern char** environ;
INLINE char **__hscore_environ(void) { return environ; }
#endif

/* lossless conversions between pointers and integral types */
INLINE void *    __hscore_from_uintptr(uintptr_t n) { return (void *)n; }
INLINE void *    __hscore_from_intptr (intptr_t n)  { return (void *)n; }
INLINE uintptr_t __hscore_to_uintptr  (void *p)     { return (uintptr_t)p; }
INLINE intptr_t  __hscore_to_intptr   (void *p)     { return (intptr_t)p; }

void errorBelch2(const char*s, char *t);
void debugBelch2(const char*s, char *t);

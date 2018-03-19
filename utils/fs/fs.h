/* -----------------------------------------------------------------------------
 *
 * (c) Tamar Christina 2018
 *
 * Windows I/O routines for file opening.
 *
 * ---------------------------------------------------------------------------*/

#pragma once

#if defined(_WIN32)
#include <wchar.h>

int __hs_swopen (const wchar_t* filename, int oflag,
                       int shflag, int pmode);
FILE *__hs_fwopen (const wchar_t* filename, const wchar_t* mode);
FILE *__hs_fopen (const char* filename, const char* mode);
#else

FILE *__hs_fopen (const char* filename, const char* mode);
#endif
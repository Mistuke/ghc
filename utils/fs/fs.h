/* -----------------------------------------------------------------------------
 *
 * (c) Tamar Christina 2018
 *
 * Windows I/O routines for file opening.
 *
 * NOTE: Only modify this file in utils/fs/ and rerun configure. Do not edit
 *       this file in any other directory as it will be overwritten.
 *
 * ---------------------------------------------------------------------------*/

#pragma once

#include <stdio.h>
#define WEAK_SYM __attribute__ ((weak))

#if defined(_WIN32)
#include <wchar.h>

WEAK_SYM int __hs_swopen (const wchar_t* filename, int oflag,
                          int shflag, int pmode);
WEAK_SYM FILE *__hs_fwopen (const wchar_t* filename, const wchar_t* mode);
WEAK_SYM FILE *__hs_fopen (const char* filename, const char* mode);
#else

WEAK_SYM FILE *__hs_fopen (const char* filename, const char* mode);
#endif

#undef WEAK_SYM

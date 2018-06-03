/* AsyncIO.h
 *
 * Integrating Win32 asynchronous I/O with the GHC RTS.
 *
 * (c) sof, 2002-2003.
 *
 * NOTE: This is the WinIO manager, only used for --io-manager=native.
 *       For the MIO manager see AsyncIO.h.
 */

#pragma once

#include "Rts.h"
#include <stdbool.h>

extern bool  startupAsyncWinIO(void);
extern void shutdownAsyncWinIO(bool wait_threads);

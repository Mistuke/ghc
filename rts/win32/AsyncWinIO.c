/* AsyncIO.h
 *
 * Integrating Win32 asynchronous IOCP with the GHC RTS.
 *
 * (c) Tamar Christina, 2018
 *
 * NOTE: This is the WinIO manager, only used for --io-manager=native.
 *       For the MIO manager see AsyncIO.h.
 */

#include "Rts.h"
#include <rts/IOManager.h>
#include "AsyncWinIO.h"
#include "Prelude.h"
#include "Capability.h"

#include <stdbool.h>
#include <windows.h>
#include <stdint.h>
#include <stdio.h>

static HANDLE completionPortHandle = INVALID_HANDLE_VALUE;
uint64_t outstanding_requests = 0;
uint64_t completed_requests = 0;
bool running = false;
DWORD timeout = INFINITE;
DWORD WINAPI runner (LPVOID lpParam);
HANDLE workerThread = NULL;
DWORD workerThreadId = 0;

SRWLOCK lock;
CONDITION_VARIABLE wakeEvent;

uint32_t num_callbacks = 32;
OVERLAPPED_ENTRY *entries;
uint32_t num_last_completed;

static void notifyRtsOfFinishedCall (uint32_t num);

bool startupAsyncWinIO(void)
{
  running = true;

  InitializeSRWLock (&lock);
  InitializeConditionVariable (&wakeEvent);

  entries = calloc (sizeof (OVERLAPPED_ENTRY), num_callbacks);

  /* Start the I/O manager before creating the worker thread to prevent a busy
     wait or spin-lock.  */
  ioManagerStart ();

  workerThread = CreateThread (NULL, 0, runner, NULL, 0, &workerThreadId);
  if (!workerThread)
    barf ("could not create I/O manager thread.");
  return true;
}

void shutdownAsyncWinIO(bool wait_threads)
{
  if (workerThread != NULL)
    {
      if (wait_threads)
        {
          AcquireSRWLockExclusive (&lock);

          running = false;
          PostQueuedCompletionStatus (completionPortHandle, 0, 0, NULL);
          WakeConditionVariable (&wakeEvent);

          ReleaseSRWLockExclusive (&lock);
        }
      completionPortHandle = INVALID_HANDLE_VALUE;
      workerThread = NULL;
      workerThreadId = 0;
      free (entries);
      entries = NULL;
    }

  ioManagerDie ();
}

void registerNewIOCPHandle (HANDLE port)
{
  fprintf (stderr, "$ registerNewIOCPHandle: %p\n", port);
  AcquireSRWLockExclusive (&lock);

  completionPortHandle = port;

  ReleaseSRWLockExclusive (&lock);
}
void registerAlertableWait (HANDLE port, DWORD mssec)
{
  fprintf (stderr, "$ registerAlertableWait: %p (%ld)\n", port, mssec);
  bool interrupt = false;
  bool wakeup = false;
  AcquireSRWLockExclusive (&lock);

  outstanding_requests++;
  if (timeout > mssec)
    {
      timeout = mssec;
      interrupt = outstanding_requests > 1;
    }

  /* Decide if we may have to wake up the I/O manager.  */
  wakeup = outstanding_requests == 1;

  ReleaseSRWLockExclusive (&lock);

  if (interrupt)
    PostQueuedCompletionStatus (port, 0, 0, NULL);
  if (wakeup)
    WakeConditionVariable (&wakeEvent);
}

OVERLAPPED_ENTRY* getOverlappedEntries (uint32_t *num)
{
  *num = num_last_completed;
  return entries;
}

static void notifyRtsOfFinishedCall (uint32_t num)
{
  fprintf (stderr, "$ (0x%u) notifyRtsOfFinishedCall: %d\n", GetCurrentThreadId (), num);
  num_last_completed = num;
#if !defined(THREADED_RTS)
  Capability *cap = &MainCapability;
  StgTSO * tso = createStrictIOThread (cap, RtsFlags.GcFlags.initialStkSize,
                                       processRemoteCompletion_closure);
  scheduleThread (cap, tso);
#endif
  fprintf (stderr, "$ rts notified of %d completions.\n", num);
}

DWORD WINAPI runner (LPVOID lpParam STG_UNUSED)
{
  while (running)
    {
      AcquireSRWLockExclusive (&lock);

      while (completionPortHandle == INVALID_HANDLE_VALUE || outstanding_requests == 0)
        {
          fprintf (stderr, "$ runner:SleepConditionVariableSRW\n");
          SleepConditionVariableSRW (&wakeEvent, &lock, INFINITE, 0);
        }

      ReleaseSRWLockExclusive (&lock);
      fprintf (stderr, "$ runner:GetQueuedCompletionStatusEx\n");

      ULONG num_removed = -1;
      if (GetQueuedCompletionStatusEx (completionPortHandle, entries,
                                       num_callbacks, &num_removed, timeout,
                                       false))
        {
          notifyRtsOfFinishedCall (num_removed);
          if (num_removed == num_callbacks)
            {
              num_callbacks *= 2;
              OVERLAPPED_ENTRY *new
                = realloc (entries, sizeof (OVERLAPPED_ENTRY) * num_callbacks);
              if (new)
                entries = new;
            }
        }
      else if (WAIT_TIMEOUT == GetLastError ())
        {
          num_removed = 0;
        notifyRtsOfFinishedCall (num_removed);
        }

      AcquireSRWLockExclusive (&lock);

      if (num_removed > 0)
        outstanding_requests -= num_removed;
      if (outstanding_requests == 0)
        timeout = INFINITE;

      fprintf (stderr, "$ runner:running: %d, num_removed: %ld, oustanding: %lld, timeout: 0x%lx\n",
               running, num_removed, outstanding_requests, timeout);

      if (!running)
        ExitThread (0);

      ReleaseSRWLockExclusive (&lock);
    }
    return 0;
}
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
#include "Schedule.h"

#include <stdbool.h>
#include <windows.h>
#include <stdint.h>
#include <stdio.h>

static HANDLE completionPortHandle = INVALID_HANDLE_VALUE;
uint64_t outstanding_requests = 0;
uint64_t completed_requests = 0;
bool running = false;
bool outstanding_service_requests = false;
bool queue_full = false;
DWORD timeout = INFINITE;
DWORD WINAPI runner (LPVOID lpParam);
HANDLE workerThread = NULL;
DWORD workerThreadId = 0;

SRWLOCK lock;
CONDITION_VARIABLE wakeEvent;
HsWord32 lastEvent = 0;

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
          ioManagerWakeup ();
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
void registerAlertableWait (HANDLE port, DWORD mssec, uint64_t num_req)
{
  fprintf (stderr, "$ registerAlertableWait: %p (%ld) %lld\n", port, mssec, num_req);
  bool interrupt = false;
  bool wakeup = false;
  AcquireSRWLockExclusive (&lock);

  /* Decide if we may have to wake up the I/O manager.  */
  wakeup = outstanding_requests == 0; // && num_req > 0;

  outstanding_requests = num_req;
  if (timeout > mssec)
    {
      timeout = mssec;
      interrupt = outstanding_requests > 1;
    }

  ReleaseSRWLockExclusive (&lock);

  if (wakeup)
    WakeConditionVariable (&wakeEvent);
  else if (interrupt)
    PostQueuedCompletionStatus (port, 0, 0, NULL);
}

OVERLAPPED_ENTRY* getOverlappedEntries (uint32_t *num)
{
  *num = num_last_completed;
  return entries;
}

static void notifyRtsOfFinishedCall (uint32_t num)
{
  fprintf (stderr, "$ (0x%lu) notifyRtsOfFinishedCall: %d\n", GetCurrentThreadId (), num);
  num_last_completed = num;
#if !defined(THREADED_RTS)
  Capability *cap = &MainCapability;
  StgTSO * tso = createStrictIOThread (cap, RtsFlags.GcFlags.initialStkSize,
                                       processRemoteCompletion_closure);
  AcquireSRWLockExclusive (&lock);
  outstanding_service_requests = true;
  ReleaseSRWLockExclusive (&lock);

  scheduleThread (cap, tso);
#endif
  fprintf (stderr, "$ rts notified of %d completions.\n", num);
}

void servicedIOEntries (uint64_t remaining)
{
  fprintf (stderr, "$ runner:servicedIOEntries %lld\n", remaining);

  AcquireSRWLockExclusive (&lock);

  outstanding_requests = remaining;
  if (outstanding_requests <= 0)
    timeout = INFINITE;
  outstanding_service_requests = false;

  if (queue_full)
  {
    num_callbacks *= 2;
    OVERLAPPED_ENTRY *new
      = realloc (entries,
                  sizeof (OVERLAPPED_ENTRY) * num_callbacks);
    if (new)
      entries = new;
  }

  ReleaseSRWLockExclusive (&lock);

  WakeConditionVariable (&wakeEvent);
}
DWORD WINAPI runner (LPVOID lpParam STG_UNUSED)
{
  while (running)
    {
      AcquireSRWLockExclusive (&lock);

      lastEvent = readIOManagerEvent ();
      while (completionPortHandle == INVALID_HANDLE_VALUE
             || lastEvent == IO_MANAGER_DIE
             || outstanding_service_requests)
        {
          fprintf (stderr, "$ runner:SleepConditionVariableSRW\n");
          SleepConditionVariableSRW (&wakeEvent, &lock, INFINITE, 0);
          HsWord32 nextEvent = readIOManagerEvent ();
          lastEvent = nextEvent ? nextEvent : lastEvent;
        }

      ReleaseSRWLockExclusive (&lock);
      fprintf (stderr, "$ runner:GetQueuedCompletionStatusEx\n");

      ULONG num_removed = -1;
      if (GetQueuedCompletionStatusEx (completionPortHandle, entries,
                                       num_callbacks, &num_removed, timeout,
                                       false))
        {
          if (num_removed > 0)
            {
              notifyRtsOfFinishedCall (num_removed);
              queue_full = num_removed == num_callbacks;
            }
        }
      else if (WAIT_TIMEOUT == GetLastError ())
        {
          num_removed = 0;
          notifyRtsOfFinishedCall (num_removed);
        }

      AcquireSRWLockExclusive (&lock);

      fprintf (stderr, "$ runner:running: %d, num_removed: %ld, oustanding: %lld, timeout: 0x%lx\n",
               running, num_removed, outstanding_requests, timeout);

      if (!running)
        ExitThread (0);

      ReleaseSRWLockExclusive (&lock);
    }
    return 0;
}
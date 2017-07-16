/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 1998-2000
 *
 * Main function for a standalone Haskell program.
 *
 * ---------------------------------------------------------------------------*/

#define COMPILING_RTS_MAIN

#include "PosixSource.h"
#include "Rts.h"
#include "RtsAPI.h"

#include "RtsUtils.h"
#include "Prelude.h"
#include "Task.h"
#include "Excn.h"

#if defined(DEBUG)
# include "Printer.h"   /* for printing        */
#endif

// Hack: we assume that we're building a batch-mode system unless
// INTERPRETER is set

#if !defined(INTERPRETER) /* Hack */

// The rts entry point from a compiled program using a Haskell main
// function.  This gets called from a tiny main function generated by
// GHC and linked into each compiled Haskell program that uses a
// Haskell main function.
//
// We expect the caller to pass ZCMain_main_closure for
// main_closure. The reason we cannot refer to this symbol directly
// is because we're inside the rts and we do not know for sure that
// we'll be using a Haskell main function.
//
// NOTE: This function is marked as _noreturn_ in Main.h

int hs_main ( int argc, char *argv[],       // program args
              StgClosure *main_closure,     // closure for Main.main
              RtsConfig rts_config)         // RTS configuration

{
    //BEGIN_WINDOWS_VEH_HANDLER

    int exit_status;
    SchedulerStatus status;

    hs_init_ghc(&argc, &argv, rts_config);

    // kick off the computation by creating the main thread with a pointer
    // to mainIO_closure representing the computation of the overall program;
    // then enter the scheduler with this thread and off we go;
    //
    // in a parallel setup, where we have many instances of this code
    // running on different PEs, we should do this only for the main PE
    // (IAmMainThread is set in startupHaskell)

    // ToDo: want to start with a larger stack size
    {
        Capability *cap = rts_lock();
        rts_evalLazyIO(&cap, main_closure, NULL);
        status = rts_getSchedStatus(cap);
        rts_unlock(cap);
    }

    // check the status of the entire Haskell computation
    switch (status) {
    case Killed:
        errorBelch("main thread exited (uncaught exception)");
        exit_status = EXIT_KILLED;
        break;
    case Interrupted:
        errorBelch("interrupted");
        exit_status = EXIT_INTERRUPTED;
        break;
    case HeapExhausted:
        exit_status = EXIT_HEAPOVERFLOW;
        break;
    case Success:
        exit_status = EXIT_SUCCESS;
        break;
    default:
        barf("main thread completed with invalid status");
    }

    //END_WINDOWS_VEH_HANDLER

    shutdownHaskellAndExit(exit_status, 0 /* !fastExit */);
    // No code beyond this point. Dead code elimination will remove it
}
# endif /* BATCH_MODE */

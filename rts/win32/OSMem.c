/* -----------------------------------------------------------------------------
 *
 * (c) The University of Glasgow 2006-2007
 *
 * OS-specific memory management, Windows now uses a Two Level Segregated
 * pooled memory manager to more dynamically address the modern needs of GHC.
 *
 * ---------------------------------------------------------------------------*/

#include "Rts.h"
#include "sm/OSMem.h"
#include "sm/HeapAlloc.h"
#include "RtsUtils.h"
#include "winmem.h"

#include <windows.h>

/* Mingw-w64 does not currently have this in their header. So we have to import it.*/
typedef LPVOID(WINAPI *VirtualAllocExNumaProc)(HANDLE, LPVOID, SIZE_T, DWORD, DWORD, DWORD);

/* Cache NUMA API call. */
VirtualAllocExNumaProc VirtualAllocExNuma;

void
osMemInit(void)
{
    /* Make sure Memory manager has been initialized.  */
    winmem_init ();

    /* Resolve and cache VirtualAllocExNuma. */
    if (osNumaAvailable() && RtsFlags.GcFlags.numa)
    {
        VirtualAllocExNuma = (VirtualAllocExNumaProc)GetProcAddress(GetModuleHandleW(L"kernel32"), "VirtualAllocExNuma");
        if (!VirtualAllocExNuma)
        {
            sysErrorBelch(
                "osBindMBlocksToNode: VirtualAllocExNuma does not exist. How did you get this far?");
        }
    }
}

void *
osGetMBlocks(uint32_t n) {
    void* ret;
    uint32_t size = (n+1) * MBLOCK_SIZE; // TODO: stop wasting this.
    ret = winmem_malloc (WriteAccess, size);
    ret = MBLOCK_ROUND_UP (ret);

    if (ret && ((W_)ret & MBLOCK_MASK) != 0) {
        barf("getMBlocks: misaligned block returned");
    }

    return ret;
}

void osFreeMBlocks(void *addr, uint32_t n)
{
    (void)n;
    winmem_free (WriteAccess, addr);
}

void osReleaseFreeMemory(void)
{
    /* This is done automatically by the TLSF2 allocator.
       So no code needed here.  */
}

void
osFreeAllMBlocks(void)
{
    /* This one I don't quite get.  Presumably it's only called when we're not
       blocked on a foreign call.  But if we're terminating, then why leave the
       heap around? Surely the OS will clean it up regardless of the foreign
       call finished or not? So shouldn't the entire RTS not exit with
       outstanding foreign calls?

       Supporting this means I have to separate out memory allocated from this
       file. Which is possible, but not ideal.  */
}

size_t getPageSize (void)
{
    static size_t pagesize = 0;

    if (pagesize == 0) {
        SYSTEM_INFO sSysInfo;
        GetSystemInfo(&sSysInfo);
        pagesize = sSysInfo.dwPageSize;
    }

    return pagesize;
}

/* Returns 0 if physical memory size cannot be identified */
StgWord64 getPhysicalMemorySize (void)
{
    static StgWord64 physMemSize = 0;
    if (!physMemSize) {
        MEMORYSTATUSEX status;
        status.dwLength = sizeof(status);
        if (!GlobalMemoryStatusEx(&status)) {
#if defined(DEBUG)
            errorBelch("warning: getPhysicalMemorySize: cannot get physical "
                       "memory size");
#endif
            return 0;
        }
        physMemSize = status.ullTotalPhys;
    }
    return physMemSize;
}

void setExecutable (void *p, W_ len, bool exec)
{
    (void)len; (void)exec;
    sysErrorBelch("setExecutable: should not be called. use winmem_malloc to"
                  " allocate executable memory directly.");
    if (p)
        stg_exit(EXIT_FAILURE);
}

#if defined(USE_LARGE_ADDRESS_SPACE)

static void* heap_base = NULL;

void *osReserveHeapMemory (void *startAddress, W_ *len)
{
    void *start;

    heap_base = VirtualAlloc(startAddress, *len + MBLOCK_SIZE,
                              MEM_RESERVE, PAGE_READWRITE);
    if (heap_base == NULL) {
        if (GetLastError() == ERROR_NOT_ENOUGH_MEMORY) {
            errorBelch("out of memory");
        } else {
            sysErrorBelch(
                "osReserveHeapMemory: VirtualAlloc MEM_RESERVE %llu bytes \
                at address %p bytes failed",
                len + MBLOCK_SIZE, startAddress);
        }
        stg_exit(EXIT_FAILURE);
    }

    // VirtualFree MEM_RELEASE must always match a
    // previous MEM_RESERVE call, in address and size
    // so we necessarily leak some address space here,
    // before and after the aligned area
    // It is not a huge problem because we never commit
    // that memory
    start = MBLOCK_ROUND_UP(heap_base);

    return start;
}

void osCommitMemory (void *at, W_ size)
{
    void *temp;
    temp = VirtualAlloc(at, size, MEM_COMMIT, PAGE_READWRITE);
    if (temp == NULL) {
        sysErrorBelch("osCommitMemory: VirtualAlloc MEM_COMMIT failed");
        stg_exit(EXIT_FAILURE);
    }
}

void osDecommitMemory (void *at, W_ size)
{
    if (!VirtualFree(at, size, MEM_DECOMMIT)) {
        sysErrorBelch("osDecommitMemory: VirtualFree MEM_DECOMMIT failed");
        stg_exit(EXIT_FAILURE);
    }
}

void osReleaseHeapMemory (void)
{
    VirtualFree(heap_base, 0, MEM_RELEASE);
}

#endif

bool osNumaAvailable(void)
{
    return osNumaNodes() > 1;
}

uint32_t osNumaNodes(void)
{
    /* Cache the amount of NUMA values. */
    static ULONG numNumaNodes = 0;

    /* Cache the amount of NUMA nodes. */
    if (!numNumaNodes && !GetNumaHighestNodeNumber(&numNumaNodes))
    {
        numNumaNodes = 1;
    }

    return numNumaNodes;
}

uint64_t osNumaMask(void)
{
    uint64_t numaMask;
    if (!GetNumaNodeProcessorMask(0, &numaMask))
    {
        return 1;
    }
    return numaMask;
}

void osBindMBlocksToNode(
    void *addr,
    StgWord size,
    uint32_t node)
{
    if (osNumaAvailable())
    {
        void* temp;
        if (RtsFlags.GcFlags.numa) {
            /* Note [base memory]
               I would like to use addr here to specify the base
               memory of allocation. The problem is that the address
               we are requesting is too high. I can't figure out if it's
               because of my NUMA-emulation or a bug in the code.

               On windows also -xb is broken, it does nothing so that can't
               be used to tweak it (see #12577). So for now, just let the OS decide.
            */
            temp = VirtualAllocExNuma(
                          GetCurrentProcess(),
                          NULL, // addr? See base memory
                          size,
                          MEM_RESERVE | MEM_COMMIT,
                          PAGE_READWRITE,
                          node
                        );

            if (!temp) {
                if (GetLastError() == ERROR_NOT_ENOUGH_MEMORY) {
                    errorBelch("out of memory");
                }
                else {
                    sysErrorBelch(
                        "osBindMBlocksToNode: VirtualAllocExNuma MEM_RESERVE %" FMT_Word " bytes "
                        "at address %p bytes failed",
                                        size, addr);
                }
                stg_exit(EXIT_FAILURE);
            }
        }
    }
}

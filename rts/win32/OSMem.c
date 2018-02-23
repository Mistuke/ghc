/* -----------------------------------------------------------------------------
 *
 * (c) The University of Glasgow 2006-2007
 *
 * OS-specific memory management, Windows now uses a Two Level Segregated
 * pooled memory manager to more dynamically address the modern needs of GHC.
 * Our pools are 1mb in size so we get 32 allocation units, which should fit
 * approx 65 MBlocks before requiring another call to the OS.
 *
 * Since we only allocate MBlocks here we will waste memory only once per pool
 * when we align the pool itself.
 *
 * ---------------------------------------------------------------------------*/

#include "Rts.h"
#include "sm/OSMem.h"
#include "sm/HeapAlloc.h"
#include "RtsUtils.h"
#include "tlsf.h"

#include <windows.h>

/* Mingw-w64 does not currently have this in their header. So we have to import it.*/
typedef LPVOID(WINAPI *VirtualAllocExNumaProc)(HANDLE, LPVOID, SIZE_T, DWORD, DWORD, DWORD);

/* Cache NUMA API call. */
VirtualAllocExNumaProc VirtualAllocExNuma;

/* Reserve 2mb at a time, which fits ~20 MBlocks + Overhead bits on average.
   If adjusting this number in the future please keep alignment + overhead in
   mind.  The overhead for TLSF2 is POOL_OVERHEAD + TLSF_SIZE and BLOCK_OVERHEAD
   for each block.  */
static size_t default_blocks_allocate = 5;
static uint32_t default_protection    = PAGE_READWRITE;

static tlsf_t m_alloc;

static size_t getAllocationSize (void) {
  static size_t allocsize = 0;

  if (allocsize == 0) {
      SYSTEM_INFO sSysInfo;
      GetSystemInfo(&sSysInfo);
      allocsize = sSysInfo.dwAllocationGranularity;
  }

  return allocsize;
}

static void* osmem_cback_map (size_t* size, void* user) {
  (void)user;
  size_t allocsize = getAllocationSize ();
  size_t m_size = allocsize * default_blocks_allocate;
  if (m_size < *size)
  {
    m_size = *size;
    /* Now round up to next page size to keep aligned to the page boundary.  */
    size_t pages    = m_size / allocsize;
    size_t overflow = pages * allocsize;
    if (m_size > overflow)
      pages++;
    m_size = pages * allocsize;
  }

  void* cache
    = VirtualAlloc (NULL, m_size, MEM_COMMIT | MEM_RESERVE, default_protection);
  *size = m_size;
  if (!cache)
    barf ("Could not allocate any more memory from OS in OSMem.c.");
  return cache;
}

static void osmem_cback_unmap (void* mem, size_t size, void* user) {
  (void)user;
  VirtualFree (mem, 0, (uint32_t)size);
}

void
osMemInit(void) {
    /* Make sure Memory manager has been initialized.  */
    if (!m_alloc)
      m_alloc = tlsf_create (osmem_cback_map, osmem_cback_unmap, NULL);

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
    /* TLSF2 won't solve the fragmentation issues here. The alignment
       requirements are so huge that we will always waste large blocks.  But
       at least down calls to the OS should be less and should be faster than
       the buddy allocator used before.  */
    uint32_t size = (n+1) * MBLOCK_SIZE;
    ret = tlsf_malloc (m_alloc, size);
    ret = MBLOCK_ROUND_UP (ret);

    if (ret && ((W_)ret & MBLOCK_MASK) != 0) {
        barf("getMBlocks: misaligned block returned");
    }

    return ret;
}

void osFreeMBlocks(void *addr, uint32_t n) {
    (void)n;
    tlsf_free (m_alloc, addr);
}

void osReleaseFreeMemory(void) {
    /* This is done automatically by the TLSF2 allocator.
       So no code needed here.  */
}

void
osFreeAllMBlocks(void) {
    if (m_alloc)
    {
        tlsf_destroy (m_alloc);
        m_alloc = NULL;
    }
}

size_t getPageSize (void) {
    static size_t pagesize = 0;

    if (pagesize == 0) {
        SYSTEM_INFO sSysInfo;
        GetSystemInfo(&sSysInfo);
        pagesize = sSysInfo.dwPageSize;
    }

    return pagesize;
}

/* Returns 0 if physical memory size cannot be identified */
StgWord64 getPhysicalMemorySize (void) {
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

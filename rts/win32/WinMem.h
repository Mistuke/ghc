/* -----------------------------------------------------------------------------
*
* (c) Tamar Christina 2018
*
* Secured pooled memory allocator for the GHC runtime to manage object code
* and data on Windows.
*
* ---------------------------------------------------------------------------*/
#include <stddef.h>

/* Note [Pooled Memory Manager]
 *
 * On Windows when using the low level memory management routines (such as
 * VirtualAlloc) memory is always allocated as dictated by the SYSTEM_INFO
 * structure.
 *
 * Concretely the allocated memory is always page aligned (dwPageSize) and you
 * always get memory backed by a multiple of block (dwAllocationGranularity)
 * granularity.
 *
 * Which means if you're asking for 3k of memory, you'll get a pointer that's 4k
 * aligned, and backed by a 32k virtual memory block (8 pages). The rest of the
 * unused 29k of memory is thus inaccessible and gets marked as unusable memory.
 *
 * The other issue is that as most operating systems, memory protection can only
 * be applied to whole pages. So this necessitates the use of virtualAlloc =
 * instead of a much higher level interface such as malloc.
 *
 * HeapAlloc is not an option since you cannot call VirtualProtect on memory
 * allocated with HeapAlloc.
 *
 * This memory allocator has been written to solve all of these problems by
 * creating and managing pools of memory with the same protection level.
 * It's based on the TLSF memory allocator which allows pools to dynamically be
 * added or removed from the allocator with the guarantees:
 *
 * - O(1) cost for malloc, free, realloc
 * - Extremely low overhead per allocation (4 bytes)
 * - Low overhead per TLSF management of pools (~3kB)
 * - Low fragmentation
 * - Compiles to only a few kB of code and data
 * - Requests memory from the os on demand via callback
 *
 * See http://www.gii.upv.es/tlsf/main/docs
 *
 * The allocator keeps it's internal book keeping in the first few bytes of
 * every pool. Which means that you cannot allocate new memory (other than those
 * with the protection ReadWrite) while memory protection is being enforced.
 * This is also not a problem since if memory protection is being enforced, you
 * wouldn't be able to use the allocated memory to write the content there in
 * e.g. Read only mode.
 *
 * This restriction can be slightly lifted with a slight tweak of the TLSF
 * implementation, but I deemed it not useful to be able to get Read-only
 * memory back. TLSF is quite fast due to it's local bookkeeping and guarantees
 * bounded time allocation and de-allocation. So there should be a performance
 * overhead over malloc and free.
 */

/* The different memory protection modes that can be enforced by the
   manager. These can be combined.  */
typedef enum _AccessType {
  /* Read-Only access.  */
  ReadAccess = 0x1,
  /* ReadWrite-Only access.  */
  WriteAccess = 0x2,
  /* Execute-Only access.  */
  ExecuteAccess = 0x4
} AccessType_t;

/* Initialize the memory manager, must be done before using it.  */
void winmem_init (void);

/* Uninitialize the memory manager, will free and cleanup all memory.  */
void winmem_deinit (void);

/* Allocate N bytes of TYPE protected memory.  */
void* winmem_malloc (AccessType_t type, size_t n);

/* Reallocate N bytes of TYPE protected memory at P.  */
void* winmem_realloc (AccessType_t type, void* p, size_t n);

/* Allocate N bytes of TYPE protected memory.  */
void* winmem_calloc (AccessType_t type, size_t n, size_t m);

/* De-allocate MEMPTR having TYPE protected memory.  */
void winmem_free (AccessType_t type, void* memptr);

/* Disable all memory protection which sets all memory allocated to RW mode to
   allow manipulation of the content.  */
void winmem_memory_unprotect (void*);

/* Enforce all requested protection. During this time the allocator only allows
   further allocation of RW memory.  */
void winmem_memory_protect (void*);
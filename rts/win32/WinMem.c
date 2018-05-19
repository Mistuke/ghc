/* -----------------------------------------------------------------------------
*
* (c) Tamar Christina 2018
*
* Secured pooled memory allocator for the GHC runtime to manage object code
* and data on Windows. This makes the GHC runtime DEP compliant by controlling
* the NX bit.
*
* ---------------------------------------------------------------------------*/

#include <win32/tlsf.h>
#include <windows.h>
#include <stdint.h>
#include <stdbool.h>
#include <assert.h>

#include <rts/OSThreads.h>

#include "winmem.h"

/* This structure helps map from a requested AccessType to the protection modes
   to apply and where to find the manager for such type.  */
typedef struct _AccessMap
{
  /* The user mask that is the input.  */
  uint64_t mask;
  /* Index into mem_manager for the given type.  */
  uint64_t index;
  /* The Windows protection mode to apply to the manager's memory.  */
  uint32_t access;
} AccessMap_t;

/* Extra user information for the TLSF memory allocator.  */
typedef struct _userInfo
{
  /* The access protection the memory pages in the allocator should be
     protected with.  */
  uint32_t access;
  /* Indicates that pages for this allocator need to be unprotected to allocate
     or de-allocate memory.  */
  bool is_lockable;
  /* Indicates if the pages are currently locked.  If they are new allocations
     require them to be unlocked first.  */
  bool is_locked;
  /* A reference to the allocator itself. Not that this value should not be
     dereferenced in call-backs from the allocator as it may not have been
     initialized yet.  */
  tlsf_t m_alloc;
} userInfo_t;

/* This structure keeps track of all the currently reserved memory and where
   they have been allocated.  */
typedef struct _PoolBuffer
{
  /* Size of buffer in bytes.  */
  size_t size;
  /* Pointer to start of allocated memory.  */
  void* buffer;
  /* The allocator managing the pool.  */
  userInfo_t* m_alloc;
  /* The memory protection the pool needs to use.  */
  uint32_t m_flags;
  /* Next pool buffer if any.  */
  struct _PoolBuffer* next;
} PoolBuffer_t;

/* Default access mapping used by the manager.  */
AccessMap_t map[] = {
  { ReadAccess                               , 0, PAGE_READONLY          },
  { WriteAccess                              , 1, PAGE_READWRITE         },
  { ReadAccess  | WriteAccess                , 1, PAGE_READWRITE         },
  { ExecuteAccess                            , 2, PAGE_EXECUTE           },
  { ReadAccess  | ExecuteAccess              , 3, PAGE_EXECUTE_READ      },
  { WriteAccess | ExecuteAccess              , 4, PAGE_EXECUTE_READWRITE },
  { ReadAccess  | WriteAccess | ExecuteAccess, 4, PAGE_EXECUTE_READWRITE }
 };

enum { NUM_ACCESS = sizeof(map) / sizeof (AccessMap_t) };
static userInfo_t* mem_manager[NUM_ACCESS];
static PoolBuffer_t* buffers = NULL;

static size_t default_blocks_allocate = 15;
static uint32_t default_protection    = PAGE_READWRITE;
static volatile bool initialized      = false;

/* This protects all the memory allocator's global state.   */
Mutex winmem_mutex;

static size_t getAllocationSize (void)
{
  static size_t allocsize = 0;

  if (allocsize == 0) {
      SYSTEM_INFO sSysInfo;
      GetSystemInfo(&sSysInfo);
      allocsize = sSysInfo.dwAllocationGranularity;
  }

  return allocsize;
}

static void addPoolBuffer (size_t size, void* buffer,
                           userInfo_t* manager, uint32_t flags)
{
  PoolBuffer_t* m_buffer = (PoolBuffer_t*)malloc (sizeof(PoolBuffer_t));
  assert (m_buffer);
  m_buffer->size    = size;
  m_buffer->buffer  = buffer;
  m_buffer->m_alloc = manager;
  m_buffer->m_flags = flags;
  m_buffer->next    = buffers;
  buffers = m_buffer;
}

static uint64_t findManager (AccessType_t type)
{
  for (int x = 0; x < NUM_ACCESS; x++)
    if (map[x].mask == type) {
      return map[x].index;
    }

  return (uint64_t)-1;
}

static size_t getAllocSize (size_t requested)
{
  size_t allocsize = getAllocationSize ();
  size_t gAlloc    = allocsize * default_blocks_allocate;
  if (requested > gAlloc) {
    gAlloc = requested;
    /* Now round up to next page size to keep aligned to the page boundary.  */
    size_t pages    = gAlloc / allocsize;
    size_t overflow = pages * allocsize;
    if (gAlloc > overflow)
      pages++;
    gAlloc = pages * allocsize;
  }

  return gAlloc;
}

static uint32_t getProtection (AccessType_t type)
{
  for (int x = 0; x < NUM_ACCESS; x++)
    if (map[x].mask == type) {
      return map[x].access;
    }

  return PAGE_NOACCESS;
}

static void* winmem_cback_map (size_t* size, void* user)
{
  /* We've failed first allocation, probably don't have enough free memory.
     Let's resize.  */
  userInfo_t* info   = (userInfo_t*)user;
  size_t m_size      = getAllocSize (*size);
  uint32_t m_protect = info->access;
  void* cache
    = VirtualAlloc (NULL, m_size, MEM_COMMIT | MEM_RESERVE, default_protection);
  addPoolBuffer (m_size, cache, info, m_protect);
  *size = m_size;

  //if (info->m_alloc)
  //  {
  //    tlsf_check (info->m_alloc);
  //    tlsf_printstats (info->m_alloc);
  //  }
  return cache;
}

static void winmem_cback_unmap (void* mem, size_t size, void* user)
{
  (void)user;
  VirtualFree (mem, 0, (uint32_t)size);
  userInfo_t* info   = (userInfo_t*)user;
  //tlsf_check (info->m_alloc);
  //tlsf_printstats (info->m_alloc);
}

void winmem_init (void)
{
  if (initialized)
    return;

#if defined(THREADED_RTS)
    initMutex(&winmem_mutex);
#endif

  initialized = true;

  winmem_memory_protect (NULL);
}

void winmem_deinit ()
{
  if (!initialized)
    return;

  winmem_memory_unprotect (NULL);

  ACQUIRE_LOCK(&winmem_mutex);
  for (PoolBuffer_t* b = buffers; b; ) {
    PoolBuffer_t* tmp = b->next;
    free (b);
    b = tmp;
  }

  for (int x = 0; x < NUM_ACCESS; x++)
      if (mem_manager[x]) {
          tlsf_destroy (mem_manager[x]->m_alloc);
          free (mem_manager[x]);
          mem_manager[x] = NULL;
        }

  initialized = false;
  RELEASE_LOCK(&winmem_mutex);

#if defined(THREADED_RTS)
   closeMutex(&winmem_mutex);
#endif
}

void* winmem_aligned_malloc (AccessType_t type, size_t n, size_t alignment)
{
  if (!initialized)
    return NULL;

  ACQUIRE_LOCK(&winmem_mutex);
  uint64_t index = findManager (type);
  userInfo_t* manager = mem_manager[index];
  bool is_lockable = type & ~WriteAccess;

  if (!manager)
    {
      manager = (userInfo_t*)calloc (1, sizeof (userInfo_t));
      manager->is_lockable = is_lockable;
      manager->is_locked = false;
      manager->access = getProtection (type);
      manager->m_alloc
        = tlsf_create (winmem_cback_map, winmem_cback_unmap, manager);
      mem_manager[index] = manager;
      //tlsf_check (manager->m_alloc);
      //tlsf_printstats (manager->m_alloc);
    }

  winmem_memory_unprotect (&type);
  void* result = tlsf_malloc (manager->m_alloc, n, alignment);
  winmem_memory_protect (&type);

  RELEASE_LOCK(&winmem_mutex);

  ASSERT (!alignment || ((uintptr_t)result % alignment == 0));

  return result;
}

void* winmem_aligned_realloc (AccessType_t type, void* p, size_t n,
                              size_t alignment)
{
  if (!initialized)
    return NULL;

  ACQUIRE_LOCK(&winmem_mutex);
  uint64_t index = findManager (type);
  userInfo_t* manager = mem_manager[index];
  bool is_lockable = type & ~WriteAccess;

  if (!manager)
    {
      manager = (userInfo_t*)malloc (sizeof (userInfo_t));
      manager->is_lockable = is_lockable;
      manager->is_locked = false;
      manager->access = getProtection (type);
      manager->m_alloc
        = tlsf_create (winmem_cback_map, winmem_cback_unmap, manager);
      mem_manager[index] = manager;
    }

  winmem_memory_unprotect (&type);
  void* result = tlsf_realloc (manager->m_alloc, p, n, alignment);
  winmem_memory_protect (&type);

  RELEASE_LOCK(&winmem_mutex);

  return result;
}

void* winmem_aligned_calloc (AccessType_t type, size_t n, size_t alignment)
{
  if (!initialized)
    return NULL;

  ACQUIRE_LOCK(&winmem_mutex);
  uint64_t index = findManager (type);
  userInfo_t* manager = mem_manager[index];
  bool is_lockable = type & ~WriteAccess;

  if (!manager)
    {
      manager = (userInfo_t*)malloc (sizeof (userInfo_t));
      manager->is_lockable = is_lockable;
      manager->is_locked = false;
      manager->access = getProtection (type);
      manager->m_alloc
        = tlsf_create (winmem_cback_map, winmem_cback_unmap, manager);
      mem_manager[index] = manager;
    }

  winmem_memory_unprotect (&type);
  void* result = tlsf_calloc (manager->m_alloc, n, alignment);
  winmem_memory_protect (&type);

  RELEASE_LOCK(&winmem_mutex);

  return result;
}

/* Allocate N bytes of TYPE protected memory.  */
void* winmem_malloc (AccessType_t type, size_t n)
{
  return winmem_aligned_malloc (type, n, 0);
}

/* Reallocate N bytes of TYPE protected memory at P.  */
void* winmem_realloc (AccessType_t type, void* p, size_t n)
{
  return winmem_aligned_realloc (type, p, n, 0);
}

/* Allocate N bytes of TYPE protected memory.  */
void* winmem_calloc (AccessType_t type, size_t n)
{
  return winmem_aligned_calloc (type, n, 0);
}

void winmem_free (AccessType_t type, void* memptr)
{
  if (!initialized)
    return;

  ACQUIRE_LOCK(&winmem_mutex);
  uint64_t index = findManager (type);
  tlsf_t manager = mem_manager[index]->m_alloc;
  assert (manager);

  winmem_memory_unprotect (&type);
  tlsf_free (manager, memptr);
  winmem_memory_protect (&type);

  RELEASE_LOCK(&winmem_mutex);
}

void winmem_memory_protect (AccessType_t *type)
{
  userInfo_t* manager = NULL;
  if (type) {
    uint64_t index = findManager (*type);
    manager = mem_manager[index];
  }

  if (   !initialized
      || (manager && (manager->is_locked || !manager->is_lockable)))
    return;

  ACQUIRE_LOCK(&winmem_mutex);

  for (PoolBuffer_t* b = buffers; b; b = b->next) {
    if (   !b->m_alloc->is_lockable
        || b->m_alloc->is_locked
        || (manager && b->m_alloc != manager))
      continue;

    b->m_alloc->is_locked = true;
    DWORD old_flags;
    bool success
      = VirtualProtect (b->buffer, b->size, b->m_flags, &old_flags);
    assert (success);
  }
  RELEASE_LOCK(&winmem_mutex);
}

void winmem_memory_unprotect (AccessType_t *type)
{
  if (!initialized)
    return;
  ACQUIRE_LOCK(&winmem_mutex);

  userInfo_t* manager = NULL;
  if (type) {
    uint64_t index = findManager (*type);
    manager = mem_manager[index];
  }

  for (PoolBuffer_t* b = buffers; b; b = b->next) {
    if (   !b->m_alloc->is_lockable
        || !b->m_alloc->is_locked
        || (manager && b->m_alloc != manager))
      continue;

    b->m_alloc->is_locked = false;
    DWORD old_flags;
    bool success
      = VirtualProtect (b->buffer, b->size, default_protection, &old_flags);
    assert (success);
  }
  RELEASE_LOCK(&winmem_mutex);
}
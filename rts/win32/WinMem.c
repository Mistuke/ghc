/* -----------------------------------------------------------------------------
*
* (c) The GHC Team 2017
*
* Contributed by Tamar Christina.
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

/* Defaukt access mapping used by the manager.  */
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

const size_t pool_resize_limit       = 10;
const size_t default_blocks_allocate = 15;
const uint32_t default_protection    = PAGE_EXECUTE_READWRITE;
static volatile bool initialized     = false;
static bool m_enforcing_mem_protect  = false;

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
  if (flags == default_protection)
    return;

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
    = VirtualAlloc (NULL, m_size, MEM_COMMIT | MEM_RESERVE,
                    m_enforcing_mem_protect ? m_protect : default_protection);
  addPoolBuffer (m_size, cache, info, m_protect);
  *size = m_size;

  if (info->m_alloc)
    {
      tlsf_check (info->m_alloc);
      tlsf_printstats (info->m_alloc);
    }
  return cache;
}

static void winmem_cback_unmap (void* mem, size_t size, void* user)
{
  (void)user;
  VirtualFree (mem, 0, (uint32_t)size);
  userInfo_t* info   = (userInfo_t*)user;
  tlsf_check (info->m_alloc);
  tlsf_printstats (info->m_alloc);
}

void winmem_init (void)
{
#if defined(THREADED_RTS)
    initMutex(&winmem_mutex);
#endif

  initialized = true;
}

void winmem_deinit ()
{
  if (!initialized)
    return;

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

void* winmem_malloc (AccessType_t type, size_t n)
{
  if (!initialized)
    return NULL;

  ACQUIRE_LOCK(&winmem_mutex);
  uint64_t index = findManager (type);
  userInfo_t* manager = mem_manager[index];

  if (!manager)
    {
      manager = (userInfo_t*)calloc (1, sizeof (userInfo_t));
      manager->access = getProtection (type);
      manager->m_alloc
        = tlsf_create (winmem_cback_map, winmem_cback_unmap, manager);
      mem_manager[index] = manager;
      tlsf_check (manager->m_alloc);
      tlsf_printstats (manager->m_alloc);
    }

  void* result = tlsf_malloc (manager->m_alloc, n);
  RELEASE_LOCK(&winmem_mutex);

  return result;
}

void* winmem_realloc (AccessType_t type, void* p, size_t n)
{
  if (!initialized)
    return NULL;

  ACQUIRE_LOCK(&winmem_mutex);
  uint64_t index = findManager (type);
  userInfo_t* manager = mem_manager[index];

  if (!manager)
    {
      manager = (userInfo_t*)malloc (sizeof (userInfo_t));
      manager->access = getProtection (type);
      manager->m_alloc
        = tlsf_create (winmem_cback_map, winmem_cback_unmap, manager);
      mem_manager[index] = manager;
    }

  void* result = tlsf_realloc (manager->m_alloc, p, n);
  RELEASE_LOCK(&winmem_mutex);

  return result;
}

void* winmem_calloc (AccessType_t type, size_t n, size_t m)
{
  (void)n;
  if (!initialized)
    return NULL;

  ACQUIRE_LOCK(&winmem_mutex);
  uint64_t index = findManager (type);
  userInfo_t* manager = mem_manager[index];

  if (!manager)
    {
      manager = (userInfo_t*)malloc (sizeof (userInfo_t));
      manager->access = getProtection (type);
      manager->m_alloc
        = tlsf_create (winmem_cback_map, winmem_cback_unmap, manager);
      mem_manager[index] = manager;
    }

  void* result = tlsf_calloc (manager->m_alloc, m);
  RELEASE_LOCK(&winmem_mutex);

  return result;
}

void winmem_free (AccessType_t type, void* memptr)
{
  if (!initialized)
    return;

  ACQUIRE_LOCK(&winmem_mutex);
  uint64_t index = findManager (type);
  tlsf_t manager = mem_manager[index]->m_alloc;
  assert (manager);
  tlsf_free (manager, memptr);
  RELEASE_LOCK(&winmem_mutex);
}

void winmem_memory_protect ()
{
  if (!initialized)
    return;

  ACQUIRE_LOCK(&winmem_mutex);
  m_enforcing_mem_protect = true;

  for (PoolBuffer_t* b = buffers; b; b = b->next ) {
    /* Note: Abort if this fails when added to GHC.  */
    DWORD old_flags;
    VirtualProtect (b->buffer, b->size, b->m_flags, &old_flags);
  }
  RELEASE_LOCK(&winmem_mutex);
}

void winmem_memory_unprotect ()
{
  if (!initialized)
    return;

  ACQUIRE_LOCK(&winmem_mutex);
  m_enforcing_mem_protect = false;

  for (PoolBuffer_t* b = buffers; b; b = b->next) {;
    /* Note: Abort if this fails when added to GHC.  */
    DWORD old_flags;
    VirtualProtect (b->buffer, b->size, PAGE_EXECUTE_READWRITE, &old_flags);
  }
  RELEASE_LOCK(&winmem_mutex);
}
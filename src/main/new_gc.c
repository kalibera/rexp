#include <stdint.h>
#include <stdlib.h>

#define USE_RINTERNALS

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#define R_USE_SIGNALS 1
#include <Defn.h>
#include <Internal.h>
#include <R_ext/GraphicsEngine.h> /* GEDevDesc, GEgetDevice */
#include <R_ext/Rdynload.h>
#include <R_ext/Rallocators.h> /* for R_allocator_t structure */
#include <Rmath.h>             // R_pow_di
#include <Print.h>             // R_print
#include <sys/mman.h>

#ifdef COMPUTE_REFCNT_VALUES
#define INIT_REFCNT(x) do {			\
	SEXP __x__ = (x);			\
	SET_REFCNT(__x__, 0);			\
	SET_TRACKREFS(__x__, TRUE);		\
    } while (0)
#else
#define INIT_REFCNT(x) do {} while (0)
#endif

#define PAGE_SIZE 0x2000L
#define PAGE_IDX 32
#define PAGE_IDX_BITS 5
#define PAGE_MASK 0x1fff
#define MAX_IDX (PAGE_SIZE / PAGE_IDX)

#define FORCE_INLINE inline __attribute__((always_inline))

// #define GCPROF 1
// #define GCDEBUG 1
// #define GCSTRESS 1

#ifdef GCDEBUG
#define CHECK(exp)                \
  if (!(exp)) {                   \
    printf("err %d\n", __LINE__); \
    asm("int3");                  \
  }
#define ON_DEBUG(exp) exp
#else
#define CHECK(exp) \
  {}
#define ON_DEBUG(exp) \
  {}
#endif

#include "gc_ht.h"

#define UNMARKED 0
uint8_t THE_MARK = 1;

static size_t gc_cnt = 0;
Rboolean fullCollection = FALSE;

#define CONS_BUCKET 0
#define ENV_BUCKET 1
#define PROM_BUCKET 2
#define GENERIC_SEXP_BUCKET 3
#define INT_BUCKET 4
#define REAL_BUCKET 5
#define LGL_BUCKET 6
// GCC f* up of more than 25!
#define NUM_BUCKETS 25
#define FIRST_GENERIC_BUCKET 7

size_t BUCKET_SIZE[NUM_BUCKETS] = {
  40, 40, 40, 40,
  32, 32, 32,
  32, 40, 48, 56, 64, 72, 80, 88, 96, 112, 128, 160, 192, 224, 256, 320, 384, 448};

#define INITIAL_PAGE_LIMIT 500
#define FREE_PAGES_SLACK 50
#define INITIAL_HEAP_LIMIT (10 * 1024 * 1024)
#define PAGE_FULL_TRESHOLD 0.01
#define HEAP_GROW_RATE 1.2
#define PAGES_GROW_RATE 1.3
#define HEAP_SHRINK_RATE 0.8
#define HEAP_SIZE_SLACK 0.77
#define HEAP_SIZE_MAX_SLACK 0.35
#define HEAP_PAGES_SLACK 0.8
#define FULL_COLLECTION_TRIGGER 0.94
#define WRITE_BARRIER_MS_TRIGGER 2000
#define MS_TRIGGER 2000
#define INITIAL_MS_SIZE 4000

typedef struct Page {
  uint8_t mark[MAX_IDX];
  size_t reclaimed_nodes;
  uint8_t bkt;
  uint8_t last_mark;
  uintptr_t start;
  uintptr_t end;
  uintptr_t sweep_end;
  uintptr_t alloc_finger;
  uintptr_t sweep_finger;
  size_t available_nodes;
  char data[];
} Page;

#define PTR2PAGE(ptr) ((Page*)((uintptr_t)(ptr) & ~PAGE_MASK))
#define PTR2IDX(ptr) (((uintptr_t)(ptr) & PAGE_MASK) >> PAGE_IDX_BITS)
#define ISNODE(s)                                \
  ((uintptr_t)HEAP.pageArena < (uintptr_t)(s) && \
   (uintptr_t)(s) < HEAP.pageArenaEnd)
#define ISMARKED(s)                                      \
  (ISNODE(s) ? PTR2PAGE(s)->mark[PTR2IDX(s)] == THE_MARK \
             : ObjHashtable_get(HEAP.bigObjectsHt, (s))->mark == THE_MARK)
#define NODE_IS_MARKED(s) ISMARKED(s)
#define INIT_NODE(s) (*(uint32_t*)&((SEXP)(s))->sxpinfo = 0)

void doGc(unsigned);
void free_unused_memory();

typedef struct SizeBucket {
  Page* to_bump;
  Page* to_sweep;
  size_t sweep_idx;
  size_t num_pages;
  PageHashtable* pagesHt;
} SizeBucket;

#define PAGE_FREELIST_BUCKET_SIZE (FREE_PAGES_SLACK/2)
typedef struct FreePage {
  Page* page;
  Rboolean commited;
} FreePage;
typedef struct FreePageBucket {
  FreePage entry[PAGE_FREELIST_BUCKET_SIZE];
  size_t size;
  struct FreePageBucket* next;
} FreePageBucket;

#define MAX_PAGES 4000000L
struct {
  SizeBucket sizeBucket[NUM_BUCKETS];
  size_t page_limit;
  size_t num_pages;
  size_t heapLimit;
  size_t size;
  void* pageArena;
  uintptr_t pageArenaEnd;
  uintptr_t pageArenaFinger;
  FreePageBucket* freePage;
  size_t numFreeCommitedPages;
  ObjHashtable* bigObjectsHt;
#ifdef CONSERVATIVE_STACK_SCAN
  PageHashtable* pagesHt;
#endif
} HEAP;

SEXP* MarkStack;
size_t MSpos = 0;
size_t MSsize = 0;

#ifdef CONSERVATIVE_STACK_SCAN
SEXP* TraceStackStack;
size_t TSpos = 0;
size_t TSsize = 0;
#endif

int heapIsInitialized = 0;
void new_gc_initHeap() {
  for (size_t i = 0; i < NUM_BUCKETS; ++i) {
    SizeBucket* bucket = &HEAP.sizeBucket[i];
    bucket->to_bump = bucket->to_sweep = NULL;
    bucket->num_pages = 0;
    PageHashtable_init(&bucket->pagesHt);
  }
  HEAP.page_limit = INITIAL_PAGE_LIMIT;
  HEAP.heapLimit = INITIAL_HEAP_LIMIT;
  HEAP.size = 0;
  HEAP.num_pages = 0;
  HEAP.numFreeCommitedPages = 0;

  size_t vmem = MAX_PAGES * PAGE_SIZE;
  HEAP.pageArena =
      mmap(NULL, vmem, PROT_NONE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
  if (HEAP.pageArena == MAP_FAILED)
    R_Suicide("Cannot setup heap");
  HEAP.pageArenaFinger = (uintptr_t)HEAP.pageArena;
  HEAP.pageArenaEnd = ((uintptr_t)HEAP.pageArena + vmem);
  size_t off = HEAP.pageArenaFinger % PAGE_SIZE;
  if (off != 0) {
    HEAP.pageArenaFinger += (PAGE_SIZE - off);
    HEAP.pageArenaEnd -= off;
  }
  CHECK(PTR2PAGE((void*)HEAP.pageArenaFinger) == (void*)HEAP.pageArenaFinger);
  HEAP.freePage = NULL;
  MarkStack = malloc(sizeof(SEXP) * INITIAL_MS_SIZE);
  MSsize = INITIAL_MS_SIZE;
  ObjHashtable_init(&HEAP.bigObjectsHt);
#ifdef CONSERVATIVE_STACK_SCAN
  PageHashtable_init(&HEAP.pagesHt);
  TraceStackStack = malloc(sizeof(SEXP) * INITIAL_MS_SIZE);
  TSsize = INITIAL_MS_SIZE;
#endif
  heapIsInitialized = 1;
}

SEXP alloc(size_t sz);

void* allocBigObj(size_t sexp_sz) {
  size_t sz = sexp_sz;

  if (HEAP.size + sz > HEAP.heapLimit)
    doGc(NUM_BUCKETS);

  SEXP data = (SEXP)malloc(sz);
  if (data == NULL)
    Rf_errorcall(R_NilValue, "error alloc");

  // printf("Malloced big %p\n", obj);

  HEAP.size += sz;

  ObjHashtableEntry e = {data, UNMARKED};
  while (!ObjHashtable_add(HEAP.bigObjectsHt, e))
    ObjHashtable_grow(&HEAP.bigObjectsHt);

  INIT_NODE(data);
  return data;
}

void verifyPage(Page* page) {
  uintptr_t pos = page->start;
  size_t last_idx = -1;
  while (pos != page->end) {
    CHECK(pos <= page->end);
    size_t idx = PTR2IDX((void*)pos);
    CHECK(idx != last_idx);
    last_idx = idx;
    CHECK(idx >= 0 && idx <= MAX_IDX);
    CHECK(pos < (uintptr_t)page + PAGE_SIZE);
    CHECK(PTR2PAGE((void*)pos) == page);
    pos += BUCKET_SIZE[page->bkt];
  }
  CHECK(page->end <= (uintptr_t)page + PAGE_SIZE);
}

Page* allocPage(unsigned bkt) {
  Page* page;
  if (HEAP.freePage != NULL) {
    FreePageBucket* freelist = HEAP.freePage;
    freelist->size--;
    page = freelist->entry[freelist->size].page;
    if (!freelist->entry[freelist->size].commited) {
      int res = mprotect((void*)page, PAGE_SIZE, PROT_READ | PROT_WRITE);
      if (res != 0)
        R_Suicide("alloc page failed");
    }
    if (freelist->size == 0) {
      HEAP.freePage = HEAP.freePage->next;
      free(freelist);
    }
  } else {
    page = (Page*)HEAP.pageArenaFinger;
    HEAP.pageArenaFinger += PAGE_SIZE;
    if (HEAP.pageArenaFinger >= HEAP.pageArenaEnd) {
      R_Suicide("Ran out of vmem");
    }
    int res = mprotect((void*)page, PAGE_SIZE, PROT_READ | PROT_WRITE);
    if (res != 0)
      R_Suicide("alloc page failed");
  }
  memset((void*)&page->mark, UNMARKED, MAX_IDX);
  uintptr_t start = (uintptr_t)page->data;
  if (start % PAGE_IDX != 0)
    start += PAGE_IDX - (start % PAGE_IDX);
  page->start = page->sweep_end = start;
  page->alloc_finger = page->start;

  uintptr_t end = (uintptr_t)page + PAGE_SIZE;
  size_t available = end - page->start;
  size_t tail = available % BUCKET_SIZE[bkt];
  end -= tail;
  page->end = page->sweep_finger = end;

  page->bkt = bkt;
  page->last_mark = UNMARKED;
  page->available_nodes = available / BUCKET_SIZE[bkt];
  // printf("allocated a %d page %p from %p to %p\n", bkt, page, page->start,
  // page->end);
#ifdef GCDEBUG
  verifyPage(page);
#endif
  return page;
}

void growBucket(unsigned bkt) {
  SizeBucket* bucket = &HEAP.sizeBucket[bkt];
  Page* page = allocPage(bkt);

  bucket->to_bump = page;
  PageHashtableEntry e = {page, page->last_mark, FRESH};
  while (!PageHashtable_add(bucket->pagesHt, e)) {
    // TODO: this is brittle... Reordering the hashtable invalidates the
    // sweep_idx and we need to start searching for pages to sweep from the
    // start of the hashmap.
    bucket->sweep_idx = 0;
    PageHashtable_grow(&bucket->pagesHt);
  }
#ifdef CONSERVATIVE_STACK_SCAN
  while (!PageHashtable_add(HEAP.pagesHt, e)) {
    PageHashtable_grow(&HEAP.pagesHt);
  }
#endif

  size_t available = page->end - page->start;

  HEAP.size += available;
  bucket->num_pages++;
  HEAP.num_pages++;
}

void findPageToSweep(SizeBucket* bucket);

void deletePage(SizeBucket* bucket, Page* p) {
  HEAP.size -= p->end - p->start;
  CHECK(bucket->num_pages > 0);
  bucket->num_pages--;
  HEAP.num_pages--;
  if (bucket->to_bump == p)
    bucket->to_bump = NULL;
  if (bucket->to_sweep == p)
    bucket->to_sweep = NULL;
  Page* del = p;
  ON_DEBUG(memset(del, 0xd3, PAGE_SIZE));
#ifdef GCSTRESS
  mprotect(del, PAGE_SIZE, PROT_NONE);
#else
  FreePageBucket* freelist = HEAP.freePage;
  if (!freelist || freelist->size == PAGE_FREELIST_BUCKET_SIZE) {
    freelist = malloc(sizeof(FreePageBucket));
    if (freelist == NULL)
      R_Suicide("int err");
    freelist->size = 0;
    freelist->next = HEAP.freePage;
    HEAP.freePage = freelist;
  }
  Rboolean keepAlive = HEAP.numFreeCommitedPages < FREE_PAGES_SLACK;
  freelist->entry[freelist->size].page = del;
  freelist->entry[freelist->size].commited = keepAlive;
  freelist->size++;
  if (!keepAlive)
    mprotect(del, PAGE_SIZE, PROT_NONE);
#endif
}

void findPageToSweep(SizeBucket* bucket) {
  size_t l = bucket->pagesHt->size * HASH_BUCKET_SIZE;
  size_t i = bucket->sweep_idx;
  for (; i < l; ++i) {
    PageHashtableEntry* e = &bucket->pagesHt->data[i];
    if (e->page != NULL && (e->state == MARKED || e->state == SPLIT)) {
      // In case of the split page, the sweep finger is already set and
      // indicates up to which point we have marked. The rest of the page is
      // currently being used for bump allocation and we should not touch it.
      if (e->state != SPLIT)
        e->page->sweep_end = e->page->alloc_finger;
      e->page->sweep_finger = e->page->start;
      Page* page = e->page;
      page->reclaimed_nodes = 0;
      // printf("Will now sweep a %d page %p from %p to %p\n", page->bkt, page,
      // page->start, page->sweep_end);
      bucket->to_sweep = page;
      bucket->sweep_idx = i + 1;
      e->state = SWEEPING;
      return;
    }
    e->state = SWEPT;
  }
  bucket->sweep_idx = l;
  bucket->to_sweep = NULL;
}

FORCE_INLINE void* sweepAllocInBucket(unsigned bkt, SizeBucket* bucket);

void* slowAllocInBucket(unsigned bkt) {
  // No luck so far. If we are below the page limit
  // we can allocate more. Otherwise we need to do a gc.
  SizeBucket* bucket = &HEAP.sizeBucket[bkt];
  if (HEAP.num_pages >= HEAP.page_limit ||
      HEAP.size + PAGE_SIZE > HEAP.heapLimit) {
    doGc(bkt);
    // TODO: something a more sane...
    return sweepAllocInBucket(bkt, bucket);
  }

  if (HEAP.page_limit <= bucket->num_pages) {
    R_Suicide("fatal, run out of space");
  }

  growBucket(bkt);

  {
    Page* page = bucket->to_bump;
    void* res = (void*)page->alloc_finger;
    size_t i = PTR2IDX(res);
    CHECK(page->mark[i] == 0);
    page->alloc_finger += BUCKET_SIZE[bkt];
    return res;
  }
}

#define IDX2PTR(p, i) ((uintptr_t)(p) + ((i) << PAGE_IDX_BITS))

void finishSweep(SizeBucket* bucket, Page* page) {
  PageHashtableEntry* e = PageHashtable_get(bucket->pagesHt, page);
  if ((double)page->reclaimed_nodes / (double)page->available_nodes <=
      PAGE_FULL_TRESHOLD && e->state == MARKED)
    e->state = FULL;
  page->sweep_finger = page->sweep_end;
}

void* slowSweepAllocInBucket(unsigned bkt, SizeBucket* bucket) {
  size_t sz = BUCKET_SIZE[bkt];

  // Lazy sweeping
  while (bucket->to_sweep != NULL) {
    Page* page = bucket->to_sweep;
    uintptr_t finger = page->sweep_finger;
    while (finger < page->sweep_end) {
      void* res = (void*)finger;
      size_t i = PTR2IDX(res);
      CHECK(i < MAX_IDX);
      finger += sz;
      if (page->mark[i] < THE_MARK) {
        ON_DEBUG(memset(res, 0xd5, sz));
#ifndef GCSTRESS
        page->sweep_finger = finger;
        page->reclaimed_nodes++;
        return res;
#endif
      }
    }
    finishSweep(bucket, page);
    findPageToSweep(bucket);
  }

  return slowAllocInBucket(bkt);
}


FORCE_INLINE void* sweepAllocInBucket(unsigned bkt, SizeBucket* bucket) {
  size_t sz = BUCKET_SIZE[bkt];

  if (bucket->to_sweep) {
    Page* page = bucket->to_sweep;
    uintptr_t finger = page->sweep_finger;
    while (finger < page->sweep_end) {
      void* res = (void*)finger;
      size_t i = PTR2IDX(res);
      CHECK(i < MAX_IDX);
      finger += sz;
      if (page->mark[i] < THE_MARK) {
        ON_DEBUG(memset(res, 0xd5, sz));
#ifndef GCSTRESS
        page->sweep_finger = finger;
        page->reclaimed_nodes++;
        return res;
#endif
      }
    }
  }

  return slowSweepAllocInBucket(bkt, bucket);
}

FORCE_INLINE void* allocInBucket(unsigned bkt) {
  SizeBucket* bucket = &HEAP.sizeBucket[bkt];
  // First try bump pointer alloc in the current page
  if (bucket->to_bump != NULL) {
    Page* page = bucket->to_bump;
    size_t next = page->alloc_finger + BUCKET_SIZE[bkt];
    if (next <= page->end) {
      void* res = (void*)page->alloc_finger;
      size_t i = PTR2IDX(res);
      CHECK((uintptr_t)res + BUCKET_SIZE[bkt] <= page->end);
      CHECK(PTR2PAGE(res) == page);
      CHECK(i > 0 && i < MAX_IDX);
      page->alloc_finger = next;
      INIT_NODE(res);
      return res;
    } else {
      bucket->to_bump = NULL;
      // printf("%d page %p is full\n", page->bkt, page);
    }
  }
  void* res = sweepAllocInBucket(bkt, bucket);
  INIT_NODE(res);
  return res;
}

SEXP alloc(size_t sz) {
  unsigned bkt = FIRST_GENERIC_BUCKET;
  while (bkt < NUM_BUCKETS && BUCKET_SIZE[bkt] < sz)
    ++bkt;
  if (bkt < NUM_BUCKETS) {
    SEXP res = (SEXP)allocInBucket(bkt);
    // printf("allo %p for %d in %d\n", res, sz, BUCKET_SIZE[bkt]);
    CHECK(!ISMARKED(res));
    return res;
  }
  return (SEXP)allocBigObj(sz);
}

#define intCHARSXP 73
SEXP new_gc_allocVector3_slow(SEXPTYPE type, R_xlen_t length);

SEXP new_gc_allocVector3(SEXPTYPE type,
                         R_xlen_t length,
                         R_allocator_t* allocator) {
  if (allocator != NULL)
    error(_("custom allocator not supported"));

  /* Handle some scalars directly to improve speed. */
  if (length == 1) {
    SEXP s;
    switch (type) {
      case REALSXP:
        s = allocInBucket(REAL_BUCKET);
        break;
      case INTSXP:
        s = allocInBucket(INT_BUCKET);
        break;
      case LGLSXP:
        s = allocInBucket(LGL_BUCKET);
        break;
      default:
        return new_gc_allocVector3_slow(type, length);
    }
    ATTRIB(s) = R_NilValue;
    SET_TYPEOF(s, type);
    SET_SHORT_VEC_LENGTH(s, (R_len_t)length);  // is 1
    SET_SHORT_VEC_TRUELENGTH(s, 0);
    SET_NAMED(s, 0);
    INIT_REFCNT(s);
    return s;
  }
  return new_gc_allocVector3_slow(type, length);
}

SEXP new_gc_allocVector3_slow(SEXPTYPE type, R_xlen_t length) {
  if (length > R_XLEN_T_MAX)
    error(_("vector is too large")); /**** put length into message */
  else if (length < 0)
    error(_("negative length vectors are not allowed"));

  size_t size = 0;

  /* number of vector cells to allocate */
  switch (type) {
    case NILSXP:
      return R_NilValue;
    case RAWSXP:
      size = BYTE2VEC(length);
      break;
    case CHARSXP:
      error("use of allocVector(CHARSXP ...) is defunct\n");
    case intCHARSXP:
      type = CHARSXP;
      size = BYTE2VEC(length + 1);
      break;
    case LGLSXP:
    case INTSXP:
      if (length > R_SIZE_T_MAX / sizeof(int))
        error(_("cannot allocate vector of length %d"), length);
      size = INT2VEC(length);
      break;
    case REALSXP:
      if (length > R_SIZE_T_MAX / sizeof(double))
        error(_("cannot allocate vector of length %d"), length);
      size = FLOAT2VEC(length);
      break;
    case CPLXSXP:
      if (length > R_SIZE_T_MAX / sizeof(Rcomplex))
        error(_("cannot allocate vector of length %d"), length);
      size = COMPLEX2VEC(length);
      break;
    case STRSXP:
    case EXPRSXP:
    case VECSXP:
      if (length > R_SIZE_T_MAX / sizeof(SEXP))
        error(_("cannot allocate vector of length %d"), length);
      size = PTR2VEC(length);
      break;
    case LANGSXP:
      if (length == 0)
        return R_NilValue;
#ifdef LONG_VECTOR_SUPPORT
      if (length > R_SHORT_LEN_MAX)
        error("invalid length for pairlist");
#endif
      {
        SEXP s = allocList((int)length);
        SET_TYPEOF(s, LANGSXP);
        return s;
      }
    case LISTSXP:
#ifdef LONG_VECTOR_SUPPORT
      if (length > R_SHORT_LEN_MAX)
        error("invalid length for pairlist");
#endif
      return allocList((int)length);
    default:
      error(_("invalid type/length (%s/%d) in vector allocation"),
            type2char(type),
            length);
  }

  R_size_t hdrsize = sizeof(SEXPREC_ALIGN);

#ifdef LONG_VECTOR_SUPPORT
  if (length > R_SHORT_LEN_MAX)
    hdrsize = sizeof(SEXPREC_ALIGN) + sizeof(R_long_vec_hdr_t);
#endif

  SEXP s = (SEXP)alloc(hdrsize + sizeof(VECREC) * size);

#ifdef LONG_VECTOR_SUPPORT
  if (length > R_SHORT_LEN_MAX) {
    s = (SEXP)(((char*)s) + sizeof(R_long_vec_hdr_t));
    SET_SHORT_VEC_LENGTH(s, R_LONG_VEC_TOKEN);
    SET_LONG_VEC_LENGTH(s, length);
    SET_LONG_VEC_TRUELENGTH(s, 0);
  } else {
    SET_SHORT_VEC_LENGTH(s, (R_len_t)length);
    SET_SHORT_VEC_TRUELENGTH(s, 0);
  }
#else
  SET_SHORT_VEC_LENGTH(s, (R_len_t)length);
  SET_SHORT_VEC_TRUELENGTH(s, 0);
#endif

  INIT_REFCNT(s);
  ATTRIB(s) = R_NilValue;
  SET_TYPEOF(s, type);
  SET_NAMED(s, 0);

  /* The following prevents disaster in the case */
  /* that an uninitialised string vector is marked */
  /* Direct assignment is OK since the node was just allocated and */
  /* so is at least as new as R_NilValue and R_BlankString */
  if (type == EXPRSXP || type == VECSXP) {
    SEXP* data = STRING_PTR(s);
    for (R_xlen_t i = 0; i < length; i++)
      data[i] = R_NilValue;
  } else if (type == STRSXP) {
    SEXP* data = STRING_PTR(s);
    for (R_xlen_t i = 0; i < length; i++)
      data[i] = R_BlankString;
  } else if (type == CHARSXP || type == intCHARSXP) {
    CHAR_RW(s)[length] = 0;
  }
  return s;
}

double heap_pressure() {
  return (double)HEAP.size / (double)HEAP.heapLimit;
}
double page_pressure() {
  return (double)(HEAP.num_pages) / (double)HEAP.page_limit;
}

void finish_sweep();
void heapStatistics() {
  printf("HEAP statistics after gc %d of gen %d: size %d / %d (%f), pages %d / %d (%f)\n",
         gc_cnt,
         fullCollection,
         HEAP.size / 1024 / 1024,
         HEAP.heapLimit / 1024 / 1024,
         heap_pressure(),
         HEAP.num_pages,
         HEAP.page_limit,
         page_pressure());

  size_t pages_size = 0;
  for (size_t i = 0; i < NUM_BUCKETS; ++i) {
    size_t available = 0;
    size_t l = HEAP.sizeBucket[i].pagesHt->size * HASH_BUCKET_SIZE;
    for (size_t j = 0; j < l; ++j) {
      Page* p = HEAP.sizeBucket[i].pagesHt->data[j].page;
      if (p != NULL)
        available += p->available_nodes;
    }
    pages_size += available * BUCKET_SIZE[i];
    printf(" Bucket %d (%d) : pages %d, nodes %d\n",
           i,
           BUCKET_SIZE[i],
           HEAP.sizeBucket[i].num_pages,
           available);
  }
  printf(" total in pages %d\n", pages_size / 1024 / 1024);
  size_t osz = HEAP.bigObjectsHt->size * HASH_BUCKET_SIZE;
  size_t bigObjs = 0;
  for (size_t i = 0; i < osz; ++i) {
    if (HEAP.bigObjectsHt->data[i].entry != NULL)
      bigObjs++;
  }
  printf(" big objects count %d\n", bigObjs);
  size_t freePages = 0;
  size_t freeCommitedPages = 0;
  size_t freePageBuckets = 0;
  FreePageBucket* b = HEAP.freePage;
  while (b) {
    freePages += b->size;
    ++freePageBuckets;
    for (size_t i; i < b->size; ++i) {
      if (b->entry[i].commited)
        freeCommitedPages++;
    }
    b = b->next;
  }
  printf(" Page freelist has %d buckets with %d entries of which %d are commited\n", freePageBuckets, freePages, freeCommitedPages);
}

static void traceHeap();
void traceStack();
FORCE_INLINE void PROCESS_NODE(SEXP);

#include <time.h>

double toMS(struct timespec* ts) {
  return (double)ts->tv_sec * 1000L + (double)ts->tv_nsec / 1000000.0;
}

size_t marked = 0;

SEXP intProtect[3] = {NULL, NULL, NULL};

FORCE_INLINE void PROCESS_NODES();
FORCE_INLINE void PUSH_NODE(SEXP);

void updatePageMark(Page* p) {
  PageHashtable_get(HEAP.sizeBucket[p->bkt].pagesHt, p)->last_mark = THE_MARK;
  p->last_mark = THE_MARK;
}

#define markIfUnmarked(s, what)                                        \
  if (ISNODE((s))) {                                                   \
    Page* _p_ = PTR2PAGE((s));                                         \
    CHECK((uintptr_t)(s) < _p_->alloc_finger);                         \
    size_t _i_ = PTR2IDX((s));                                         \
    if (_p_->mark[_i_] < THE_MARK) {                                   \
      if (_p_->last_mark < THE_MARK)                                   \
        updatePageMark(_p_);                                           \
      _p_->mark[_i_] = THE_MARK;                                       \
      { what; }                                                        \
    }                                                                  \
  } else {                                                             \
    ObjHashtableEntry* e = ObjHashtable_get(HEAP.bigObjectsHt, s);     \
    if ((s)->sxpinfo.old == 0 || e->mark < THE_MARK) {                 \
      e->mark = THE_MARK;                                              \
      { what; }                                                        \
    }                                                                  \
  }

void clear_marks();
void processStackNodes();

void doGc(unsigned bkt) {
#ifdef GCPROF
  struct timespec time1, time11, time2, time3, time4;
  marked = 0;
  clock_gettime(CLOCK_MONOTONIC, &time1);
#endif

#ifdef CONSERVATIVE_STACK_SCAN
  traceStack();
#endif

#ifdef GCPROF
  clock_gettime(CLOCK_MONOTONIC, &time11);
#endif

  // clear mark bits
  if (fullCollection) {
    if (THE_MARK == 0xff) {
      clear_marks();
      THE_MARK = 1;
    } else {
      THE_MARK++;
    }
  }

#ifdef GCPROF
  clock_gettime(CLOCK_MONOTONIC, &time2);
#endif

  markIfUnmarked(R_NilValue, {});
  PROCESS_NODE(R_NilValue);

#ifdef CONSERVATIVE_STACK_SCAN
  processStackNodes();
#else
  if (intProtect[0]) {
    PUSH_NODE(intProtect[0]);
    intProtect[0] = NULL;
  }
  if (intProtect[1]) {
    PUSH_NODE(intProtect[1]);
    intProtect[1] = NULL;
  }
  if (intProtect[2]) {
    PUSH_NODE(intProtect[2]);
    intProtect[2] = NULL;
  }
#endif

  traceHeap();
  CHECK(MSpos == 0);

#ifdef GCPROF
  clock_gettime(CLOCK_MONOTONIC, &time3);
#endif

  free_unused_memory();
  gc_cnt++;
#if GCPROF
  if (fullCollection || gc_cnt % 10 == 0)
    heapStatistics();
#endif

  double heapPressure = heap_pressure();
  double pagePressure = page_pressure();
  Rboolean oversize = FALSE;
  if (heapPressure > HEAP_SIZE_SLACK && fullCollection) {
    HEAP.heapLimit *= HEAP_GROW_RATE;
    if (HEAP.size > HEAP.heapLimit) {
      oversize = TRUE;
      HEAP.heapLimit = HEAP.size + HEAP.size * (1-HEAP_SIZE_SLACK);
    }
#ifdef GCPROF
    printf("Growing heap limit to %f\n", HEAP.heapLimit/1024.0/1024.0);
#endif
  } else if (heapPressure < HEAP_SIZE_MAX_SLACK && fullCollection) {
    HEAP.heapLimit *= HEAP_SHRINK_RATE;
  }

  if (pagePressure > HEAP_PAGES_SLACK && fullCollection) {
    HEAP.page_limit *= PAGES_GROW_RATE;
#ifdef GCPROF
    printf("Growing page limit to %d\n", HEAP.page_limit);
#endif
  }

#ifdef GCPROF
  clock_gettime(CLOCK_MONOTONIC, &time4);
  printf(
      "Gc %d (%d) of gen %d : took %f for ss, %f to clear, %f to mark %d, %f to free, "
      "total %fms\n",
      gc_cnt,
      bkt,
      fullCollection,
      toMS(&time11) - toMS(&time1),
      toMS(&time2) - toMS(&time11),
      toMS(&time3) - toMS(&time2),
      toMS(&time4) - toMS(&time3),
      toMS(&time4) - toMS(&time1),
      marked);
#endif
  fullCollection =
    heapPressure > FULL_COLLECTION_TRIGGER ||
    pagePressure > FULL_COLLECTION_TRIGGER ||
    oversize;
}

void clear_marks() {
  for (size_t s = 0; s < NUM_BUCKETS; ++s) {
    SizeBucket* bucket = &HEAP.sizeBucket[s];

    size_t ht_size = bucket->pagesHt->size * HASH_BUCKET_SIZE;
    for (size_t i = 0; i < ht_size; ++i) {
      PageHashtableEntry* e = &bucket->pagesHt->data[i];
      if (e->page != NULL) {
        Page* p = e->page;
        CHECK(p->sweep_finger <= p->sweep_end &&
              p->sweep_end <= p->alloc_finger);
        p->last_mark = UNMARKED;
        e->last_mark = UNMARKED;
        p->sweep_finger = p->sweep_end;
        memset(&p->mark, UNMARKED, MAX_IDX);
      }
    }
  }
  size_t ht_size = HEAP.bigObjectsHt->size * HASH_BUCKET_SIZE;
  for (int i = 0; i < ht_size; ++i) {
    ObjHashtableEntry* e = &HEAP.bigObjectsHt->data[i];
    if (e->entry) {
      e->mark = UNMARKED;
    }
  }
}

static R_INLINE R_size_t getVecSizeInVEC(SEXP);
void free_unused_memory() {
  uint8_t M = THE_MARK;
  for (size_t s = 0; s < NUM_BUCKETS; ++s) {
    SizeBucket* bucket = &HEAP.sizeBucket[s];

    size_t ht_size = bucket->pagesHt->size * HASH_BUCKET_SIZE;
    for (size_t i = 0; i < ht_size; ++i) {
      PageHashtableEntry* e = &bucket->pagesHt->data[i];
      if (e->page != NULL) {
        if (e->last_mark < M) {
          CHECK(e->last_mark == e->page->last_mark);
          deletePage(bucket, e->page);
#ifdef CONSERVATIVE_STACK_SCAN
          PageHashtable_remove(HEAP.pagesHt, e->page);
#endif
          PageHashtable_remove_el(bucket->pagesHt, i);
          --i;
        } else {
          if (e->page == bucket->to_bump) {
            e->state = SPLIT;
            // split pages has still alloc space left. We need to remember
            // where the bump space begins, and cannot set the sweep_end
            // lazily.
            e->page->sweep_end = e->page->alloc_finger;
          } else {
            if (e->state != FULL || fullCollection)
              e->state = MARKED;
          }
        }
      }
    }

    bucket->sweep_idx = 0;
    findPageToSweep(bucket);
  }

  size_t ht_size = HEAP.bigObjectsHt->size * HASH_BUCKET_SIZE;
  for (int i = 0; i < ht_size; ++i) {
    ObjHashtableEntry* e = &HEAP.bigObjectsHt->data[i];
    if (e->entry && e->mark < M) {
      size_t sz = getVecSizeInVEC(e->entry) * sizeof(VECREC) + sizeof(VECTOR_SEXPREC);
#ifdef LONG_VECTOR_SUPPORT
      if (XLENGTH(e->entry) > R_SHORT_LEN_MAX)
        sz += sizeof(R_long_vec_hdr_t);
#endif
      HEAP.size -= sz;
      ON_DEBUG(memset(e->entry, 0xd0, sz));
      free(e->entry);
      ObjHashtable_remove_el(HEAP.bigObjectsHt, i);
      --i;
    }
  }
}

static inline void FORWARD_NODE(SEXP s) {
  if (s == NULL || s == R_NilValue)
    return;
  markIfUnmarked(s, {
    PROCESS_NODE(s);
#ifdef GCPROF
    ++marked;
#endif
  });
  if (MSpos > MS_TRIGGER)
    PROCESS_NODES();
}

FORCE_INLINE void PROCESS_NODES() {
  while (MSpos > 0) {
    PROCESS_NODE(MarkStack[--MSpos]);
  }
}

void growMarkStack() {
  size_t old_size = MSsize*sizeof(SEXP);
  MSsize *= 1.5;
  size_t new_size = MSsize*sizeof(SEXP);
  SEXP* newMS = malloc(new_size);
  memcpy(newMS, MarkStack, old_size);
  free(MarkStack);
  MarkStack = newMS;
}

FORCE_INLINE void PUSH_NODE(SEXP s) {
  if (s == NULL || s == R_NilValue)
    return;
  if (MSpos >= MSsize) {
    growMarkStack();
  }
  markIfUnmarked(s, {
#ifdef GCPROF
    ++marked;
#endif
    MarkStack[MSpos++] = s;
  });
}

#define MARK_OLD(s)               \
  if ((s)->sxpinfo.old != 1)      \
    (s)->sxpinfo.old = 1

FORCE_INLINE void PROCESS_NODE(SEXP cur) {
  SEXP attrib = ATTRIB(cur);
  switch (TYPEOF(cur)) {
    case CHARSXP:
      // not marked old, see ATTRIB_WRITE_BARRIER
      if (attrib != R_NilValue && TYPEOF(attrib) != CHARSXP)
        PUSH_NODE(ATTRIB(cur));
      break;
    case NILSXP:
    case BUILTINSXP:
    case SPECIALSXP:
    case LGLSXP:
    case INTSXP:
    case REALSXP:
    case CPLXSXP:
    case WEAKREFSXP:
    case RAWSXP:
    case S4SXP:
      // not marked old, see ATTRIB_WRITE_BARRIER
      if (attrib != R_NilValue)
        PUSH_NODE(ATTRIB(cur));
      break;
    case STRSXP:
    case EXPRSXP:
    case VECSXP:
      MARK_OLD(cur);
      if (attrib != R_NilValue)
        PUSH_NODE(ATTRIB(cur));
      {
        R_xlen_t i;
        for (i = 0; i < XLENGTH(cur); i++)
          PUSH_NODE(VECTOR_ELT(cur, i));
      }
      break;
    case ENVSXP:
      MARK_OLD(cur);
      if (attrib != R_NilValue)
        PUSH_NODE(ATTRIB(cur));
      PUSH_NODE(FRAME(cur));
      PUSH_NODE(ENCLOS(cur));
      PUSH_NODE(HASHTAB(cur));
      break;
    case CLOSXP:
    case PROMSXP:
    case LISTSXP:
    case LANGSXP:
    case DOTSXP:
    case SYMSXP:
    case BCODESXP:
      MARK_OLD(cur);
      if (attrib != R_NilValue)
        PUSH_NODE(ATTRIB(cur));
      PUSH_NODE(TAG(cur));
      PUSH_NODE(CAR(cur));
      PUSH_NODE(CDR(cur));
      break;
    case EXTPTRSXP:
      MARK_OLD(cur);
      if (attrib != R_NilValue)
        PUSH_NODE(ATTRIB(cur));
      PUSH_NODE(EXTPTR_PROT(cur));
      PUSH_NODE(EXTPTR_TAG(cur));
      break;
    default:
      CHECK(0);
  }
}

// #define WRITE_BARRIER_PROMOTE
#define WRITE_BARRIER_BLACK_TO_WHITE
void write_barrier_trigger(SEXP x, SEXP y) {
#ifdef WRITE_BARRIER_BLACK_TO_WHITE
  // To avoid the barrier triggering multiple times we clear the old bit for as
  // long as the node is in the mark queue.
  x->sxpinfo.old = 0;
  if (ISNODE(x)) {
    PTR2PAGE(x)->mark[PTR2IDX(x)] = UNMARKED;
  } else {
    ObjHashtable_get(HEAP.bigObjectsHt, x)->mark = UNMARKED;
  }
  PUSH_NODE(x);
#endif
#ifdef WRITE_BARRIER_PROMOTE
  PUSH_NODE(y);
#endif
  if (MSpos > WRITE_BARRIER_MS_TRIGGER)
    PROCESS_NODES();
}

#define WRITE_BARRIER(x, y)                                       \
  if (!fullCollection && (x)->sxpinfo.old && !(y)->sxpinfo.old) { \
    write_barrier_trigger(x, y);                                  \
  }

void ATTRIB_WRITE_BARRIER(SEXP x, SEXP y) {
  switch (TYPEOF(x)) {
  case CHARSXP:
  case NILSXP:
  case BUILTINSXP:
  case SPECIALSXP:
  case LGLSXP:
  case INTSXP:
  case REALSXP:
  case CPLXSXP:
  case WEAKREFSXP:
  case RAWSXP:
  case S4SXP:
    // SEXPs with just the attrib pointer are not marked old for efficiency
    // reasons. That means that we need a more expensive write barrier.
    if (!fullCollection && NODE_IS_MARKED(x) && !NODE_IS_MARKED(y)) {
      write_barrier_trigger(x, y);
    }
    break;
  case STRSXP:
  case EXPRSXP:
  case VECSXP:
  case ENVSXP:
  case CLOSXP:
  case PROMSXP:
  case LISTSXP:
  case LANGSXP:
  case DOTSXP:
  case SYMSXP:
  case BCODESXP:
  case EXTPTRSXP:
    WRITE_BARRIER(x, y);
    break;
  default:
    CHECK(0);
  }
}

#define GET_FREE_NODE(s)                      \
  do {                                        \
    (s) = allocInBucket(GENERIC_SEXP_BUCKET); \
  } while (0)

#define ALLOC_SEXP(s, t)                      \
  do {                                        \
    (s) = allocInBucket(GENERIC_SEXP_BUCKET); \
  } while (0)

#define ALLOC_CONS(s, p1, p2)         \
  do {                                \
    intProtect[0] = (p1);             \
    intProtect[1] = (p2);             \
    (s) = allocInBucket(CONS_BUCKET); \
  } while (0)

#define ALLOC_ENV(s, p1, p2, p3)     \
  do {                               \
    intProtect[0] = (p1);            \
    intProtect[1] = (p2);            \
    intProtect[2] = (p3);            \
    (s) = allocInBucket(ENV_BUCKET); \
  } while (0)

#define ALLOC_PROM(s, p1, p2)         \
  do {                                \
    intProtect[0] = (p1);             \
    intProtect[1] = (p2);             \
    (s) = allocInBucket(PROM_BUCKET); \
  } while (0)


#ifdef CONSERVATIVE_STACK_SCAN

void growTSStack() {
  size_t old_size = TSsize*sizeof(SEXP);
  TSsize *= 2;
  size_t new_size = TSsize*sizeof(SEXP);
  SEXP* newTS = malloc(new_size);
  memcpy(newTS, TraceStackStack, old_size);
  free(TraceStackStack);
  TraceStackStack = newTS;
}

FORCE_INLINE Rboolean isNewValidSexp(void* ptr) {
  if (ptr == NULL || (uintptr_t)ptr % 8 != 0)
    return FALSE;
  Page* page = PTR2PAGE(ptr);
  if (page == NULL)
    return FALSE;
  PageHashtableEntry* e;
  if ((e = PageHashtable_get(HEAP.pagesHt, page))) {
    CHECK(e->page == page);

    uintptr_t pos = (uintptr_t)ptr;
    Rboolean aligned = (pos - page->start) % BUCKET_SIZE[page->bkt];
    if (aligned != 0 || pos < page->start)
      return FALSE;

    if (pos >= page->alloc_finger)
      return FALSE;

    // fresh allocation
    if (pos >= page->sweep_end)
      return TRUE;

    // swept space
    if ((e->state == SWEEPING || e->state == SWEPT) && pos < page->sweep_finger) {
      if (fullCollection)
        return TRUE;
      return page->mark[PTR2IDX(ptr)] < THE_MARK;
    }

    // Not yet swept space
    return fullCollection && page->mark[PTR2IDX(ptr)] == THE_MARK;
  }
  return ObjHashtable_exists(HEAP.bigObjectsHt, ptr) &&
      (fullCollection || PTR2BIG(ptr)->mark < THE_MARK);
}


extern void doTraceStack() {
  void ** p = (void**)__builtin_frame_address(0);

  while ((uintptr_t)p != R_CStackStart) {
    if (isNewValidSexp(*p)) {
      //printf("found %p\n", *p);
      CHECK((uintptr_t)*p % 8 == 0);
      CHECK(fullCollection || !ISMARKED(*p));
      CHECK(ATTRIB((SEXP)*p) == NULL ||
          (!ISMARKED(ATTRIB((SEXP)*p)) || ISMARKED(ATTRIB((SEXP)*p))));
#ifdef GCPROF
        ++marked;
#endif
        if (TSpos >= TSsize)
          growTSStack();
        TraceStackStack[TSpos++] = *p;
    }
    p += R_CStackDir;
  }
}

void processStackNodes() {
  while (TSpos > 0) {
    SEXP e = TraceStackStack[--TSpos];
    // On full collection marks where cleared before tracing starts. We need to
    // set them again.
    markIfUnmarked(e, {
      PROCESS_NODE(e);
    });
  }
}

void traceStack() {
  // Clobber all registers, this should spill them to the stack.
  // -> force all variables currently hold in registers to be spilled
  //    to the stack where our stackScan can find them.
  __asm__ __volatile__(
      #ifdef __APPLE__
      "push %%rbp \n\t call _doTraceStack \n\t pop %%rbp"
      #else
      "push %%rbp \n\t call doTraceStack \n\t pop %%rbp"
      #endif
      : : 
      : "%rax", "%rbx", "%rcx", "%rdx", "%rsi", "%rdi",
      "%r8", "%r9", "%r10", "%r11", "%r12",
      "%r13", "%r14", "%r15");
}
#endif

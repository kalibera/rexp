#define HASH_BUCKET_SIZE 8
#define HASH_TABLE_INIT_SIZE (1024/HASH_BUCKET_SIZE)

struct Page;

enum PageState { FRESH, SPLIT, MARKED, SWEEPING, SWEPT, FULL };

#pragma pack(push, 1)
typedef struct PageHashtableEntry {
  struct Page* page;
  uint8_t last_mark;
  uint8_t state;
} PageHashtableEntry;
#pragma pack(pop)

typedef struct PageHashtable {
  size_t size;
  PageHashtableEntry data[];
} PageHashtable;

void PageHashtable_init(PageHashtable** ht) {
  size_t size = 64;

  // make the size 1 too big to make it safe to read one element after the
  // bounds. See PageHashtable_remove_el for an example.
  size_t sz = sizeof(PageHashtable) + size * HASH_BUCKET_SIZE * sizeof(PageHashtableEntry) + sizeof(PageHashtableEntry);
  PageHashtable* h = malloc(sz);
  if (h == NULL)
    exit(1);
  memset(h, 0, sz);
  h->size = size;
  *ht = h;
}

Rboolean PageHashtable_add(PageHashtable* ht, PageHashtableEntry);
void PageHashtable_grow(PageHashtable** ht) {
  PageHashtable* old = *ht;
  size_t size = old->size * 2;
  size_t sz = sizeof(PageHashtable) + size * HASH_BUCKET_SIZE * sizeof(PageHashtableEntry) + sizeof(PageHashtableEntry);
  PageHashtable* h = malloc(sz);
  if (h == NULL)
    exit(1);
  memset(h, 0, sz);
  h->size = size;
  size_t max = old->size * HASH_BUCKET_SIZE;
  for (size_t i = 0; i < max; ++i) {
    if (old->data[i].page != NULL)
      PageHashtable_add(h, old->data[i]);
  }
  *ht = h;
  free(old);
}

FORCE_INLINE uint32_t PageHashtable_h(void* k) {
  uint32_t a = (uintptr_t)k >> PAGE_IDX_BITS;
  a = (a ^ 61) ^ (a >> 16);
  a = a + (a << 3);
  a = a ^ (a >> 4);
  a = a * 0x27d4eb2d;
  a = a ^ (a >> 15);
  return a;
}

Rboolean PageHashtable_add(PageHashtable* ht, PageHashtableEntry e) {
  long key = PageHashtable_h(e.page);
  long idx = HASH_BUCKET_SIZE * (key & (ht->size - 1));
  long el = 0;
  while (ht->data[idx + el].page != NULL && el < HASH_BUCKET_SIZE) {
    if (ht->data[idx + el].page == e.page) {
      R_Suicide("ht err 3");
    }
    ++el;
  }
  if (el == HASH_BUCKET_SIZE) {
    return FALSE;
  }
  CHECK(ht->data[idx + el].page == NULL);
  CHECK(el >= 0 && el < HASH_BUCKET_SIZE);
  ht->data[idx + el] = e;
  return TRUE;
}

PageHashtableEntry* PageHashtable_get(PageHashtable* ht, struct Page* p) {
  long key = PageHashtable_h(p);
  long idx = HASH_BUCKET_SIZE * (key & (ht->size - 1));
  long el = 0;
  while (el < HASH_BUCKET_SIZE && ht->data[idx + el].page != NULL) {
    if (ht->data[idx + el].page == p)
      return &ht->data[idx + el];
    ++el;
  }
  return NULL;
}

void PageHashtable_remove_el(PageHashtable* ht, size_t pos) {
  do {
    ht->data[pos] = ht->data[pos + 1];
    ++pos;
  } while (pos % HASH_BUCKET_SIZE != 0);
  ht->data[pos - 1].page = NULL;
}

void PageHashtable_remove(PageHashtable* ht, void* p) {
  long key = PageHashtable_h(p);
  long idx = HASH_BUCKET_SIZE * (key & (ht->size - 1));
  long el = 0;
  while (el < HASH_BUCKET_SIZE) {
    if (ht->data[idx + el].page == p) {
      PageHashtable_remove_el(ht, idx + el);
      return;
    }
    el++;
  }
  CHECK(0);
}

#pragma pack(push, 1)
typedef struct ObjHashtableEntry {
  SEXP entry;
  uint8_t mark;
} ObjHashtableEntry;
#pragma pack(pop)

typedef struct ObjHashtable {
  size_t size;
  ObjHashtableEntry data[];
} ObjHashtable;

void ObjHashtable_init(ObjHashtable** ht) {
  size_t size = 64;

  size_t sz = sizeof(ObjHashtable) + size * HASH_BUCKET_SIZE * sizeof(ObjHashtableEntry) + sizeof(ObjHashtableEntry);
  ObjHashtable* h = malloc(sz);
  if (h == NULL)
    exit(1);
  memset(h, 0, sz);
  h->size = size;
  *ht = h;
}

Rboolean ObjHashtable_add(ObjHashtable* ht, ObjHashtableEntry);
void ObjHashtable_grow(ObjHashtable** ht) {
  ObjHashtable* old = *ht;
  size_t size = old->size * 2;
  size_t sz =
      sizeof(ObjHashtable) + size * HASH_BUCKET_SIZE * sizeof(ObjHashtableEntry) + sizeof(ObjHashtableEntry);
  ObjHashtable* h = malloc(sz);
  if (h == NULL)
    exit(1);
  memset(h, 0, sz);
  h->size = size;
  size_t max = old->size * HASH_BUCKET_SIZE;
  for (size_t i = 0; i < max; ++i) {
    if (old->data[i].entry != NULL)
      ObjHashtable_add(h, old->data[i]);
  }
  *ht = h;
  free(old);
}

FORCE_INLINE uint32_t ObjHashtable_h(void* k) {
  uint32_t a = (uintptr_t)k >> 2;
  a = (a ^ 61) ^ (a >> 16);
  a = a + (a << 3);
  a = a ^ (a >> 4);
  a = a * 0x27d4eb2d;
  a = a ^ (a >> 15);
  return a;
}

Rboolean ObjHashtable_add(ObjHashtable* ht, ObjHashtableEntry e) {
  long key = ObjHashtable_h(e.entry);
  long idx = HASH_BUCKET_SIZE * (key & (ht->size - 1));
  long el = 0;
  while (ht->data[idx + el].entry != NULL && el < HASH_BUCKET_SIZE) {
    if (ht->data[idx + el].entry == e.entry) {
      R_Suicide("ht err 3");
    }
    ++el;
  }
  if (el == HASH_BUCKET_SIZE) {
    return FALSE;
  }
  CHECK(ht->data[idx + el].entry == NULL);
  CHECK(el >= 0 && el < HASH_BUCKET_SIZE);
  ht->data[idx + el] = e;
  return TRUE;
}

FORCE_INLINE ObjHashtableEntry* ObjHashtable_get(ObjHashtable* ht, SEXP p) {
  long key = ObjHashtable_h(p);
  long idx = HASH_BUCKET_SIZE * (key & (ht->size - 1));
  long el = 0;
  while (el < HASH_BUCKET_SIZE && ht->data[idx + el].entry != NULL) {
    if (ht->data[idx + el].entry == p)
      return &ht->data[idx + el];
    ++el;
  }
  R_Suicide("ht expt 6");
  return NULL;
}


Rboolean ObjHashtable_exists(ObjHashtable* ht, SEXP p) {
  long key = ObjHashtable_h(p);
  long idx = HASH_BUCKET_SIZE * (key & (ht->size - 1));
  long el = 0;
  while (el < HASH_BUCKET_SIZE && ht->data[idx + el].entry != NULL) {
    if (ht->data[idx + el].entry == p)
      return TRUE;
    ++el;
  }
  return FALSE;
}

void ObjHashtable_remove_el(ObjHashtable* ht, size_t pos) {
  do {
    ht->data[pos] = ht->data[pos + 1];
    ++pos;
  } while (pos % HASH_BUCKET_SIZE != 0);
  ht->data[pos - 1].entry = NULL;
}

void ObjHashtable_remove(ObjHashtable* ht, SEXP p) {
  long key = ObjHashtable_h(p);
  long idx = HASH_BUCKET_SIZE * (key & (ht->size - 1));
  long el = 0;
  while (el < HASH_BUCKET_SIZE) {
    if (ht->data[idx + el].entry == p) {
      ObjHashtable_remove_el(ht, idx + el);
      return;
    }
    el++;
  }
  CHECK(0);
}



#include "Common.h"
#include <sys/mman.h>


#define ALLOC_SIZE 65536

#define MURMUR_CONST 0xC6A4A7935BD1E995
#define MURMUR_SEED  0xA06A43342758469B


static uint8 *alloc_area = NULL;
static uint64 alloc_top = 0;
static uint64 last_alloc_size = 0;
static bool reduce = FALSE;


void BugOn(char *expr_str, char *file, int line)
{
	printf("\x1B[1m%s:%d:\x1B[31;22m Assertion failed: \x1B[0m%s\n", file, line, expr_str);
	exit(1);
}

void *Alloc(uint64 size)
{
	Assert(size < ALLOC_SIZE);
	Assert(!reduce);
	last_alloc_size = size;

	if(alloc_area == NULL || alloc_top + size >= ALLOC_SIZE) {
		alloc_area = mmap(NULL, ALLOC_SIZE, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
		Assert(alloc_area != MAP_FAILED);
		alloc_top = 0;
	}

	if((alloc_top & 7) != 0)
		alloc_top += 8 - (alloc_top & 7);

	alloc_top += size;
	return &alloc_area[alloc_top - size];
}

void *AllocReduced(uint64 size)
{
	void *ptr = Alloc(size);
	reduce = TRUE;
	return ptr;
}

void Reduce(uint64 size)
{
	Assert(size <= last_alloc_size);
	Assert(reduce);
	alloc_top -= last_alloc_size - size;
	reduce = FALSE;
}

// MurmurHash64A by Austin Appleby
uint64 Hash(void *ptr, uint64 length)
{
	char *str = ptr;
	uint64 hash = MURMUR_SEED ^ (length * MURMUR_CONST);
	uint64 *start = (uint64 *) str;
	uint64 *end = start + length / 8;

	while(start != end) {
		uint64 factor = *start++;
		factor *= MURMUR_CONST;
		factor ^= factor >> 47;
		factor *= MURMUR_CONST;
		hash ^= factor;
		hash *= MURMUR_CONST;
	}

	str = (char *) start;

	switch(length & 7)
	{
	case 7: hash ^= (uint64) str[6] << 48;
	case 6: hash ^= (uint64) str[5] << 40;
	case 5: hash ^= (uint64) str[4] << 32;
	case 4: hash ^= (uint64) str[3] << 24;
	case 3: hash ^= (uint64) str[2] << 16;
	case 2: hash ^= (uint64) str[1] << 8;
	case 1: hash ^= (uint64) str[0];
	case 0: hash *= MURMUR_CONST;
	}

	hash ^= hash >> 47;
	hash *= MURMUR_CONST;
	hash ^= hash >> 47;
	return hash;
}

char *Reverse(char *str)
{
	uint64 len = strlen(str);
	char *new_str = Alloc(len + 1);

	for(uint64 i = 0; i < len; i++)
		new_str[len - i - 1] = str[i];

	new_str[len] = '\0';
	return new_str;
}

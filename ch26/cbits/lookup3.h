#ifndef _lookup3_h
#define _lookup3_h

#include <stdint.h>
#include <sys/types.h>

void hashword2(const uint32_t *key,
               size_t length,
               uint32_t *pc,
               uint32_t *pb);

void hashlittle2(const void *key,
                 size_t length,
                 uint32_t *pc,
                 uint32_t *pb);

#endif /* _lookup3_h */

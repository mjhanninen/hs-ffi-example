#ifndef __FROBNICATOR_LIB_H_INCLUDED__
#define __FROBNICATOR_LIB_H_INCLUDED__

#include <inttypes.h>

typedef struct context_s context_t;
typedef struct measurement_s measurement_t;

struct measurement_s
{
  uint32_t time;
  double value;
};

/**
 * Allocates and initalizes a new context returning a poiter to it. Returns a
 * null pointer upon failure.
 */
context_t *alloc_context();

/**
 * Disposes the context releasing all the resources tied to it. Returns 0 on
 * success and a negative value upon failure.
 */
int release_context(context_t *context);

/**
 * Makes a measurement and updates `measurement` accordingly. Returns 0 on
 * success and a negative value upon failure.
 */
int measure(context_t *context, measurement_t *measurement);

#endif // __FROBNICATOR_LIB_H_INCLUDED__

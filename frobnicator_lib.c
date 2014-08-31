#include "frobnicator_lib.h"

#include <stdio.h>
#include <stdlib.h>
#include <time.h>

static void
_rinit()
{
  unsigned int t = (unsigned int) time(NULL);
  printf("C:  Using random seed %d\n", t);
  srand(t);
  // WTF?! The very first call to rand() after srand() seemd to yield always
  // the same deviate whatever the seed we use! We have to pop the first one
  // away here.
  rand();
}

static double
_runif(double low, double high)
{
  double u = (double) rand() / (double) RAND_MAX;
  return (1.0 - u) * low + u * high;
}

struct context_s
{
  uint32_t time;
  double prev_value;
  double mean;
  double variation;
  double alpha;
};

context_t *
alloc_context()
{
  context_t *context;
  _rinit();
  if (_runif(0, 1) > 0.25)
    {
      printf("C:  Allocating context\n");
      context = (context_t *) malloc(sizeof(context_t));
    }
  else
    {
      printf("C:  Simulating allocation failure\n");
      context = NULL;
    }
  if (context)
    {
      context->time = 0;
      context->prev_value = 0.5;
      context->mean = 0.5;
      context->variation = 0.2;
      context->alpha = 0.8;
    }
  return context;
}

int
release_context(context_t *p_context)
{
  printf("C:  Releasing context\n");
  free(p_context);
  return 0;
}

int
measure(context_t *context, measurement_t *measurement)
{
  printf("C:  Measuring value\n");
  if (context)
    {
      printf("C:  Has context\n");
      if (_runif(0, 1) > 0.25)
        {
          double eps = _runif(-0.5, 0.5);
          double new_value =
            context->alpha * context->mean +
            (1.0 - context->alpha) * context->prev_value +
            context->variation * eps;
          measurement->time = context->time;
          measurement->value = new_value;
          context->time += 1;
          context->prev_value = new_value;
          printf("C:  Simulating success -- time = %u, value = %.3f\n",
                 measurement->time, measurement->value);
          return 0;
        }
      else
        {
          printf("C:  Simulating read failure\n");
          return -1;
        }
    }
  else
    {
      printf("C:  Missing context\n");
      return -1;
    }
}

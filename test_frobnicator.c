#include <stdio.h>

#include "frobnicator_lib.h"

int
main(void)
{
  context_t *context = alloc_context();
  if (!context)
    {
      printf("T:  Failed to create new context.\n");
      return 1;
    }
  for (int i = 0; i < 5; i++)
    {
      measurement_t measurement;
      if (measure(context, &measurement) == 0)
        {
          printf("T:  Time = %.1u, Value = %.3f\n",
                 measurement.time, measurement.value);
        }
      else
        {
          printf("T:  Failed to obtain measurement.\n");
        }
    }
  if (release_context(context) != 0)
    {
      printf("T:  Failed to release context.\n");
      return 1;
    }
  return 0;
}

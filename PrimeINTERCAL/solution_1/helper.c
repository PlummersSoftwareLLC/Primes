#define _POSIX_C_SOURCE 200809L

#include <ick_ec.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

static uint32_t current_micros(void)
{
  struct timespec ts;
  uint64_t micros;

  if (clock_gettime(CLOCK_MONOTONIC, &ts) != 0) {
    perror("clock_gettime");
    exit(EXIT_FAILURE);
  }

  micros = (uint64_t)ts.tv_sec * 1000000ULL + (uint64_t)ts.tv_nsec / 1000ULL;
  return (uint32_t)micros;
}

ICK_EC_FUNC_START(cwager_bench_support)
{
  ick_linelabel(6000);
  ick_settwospot(1, current_micros());
  ick_resume(1);
  return;

  ick_linelabel(6010);
  printf(
      "cwager_intercal;%u;%.6f;1;algorithm=base,faithful=yes,bits=1\n",
      (unsigned)ick_getonespot(1),
      ick_gettwospot(1) / 1000000.0);
  fflush(stdout);
  ick_resume(1);
  return;
}
ICK_EC_FUNC_END

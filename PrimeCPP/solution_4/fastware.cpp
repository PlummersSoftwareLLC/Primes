#include "primes.h"

#include <chrono>
#include <cstdio>
#include <future>
#include <thread>
#include <vector>

constexpr uint64_t numbers[] = {10ul,     100ul,     1000ul,    10000ul,
                                100000ul, 1000000ul, 10000000ul};
constexpr uint64_t prime_counts[] = {4, 25, 168, 1229, 9592, 78498, 664579};

int64_t count_primes(int64_t number) {
  int64_t count = 0;
  const int32_t loops = ((number + 64) / 64) - 1;
  for (int32_t idx = 0; idx < loops; idx++) {
    count += __builtin_popcountl(primes[idx]);
  }

  const int32_t remainder = number % 64;
  const uint64_t mask = (1ul << remainder) - 1;
  return count + __builtin_popcountl(primes[loops] & mask);
}

void test() {

  for (int32_t i = 0; i < sizeof(numbers) / sizeof(uint64_t); i++) {
    const auto prime_count = count_primes(numbers[i]);
    printf("Prime count under %10lu. Expected: %10lu, Got: %10lu, Match: %s\n",
           numbers[i], prime_counts[i], prime_count,
           prime_counts[i] == prime_count ? "yes" : "no");
  }
}

void print_results(double duration, int32_t passes, int32_t threads) {
  printf("fastware;%d;%f;%d;algorithm=table,faithful=no,bits=1\n", passes,
         duration, threads);
}

static void do_not_optimize(const uint64_t &value) {
  asm volatile("" : : "r,m"(value) : "memory");
}

struct run_res_t {
  uint64_t passes;
  double duration;
};

run_res_t run(uint32_t req_duration, uint64_t number) {
  using namespace std::chrono;

  run_res_t res{0, 0};
  const auto tStart = high_resolution_clock::now();

  while (true) {
    const uint64_t count = count_primes(number);
    do_not_optimize(count);
    res.passes++;

    const auto duration = high_resolution_clock::now() - tStart;
    if (duration_cast<seconds>(duration).count() >= req_duration) {
      res.duration = duration_cast<microseconds>(duration).count() / 1000000.0;
      break;
    }
  }
  return res;
}

void run(uint32_t duration, uint32_t thread_count, uint64_t number) {

  std::vector<std::future<run_res_t>> results;
  results.reserve(thread_count);

  for (int32_t i = 0; i < thread_count; i++) {
    results.push_back(std::async(std::launch::async, [duration, number]() {
      return run(duration, number);
    }));
  }

  run_res_t final_result{0, 0};

  for (auto &res : results) {
    const run_res_t val = res.get();
    final_result.passes += val.passes;
    final_result.duration = std::max(final_result.duration, val.duration);
  }
  print_results(final_result.duration, final_result.passes, thread_count);
}

int main(int argc, char **argv) {

  test();

  const int32_t duration = 5; // seconds
  const uint64_t problem_size = 1000000ul;

  run(duration, 1, problem_size);
  run(duration, std::thread::hardware_concurrency(), problem_size);

  return 0;
}

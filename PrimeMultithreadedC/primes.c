#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <pthread.h>
#include <unistd.h>
#include <sys/sysinfo.h>
#include <sys/time.h>

typedef unsigned long num_t;

num_t limit;
num_t* primes;
num_t primes_size = 0;
num_t primes_upto = 3;
pthread_mutex_t primes_lock;
num_t* workers_upto;
int cpus;

const num_t myDict[10][2] =
{
      {          10L, 4         },                // Historical data for validating our results - the number of primes
      {         100L, 25        },               // to be found under some limit, such as 168 primes under 1000
      {        1000L, 168       },
      {       10000L, 1229      },
      {      100000L, 9592      },
      {     1000000L, 78498     },
      {    10000000L, 664579    },
      {   100000000L, 5761455   },
      {  1000000000L, 50847534  },
      { 10000000000L, 455052511 },
};

int num_compare(const void* va, const void* vb)
{
	num_t a = *((num_t*)va);
	num_t b = *((num_t*)vb);

	if(a == b) return 0;
	if(a < b) return -1;
	else return 1;
}

void* statsworker(void* p)
{
	for(;;)
	{
		// 10 second interval
		usleep(10000000L);

		pthread_mutex_lock(&primes_lock);

		if(primes_upto >= limit)
		{
			pthread_mutex_unlock(&primes_lock);

			return NULL;
		}

		fprintf(stderr, "Found %lu primes, %f%% done\n", primes_size + 1, primes_upto / (double)limit * 100);

		pthread_mutex_unlock(&primes_lock);
	}
}

void* worker(void* p)
{
	int id = *((int*)p);

	num_t i, j, k;
	num_t* worker_upto = workers_upto + id;
	
	pthread_mutex_lock(&primes_lock);

	for(;;)
	{
			
nextprime:

		// get the next number to check if prime
		i = primes_upto;
		primes_upto += 2;
		*worker_upto = primes_size;

		num_t primes_done = workers_upto[0];
		
		for(int i=1;i<cpus;i++)
		{
			if(workers_upto[i] < primes_done)
			{
				primes_done = workers_upto[i];
			}
		}

		pthread_mutex_unlock(&primes_lock);
		
		if(primes_upto >= limit)
		{
			break;
		}

		// Check if the number is prime
		for(j = 0; j < primes_done; j++)
		{
			if(i % primes[j] == 0)
			{
				// Lock and get the next prime
				pthread_mutex_lock(&primes_lock);
				goto nextprime;
			}
		}

		for(k = primes[j]; k <= i / 2; k++)
		{
			if(i % k == 0)
			{
				// Lock and get the next prime
				pthread_mutex_lock(&primes_lock);
				goto nextprime;
			}
		}
		
		// Add the prime to the primes list
		pthread_mutex_lock(&primes_lock);
		primes[primes_size++] = i;

	}

	pthread_mutex_unlock(&primes_lock);
}

bool checkerrors()
{
	num_t nearest_limit = 0;
	num_t valid_primes = 1;

	for(int i = 0; i < 10; i++)
	{
		if(myDict[i][0] <= limit && myDict[i][0] > nearest_limit)
		{
			nearest_limit = myDict[i][0];
			valid_primes = myDict[i][1];
		}
	}

	num_t j;
	
	for(j = 0; j < primes_size && primes[j] < nearest_limit; j++) {}
	
	return (j + 1 != valid_primes);
}

int main(int cargs, const char** vargs)
{
	if(cargs == 2)
	{
		limit = atol(vargs[1]);
		cpus = get_nprocs();
	}

	else if(cargs == 3)
	{
		limit = atol(vargs[2]);
		cpus = atoi(vargs[1]);
	}

	else
	{
		fprintf(stderr, "Usage: ");
		fprintf(stderr, vargs[0]);
		fprintf(stderr, " <limit>\nOr:    ");
		fprintf(stderr, vargs[0]);
		fprintf(stderr, " <cpus> <limit>\n");

		return 1;
	}

	primes = (num_t*)malloc(sizeof(num_t)*limit/2);
	workers_upto = (num_t*)malloc(sizeof(num_t)*cpus);
	pthread_t* threads = (pthread_t*)malloc(sizeof(pthread_t)*cpus);
	pthread_t thread_stats;

	// fill the workers array with 0 as this is the first one that will be assigned
	for(int i = 0; i < cpus; i++)
	{
		workers_upto[i] = 0;
	}

	primes[0] = 3;

	fprintf(stderr, "Finding primes up to %lu with %d workers\n", limit, cpus);

	int* ids = (int*)malloc(sizeof(int) * cpus);
	struct timeval start, end;

	// find all the primes and start/wait for all the workers
	pthread_create(&thread_stats, NULL, statsworker, NULL);
	gettimeofday(&start, NULL);
	
	for(int i = 0; i < cpus; i++)
	{
		ids[i] = i;

		pthread_create(threads + i, NULL, worker, ids + i);
	}

	for(int i = 0; i < cpus; i++)
	{
		pthread_join(threads[i], NULL);
	}

	gettimeofday(&end, NULL);

	// final primes stats
	fprintf(stderr, "Found %lu primes in %f seconds\n", primes_size + 1, (end.tv_sec - start.tv_sec) + (end.tv_usec - start.tv_usec) / 1000000.0);

	if(checkerrors())
	{
		fprintf(stderr, "Found errors\n");
	}

	else
	{
		fprintf(stderr, "Found no errors\n");
	}

	/*
	qsort(primes, primes_size, sizeof(num_t), num_compare);
	
	// display all the primes
	printf("2\n");

	for(num_t i = 0; i < primes_size; i++)
	{
		printf("%lu\n", primes[i]);
	}
	*/

	// wait for the stats thread to exit
	//pthread_join(thread_stats, NULL);
	
	free(primes);
	free(workers_upto);
	free(threads);
	free(ids);

	return 0;
}

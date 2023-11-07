#include <cstdio>
#include <cstdlib>
#include <cmath>
#include <cstring>
#include <chrono>
#include <thread>
#include <vector>

using namespace std;
using namespace std::chrono;

namespace
{
	// All assembler code is contained within this function.
	inline void clear(unsigned char* primes, unsigned int pos, unsigned int num)
	{
		// Inner loop code for clearing one bit in memory.
		// Instructions have been reordered to help the cpu pipeline a bit.
		#define CLEARBIT \
			"movl		%3, %%edx\n\t" \
			"addl		%1, %3\n\t" \
			"shrl		$3, %%edx\n\t" \
			"andb		%%ch, (%0, %%rdx)\n\t" \
			"rolb		%%cl, %%ch\n\t"

		#define CLEARBIT16 \
			CLEARBIT \
			CLEARBIT \
			CLEARBIT \
			CLEARBIT \
			CLEARBIT \
			CLEARBIT \
			CLEARBIT \
			CLEARBIT \
			CLEARBIT \
			CLEARBIT \
			CLEARBIT \
			CLEARBIT \
			CLEARBIT \
			CLEARBIT \
			CLEARBIT \
			CLEARBIT 

		unsigned int startPos = pos * pos;
		unsigned int step = pos + pos;
		unsigned int iterations = ((num - startPos) + step - 1) / step;

		// The code will begin with clearing all iterations modulo 16 bits.
		// Then continue with clearing the rest using an unrolled 16 long loop.
		__asm__(
			"movb		$7, %%cl\n\t"
			"andb		%%al, %%cl\n\t"
			"movb		$0xfe, %%ch\n\t"
			"rolb		%%cl, %%ch\n\t"
			"movb		$7, %%cl\n\t"
			"andb		%%bl, %%cl\n\t"
			"and		$0xf, %4\n\t"
			"jmp		2f\n\t"
			"3:\n\t"
			CLEARBIT
			"dec		%4\n\t"
			"2:\n\t"
			"jne		3b\n\t"
			"jmp		4f\n\t"
			"1:\n\t"
			CLEARBIT16
			"4:\n\t"
			"cmpl		%3, %2\n\t"
			"ja			1b\n\t"
			
			// Outputs. None.
			:
			
			// Inputs. Specify specific registers for the ones we care about.
			// "D" = rdi, "b" = rbx, "a" = rax, "r" = any register.
			: "D" (primes), "b" (step), "r" (num), "a" (startPos), "r" (iterations)
			
			// Clobbers. Tell the compiler that we make a mess of the following registers.
			: "rcx", "rdx"
		);
		#undef CLEARBIT16
		#undef CLEARBIT
	}
}

class Prime
{
public:
	Prime(unsigned int n, int sec);
	~Prime(void);
	
	void Start(time_point<steady_clock> ts);
	void Join(void);
	unsigned int Passes(void) const {return passes;}
	
	unsigned int Count(void);

private:
	static void Run(Prime* p);
	void p_Run(void);
	
private:
	unsigned int num;
    unsigned int passes;
	unsigned char* primes;
	unsigned int numBytes;
	unsigned int rootNum;
	time_point<steady_clock> timeStart;
	int seconds;
	std::thread* t;
	
}; // class Prime

Prime::Prime(unsigned int n, int sec)
	: num(n)
	, passes(0)
	, seconds(sec)
{
	numBytes = (num >> 3) + 1;
	primes = new unsigned char[numBytes];
	rootNum = (unsigned int)sqrt((double)num);
}

Prime::~Prime(void)
{
	delete[] primes;
}

void Prime::Start(time_point<steady_clock> ts)
{
	timeStart = ts;
	t = new std::thread(Run, this);
}

void Prime::Join(void)
{
	t->join();
	delete t;
	t = NULL;
}


unsigned int Prime::Count(void)
{
	unsigned int numPrimes = 1;
	for (unsigned int i = 3; i < num; i += 2)
	{
		numPrimes += (primes[i >> 3] & (1 << (i & 7))) ? 1 : 0;
	}
	return numPrimes;
}

void Prime::Run(Prime* p)
{
	p->p_Run();
}

void Prime::p_Run(void)
{
	do
	{
		// The bits are layed out starting from zero.
		// So first byte contains numbers 0 to 7.
		// Meaning that even bits contain odd numbers.
		// So memory init below sets 0xaa to clear all even numbers.
		// Then sets first byte to 0xac to keep number two.
		memset(primes, 0xaa, numBytes);
		primes[0] = 0xac;
		for (unsigned int i = 3; i <= rootNum; i += 2)
		{
			// i is the number to test.
			unsigned char testByte = primes[i >> 3];
			if ((testByte & (1 << (i & 7))) != 0)
			{
				// It was a prime, so clear all its factors
				clear(primes, i, num);
			} 
		}
		++passes;
	} while(duration_cast<std::chrono::seconds>(steady_clock::now() - timeStart).count() < 5);
}

int main(void)
{
	unsigned int num = 1000000;
	int secs = 5;
	unsigned int cores = std::thread::hardware_concurrency();

	// Create workers and start sieve passes.
    auto timeStart = steady_clock::now();
	vector<Prime*> workers;
	for (unsigned int c = 0; c < cores; ++c)
	{
		workers.push_back(new Prime(num, secs));
		workers[c]->Start(timeStart);
	}
	// Wait for all workers to finish.
	unsigned int passes = 0;
	for (unsigned int c = 0; c < cores; ++c)
	{
		workers[c]->Join();
		passes += workers[c]->Passes();
	}

	//Uncomment to output number of primes found.
//	unsigned int numPrimes = workers[0]->Count();
//	printf("num %d\n", numPrimes);

	// Cleanup
	for (unsigned int c = 0; c < cores; ++c)
	{
		delete workers[c];
	}
    auto timeEnd = steady_clock::now();
	
	// Print info
	double duration = duration_cast<std::chrono::microseconds>(timeEnd - timeStart).count() / 1000000.0;
	printf("\n");       
	printf("mikaelhildenborg_cpp_inline_amd64;%d;%0.4f;%d;algorithm=base,faithful=yes,bits=1\n", passes, duration, cores);
	return 0;
}
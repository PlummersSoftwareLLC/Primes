// ---------------------------------------------------------------------------
// PrimeCPP.cpp : Dave's Garage Prime Sieve in C++
// ---------------------------------------------------------------------------

#include <chrono>
#include <iostream>
#include <vector>
#include <map>

using namespace std;

class prime_sieve_custom_bit_array
{
private:
  unsigned char* rawbits = nullptr;

public:
  prime_sieve_custom_bit_array(int n) { rawbits = static_cast<unsigned char*>(malloc(n / 8 + 1)); if (rawbits) memset(rawbits, 0xff, n / 8 + 1); }
  ~prime_sieve_custom_bit_array() { free(rawbits); }
  bool GetBit(unsigned int index) { index = index / 2; return ((rawbits[index / 8]) & (1 << (index % 8))) != 0; }
  void ClearBit(unsigned int index) { index = index / 2; rawbits[index / 8] &= ~(1 << (index % 8)); }
  void PrintName() { cout << "prime_sieve_custom_bit_array" << endl; }
};

class prime_sieve_vector_storage
{
private:
  vector<bool> rawbits;

public:
  prime_sieve_vector_storage(int n) : rawbits(n, true) {}
  bool GetBit(unsigned int index) { return rawbits[index]; }
  void ClearBit(unsigned int index) { rawbits[index] = false; }
  void PrintName() { cout << "prime_sieve_vector_storage" << endl; }
};

template<typename TStorage>
class prime_sieve
{
private:

  int sieveSize = 0;
  TStorage storage;

  const map<const int, const int> myDict =
  {
        { 10 , 1 },                 // Historical data for validating our results - the number of primes
        { 100 , 25 },               // to be found under some limit, such as 168 primes under 1000
        { 1000 , 168 },
        { 10000 , 1229 },
        { 100000 , 9592 },
        { 1000000 , 78498 },
        { 10000000 , 664579 },
        { 100000000 , 5761455 }
  };

  bool validateResults()
  {
    if (myDict.end() == myDict.find(sieveSize))
      return false;
    return myDict.find(sieveSize)->second == countPrimes();
  }

  bool GetBit(unsigned int index)
  {
    if (index % 2 == 0)
      return false;

    return storage.GetBit(index);
  }

  void ClearBit(unsigned int index)
  {
    if (index % 2 == 0)
    {
      cout << "You're setting even bits, which is sub-optimal." << endl;
      return;
    }
    storage.ClearBit(index);
  }

public:

  prime_sieve(int n) : storage(n)
  {
    sieveSize = n;
  }

  ~prime_sieve()
  {
  }

  void runSieve()
  {
    int factor = 3;
    int q = sqrt(sieveSize);

    while (factor < q)
    {
      for (int num = factor; num < sieveSize; num++)
      {
        if (GetBit(num))
        {
          factor = num;
          break;
        }
      }
      for (int num = factor * 3; num < sieveSize; num += factor * 2)
        ClearBit(num);

      factor += 2;
    }
  }

  void printResults(bool showResults, double duration, int passes)
  {
    storage.PrintName();

    if (showResults)
      cout << "2, ";

    int count = 1;
    for (int num = 3; num <= sieveSize; num++)
    {
      if (GetBit(num))
      {
        if (showResults)
          cout << num << ", ";
        count++;
      }
    }

    if (showResults)
      cout << endl;


    cout <<
      "Passes: " << passes << ", "
      "Time: " << duration << ", "
      "Avg: " << duration / passes <<", "
      "Limit: " << sieveSize << ", "
      "Count: " << count << ", "
      "Valid: " << validateResults() << endl;
  }

  int countPrimes()
  {
    int count = 0;
    for (int i = 0; i < sieveSize; i++)
      if (GetBit(i))
        count++;
    return count;
  }
};

template<typename TStorage>
void measure()
{
  auto passes = 0;
  unique_ptr<prime_sieve<TStorage>> sieve;

  auto tStart = chrono::steady_clock::now();
  while (chrono::duration_cast<chrono::seconds>(chrono::steady_clock::now() - tStart).count() < 10)
  {
    sieve = make_unique<prime_sieve<TStorage>>(1000000);
    sieve->runSieve();
    passes++;
  }
  auto tD = chrono::steady_clock::now() - tStart;

  if (sieve)
  {
    sieve->printResults(false, chrono::duration_cast<chrono::microseconds>(tD).count() / 1000000, passes);
  }
}

int main()
{
  measure<prime_sieve_custom_bit_array>();
  measure<prime_sieve_vector_storage>();
}

// SeiveBitOrs.cpp : 
//
// ---------------------------------------------------------------------------
//  : 
// ---------------------------------------------------------------------------

#include <stdlib.h>
#include <chrono>
#include <ctime>
#include <iostream>
#include <vector>
#include <map>
#include <cmath>

using namespace std;
using namespace std::chrono;

#include "Helpor.h"
//#define PRIMEBUCKETS
#include "PrimeBuckets.h"
#include "Indexor.h"
//#define R_CAST_SHIFT
#include "Replicator.h"
//#define S_NO_NEW
#include "Seiveor.h"

int main()
{
   auto passes = 0;
   auto tStart = steady_clock::now();
   uint64_t maxBit = 1000000L;
//   uint64_t maxBit = 1000L;
   while (true)
   {
      Seiveor64 seiveor;
      seiveor.Sievey(maxBit, 2);
      passes++;
      if (duration_cast<seconds>(steady_clock::now() - tStart).count() >= 5)
      {
         seiveor.printResults(maxBit, false, duration_cast<microseconds>(steady_clock::now() - tStart).count() / maxBit, passes);
         break;
      }
   }
}

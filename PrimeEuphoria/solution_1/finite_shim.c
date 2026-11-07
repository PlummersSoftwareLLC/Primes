/*
 * The Euphoria 4.1.0 runtime library (eu.a) was built against glibc < 2.31
 * and references the __*_finite math entry points that newer glibc versions
 * no longer export for linking. Map them onto the regular functions.
 */
#include <math.h>

double __log_finite(double x) { return log(x); }
long double __powl_finite(long double x, long double y) { return powl(x, y); }

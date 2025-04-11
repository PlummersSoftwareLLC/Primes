// used for more reliable timing on apple and wsl
// is largely ignored in docker containers

#ifdef __APPLE__
    #include <mach/mach_time.h>
    #include <pthread.h>
    #include <dispatch/dispatch.h>
#endif

#ifdef __linux__
    #define _POSIX_C_SOURCE 200809L
    #define _GNU_SOURCE
    #include <sched.h>        // For sched_setscheduler
    #include <unistd.h>       // For nice
    #include <sys/resource.h> // For setpriority
#endif

#ifdef _OPENMP
    #include <omp.h>
#endif

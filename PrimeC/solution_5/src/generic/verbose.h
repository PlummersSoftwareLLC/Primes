
// Verbose level allows some code to only be compiled when targeting a certain verbose level
#ifdef COMPILE_EXPLAIN
  #if COMPILE_VERBOSE_LEVEL < 7
     #undef COMPILE_VERBOSE_LEVEL
     #define COMPILE_VERBOSE_LEVEL 7
  #endif
#endif

#define verbose0(statement) statement
#define verbose1(statement)
#define verbose2(statement)
#define verbose3(statement)
#define verbose4(statement)
#define verbose5(statement)
#define verbose6(statement)
#define verbose7(statement)
#define verbose8(statement)
#define verbose9(statement)
#define verbose_at2(statement)
#define verbose_at3(statement)

#if COMPILE_VERBOSE_LEVEL >= 1
  #undef verbose1
  #define verbose1(statement) if (option.verbose_level >= 1) statement
#endif
#if COMPILE_VERBOSE_LEVEL >= 2
  #undef verbose2
  #define verbose2(statement) if (option.verbose_level >= 2) statement
  #undef verbose_at2
  #define verbose_at2(statement) if (option.verbose_level == 2) statement
#endif
#if COMPILE_VERBOSE_LEVEL >= 3
  #undef verbose3
  #define verbose3(statement) if (option.verbose_level >= 3) statement
  #undef verbose_at3
  #define verbose_at3(statement) if (option.verbose_level == 3) statement
#endif
#if COMPILE_VERBOSE_LEVEL >= 4
  #undef verbose4
  #define verbose4(statement) if (option.verbose_level >= 4) statement
#endif
#if COMPILE_VERBOSE_LEVEL >= 5
  #undef verbose5
  #define verbose5(statement) if (option.verbose_level >= 5) statement
#endif
#if COMPILE_VERBOSE_LEVEL >= 6
  #undef verbose6
  #define verbose6(statement) if (option.verbose_level >= 6) statement
#endif
#if COMPILE_VERBOSE_LEVEL >= 7
  #undef verbose7
  #define verbose7(statement) if (option.verbose_level >= 7) statement
#endif
#if COMPILE_VERBOSE_LEVEL >= 8
  #undef verbose8
  #define verbose8(statement) if (option.verbose_level >= 8) statement
#endif
#if COMPILE_VERBOSE_LEVEL >= 9
  #undef verbose9
  #define verbose9(statement) if (option.verbose_level >= 9) statement
#endif
#if defined preset_uint64v8
  #define variant_base_type_t uint64_t
  #define variant_elements 8
  #define variant uint64v8
#endif

#if defined preset_uint64v4
  #define variant_base_type_t uint64_t
  #define variant_elements 4
  #define variant uint64v4
#endif

#if defined preset_uint64v2
  #define variant_base_type_t uint64_t
  #define variant_elements 2
  #define variant uint64v2
#endif

#if defined preset_uint32v16
  #define variant_base_type_t uint32_t
  #define variant_elements 16
  #define variant uint32v16
#endif

#if defined preset_uint32v8
  #define variant_base_type_t uint32_t
  #define variant_elements 8
  #define variant uint32v8
#endif

#if defined preset_uint32v4
  #define variant_base_type_t uint32_t
  #define variant_elements 4
  #define variant uint32v4
#endif

#if defined preset_uint32v2
  #define variant_base_type_t uint32_t
  #define variant_elements 2
  #define variant uint32v2
#endif

#if defined preset_uint16v32
  #define variant_base_type_t uint16_t
  #define variant_elements 32
  #define variant uint16v32
#endif

#if defined preset_uint16v16
  #define variant_base_type_t uint16_t
  #define variant_elements 16
  #define variant uint16v16
#endif

#if defined preset_uint16v8
  #define variant_base_type_t uint16_t
  #define variant_elements 8
  #define variant uint16v8
#endif

#if defined preset_uint16v4
  #define variant_base_type_t uint16_t
  #define variant_elements 4
  #define variant uint16v4
#endif

#if defined preset_uint16v2
  #define variant_base_type_t uint16_t
  #define variant_elements 2
  #define variant uint16v2
#endif

#if defined preset_uint8v32
  #define variant_base_type_t uint8_t
  #define variant_elements 32
  #define variant uint8v32
#endif

#if defined preset_uint8v16
  #define variant_base_type_t uint8_t
  #define variant_elements 16
  #define variant uint8v16
#endif

#if defined preset_uint8v8
  #define variant_base_type_t uint8_t
  #define variant_elements 8
  #define variant uint8v8
#endif

#if defined preset_uint8v4
  #define variant_base_type_t uint8_t
  #define variant_elements 4
  #define variant uint8v4
#endif

#if defined preset_uint8v2
  #define variant_base_type_t uint8_t
  #define variant_elements __builtin_vectorelements(variant_base_type_t)
  #define variant uint8v2
#endif

#undef BITBUCKET_ELEMENTS
#undef BITBUCKET_BASE
#undef BITBUCKET_BYTEINDEX

#define BITBUCKET_ELEMENTS variant_elements
#if BITBUCKET_ELEMENTS == 32
  #define BITBUCKET_BASE(pattern)      ((bitbucket_t){ pattern, pattern, pattern, pattern, pattern, pattern, pattern, pattern, pattern, pattern, pattern, pattern, pattern, pattern, pattern, pattern, pattern, pattern, pattern, pattern, pattern, pattern, pattern, pattern, pattern, pattern, pattern, pattern, pattern, pattern, pattern, pattern })
  #define BITBUCKET_BYTEINDEX          ((bitbucket_t){ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31 })
#elif BITBUCKET_ELEMENTS == 16
  #define BITBUCKET_BASE(pattern)      ((bitbucket_t){ pattern, pattern, pattern, pattern, pattern, pattern, pattern, pattern, pattern, pattern, pattern, pattern, pattern, pattern, pattern, pattern })
  #define BITBUCKET_BYTEINDEX          ((bitbucket_t){ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15 })
#elif BITBUCKET_ELEMENTS == 8
  #define BITBUCKET_BASE(pattern)      ((bitbucket_t){ pattern, pattern, pattern, pattern, pattern, pattern, pattern, pattern })
  #define BITBUCKET_BYTEINDEX          ((bitbucket_t){ 0, 1, 2, 3, 4, 5, 6, 7 })
#elif BITBUCKET_ELEMENTS == 4
  #define BITBUCKET_BASE(pattern)      ((bitbucket_t){ pattern, pattern, pattern, pattern })
  #define BITBUCKET_BYTEINDEX          ((bitbucket_t){ 0, 1, 2, 3 })
#elif BITBUCKET_ELEMENTS == 2
  #define BITBUCKET_BASE(pattern)      ((bitbucket_t){ pattern, pattern })
  #define BITBUCKET_BYTEINDEX          ((bitbucket_t){ 0, 1})
#endif


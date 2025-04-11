
// helper calc functions
#define pow(base,pow)             (pow*((base>>pow)&1U))
#define min(a,b)                  ((a<b) ? a : b)
#define max(a,b)                  ((a<b) ? b : a)

// helper compile time check functions
#define likely(x)                 (__builtin_expect((x),1))
#define unlikely(x)               (__builtin_expect((x),0))
#define is_signed(type)           (((type)-1)<0)

#define safe_diff(a,b)            ((a>b) ? (a-b) : 0)
#define safe_diff_type(a,b,type)  (is_signed(type) ? a-b : ((a>b) ? (a-b) : 0))

#define CONCAT(a, b) a##b
#define NAME(a, b) CONCAT(a, b)

#define STR_HELPER(x) #x
#define STR(x) STR_HELPER(x)

#define function(name, suffix) CONCAT(name, suffix)

#define TYPE_SHORT_NAME(x) _Generic((x)0, \
uint32_t: "u32", \
uint64_t: "u64", \
int32_t:  "i32", \
int64_t:  "i64", \
default:  "unknown" )

static inline void local_memcpy(void *dest, void *src, size_t n) 
{ 
    // Typecast src and dest addresses to (char *) 
    char *csrc = (char *)src; 
    char *cdest = (char *)dest; 

    // Copy contents of src[] to dest[] 
    for (int i=0; i<n; i++) cdest[i] = csrc[i]; 
} 

// ANSI color codes
#define COLOR_RED         "\033[31m"
#define COLOR_GREEN       "\033[32m"
#define COLOR_YELLOW      "\033[33m"
#define COLOR_BLUE        "\033[34m"
#define COLOR_MAGENTA     "\033[35m"
#define COLOR_BOLD        "\033[1m"
#define COLOR_BLINK       "\033[5m"
#define COLOR_BLINK_OFF   "\033[25m"
#define COLOR_UNDERLINE   "\033[4m"
#define COLOR_RESET       "\033[0m"
#define COLOR_BOLD_YELLOW "\033[1;33m"
#define COLOR_BOLD_GREEN  "\033[1;32m"
#define COLOR_DARK_GRAY   "\033[0;90m"
#define COLOR_CLEAR_LINE  "\33[2K\r"

#define startAnalysis0(timer, printf_args...) verbose0(printf(printf_args);) timer_lapstart(timer); 
#define startAnalysis1(timer, printf_args...) verbose1(printf(printf_args);) timer_lapstart(timer); 
#define startAnalysis2(timer, printf_args...) verbose2(printf(printf_args);) timer_lapstart(timer); 
#define startAnalysis3(timer, printf_args...) verbose3(printf(printf_args);) timer_lapstart(timer); 
#define startAnalysis4(timer, printf_args...) verbose4(printf(printf_args);) timer_lapstart(timer); 
#define startAnalysis5(timer, printf_args...) verbose5(printf(printf_args);) timer_lapstart(timer); 
#define startAnalysis6(timer, printf_args...) verbose6(printf(printf_args);) timer_lapstart(timer); 
#define startAnalysis7(timer, printf_args...) verbose7(printf(printf_args);) timer_lapstart(timer); 
#define startAnalysis8(timer, printf_args...) verbose8(printf(printf_args);) timer_lapstart(timer); 
#define endAnalysis0(timer, ...) timer_laptime(timer); __VA_OPT__(verbose0(printf(__VA_ARGS__);))
#define endAnalysis1(timer, ...) timer_laptime(timer); __VA_OPT__(verbose1(printf(__VA_ARGS__);))
#define endAnalysis2(timer, ...) timer_laptime(timer); __VA_OPT__(verbose2(printf(__VA_ARGS__);))
#define endAnalysis3(timer, ...) timer_laptime(timer); __VA_OPT__(verbose3(printf(__VA_ARGS__);))
#define endAnalysis4(timer, ...) timer_laptime(timer); __VA_OPT__(verbose4(printf(__VA_ARGS__);))
#define endAnalysis5(timer, ...) timer_laptime(timer); __VA_OPT__(verbose5(printf(__VA_ARGS__);))
#define endAnalysis6(timer, ...) timer_laptime(timer); __VA_OPT__(verbose6(printf(__VA_ARGS__);))
#define endAnalysis7(timer, ...) timer_laptime(timer); __VA_OPT__(verbose7(printf(__VA_ARGS__);))
#define endAnalysis8(timer, ...) timer_laptime(timer); __VA_OPT__(verbose8(printf(__VA_ARGS__);))

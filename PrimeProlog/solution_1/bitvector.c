#include <SWI-Prolog.h>
#include <limits.h>

#ifdef VALIDATE
#define CHECK_PRED(Pred) if(!Pred) PL_warning(#Pred)
#define CHECK_TEST(Test) if(!Test) PL_warning(#Test)
#else
#define CHECK_PRED(Pred) Pred
#define CHECK_TEST(Test) 
#endif

#define GETBIT(s, i) ((s[i / CHAR_BIT] >> (i % CHAR_BIT)) & 1)
#define SETBIT(s, i) (s[i / CHAR_BIT] = (s[i / CHAR_BIT] | (1 << (i % CHAR_BIT))))

static functor_t FUNCTOR_bitvector2;

static int
unwrap_bitvector(term_t in, long *len, unsigned char **data)
{ CHECK_TEST( PL_is_functor(in, FUNCTOR_bitvector2) );
  
  term_t n = PL_new_term_ref(), str = PL_new_term_ref();
  CHECK_PRED( PL_get_arg(1, in, n) );
  CHECK_PRED( PL_get_arg(2, in, str) );
  
  size_t _len;
  CHECK_PRED( PL_get_string(str, (char **)data, &_len) );
  CHECK_PRED( PL_get_long(n, len) );

  PL_succeed;  
}

static foreign_t
pl_empty_bitvector(term_t n, term_t bv)
{ CHECK_TEST( PL_is_integer(n) );
 
  int len;
  CHECK_PRED( PL_get_integer(n, &len) );

  size_t compressed_len = (len + CHAR_BIT - 1) / CHAR_BIT / 2;
  char *bits = calloc(compressed_len, sizeof(char));
  CHECK_TEST( bits != NULL );

  term_t str = PL_new_term_ref(), tmp_bv = PL_new_term_ref();
  CHECK_PRED( PL_put_string_nchars(str, compressed_len, bits) );
  CHECK_PRED( PL_cons_functor(tmp_bv, FUNCTOR_bitvector2, n, str) );
  CHECK_PRED( PL_unify(bv, tmp_bv) );

  free(bits);

  PL_succeed;
}

static foreign_t
pl_bitvector_getbit(term_t bv, term_t i, term_t bit)
{ unsigned char *bits;
  long l, idx;

  CHECK_PRED( unwrap_bitvector(bv, &l, &bits) );
  CHECK_TEST( PL_is_integer(i) );
  CHECK_PRED( PL_get_long(i, &idx) );
  
  return PL_unify_bool(bit, GETBIT(bits, idx / 2));
}

static foreign_t
pl_bitvector_setbits(term_t bv, term_t p) 
{ unsigned char *bits;
  long n, x, i;

  CHECK_PRED( unwrap_bitvector(bv, &n, &bits) );
  CHECK_TEST( PL_is_integer(p) );
  CHECK_PRED( PL_get_long(p, &x) );

  for (i = (x * (long) x) / 2; i <= (n + 1) / 2; i += x)
    SETBIT(bits, i);

  PL_succeed;
}


install_t
install()
{ PL_register_foreign("empty_bitvector", 2, pl_empty_bitvector, 0);
  PL_register_foreign("bitvector_getbit", 3, pl_bitvector_getbit, 0);
  PL_register_foreign("bitvector_setbits", 2, pl_bitvector_setbits, 0);

  FUNCTOR_bitvector2 = PL_new_functor(PL_new_atom("bitvector"), 2);
}
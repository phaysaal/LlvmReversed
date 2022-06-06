extern void abort(void);
extern void __assert_fail(const char *, const char *, unsigned int, const char *) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__noreturn__));
void reach_error() { __assert_fail("0", "array_init_pair_symmetr.c", 3, "reach_error"); }
extern void abort(void);
void assume_abort_if_not(int cond) {
  if(!cond) {abort();}
}
void __VERIFIER_assert(int cond) { if(!(cond)) { ERROR: {reach_error();abort();} } }
extern int __VERIFIER_nondet_int();
//int N = 100000;
int N = 4;
int main()
{
  int i;
  //int a[N];
  //int b[N];
  //int c[N];
  int a[4];
  int b[4];
  int c[4];

  for(i=0;i<N;i++) {
    int x=__VERIFIER_nondet_int();
    //assume_abort_if_not(x > -100000 && x < 100000);
    assume_abort_if_not(x > -4 && x < 4);
    a[i]=x;
    b[i]=-x;
  }

  for(i=0;i<N;i++){
    c[i]=a[i]+b[i];
  }

  for(i=1;i<N;i++)
    __VERIFIER_assert(c[i] == 0);
  return 0;
}

#include<assert.h>

int x;
int y;
int f(int n) {
  return n+x;
}

int f2(int *p) {
  *p = 1 + *p;
  return (*p + 1);
}

int main(){
  x = 1;
  int z = 3;
  y = f(z);
  y = f2(&z);
  assert (z==4 && y==5);
  return 0;
}

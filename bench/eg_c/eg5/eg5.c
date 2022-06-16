// #include <stdio.h>

int result;

int Len(char *p) {
  int i = 0;
  while (*p != NULL) { p++; i++; }
  return i;
}

void main() {
  result = Len("teststring") ; /* 10 */
  return;
}

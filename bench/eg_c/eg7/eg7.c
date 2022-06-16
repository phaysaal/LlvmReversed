int a[100];

int f(int y) {
int x = 0;

 while (x + 2 < 2 * (y + 1)) { // x < 2*y // x < 4
	x ++;
}

return x;
}

void main() {
a[f(2)] = 0; /* 4 */
return;
}

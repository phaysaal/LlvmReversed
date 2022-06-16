int a[100];

int f(int y) {
int x = 0;

while (2 * (x + 1) < y + 2) { 
	x ++;
}

return x;
}

void main() {
a[f(2)] = 0; /* 1 */
return;
}

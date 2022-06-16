int a[100];

int f(int y) {
int x = 0;
int i = 0;

while (x < 10) { 
	if (y > 5) x++;
	else x+=2;
	i ++;
}

return i;
}

void main() {
a[f(2)] = 0; /* 5 */
return;
}

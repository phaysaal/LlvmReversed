#include<assert.h>

int f(int n, int m){
	int i = 0;
	while(i < n){
		if (i > m)
			break;
		i++;
	}
	return i;
}

int g(int n, int m){
	int i = 0;
	while(i < n){
		if (i > m)
			break;
		else
			n--;
		i++;
	}
	return i;
}

int h(int n, int m){
	int i = 0;
	while(i < n){
		if (i > m)
			n--;
		else
			break;
		i++;
	}
	return i;
}

int main(){
	int x = 3;
	int y = 7;
	int z = f(x, y); // 4
	int w = g(x, y); // 2
	int u = h(x, y); // 0

	assert(z==w+u+1);
	return 0;
}

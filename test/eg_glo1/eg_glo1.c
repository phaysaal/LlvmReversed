#include<assert.h>
#include<stdlib.h>

int x = 1;
int y = 2;
int z = 3;
int ww;

int f1(int x){
	y = x;
	int y;
	y = x + 2;
	return y;
}

int f2(int w){
	return f1(w);
}

int f3(){
	return f2(z);
}

int f4(){
	return x;
}

int f5(int p){
	return p+x;
}


void f6(){
	x = 0;
}

int main(){
	int a = 10;
	int n = f3();
	int m = f4();
	int q = f5(m);
	f6();
	assert(n == 5);
	assert(m == 1);
	assert(q == 2);
}






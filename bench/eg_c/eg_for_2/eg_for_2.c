#include<assert.h>

int f1(){
	int x = 10;
	int i = 0;
	for(;i<x;i++){
		x--;
	}
	return x;
}

int main() {
	int b;
	b = f1();
	assert (b==5);
	return b;
}

#include<assert.h>

int f0(){
	int x = 10;
	int i;
	for(i=0;i<x;i++){
		x--;
	}
	return x;
}

int main() {
	int a;
	a = f0();
	assert (a==5);
	return a;
}

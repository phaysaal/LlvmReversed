#include<assert.h>


int f2(){
	int x = 10;
	int i = 0;
	for(;i<x;){
		x--;
	}
	return x;
}

int main() {
	int c;
	c = f2();
	assert (c==0);
	return c;
}

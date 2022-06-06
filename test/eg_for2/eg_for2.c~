#include<assert.h>

int f0(){
	int x = 10;
	int i;
	for(i=0;i<x;i++){
		x--;
	}
	return x;
}

int f1(){
	int x = 10;
	int i = 0;
	for(;i<x;i++){
		x--;
	}
	return x;
}

int f2(){
	int x = 10;
	int i = 0;
	for(;i<x;){
		x--;
	}
	return x;
}

int main() {
	int a,b,c;
	a = f0();
	b = f1();
	c = f2();
	assert (a==5);
	assert (b==5);
	assert (c==0);
	return 0;
}

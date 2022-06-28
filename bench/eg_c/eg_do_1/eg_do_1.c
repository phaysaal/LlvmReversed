#include<stdio.h>
#include<assert.h>

int f(){
	int i=0;
	int n=10;
	do{
		i++;
	}while(i<n);
	return i;
}




int main(){
	int a1;
	a1 = f();

	assert(a1==10);

	return a1;
}

#include<stdio.h>
#include<assert.h>


int h1(){
	int i=0;
	int n=10;
	do{
		i++;
		if(i>3){
			i+=2;
			continue;
		}
		else
			i+=3;
		i+=4;
	}while(i<n);
	return i;
}

int main(){
	int a6;
	a6 = h1();

	assert(a6==11);
	return a6;
}

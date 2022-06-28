#include<stdio.h>
#include<assert.h>

int g1(){
	int i=0;
	int n=10;
	do{
		i++;
		if(i>3){
			i+=2;
			break;
		}
		else
			i+=3;
		i+=4;
	}while(i<n);
	return i;
}


int main(){
	int a3;
	a3 = g1();

	assert(a3==11);
	return a3;
}

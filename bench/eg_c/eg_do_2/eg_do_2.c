#include<stdio.h>
#include<assert.h>

int g(){
	int i=0;
	int n=10;
	do{
		i++;
		if(i>3){
			i+=2;
			break;
		}
		i+=4;
	}while(i<n);
	return i;
}


int main(){
	int a2;
	a2 = g();

	assert(a2==8);
	return a2;
}

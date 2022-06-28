#include<stdio.h>
#include<assert.h>

int h2(){
	int i=0;
	int n=10;
	do{
		i++;
		if(i>3){
		    n+=2;
			i+=3;
		}
		else{
		    n++;
			continue;
		}
		n*=3;
	}while(i<n);
	return i;
}

int main(){
	int a7;

	a7 = h2();
	assert(a7==151);
	return a7;
}

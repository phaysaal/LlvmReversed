#include<stdio.h>
#include<assert.h>

int h(){
	int i=0;
	int n=10;
	do{
		i++;
		if(i>3){
			i+=2;
			continue;
		}
		i+=4;
	}while(i<n);
	return i;
}

int main(){
	int a5;
	a5 = h();

	assert(a5==11);
	return a5;
}

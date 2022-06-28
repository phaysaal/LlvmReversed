#include<stdio.h>
#include<assert.h>

int gh1(){
	int i=0;
	int n=10;
	do{
		i++;
		if(i>3){
		    n+=2;
			i+=3;
			break;
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
	int a9;
	a9 = gh1();

	assert(a9==7);
	return a9;
}

#include<stdio.h>
#include<assert.h>

int g2(){
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
			break;
		}
		n*=3;
	}while(i<n);
	return i;
}

int main(){
	int a4;
	a4 = g2();
	assert(a4==1);
	return a4;
}

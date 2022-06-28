#include<stdio.h>
#include<assert.h>
int gh(){
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
		if(i>6){
		    n+=6;
			i+=6;
		}
		else{
		    n--;
			break;
		}
		
		n*=3;
	}while(i<n);
	return i;
}

int main(){
	int a8;
	a8 = gh();
	assert(a8==383);
	return a8;
}

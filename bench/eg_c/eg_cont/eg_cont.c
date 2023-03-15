#include<assert.h>
int f(int n, int m){
	int i = 0;
	while(i<n){
		if(i >= m){
			i+=5;
			continue;
		}
		i++;	
	}
	assert(i>n || m>=n);
	return i;
}

void g(int n, int m){
	int i = 0;
	while(i<n){
		if(i == m){
		    i+=5;
		}else{
			i+=2;
			continue;
		}
		i++;	
	}
}

void h(int n, int m){
	int i = 0;
	while(i<n){
		if(i == m){
		    i+=5;
		    continue;
		}else{
			i+=2;
			
		}
		i++;	
	}
}

int main(){
	return f(20,6);
	//return f(20,20);
}

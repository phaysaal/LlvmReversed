#include<assert.h>

int f1(int *z){
	*z = *z+1;
	return *z;
}

int f2(int x){
	x = x + 1;
	return x;
}

int main(){
	int n = 8;
	int m ;
	m = f1(&n);
	assert(m==n);
	//m = f2(n);
	//assert(m==n+1);
	return m;
}

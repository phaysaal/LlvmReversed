#include<assert.h>

int f2(){
	int x = 20, y = 3;

	if(x < 3 || y < 5 || x >= 20)
		x = x*y;
	else
		x = x+y;

	return x;
}


int main(){
    int n = f2();
    assert(n==60);
    return n;
}



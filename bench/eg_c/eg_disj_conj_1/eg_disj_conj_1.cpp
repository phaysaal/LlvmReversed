#include<assert.h>

int f5(){
	int x = 20, y = 30;

	if((x < 3 && y < 5) || x > 0)
		x = x*y;
	else
		x = x+y;

	return x;
}

int main(){
    int n = f5();
    assert(n==600);
    return n;
}


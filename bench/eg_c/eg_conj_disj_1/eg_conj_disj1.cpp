#include<assert.h>

int f4(){
	int x = -2, y = 20;

	if((x < 3 || y > 5) && x > 0)
		x = x*y;
	else
		x = x+y;

	return x;
}

int main(){
    int n = f4();
    assert(n==18);
    return n;
}


int f1(){
	int x = 2, y = 3;

	if(x < 3 && y < 5)
		x = x*y;
	else
		x = x+y;
	
	return x;
}

int main(){
	int n = f1();
	assert(n==6);
	return n;
}


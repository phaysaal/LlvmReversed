int x = 1;
int y = 2;
int z = 3;
int ww;

int f1(int x){
	y = x;
	int y;
	y = x + 2;
	return y;
}

int f2(int w){
	return f1(w); // BUG
}

int f3(){
	return f2(z); // BUG
}

int f4(){
	return x;
}

int f5(int p){
	return p+x;
}


void f6(){
	x = 0;
}


void* f7(){
	int *x = malloc(sizeof(int));
	return x;
}

int main(){
	int a = 10;
	int n = f3();
	int m = f4();
	int q = f5(m); //BUG
	f6();
	int *p = f7();
}






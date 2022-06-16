 int ifonly(int x, int y){
	int n = y;
	n++;
	if(x<y){
		n--;
	}
	n+=x;
	return n;
}

int main(){
	int a = 7;
	int b = 3;
	int c;
	c = ifonly(a, b);
	assert(c==a+b+1);
	return 0;
}

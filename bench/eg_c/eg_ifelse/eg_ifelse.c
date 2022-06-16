int ifelse(int x, int y){
	int n = 0;
	if(x<y)
		n=10;
	else
		n=0;
	return n;
}

int main(){
	int a = 2;
	int b = 4;
	int c = ifelse(a,b);
	assert(c==10);
	return c;
}

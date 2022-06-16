
int ifwhile(int x, int y){
	int n = 0;
	n++;
	if(x<y){
		while(n<10){
			n=1+n;
			if(n>2)
				break;
		}
	}
	n+=x;
	return n;
}

int main(){
	int a = 3;
	int b = 4;
	int c = ifwhile(a, b);
	assert(c==6);
	return 0;
}

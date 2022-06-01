int x = 1;
int y = 1;
int f1(){
	x++;
}

int f2(){
	y++;
}

int main(){
	f1();
	f2();
	assert(x==y);
}

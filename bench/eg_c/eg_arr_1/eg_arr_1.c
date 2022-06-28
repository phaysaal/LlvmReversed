int bb[3];

int f1() {
	int b[3];
	int c;
	b[0] = 3;
	b[2] = 4;
	c = b[2];
	assert(b[0] < c);
	return c;
}


int main(){
	int x = f1();

	return x;
} 

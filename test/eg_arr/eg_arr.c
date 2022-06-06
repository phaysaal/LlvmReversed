int bb[3];

int f1() {
	int b[3];
	int c;
	b[0] = 3;
	b[2] = 4;
	c = b[2];
	assert(b[0] < c);
	return 0;
}

/*
int f2() {
	int n = 10;
	int aoi[n*10];
	aoi[10] = 3;
	aoi[12] = 4;
	assert(aoi[10] < aoi[12]);
	return 0;
}
*/


int f3() {
	bb[0] = 33;
	bb[2] = 44;
	assert(bb[0] < bb[2]);
	return 0;
}


int main(){
	int x = f1();
//	int y = f2();
	int z = f3();
	return 0;
} 

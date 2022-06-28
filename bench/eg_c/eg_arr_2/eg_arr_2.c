int bb[3];

int f2() {
	int n = 10;
	int aoi[n*10];
	aoi[10] = 3;
	aoi[12] = 4;
	assert(aoi[10] < aoi[12]);
	return aoi[10]+aoi[12];
}

int main(){
	int y = f2();
	return y;
} 

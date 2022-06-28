int bb[3];

int f3() {
	bb[0] = 33;
	bb[2] = 44;
	assert(bb[0] < bb[2]);
	int x = bb[0];
	return x;
}


int main(){
	int z = f3();
	return z;
} 

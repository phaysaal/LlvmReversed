int main(){
	int x = 100;
	int i;
	for(i=0;i<x;i++){
		x--;
	}
	return x;
}

int f1(){
	int x = 100;
	int i = 0;
	for(;i<x;i++){
		x--;
	}
	return x;
}

int f2(){
	int x = 100;
	int i = 0;
	for(;i<x;){
		x--;
	}
	return x;
}

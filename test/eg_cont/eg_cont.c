int f(int n, int m){
	int i = 0;
	while(i<n){
		if(i == m){
			i+=2;
			continue;
		}
		i++;	
	}
}

int g(int n, int m){
	int i = 0;
	while(i<n){
		if(i == m){
		    i+=5;
		}else{
			i+=2;
			continue;
		}
		i++;	
	}
}

int h(int n, int m){
	int i = 0;
	while(i<n){
		if(i == m){
		    i+=5;
		    continue;
		}else{
			i+=2;
			
		}
		i++;	
	}
}

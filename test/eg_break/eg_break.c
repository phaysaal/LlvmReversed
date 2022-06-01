int f(int n, int m){
	int i = 0;
	while(i < n){
		if (i > m)
			break;
		i++;
	}
	return i;
}

int g(int n, int m){
	int i = 0;
	while(i < n){
		if (i > m)
			break;
		else
			n--;
		i++;
	}
	return i;
}

int h(int n, int m){
	int i = 0;
	while(i < n){
		if (i > m)
			n--;
		else
			break;
		i++;
	}
	return i;
}

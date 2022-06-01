int ifonly(int x, int y){
	int n = 0;
	n++;
	if(x<y){
		n=1;
	}
	n+=x;
	return n;
}


int ifelse(int x, int y){
	int n = 0;
	if(x<y)
		n=1;
	else
		n=0;
	return n;
}


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

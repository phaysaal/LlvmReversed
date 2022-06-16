int f1(){
	int x = 2, y = 3;

	if(x < 3 && y < 5 && x > 0)
		x = 9;
	else
		y = 9;
	
	return 0;
}

int f2(){
	int x = 2, y = 3;

	if(x < 3 || y < 5)
		x = 9;
	else
		y = 9;
	
	return 0;
}

int f3(){
	int x = 2, y = 3;

	if(x < 3 || y < 5 || x > 0)
		x = 9;
	else
		y = 9;
	
	return 0;
}


int f4(){
	int x = 2, y = 3;

	if((x < 3 || y < 5) && x > 0)
		x = 9;
	else
		y = 9;
	
	return 0;
}

int f5(){
	int x = 2, y = 3;

	if((x < 3 && y < 5) || x > 0)
		x = 9;
	else
		y = 9;
	
	return 0;
}



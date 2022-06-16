/*
int f(){
	int i=0;
	int n=10;
	while(i<n){
		i++;
	}
	return i;
}


int g(){
	int i=0;
	int n=10;
	while(i<n){
		i++;
		if(i>3){
			i+=2;
			break;
		}
		i+=4;
	}
	n--;
	return i;
}

int g1(){
	int i=0;
	int n=10;
	while(i<n){
		i++;
		if(i>3){
			i+=2;
			break;
		}
		else
			i+=3;
		i+=4;
	}
	return i;
} */

/*
int g2(){
	int i=0;
	int n=10;
	while(i<n){
		i++;
		if(i>3){
		    n+=2;
			i+=3;
		}
		else{
		    n++;
			break;
		}
		n*=3;
	}
	return i;
}
//*/

//*
int h(){
	int i=0;
	int n=10;
	while(i<n){
		i++;
		if(i>3){
			i+=2;
			continue;
		}
		i+=4;
	}
	return i;
}
//*/

//*
int h1(){
	int i=0;
	int n=10;
	while(i<n){
		i++;
		if(i>3){
			i+=2;
			continue;
		}
		else
			i+=3;
		i+=4;
	}
	return i;
}
//*/

/*
int h2(){
	int i=0;
	int n=10;
	while(i<n){
		i++;
		if(i>3){
		    n+=2;
			i+=3;
		}
		else{
		    n++;
			continue;
		}
		n*=3;
	}
	return i;
}

int gh(){
	int i=0;
	int n=10;
	while(i<n){
		i++;
		if(i>3){
		    n+=2;
			i+=3;
		}
		else{
		    n++;
			continue;
		}
		if(i>6){
		    n+=6;
			i+=6;
		}
		else{
		    n--;
			break;
		}
		
		n*=3;
	}
	return i;
}

// Not covered yet
int gh(){
	int i=0;
	int n=10;
	while(i<n){
		i++;
		if(i>3){
		    n+=2;
			i+=3;
			break;
		}
		else{
		    n++;
			continue;
		}
		
		n*=3;
	}
	return i;
}
*/

#include<stdio.h>
#include<assert.h>

int f(){
	int i=0;
	int n=10;
	do{
		i++;
	}while(i<n);
	return i;
}


int g(){
	int i=0;
	int n=10;
	do{
		i++;
		if(i>3){
			i+=2;
			break;
		}
		i+=4;
	}while(i<n);
	return i;
}


int g1(){
	int i=0;
	int n=10;
	do{
		i++;
		if(i>3){
			i+=2;
			break;
		}
		else
			i+=3;
		i+=4;
	}while(i<n);
	return i;
}


int g2(){
	int i=0;
	int n=10;
	do{
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
	}while(i<n);
	return i;
}


int h(){
	int i=0;
	int n=10;
	do{
		i++;
		if(i>3){
			i+=2;
			continue;
		}
		i+=4;
	}while(i<n);
	return i;
}


int h1(){
	int i=0;
	int n=10;
	do{
		i++;
		if(i>3){
			i+=2;
			continue;
		}
		else
			i+=3;
		i+=4;
	}while(i<n);
	return i;
}

int h2(){
	int i=0;
	int n=10;
	do{
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
	}while(i<n);
	return i;
}

int gh(){
	int i=0;
	int n=10;
	do{
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
	}while(i<n);
	return i;
}

// Not covered yet
int gh1(){
	int i=0;
	int n=10;
	do{
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
	}while(i<n);
	return i;
}

int main(){
	int a1, a2, a3, a4, a5, a6, a7, a8, a9;
	a1 = f();
	a2 = g();
	a3 = g1();
	a4 = g2();
	a5 = h();
	a6 = h1();
	a7 = h2();
	a8 = gh();
	a9 = gh1();

	assert(a1==10);
	assert(a2==8);
	assert(a3==11);
	assert(a4==1);
	assert(a5==11);
	assert(a6==11);
	assert(a7==83);
	//assert(a8==173);
	//assert(a9==7);
	return 0;
}

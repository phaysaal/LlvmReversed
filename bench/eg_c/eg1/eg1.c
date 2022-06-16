/*
void local(){       // OK (v2)
    int x;
    int y = 10;
    y = x + 12;
    x = 23;
}
//*/

/*
int loop(int n){        // OK (v2)
    for(int i=0; i<n; i++);
    int x;
    for(int i=0; i<n; i++)
        x += 2;
    return x;
}
//*/

/*
int dowhile(int m, int n){  //NOT OK (v1,v2)
    do{
        m *= 2;
    }while(m < n);
    return m;
}
//*/

/*
int cond(int x, int y){   OK (v2)  
    int z = 0;
    if(x<10)
        z = 20;
        
    if(z<10){
        return 0;
    } else {
        return y;
    }
}
//*/

/*
int nestedloop(int n, int m){   // OK (v2)
    for(int i=0; i<n; i++){
        while(i < m){
            i++;
            n -= m;
        }
    }
    return n;
}
//*/

/*
int nestedcond(int n, int m){   // OK (v2)
    if(n < 0){
        if(m < 1000){
            m -= 10;
        }
    } else {
        if(m < 1000){
            m -= 20;
        } else {
            m -= 30;
        }
    }
    return m;
}
//*/

/*
int stmtloop(int m){        // OK (v2)
    int n = m + 10;
    n -= 3;
    int x = 0;
    for(int i=0; i<n; i++){
        x += 2;
        m ++;
    }
    return n;      
}
//*/

/*
int stmtcond(int m){      // OK (v2)

    int n = m + 10;
    n -= 3;
    int x = 0;
    if(m < 1000){
        m -= 10;
        m += n;
    }
    return n;      
}
//*/

/*
int loopcond(int m, int nnn){
    int i;
    for(i=0;i<m;)
        if(i<m/2)
            i+=5;
        else
            i+=4;
    return i;
}
//*/

/*
int condloop(int i, int nnn){
    if(nnn>i)                   // OK (v2)
        for(i=1; i<nnn;)
            i++;
    
    return i;
}
//*/

/*
void voidfn(int n){     // OK (v2)
    n = 0;
    return;
}
//*/

/*
int callfn(int i, int n){       // OK (v2)
    voidfn(n);  
    assert(n<i);  
    return condloop(i,n);
}
//*/

/*
int sassert(int n, int m){      // OK (v2)
    assert(n==m);
}
*/

/*
int div(int x, int y){      // OK (v2)
    int z;
    z = y / x;
    assert(x == (z/2)) ;
    int i;
    if(i<x/2)
        i+=5;   
    else
        i+=4;
    return x/y;
}
*/

//*
int cond_in_fcall(int n){
    for(int i=0;i<10;i++){
        assume_abort_if_not(n>10 && n < 100000);
    }
    return 0;
}
//*/

#include<assert.h>

int main(){
    int x = 0, i = 0;
    while(i<20){
        if(i  < 10)
            x += 2;
        else
            x += 4;
        i++;
    }
    assert(x==60);
    return x;
}

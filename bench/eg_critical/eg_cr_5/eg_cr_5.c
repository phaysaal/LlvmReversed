#include<assert.h>
int main(){
    int x = 0;
    do{
        x = 1;
    }while(0);
    assert(x==0);
    return x;
}

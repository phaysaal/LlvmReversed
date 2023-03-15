#include<assert.h>
int main(){
    int i = 0;
    do {
        if (i>50)
            break;
        i++;
    }while(i<100);
    assert(i==51);
    return i;
}

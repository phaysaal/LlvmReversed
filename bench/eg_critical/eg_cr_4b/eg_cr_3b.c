#include<assert.h>
int main(){
    int i;
    do {
        if (i > 50){
            i = 101;
            continue;
        }
        i++;
    } while (i < 100) ;
    assert(i==101);
    return i;
}

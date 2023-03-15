#include<assert.h>
int main(){
    int i;
    for (i=0;
         i < 100;
         i++) {
        if (i > 50)
            continue;
        i++;
    }
    assert(i==100);
    return i;
}

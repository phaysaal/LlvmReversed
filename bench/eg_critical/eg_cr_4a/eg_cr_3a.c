#include<assert.h>
int main(){
    int i=0;
    while (i < 100) {
        if (i > 50) {
            i += 10;
            continue;
        }
        i++;
        i++;
    }
    assert(i==102);
    return i;
}

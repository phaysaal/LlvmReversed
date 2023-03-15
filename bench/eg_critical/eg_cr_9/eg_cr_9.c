#include<assert.h>

int main(){
    int i = 0;
    int k = 2;
    for(i=0;i<10;i++){
        if(i == 8)
            break;
        else if (i == 3)
            continue;
        i += k;
    }
    assert(i==7);
    return i;
}

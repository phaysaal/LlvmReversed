#include<assert.h>

class A {
public: int x;
        void set(int y){
            x = y;
        }
        int get(){
            return x;
        }
};

int main(){
    int p = 9;
    int q;
    A a;
    a.set(p);
    q = a.get();
    assert(p==q);
    return 0;
}

#include<assert.h>

class B {
public:
    int x;
    int y;
    void set(int y){
            x = y;
        }
        int get(){
            return x;
        }
};

int main(){
    int p = 8;
    int q;
    B *b;
    b = new B();
    b->set(p);
    q = b->get();
    assert(p==q);
    return q;
}

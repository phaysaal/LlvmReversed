#include<assert.h>
//using namespace std;

class B {
private:
    int x;
public:
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
    b->y = 10;
    q = b->y;
    assert(q==10);
    return 0;
}

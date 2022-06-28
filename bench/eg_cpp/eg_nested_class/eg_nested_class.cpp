#include<assert.h>
class A{
public:
    int field1;
    int field2;
};

class B{
public:
    A a;
    A b;
    int field3;
};

int main(){
    B b;
    b.field3 = 100;
    b.a.field1 = 10;
    b.b.field2 = 1;
    int y = b.field3+b.a.field1+b.b.field2;
    assert(y==111);
    return y;
}

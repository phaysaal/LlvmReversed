#include<assert.h>
class X{
public:
    int fieldx;
};

class A{
public:
    X field1;
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
    b.b.field2 = 100;
    b.a.field1.fieldx = 10;
    b.b.field2 = 1;
    int y = b.b.field2+b.a.field1.fieldx+b.b.field2;
    assert(y==12);
    return y;
}

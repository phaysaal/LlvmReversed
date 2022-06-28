#include<assert.h>
class D{
private:
    int d2;
public:
    int d1;
    void setD(int x){
        d2 = x;
    }
    int getD(){
        return d2;
    }
};

class C : public D{
public:
private:
    int c3;
    void setC(int x){
        c3 = x;
    }
    int getC(){
        return c3;
    }
};

int main(){
    C c;
    c.setD(10);
    int q = c.getD();
    assert(q == 10);
    return q;
}

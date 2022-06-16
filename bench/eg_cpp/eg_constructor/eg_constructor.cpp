#include<assert.h>
class D{
private:
    int d2;
public:
    int d1;
    D(int x){
        d2 = x;
    }
    int getD(){
        return d2;
    }
};

int main(){
    D dobj(109);
    int q = dobj.getD();
    assert(q == 109);
    return 0;
}

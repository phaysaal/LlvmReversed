int main(){
        int x = 120;
        int y ;
        if(x>0 && (x<100 || x>200))
            y = 1;
        else
            y = 2;
        assert(y == 2);
        return y;
}

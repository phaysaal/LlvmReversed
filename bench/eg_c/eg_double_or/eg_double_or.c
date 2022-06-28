/*
int f(){
        int x = 1000;
        int y ;
        if(x>0)
            y = 1;
        else
            y = 2;
        assert(y == 1);
        return y;
}
*/
int g1(int x){
    return x == 300;
}
int g(){
        int x = 1000;
        int y = 300;

        assert(y == 2 || y == 4 || g1(y));
        return y;
}
/*
int h(){
        int x = 1000;
        int y ;
        if((x>0 || x<100) || x == 120)
            y = 1;
        else
            y = 2;
        assert(y == 1);
        return y;
}
int i(){
        int x = 1000;
        int y ;
        if((x>0 || (x<100) || x == 120))
            y = 1;
        else
            y = 2;
        assert(y == 1);
        return y;
}
int j(){
        int x = 1000;
        int y ;
        if((x>0 && x<100) || x == 120)
            y = 1;
        else
            y = 2;
        assert(y == 1);
        return y;
}
int k(){
        int x = 120;
        int y ;
        if(!((x>0 && x<100) || (x == 120 && x < 121)))
            y = 1;
        else
            y = 2;
        assert(y == 2);
        return y;
}
int l(){
        int x = 120;
        int y ;
        if((x>0 && x<100) || !(x != 120 && x < 121) && x > 30)
            y = 1;
        else
            y = 2;
        assert(y == 1);
        return y;
}

int m(){
        int x = 101;
        int y ;
        if((x>0 && x<100) || (x == 120 && x < 121) && !(x > 30))
            y = 1;
        else
            y = 2;
        assert(y == 2);
        return y;
} */
int main(){
        int x = 1000;
        int y = g();
//        y = m();
//        y += l();
//        y += k();
//        y += f();
        return y;
}


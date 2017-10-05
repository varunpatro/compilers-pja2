class MainC {

void main (){
	Int i ; 
    Functional fork ;
	i = i + 1 * 2 ;
//	i = i || true && i ;
//	i = (i < 0) && true  ;
	i = i + fork.f(5) ;
	println(i) ;
	return ;
}

}

class Functional {
Int a;
Kappa k ;

Int f (Int b){
	return k.kap(b,f(3));
}

}

class Kappa {
   Int j ;
   Functional ff ;
   
   Int kap(Int x, Int y) {
     if (x > 0) {
       return kap(x, y) ;
     } else {
     return ff.k.kap(x,y) ;
     }
   }

}

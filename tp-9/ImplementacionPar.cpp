#include <iostream>
#include "Par.h"

using namespace std;

Par consPar(int x, int y){
// Propósito: construye un par
    Par p;
    p.x=x;
    p.y=y;
    return(p);
}

int fst(Par p){
// Propósito: devuelve la primera componente
    return(p.x);
}

int snd(Par p){
// Propósito: devuelve la segunda componente
    return(p.y);
}

int maxDelPar(Par p){
// Propósito: devuelve la mayor componente
    if (p.x > p.y) {
        return (p.x);
    }
    return(p.y);
}

Par swap(Par p){
// Propósito: devuelve un par con las componentes intercambiadas
    int z = p.x;
    p.x = p.y;
    p.y = p.x;
    return(p);
}

Par divisionYResto(int n, int m) {
// Propósito: devuelve un par donde la primer componente
// es la división y la segunda el resto entre ambos números
    Par p;
    p.x = n/m;
    p.y = (n * p.x) - m; 
    return(p);
}
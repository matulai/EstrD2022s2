#include <iostream>
#include "Fraccion.h"

using namespace std;

Fraccion consFraccion(int numerador, int denominador){
// Propósito: construye una fraccion
// Precondición: el denominador no es cero
    Fraccion f;
    f.numerador = numerador;
    f.denominador = denominador;
    return(f);
}

int numerador(Fraccion f) {
// Propósito: devuelve el numerador
    return (f.numerador);
}

int denominador(Fraccion f) {
// Propósito: devuelve el denominador
    return(f.denominador);
}

float division(Fraccion f) {
// Propósito: devuelve el resultado de hacer la división
    int x = f.numerador / f.denominador;
    return (x); 
}

Fraccion multF(Fraccion f1, Fraccion f2) {
// Propósito: devuelve una fracción que resulta de multiplicar las fracciones
// (sin simplificar)
    int x = f1.numerador * f2.numerador;
    int y = f1.denominador * f2.denominador;
    return (consFraccion (x,y));
}

int mcd(int x,int y) {
    int a = max(x,y);
    int b = min(x,y);
    while (b) {
        int r = a%b;
        a = b;
        b = r; 
    }
    return a;
}

Fraccion simplificada(Fraccion p){
// Propósito: devuelve una fracción que resulta
// de simplificar la dada por parámetro
    int z = mcd(p.denominador, p.numerador);
    int x = p.numerador/z;
    int y = p.denominador/z;
    return (consFraccion (x,y));
}

Fraccion sumF(Fraccion f1, Fraccion f2) {
// Propósito: devuelve la primera componente
    int x = f1.numerador + f2.numerador;
    int y = f1.denominador + f2.denominador;
    return (consFraccion (x,y));
}
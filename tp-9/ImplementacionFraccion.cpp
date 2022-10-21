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
    int x = f.numerador / f.denominador
    return (x); 
}

Fraccion multF(Fraccion f1, Fraccion f2) {
// Propósito: devuelve una fracción que resulta de multiplicar las fracciones
// (sin simplificar)
    int x = f1.numerador * f2.numerador;
    int y = f1.denominador * f2.denominador;
    return (consFraccion (x,y));
}

Fraccion simplificada(Fraccion p){
// Propósito: devuelve una fracción que resulta
// de simplificar la dada por parámetro
    if (resto(p) !=0) {
        return (simplificar(p))
    }
    return (consFraccion(0,0))
}

int resto(Fraccion p) {
    if (p.numerador < p.denominador) {
        return (p.denominador - division(p) * p.numerador)
    }
    return (p.numerador - division(p) * p.denominador )
}

Fraccion sumF(Fraccion f1, Fraccion f2) {
// Propósito: devuelve la primera componente
    int x = f1.numerador + f2.numerador;
    int y = f1.denominador + f2.denominador;
    return (consFraccion (x,y));
}
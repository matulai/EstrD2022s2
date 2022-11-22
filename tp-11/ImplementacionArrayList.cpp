#include <iostream>
#include "arrayList.h"
using namespace std;

ArrayList newArrayList() {
// Crea una lista con 0 elementos.
// Nota: empezar el array list con capacidad 16.
    ArrayListSt* a  = new ArrayListSt;
    a->cantidad  = 0;
    a->elementos = new int [16];
    a->capacidad = 16;
    return a;
}
ArrayList newArrayListWith(int capacidad) {
// Crea una lista con 0 elementos y una capacidad dada por parámetro.
    ArrayListSt* a  = new ArrayListSt;
    a->cantidad  = 0;
    a->elementos = new int [capacidad];
    a->capacidad = capacidad;
    return a;
}
int lengthAL(ArrayList xs) {
// Devuelve la cantidad de elementos existentes.
    return xs->cantidad;
}

int get(int i, ArrayList xs) {
// Devuelve el iésimo elemento de la lista.
    if (i<= xs->cantidad) {
        return xs->elementos[i];
    } else {
        return 00;
    }
}

void set(int i, int x, ArrayList xs){
// Reemplaza el iésimo elemento por otro dado.
    if (i <= xs->cantidad) {
        xs->elementos[i] = x;
    } 
}

void resize(int capacidad, ArrayList xs) {
// Decrementa o aumenta la capacidad del array.
// Nota: en caso de decrementarla, se pierden los elementos del final de la lista.
    int i = 0;
    int* nw = new int [capacidad];
    if (capacidad != 0) {
        while (i < capacidad && i < xs->cantidad) {
            nw[i] = xs->elementos[i];
            i++;
        }
    } 
    xs->elementos = nw;
    xs->capacidad = 0;
    xs->cantidad  = 0;
}

void add(int x, ArrayList xs) {
// Agrega un elemento al final de la lista.
    if (xs->cantidad == xs->capacidad) {
        resize(xs->capacidad * 2, xs);
    }
    xs->elementos[xs->cantidad + 1] = x;
}

void remove(ArrayList xs) {
// Borra el último elemento de la lista.
    xs->cantidad = xs->cantidad - 1;
}
#include <iostream>
#include "Pokemon.h"
#include "arrayList.h"
#include "Entrenador.h"
#include "Persona.h"
using namespace std;

int main() {
    cout<<energia(consPokemon("fuego"))<<endl;
    cout<<tipoDePokemon(consPokemon("agua"))<<endl;
    // cout<<nombre(consPersona("German",5));
}

int sumatoria(ArrayList xs) {
// Devuelve la suma de todos los elementos.
    int i = 0;
    int x = 0;
    while (i<lengthAL(xs)) {
        x += get(i,xs);
        i++;
    }
    return x;
}

void sucesores(ArrayList xs) {
// Incrementa en uno todos los elementos.
    int i= 0;
    while (i<lengthAL(xs)) {
        set(i,get(i,xs) + 1,xs);
    }
}

bool pertenece(int x, ArrayList xs) {
// Indica si el elemento pertenece a la lista.
    int i= 0;
    while (i<lengthAL(xs)) {
        if (x == get(i,xs)) {
            return true;
        }
    }
    return false;
}

int apariciones(int x, ArrayList xs) {
// Indica la cantidad de elementos iguales a x.
    int i= 0;
    int x= 0;
    while (i<lengthAL(xs)) {
        if (x == get(i,xs)) {
            x++;
        }
    }
    return x;
}

ArrayList append(ArrayList xs, ArrayList ys) {
// Crea una nueva lista a partir de la primera y la segunda (en ese orden).
    int i = 0;
    while (i<lengthAL(ys)) {
        add(get(i,ys),xs);
        i++;
    }
}

int minimo(ArrayList xs) {
// Devuelve el elemento más chico de la lista.
    int x= get(0,xs);
    int i=1;
    while (i<lengthAL(xs)) {
        int y = get(i,xs);
        if (x > y) {
            x = y;
        }
    }
    return x;
}
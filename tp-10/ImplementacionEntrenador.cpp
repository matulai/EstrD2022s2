#include <iostream>
#include "Pokemon.h"
#include "Entrenador.h"

Entrenador consEntrenador(string nombre, int cantidad, Pokemon* pokemon) {
// Dado un nombre, una cantidad de pokémon, y un array de pokémon de ese tamaño, devuelve
// un entrenador.
    EntrenadorSt* e = new EntrenadorSt;
    e->nombre = nombre;
    e->cantPokemon = cantidad;
    e->pokemon = pokemon;
    return e;
}
string nombreDeEntrenador(Entrenador e) {
// Devuelve el nombre del entrenador.
    return e->nombre;
}

int cantidadDePokemon(Entrenador e) {
// Devuelve la cantidad de pokémon que posee el entrenador.
    return e->cantPokemon;
}

int cantidadDePokemonDe(TipoDePokemon tipo, Entrenador e) {
// Devuelve la cantidad de pokémon de determinado tipo que posee el entrenador.
    int x = 0;
    for (int i = 0; i < e->cantPokemon; i++) {
        if (tipoDePokemon(e->pokemon[i]) == tipo) {
            x++;
        };
    };
    return x;
}

Pokemon pokemonNro(int i, Entrenador e){
// Devuelve el pokémon número i de los pokémon del entrenador.
// Precondición: existen al menos i − 1 pokémon.
    return e->pokemon[i-1];
}

bool leGanaATodos(Entrenador e1, Entrenador e2) {
// Dados dos entrenadores, indica si, para cada pokémon del segundo entrenador, el primero
// posee al menos un pokémon que le gane.
    int i = 0;
    while (i < e1->cantPokemon) {
        int cantidadSuperada = 0;
        for (int x = 0; x< e2->cantPokemon; x++) {
            if (superaA(e1->pokemon[i], e2->pokemon[i])) {
                cantidadSuperada++;
            }
        }
        if (cantidadSuperada == e2->cantPokemon) {
            return true;
        }
    }
}

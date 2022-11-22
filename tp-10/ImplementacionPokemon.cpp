#include <iostream>
#include "Pokemon.h"
using namespace std;

Pokemon consPokemon(TipoDePokemon tipo) {
// Dado un tipo devuelve un pokémon con 100 % de energía.
    PokeSt* p = new PokeSt;
    p->tipo = tipo;
    p->vida = 100;
    return (p);
}

TipoDePokemon tipoDePokemon(Pokemon p) {
// Devuelve el tipo de un pokémon.
    return p->tipo;
}

int energia(Pokemon p) {
// Devuelve el porcentaje de energía.
    return p->vida;
}

void perderEnergia(int energia, Pokemon p) {
// Le resta energía al pokémon.
    p->vida = p->vida - energia; 
}

bool superaA(Pokemon p1, Pokemon p2) {
// Dados dos pokémon indica si el primero, en base al tipo, es superior al segundo. Agua supera
// a fuego, fuego a planta y planta a agua. Y cualquier otro caso es falso.
    TipoDePokemon tp1 = p1->tipo;
    TipoDePokemon tp2 = p2->tipo;
    if (tp1 == "fuego" && tp2 == "agua") {
        return false;
    } else if (tp1 == "planta" && tp2 == "fuego") {
        return false;
    } else if (tp1 == "agua" && tp2 == "planta") {
        return false;
    } else {
        return true;
    }
}
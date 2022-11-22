#include "Set.h"
#include "LinkedList.h"
using namespace std;

Set emptyS() {
// Crea un conjunto vacío.
    SetSt* s = new SetSt;
    s->cantidad = 0;
    s->primero = nullptr;
}

bool isEmptyS(Set s) {
// Indica si el conjunto está vacío.
    return s->cantidad == 0;
}

bool belongsS(int x, Set s) {
// Indica si el elemento pertenece al conjunto.
    NodoS* temp = s->primero;
    while (temp->siguiente != nullptr) {
        if (temp->elem == x) {
            return true;
        }
        temp = temp->siguiente;
    }
    return false;
}

void AddS(int x, Set s) {
// Agrega un elemento al conjunto.
    if (!belongsS(x,s)) {
        NodoS* nS = new NodoS;
        nS->elem = x;
        nS->siguiente = s->primero;
        s->primero = nS;
    }
}

void RemoveS(int x, Set s) {
// Quita un elemento dado.
    if (!isEmptyS(s) && s->primero->elem == x) {
        s->primero = nullptr;
    }
    NodoS* temp = s->primero;
    while (temp->siguiente != nullptr) {
        if (temp->siguiente->elem == x) {
            temp->siguiente = temp->siguiente->siguiente;
            break;
        }
        temp = temp->siguiente;
    }    
}

int sizeS(Set s) {
// Devuelve la cantidad de elementos.
    return s->cantidad;
}

LinkedList setToList(Set s) {
// Devuelve una lista con los elementos del conjunto.
    LinkedListSt* xs = new LinkedListSt;
    NodoS* temp = s->primero;
    while (temp != nullptr) {
        int x = temp->elem;
        Snoc(x, xs);
        temp = temp->siguiente;
    }
}

void DestroyS(Set s) {
// Libera la memoria ocupada por el conjunto.
    NodoS* temp = s->primero;
    while (s->primero != nullptr) {
        s->primero = temp->siguiente;
        delete temp;
        temp = s->primero;
    }
    delete s;
}
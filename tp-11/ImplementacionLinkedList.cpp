#include "LinkedList.h"
using namespace std;

LinkedList nil() {
// Crea una lista vacía.
    LinkedListSt* nL = new LinkedListSt;
    nL->cantidad = 0;
    nL->primero = nullptr;
    return nL;
}

bool isEmpty(LinkedList xs) {
// Indica si la lista está vacía.
    return xs->primero == nullptr;
}

int head(LinkedList xs) {
// Devuelve el primer elemento.
    return xs->primero->elem;
}

void Cons(int x, LinkedList xs) {
// Agrega un elemento al principio de la lista.
    NodoL* nL = new NodoL;
    nL->elem = x;
    nL->siguiente = xs->primero;
    xs->primero = nL;
    xs->cantidad++;
}

void Tail(LinkedList xs) {
// Quita el primer elemento.
// La lista no es vacia.
    NodoL* nL = xs->primero;
    xs->primero = nL->siguiente;
    xs->cantidad--;
    delete nL;
}

int length(LinkedList xs) {
// Devuelve la cantidad de elementos.
    return xs->cantidad;
}

void Snoc(int x, LinkedList xs) {
// Agrega un elemento al final de la lista.
    NodoL* nL = new NodoL;
    nL->elem = x;
    nL->siguiente = nullptr;
    NodoL* actual = xs->primero;
    while(actual->siguiente != nullptr) {
        actual = actual->siguiente;
    }
    actual->siguiente = nL;
    xs->cantidad++;
}

ListIterator getIterator(LinkedList xs) {
// Apunta el recorrido al primer elemento.
    IteratorSt* ixs = new IteratorSt;
    ixs->current = xs->primero;
    return ixs; 
}

int current(ListIterator ixs) {
// Devuelve el elemento actual en el recorrido.
    return ixs->current->elem;
}

void SetCurrent(int x, ListIterator ixs) {
// Reemplaza el elemento actual por otro elemento.
    ixs->current->elem = x;
}

void Next(ListIterator ixs) {
// Pasa al siguiente elemento.
    ixs->current = ixs->current->siguiente;
}

bool atEnd(ListIterator ixs) {
// Indica si el recorrido ha terminado.
    return ixs->current->siguiente == nullptr;
}

void DisposeIterator(ListIterator ixs) {
// Libera la memoria ocupada por el iterador.
    delete ixs;
}

void DestroyL(LinkedList xs) {
// Libera la memoria ocupada por la lista.
    while (!isEmpty(xs)) {
        Tail(xs);
    }
    delete xs;
}
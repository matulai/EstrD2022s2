#include "Queue.h"
using namespace std;

Queue emptyQ() {
// Crea una lista vacía.
// Costo: O(1).
    QueueSt* q = new QueueSt;
    q->cantidad = 0;
    q->primero = nullptr;
    q->ultimo = nullptr;
}

bool isEmptyQ(Queue q) {
// Indica si la lista está vacía.
// Costo: O(1).
    return q->primero != nullptr;
}

int firstQ(Queue q) {
// Devuelve el primer elemento.
// Costo: O(1).
    return q->primero->elem;
}

void Enqueue(int x, Queue q) {
// Agrega un elemento al final de la cola.
// Costo: O(1).
    NodoQ* nQ = new NodoQ;
    nQ->elem = x;
    nQ->siguiente = nullptr;
    q->ultimo->siguiente = nQ;
    q->ultimo = nQ;
    q->cantidad++;
}

void Dequeue(Queue q) {
// Quita el primer elemento de la cola.
// Costo: O(1).
    NodoQ* nQ = q->primero;
    q->primero = nQ->siguiente;
    delete nQ;
    q->cantidad--;
}

int lengthQ(Queue q) {
// Devuelve la cantidad de elementos de la cola.
// Costo: O(1).
    return q->cantidad;
}

void MergeQ(Queue q1, Queue q2) {
// Anexa q2 al final de q1, liberando la memoria inservible de q2 en el proceso.
// Nota: Si bien se libera memoria de q2, no necesariamente la de sus nodos.
// Costo: O(1).
    while (q2->primero != nullptr) {
        Enqueue(q2->primero->elem, q1);
        Dequeue(q2);
    }   
}

void DestroyQ(Queue q) {
// Libera la memoria ocupada por la lista.
// Costo: O(n).
    while (q->primero != nullptr) {
        Dequeue(q);
    }
}
// #include "LinkedList.h"
// #include "ArbolBinario.h"
// #include "arrayList.h"
using namespace std;
#include <iostream>

struct Nodo {
    int   elem;
    Nodo* izquierda;
    Nodo* derecha;
};

struct Arbol {
    Nodo* raiz;
};

typedef Arbol* Tree;

int main() {
    // int* a = new int;
    // *a = 12;
    // int* b = a;
    // int* c = a;
    // cout<<"[ "<<a<<" ]"<<" = "<< *a<<endl;
    // delete a;
    // cout<<"[ "<<a<<" ]"<<" = "<< *a<<endl;
    // a = b;
    // cout<<"[ "<<a<<" ]"<<" = "<< *a<<endl;
    // delete b;
    // cout<<"[ "<<a<<" ]"<<" = "<< *a<<endl;
    // int *p = NULL;
    // *p = 1;
    // cout<<"[ "<<p<<" ]"<<" = "<< *p<<endl;
    // LinkedList xs = nil();
    // Cons(1,xs);Cons(2,xs);Cons(3,xs);Cons(4,xs);Cons(5,xs);Cons(6,xs);
    // Snoc(0,xs);
    // DestroyL(xs);
    // cout<<isEmpty(xs)<<endl;
    // cout<<head(xs)<<endl;
    // int* a = nullptr;
    // cout<<a<<endl;
    // cout<<xs<<endl;
    // cout<<xs<<endl;
    Arbol* arbol = new Arbol;
    arbol->raiz = NULL;
    arbol->raiz->derecha = NULL;
    arbol->raiz->izquierda = NULL;
    arbol->raiz->elem = NULL;

    
    int x;
    cin>>x;
    while (x--) {

    }

    cout<<arbol<<" | "<<arbol->raiz<<" | "<<arbol->raiz->derecha<<" | "<<arbol->raiz->izquierda<<" | "<<endl;
    arbol->raiz = arbol->raiz->derecha;
    cout<<arbol<<" | "<<arbol->raiz<<" | "<<arbol->raiz->derecha<<" | "<<arbol->raiz->izquierda<<" | "<<endl;
    
}

// int sumatoria(LinkedList xs) {
// // Devuelve la suma de todos los elementos.
//     int total = 0;
//     ListIterator ixs = getIterator(xs);
//     while (!atEnd(ixs)) {
//         total += current(ixs);
//         Next(ixs);
//     }
//     DisposeIterator(ixs);
//     return total;
// }

// void Sucesores(LinkedList xs) {
// // Incrementa en uno todos los elementos.
//     ListIterator ixs = getIterator(xs);
//     while (!atEnd(ixs)) {
//         int x = current(ixs);
//         SetCurrent(x++, ixs);
//         Next(ixs);
//     }
//     DisposeIterator(ixs);
// }

// bool pertenece(int x, LinkedList xs) {
// // Indica si el elemento pertenece a la lista.
//     ListIterator ixs = getIterator(xs);
//     while (!atEnd(ixs)) {
//         if (current(ixs) == x) {
//             DisposeIterator(ixs);
//             return true;
//         }
//         Next(ixs);
//     }
//     DisposeIterator(ixs);
//     return false;
// }

// int apariciones(int x, LinkedList xs) {
// // Indica la cantidad de elementos iguales a x.
//     ListIterator ixs = getIterator(xs);
//     int y = 0;
//     while (!atEnd(ixs)) {
//         if (current(ixs) == x) {
//             y++;
//         }
//         Next(ixs);
//     }
//     DisposeIterator(ixs);
//     return y;
// }

// int minimo(LinkedList xs) {
// // Devuelve el elemento más chico de la lista.
//     if (isEmpty(xs)) {
//         return (0);
//     }
//     ListIterator ixs = getIterator(xs);
//     int x = current(ixs);
//     while (!atEnd(ixs)) {
//         Next(ixs);
//         int y = current(ixs);
//         if (x > y) {
//             x = y;
//         }
//     }
//     DisposeIterator(ixs);
//     return x;
// }

// LinkedList copy(LinkedList xs) {
// // Dada una lista genera otra con los mismos elementos, en el mismo orden.
// // Nota: notar que el costo mejoraría si Snoc fuese O(1), ¿cómo podría serlo?
//     ListIterator ixs = getIterator(xs);
//     LinkedList ys = nil();
//     while (!atEnd(ixs)) { 
//         int x = current(ixs);
//         Cons(x,ys);
//         Next(ixs);
//     }
//     DestroyL(xs);
//     DisposeIterator(ixs);
//     return ys; 
// }

// void Append(LinkedList xs, LinkedList ys) {
// // Agrega todos los elementos de la segunda lista al final de los de la primera.
// // La segunda lista se destruye.
// // Nota: notar que el costo mejoraría si Snoc fuese O(1), ¿cómo podría serlo?
//     ListIterator iys = getIterator(ys);
//     while (!atEnd(iys)) { 
//         int y = current(iys);
//         Snoc(y,xs);
//         Next(iys);
//     }
//     DestroyL(ys);
//     DisposeIterator(iys);
// }

// // // ÁRBOL BINARIO

// int sumarT(Tree t) {
// // Dado un árbol binario de enteros devuelve la suma entre sus elementos.
//     if (!isEmptyT(t)) {
//         return (rootT(t) + sumarT(right(t)) + sumarT(left(t)) );
//     }
//     return 0;
// }

// int sizeT(Tree t) {
// // Dado un árbol binario devuelve su cantidad de elementos, es decir, el tamaño del árbol (size
// // en inglés).
//     if (!isEmptyT(t)) {
//         return (1 + sizeT(right(t)) + sizeT(left(t)));
//     }
//     return 0;
// }

// bool perteneceT(int e, Tree t) {
// // Dados un elemento y un árbol binario devuelve True si existe un elemento igual a ese en el
// // árbol.
//     if (isEmptyT(t)) {
//         return false;
//     }
//     if (rootT(t) != e) {
//         return (false || perteneceT(e, right(t)) || perteneceT(e, left(t)));
//     }
//     return true;
// }

// int aparicionesT(int e, Tree t) {
// // Dados un elemento e y un árbol binario devuelve la cantidad de elementos del árbol que son
// // iguales a e.
//     if (isEmptyT(t)) {
//         return 0;
//     }
//     if (rootT(t) == e) {
//         return (1 + aparicionesT(e, right(t)) + aparicionesT(e,left(t)));
//     }
// }

// int heightT(Tree t) {
// // Dado un árbol devuelve su altura.
//     if (isEmptyT(t)) {
//         return 0;
//     }
//     return (1 + max(heightT(right(t)), heightT(left(t))));
// }

// ArrayList toList(Tree t) {
// // Dado un árbol devuelve una lista con todos sus elementos.
//     if (!isEmptyT(t)) {
//         add(rootT(t), toList(right(t)));
//     }   
//     return newArrayList();
// }

// ArrayList leaves(Tree t) {
// // Dado un árbol devuelve los elementos que se encuentran en sus hojas.

// }

// ArrayList levelN(int n, Tree t) {
// // Dados un número n y un árbol devuelve una lista con los nodos de nivel n

// }
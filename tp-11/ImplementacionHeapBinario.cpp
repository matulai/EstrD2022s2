using namespace std;
#include "limits.h"
#include "HeapBinario.h"

BinHeap emptyHeap() {
    BinHeapHeaderSt* hB = new BinHeapHeaderSt;
    hB->maxSize = 16;
    hB->curSize = 0;
    hB->elems = new int[hB->maxSize];
    hB->elems[0] = INT_MIN;
    return hB;
}

BinHeap crearHeap(int* elements, int cant) {

}

void AumentarEspacio(BinHeap h) {
    int* newElements = new int[h->maxSize*2];
    for(int i=0;i<=h->curSize;i++) {
        newElements[i] = h->elems[i];
    }
    delete h->elems;
    h->maxSize *= 2;
    h->elems = newElements;
}

void InsertH(int x, BinHeap h) {
    if(h->curSize==h->maxSize-1) { 
        AumentarEspacio(h); 
    }
    // Flotar el nuevo elemento (haciendo lugar para él)
    int curNode = ++h->curSize;
    while(x < h->elems[curNode/2]) {
       h->elems[curNode] = h->elems[curNode/2];
       curNode /= 2;
    }
    h->elems[curNode] = x;
}

bool isEmptyHeap(BinHeap h) {
    return h->curSize == 0;
}

int findMin(BinHeap h) {
    return h->elems[1];
}

void DeleteMin(BinHeap h) { // PRECOND: h->curSize > 0
    int child; int curNode;
    int last = h->elems[h->curSize--];
    for(curNode=1; curNode*2 <= h->curSize; curNode=child) {
            child = curNode*2;
        if ((child != h->curSize) // Elige el hijo más chico
            && (h->elems[child+1] < h->elems[child])) { 
                child++; 
            }
        // Baja un nivel, si el hijo más chico es más chico que last
        if (last > h->elems[child]) { 
            h->elems[curNode] = h->elems[child]; 
        } else {
            break; 
        } // O termina (evitando comparar dos veces lo mismo)
    }
    h->elems[curNode] = last;
}
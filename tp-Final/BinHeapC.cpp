#include <iostream>
#include <iomanip>
#include <limits.h>
using namespace std;

#include "BinHeapC.h"
#include "Cliente.h"
#include "Clientes.h"

#define MIN_DATA INT_MIN
                 // Macro definida en limits.h

/*----------------------------------------------------------------**
||                   FUNCIONES AUXILIARES                         ||
**----------------------------------------------------------------*/

bool esMenorPinOSinoCliente(int pin1, int pin2, Cliente c1, Cliente c2) {
  if (pin1 == pin2) {
    return sizeCliente(c1) < sizeCliente(c2) || (sizeCliente(c1) == sizeCliente(c2) && esClienteMayor(c1, c2));
  } else {
    return pin1 < pin2;
  }
}

/*----------------------------------------------------------------**
||                   ESTRUCTURA DEL BINHEAP                       ||
**----------------------------------------------------------------*/

struct BinHeapHeaderSt{
  int      maxSize;
  int      curSize;
  int*     pins;
  Cliente* clientes;
  /* INV.REP.
      * 0 <= curSize < maxSize
      * los arrays pins y clientes tienen la misma cantidad de celdas reservadas,
        y maxSize guarda esa cantidad
      * los elementos válidos de ambos arrays se encuentran entre las posiciones 
        0 y curSize
      * pins[0] guarda el valor más chico posible de ser almacenado        
      * el array pins guarda los elementos según el orden dado por un árbol binario
        completo que mantiene el invariante de Heap

    OBS:
      * los valores (pins[i], clientes[i]) corresponden a los diferentes elementos,
        para 1<=i<=curSize
      * se usará el nombre N como sinónimo de curSize
  */
};

BinHeapC emptyHC() { // O(1)
  BinHeapHeaderSt* bH = new BinHeapHeaderSt;
  bH->maxSize = 16;
  bH->curSize = 0;
  bH->pins = new int[bH->maxSize];
  bH->clientes = new Cliente[bH->maxSize];
  bH->pins[0] = MIN_DATA;
  bH->clientes[0] = NULL; 
  return(bH);
}

bool isEmptyHC(BinHeapC h) {  // O(1)
  return(h->curSize == 0); 
}

int  findMinPinHC(BinHeapC h) { // O(1)
  // PRECOND: la heap no está vacía
  return(h->pins[1]); 
}

Cliente findMinClienteHC(BinHeapC h) { // O(1)
  // PRECOND: la heap no está vacía
  return(h->clientes[1]); 
}

//---------------------------------------
// Auxiliar para ampliar el espacio de elementos de la heap
//  en caso de ser necesario
//---------------------------------------
void AumentarEspacio(BinHeapC h) { // O(N)
// O(N) siendo N el tamaño actual de los arrays del heap dado, ya que copiar los elementos del array anterior
// al nuevo array con mayor capacidad.
  int* newPins = new int[h->maxSize*2];
  Cliente* newClientes = new Cliente[h->maxSize*2];
  h->maxSize = h->maxSize*2;
  for(int i = 0; i <= h->curSize; i++) {
    newPins[i] = h->pins[i];
    newClientes[i] = h->clientes[i];
  }
  delete h->pins;
  delete h->clientes;
  h->pins = newPins;
  h->clientes = newClientes;
}

void InsertHC(int pin, Cliente c, BinHeapC h) { // O(log N)
  if (h->curSize == h->maxSize - 1) {
    AumentarEspacio(h);
  }
  int curNode = ++h->curSize; // Me posiciono en la posición del nuevo nodo.
  while (esMenorPinOSinoCliente(pin, h->pins[curNode/2],c, h->clientes[curNode/2])) {  
  // Pregunto si el nuevo nodo es menor que el padre y si asi es los intercambio de lugar.
    h->pins[curNode] = h->pins[curNode/2];
    h->clientes[curNode] = h->clientes[curNode/2];
    curNode = curNode/2;
  }
  h->pins[curNode] = pin;
  h->clientes[curNode] = c;
}

void DeleteMinHC(BinHeapC h) { // O(log N)
// PRECOND: h->curSize > 0
// Se coloca al último nodo de la heap en la raiz para luego comparar con los hijos si es mayor que ellos y asi 
// complir con el invariante de representación.
// Se recorre una rama completa de la heap(en peor caso) para que el arbol nos quede completo. 
  int curNode ; int childNode;
  int lastP = h->pins[h->curSize]; Cliente lastC = h->clientes[h->curSize--];
  for (curNode = 1; curNode*2 <= h->curSize; curNode = childNode) {
    childNode = curNode*2;
    if ( (childNode != h->curSize) && esMenorPinOSinoCliente(h->pins[childNode + 1], h->pins[childNode],h->clientes[childNode + 1], h->clientes[childNode])) {
      childNode++;
    } 
    if (esMenorPinOSinoCliente(h->pins[childNode], lastP,h->clientes[childNode], lastC)) {
      h->pins[curNode] = h->pins[childNode];
      h->clientes[curNode] = h->clientes[childNode];
    } else {
      break;
    }
  }
  h->pins[curNode] = lastP;
  h->clientes[curNode] = lastC;
}

void LiberarHC(BinHeapC h) { // O(1)
// Los clientes pueden ser datos compartidos por lo tanto solo se elimina la referencia a los arrays
// de clientes, los pins son numeros asi que solo borramos el array y por último se elimina la 
// referencia al encabezado del heap.
  delete h->clientes;
  delete h->pins;
  delete h;
}

//---------------------------------------
// Auxiliar para el mostrado
//---------------------------------------
void PadHC(int offset) {
  for(int i=0; i<offset; i++) {
    cout << " ";
  }
}

void ShowHC(BinHeapC h, int offset) {
  PadHC(offset); cout << "Heap[" << h->curSize << "," << h->maxSize << "] {" << endl;
  if (h->curSize>0) {
    PadHC(offset);
    cout << "  (" << setw(2) << h->pins[1] << ", " << nombre(h->clientes[1]) << ")" << endl;
  }
  for (int i=2;i<=h->curSize;i++) {
    PadHC(offset);
    cout << ", (" << setw(2) << h->pins[i] << ", " << nombre(h->clientes[i]) << ")" << endl;
  }
  if (h->curSize>0) { PadHC(offset); } else { cout << " "; }; cout << "}" << endl;
}
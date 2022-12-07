#include <iostream>
using namespace std;

#include "Switch.h"
#include "Ruta.h"
#include "Rutas.h"
#include "Cliente.h"
#include "Clientes.h"
struct SNode {
    Cliente conexion; // OBS: si es NULL, está disponible.
    SNode*  boca1;
    SNode*  boca2;
    // INV.REP.:
    //  * (BONUS) uno de los 3 campos es distinto de NULL
};

struct  SwHeaderSt {
  SNode* root;
};

/*----------------------------------------------------------------**
||                   FUNCIONES AUXILIARES                         ||
**----------------------------------------------------------------*/

SNode* newNode() {
// Crea un nuevo nodo vacio. Solo se utiliza del lado de implementador.
  SNode* newNode = new SNode;
  newNode->boca1 = NULL;
  newNode->boca2 = NULL;
  newNode->conexion = NULL;
  return newNode;
}

/*----------------------------------------------------------------**
||                   ESTRUCTURA DEL SWITCH                        ||
**----------------------------------------------------------------*/

Switch newSwitch() {
  SwHeaderSt* s = new SwHeaderSt;
  s->root = NULL;
  return (s); 
}

void Conectar(Cliente c, Ruta r, Switch s) {
// Precondicion: La conexion esta disponible.
// Dados un cliente y una ruta, conecta al cliente en la ruta dada dentro del switch dado. Se crearan nodos de 
// ser necesario para completar la ruta.
  if (s->root == NULL) {  // Para switch vacio.
    s->root = newNode();
  }
  RutaIterator ir = iniciarRuta(r);
  SNode* currentNode = s->root;  
  while (not estaAlFinalDeLaRuta(ir)) {
    if (bocaActual(ir) == Boca1) {
      if (currentNode->boca1 == NULL) {        
        currentNode->boca1 = newNode();
      }
        currentNode = currentNode->boca1;
    } else {
      if (currentNode->boca2 == NULL) {        
        currentNode->boca2 = newNode();        
      }
        currentNode = currentNode->boca2;
    }
    AvanzarEnRuta(ir);
  }
  if (currentNode->conexion != NULL) {
    cerr<<"LA CONEXION NO ESTA DISPONIBLE"<<endl; exit(1);
  } else {
    currentNode->conexion = c;
  }
  LiberarRutaIterator(ir);
}

void Desconectar(Ruta r, Switch s) {
// Dada una ruta desconecta al cliente al final de la ruta dentro del switch, si no llega al final o no hay cliente
// no hace nada.
  SNode* currentNode = s->root;
  RutaIterator ir = iniciarRuta(r);
  while (not (currentNode == NULL || estaAlFinalDeLaRuta(ir))) {
    if (bocaActual(ir) == Boca1) {
      currentNode = currentNode->boca1;
    } else {
      currentNode = currentNode->boca2;
    }
    AvanzarEnRuta(ir);  
  }
  if (estaAlFinalDeLaRuta(ir) && currentNode != NULL) {
    currentNode->conexion = NULL;
  }
  LiberarRutaIterator(ir);
}

// void DesconectarConInvariante(Ruta r, Switch s) {
// // Satisface el invariante de representacion.
//   SNode* currentNode = s->root;
//   SNode* currentNodeFather;
//   RutaIterator ir = iniciarRuta(r);
//   while (not (currentNode == NULL || estaAlFinalDeLaRuta(ir))) {
//     currentNodeFather = currentNode;
//     if (bocaActual(ir) == Boca1) {
//       currentNode = currentNode->boca1;
//     } else {
//       currentNode = currentNode->boca2;
//     }
//     AvanzarEnRuta(ir);  
//   }
//   if (estaAlFinalDeLaRuta(ir) && currentNode != NULL) {
//     currentNode->conexion = NULL;
//     if (currentNode->boca1 == NULL && currentNode->boca2 == NULL) {
//       if (currentNodeFather->boca1 == currentNode) {
//         currentNodeFather->boca1 = NULL;
//       } else {
//         currentNodeFather->boca2 = NULL;
//       }
//     }
//   }
//   LiberarRutaIterator(ir);
// }

// Si la funcion ExtenderTodasLasRutasCon() recibe como parametro una lista de rutas vacia no hace nada. 
Rutas disponiblesADistanciaN(SNode* n, int d) {
  Rutas rsTotal = emptyRutas();
  if (d != 0) {
    Rutas rsBoca1; Rutas rsBoca2;
    if (n != NULL) {
      rsBoca1 = disponiblesADistanciaN(n->boca1, d - 1);
      ExtenderTodasLasRutasCon(Boca1, rsBoca1);
      rsBoca2 = disponiblesADistanciaN(n->boca2, d - 1);
      ExtenderTodasLasRutasCon(Boca2, rsBoca2);
      AgregarA_LasRutasDe_(rsTotal, rsBoca1);
      AgregarA_LasRutasDe_(rsTotal, rsBoca2);
    } else {
      rsBoca1 = disponiblesADistanciaN(n, d - 1);
      ExtenderTodasLasRutasCon(Boca1, rsBoca1);
      rsBoca2 = disponiblesADistanciaN(n, d - 1);
      ExtenderTodasLasRutasCon(Boca2, rsBoca2);
      AgregarA_LasRutasDe_(rsTotal, rsBoca1);
      AgregarA_LasRutasDe_(rsTotal, rsBoca2);
    }
  } else {
    if (n == NULL || n->conexion == NULL) {
      ConsRuta(rutaVacia(), rsTotal);
    } 
  }
  return rsTotal; 
}

Rutas disponiblesADistancia(Switch s, int d) {
    return disponiblesADistanciaN(s->root, d);
}

void LiberarTree(SNode* s) {
  if (s != NULL) {
    LiberarTree(s->boca1);
    LiberarTree(s->boca2);
    delete s;
  }
}

void LiberarSwitch(Switch s) {
  LiberarTree(s->root);
  delete s;
}

//------------------------------------------------------------
// ESTRUCTURA AUXILIAR COLA DE SIGUIENTES PARA RECORRER LINEALMENTE EL SWITCH
//------------------------------------------------------------
struct QNodeSw {
   SNode* node;
   Ruta   ruta;
};

struct INodeSw {
   QNodeSw* elem;
   INodeSw* next; 
};

struct NextsQueueStSw {
    INodeSw* first;
    INodeSw* last;
    // INV.REP.: o ambos son NULL o ambos son distintos de NULL
};
typedef NextsQueueStSw* NextsQueueSw;        // INV.REP.: el puntero NO es NULL

NextsQueueSw emptyQSw() {
  NextsQueueStSw* newQ = new NextsQueueStSw;
  newQ->first = NULL;
  newQ->last  = NULL;  
  return (newQ);
}

bool isEmptyQSw(NextsQueueSw q) {
  return (q->first==NULL);
}

void EnqueueQSw(NextsQueueSw q, Ruta r, SNode* s) {
  INodeSw* newIN = new INodeSw;
  QNodeSw* newQN = new QNodeSw;
  newQN->node = s;
  newQN->ruta  = r;
  newIN->elem  = newQN;
  newIN->next  = NULL;
  if (q->last==NULL) { q->first      = newIN; }
  else               { q->last->next = newIN; }
  q->last = newIN;
}

QNodeSw* DequeueFirstQSw(NextsQueueSw q) {
    // PRECOND: q->first no es NULL
    INodeSw* in = q->first;
    QNodeSw* qn = in->elem;
    q->first = q->first->next;
    if (q->first == NULL) { q->last = NULL; }
    delete(in);
    return(qn);
}

void LiberarQSw(NextsQueueSw q) {
  INodeSw* temp;
  while(not (q->first==NULL)) {
    temp = q->first;
    q->first = q->first->next;
    delete(temp);
  }
  delete(q);
}
//------------------------------------------------------------
// FIN IMPLEMENTACION DE COLA DE SIGUIENTES
//------------------------------------------------------------
void PadSW(int offset) {
  for(int i=0; i<offset; i++) {
    cout << " ";
  }
}

void MostrarConexion(Ruta r, Cliente c) {
    if (not (c==NULL)) { // La conexión NO está disponible
      cout << " "; ShowRuta(r); cout << " -> " << nombre(c) << endl;
    } else {             // La conexión está disponible
      cout << " "; ShowRuta(r); cout << " -> " << "DISPONIBLE" << endl;
    }
}

void ShowSwitch(Switch s, int offset) {
    // OBS: 
    //   * hace un recorrido BFS del switch, para mostrar las rutas ordenadas
    //   * SOLAMENTE se muestran las rutas ocupadas
    //   * es muy ineficiente en el manejo de rutas
    QNodeSw* current;
    Ruta   r;
    NextsQueueSw aProcesar = emptyQSw();
    EnqueueQSw(aProcesar, rutaVacia(), s->root);
    PadSW(offset); cout << "{" << endl;
    while(not isEmptyQSw(aProcesar)) {
      current = DequeueFirstQSw(aProcesar);
      // Procesar y pasar al siguiente en cada hijo...
      if(not (current->node->boca1 == NULL)) 
        { r = copiarRuta(current->ruta);
          SnocBoca(r, Boca1);
          EnqueueQSw(aProcesar, r, current->node->boca1);
        }
      if(not (current->node->boca2 == NULL)) 
        { r = copiarRuta(current->ruta);
          SnocBoca(r, Boca2);
          EnqueueQSw(aProcesar, r, current->node->boca2);
        }
      PadSW(offset); MostrarConexion(current->ruta,current->node->conexion);
      LiberarRuta(current->ruta);
    }
    PadSW(offset); cout << "}" << endl;
    LiberarQSw(aProcesar);
}
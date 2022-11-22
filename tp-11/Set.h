using namespace std;

struct NodoS {
    int elem; // valor del nodo
    NodoS* siguiente; // puntero al siguiente nodo
};
struct SetSt {
    int cantidad; // cantidad de elementos diferentes
    NodoS* primero; // puntero al primer nodo
};
typedef SetSt* Set;


Set emptyS();
bool isEmptyS(Set s);
bool belongsS(int x, Set s);
void AddS(int x, Set s);
void RemoveS(int x, Set s);
int sizeS(Set s);
LinkedList setToList(Set s);
void DestroyS(Set s);
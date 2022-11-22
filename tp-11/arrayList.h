using namespace std;

struct ArrayListSt {
    int cantidad; // cantidad de elementos
    int* elementos; // array de elementos
    int capacidad; // tamaño del array
};

typedef ArrayListSt* ArrayList;

ArrayList newArrayList();
ArrayList newArrayListWith(int capacidad);
int lengthAL(ArrayList xs);
int get(int i, ArrayList xs);
void set(int i, int x, ArrayList xs);
void resize(int capacidad, ArrayList xs);
void add(int x, ArrayList xs);
void remove(ArrayList xs);

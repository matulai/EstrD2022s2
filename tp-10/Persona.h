using namespace std;
#include <string>

struct PersonaST {
    int edad;
    string nombre;
};

typedef struct PersonaST Persona;

Persona consPersona(string nombre, int edad);
string nombre(Persona p);
int edad(Persona p);
void crecer(Persona p);
void cambioDeNombre(string nombre, Persona p);
bool esMayorQueLaOtra(Persona p1, Persona p2);
Persona laQueEsMayor(Persona p1, Persona p2);

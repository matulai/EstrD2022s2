#include <iostream>
#include <string.h>
using namespace std;

#include "Switch.h"
#include "Ruta.h"
#include "Rutas.h"
#include "Cliente.h"
#include "Clientes.h"

int main() {
    Cliente maria     = cliente("Maria");
    Cliente juan      = cliente("Juan");
    Cliente constanza = cliente("Constanza");
    Cliente luisa     = cliente("Luisa");
    Cliente pedro     = cliente("Pedro");
    Cliente kevin     = cliente("Kevin");
    Cliente tatiana   = cliente("Tatiana");
    Cliente ana       = cliente("Ana");
    Cliente diana     = cliente("Diana");

    Ruta ruta0 = rutaVacia();
    Switch s = newSwitch();
    Conectar(maria, ruta0, s);

    Ruta ruta1 = copiarRuta(ruta0);
    SnocBoca(ruta1, Boca1);
    Conectar(constanza, ruta1, s);

    Ruta ruta2 = copiarRuta(ruta0);
    SnocBoca(ruta2, Boca2);
    Conectar(juan, ruta2, s);

    Ruta ruta12 = copiarRuta(ruta1);
    SnocBoca(ruta12, Boca2);
    Conectar(constanza, ruta12, s);

    // ShowSwitch(s, 2);

    // DesconectarConInvariante(ruta1, s);
    // Conectar(ana, ruta1, s);

    // ShowSwitch(s, 2);

    // DesconectarConInvariante(ruta0, s);
    // DesconectarConInvariante(ruta1, s);
    // DesconectarConInvariante(ruta2, s);

    ShowSwitch(s, 2);
    
    // LiberarSwitch(s);
    // Conectar(ana, ruta1, s);
    // ShowSwitch(s, 2);
    //terminate called after throwing an instance of 'std::bad_alloc' what(): std::bad_alloc

    Rutas rs = disponiblesADistancia(s, 2);
    ShowRutas(rs,2);
}
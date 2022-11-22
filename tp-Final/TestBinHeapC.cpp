#include <iostream>
using namespace std;

#include "BinHeapC.h"
#include "Cliente.h"
#include "Clientes.h"

int main() {
    Cliente maria     = cliente("Maria");
    Cliente juan      = cliente("Juan");
    Cliente constanza = cliente("Constanza");
    Cliente luisa     = cliente("Luisa");
    Cliente pedro     = cliente("Pedro");

    BinHeapC h = emptyHC();
    // ShowHC(h, 0);
    InsertHC(2, maria    , h);
    // ShowHC(h, 0);
    InsertHC(5, juan     , h);
    // ShowHC(h, 0);
    InsertHC(1, constanza, h);
    // ShowHC(h, 0);
    InsertHC(3, luisa    , h);
    // ShowHC(h, 0);
    InsertHC(7, pedro    , h);
    // ShowHC(h, 0);
    cout << "El mínimo es " << nombre(findMinClienteHC(h)) << " con pin " << findMinPinHC(h) << ".\n";
    ShowHC(h, 0);
    DeleteMinHC(h);
    cout << "El mínimo es " << nombre(findMinClienteHC(h)) << " con pin " << findMinPinHC(h) << ".\n";
    ShowHC(h, 1);
    DeleteMinHC(h);
    cout << "El mínimo es " << nombre(findMinClienteHC(h)) << " con pin " << findMinPinHC(h) << ".\n";
    ShowHC(h, 2);
    DeleteMinHC(h);
    cout << "El mínimo es " << nombre(findMinClienteHC(h)) << " con pin " << findMinPinHC(h) << ".\n";
    ShowHC(h, 3);
    DeleteMinHC(h);
    cout << "El mínimo es " << nombre(findMinClienteHC(h)) << " con pin " << findMinPinHC(h) << ".\n";
    ShowHC(h, 3);
}
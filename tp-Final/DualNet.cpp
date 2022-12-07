#include <iostream>
using namespace std;

#include "DualNet.h"
#include "Ruta.h"
#include "Rutas.h"
#include "Cliente.h"
#include "Clientes.h"
#include "MapCR.h"
#include "Switch.h"
#include "BinHeapC.h"

struct  DNHeaderSt {
  MapCR  mcr;
  Switch sw;
  /* INV.REP.:
      * si un cliente c en el map mrc está asociado a una ruta r
        entonces el switch sw tiene a c conectado en la ruta r
      * si un cliente c está conectado en la ruta r en el switch sw
        entonces c está asociado a r en el map mcr
  */
};

DualNet emptyDN() {
//  Devuelve un DualNet sin conexiones establecidas.
  DNHeaderSt* dn = new DNHeaderSt;
  dn->mcr = emptyMCR();
  dn->sw = newSwitch();
  return(dn);
}

int cantidadDeClientesConectados(DualNet dn) {
//  Devuelve la cantidad de clientes con conexión establecida en el DualNet dado.
  return(sizeMCR(dn->mcr));
}

bool estaDisponible(Ruta r, DualNet dn) {
// Indica si la ruta dada esta disponible en el DualNet dado.
  Rutas rs = disponiblesADistancia(dn->sw, lenRuta(r));
  RutasIterator irs = iniciarRecorridoDeRutas(rs);
  bool disponible = false;
  while (not (estaAlFinalDeLasRutas(irs) || mismaRuta(r,rutaActual(irs))) ) {
    AvanzarASiguienteRuta(irs);
  }
  bool disponible = mismaRuta(r,rutaActual(irs));
  LiberarRutasIterator(irs);
  LiberarRutas(rs);
  return(disponible);
}

void ConectarCliente(Ruta r, Cliente c, DualNet dn) {
// Dados una Ruta, un Cliente y un DualNet, agrega la conexion del Cliente con la Ruta dada en el DualNet, en el caso
// de que el cliente ya tenga una conexion previa esta sera desconectada ya que solo puede haber una conexion por cliente.
  if (lookupMCR(c, dn->mcr) != NULL) {  
    DesconectarClienteC(c , r, dn);    
  }
  AddMCR (c, r, dn->mcr);
  Conectar(c, r, dn->sw);
}

void DesconectarClienteC(Cliente c, Ruta r, DualNet dn) {
  Desconectar(r, dn->sw);
  DeleteMCR(c, dn->mcr);
}

void DesconectarCliente(Cliente c, DualNet dn) {
// Dados un DualNet y un cliente con conexión establecida, desconecta al cliente del DualNet.
// Precondicion: El cliente tiene una conexion establecida en el DualNet.
  Ruta r = lookupMCR(c, dn->mcr);
  if (r != NULL) { // si r es NULL es False y si no es NULL es true.
    DesconectarClienteC(c, r, dn);
  } else {
    cerr<<"EL CLIENTE NO TIENE UNA CONEXION ESTABLECIDA"<<endl; exit(1);
  }
}

BinHeapC pinPorCliente(DualNet dn) {
//  Dado un DualNet, devuelve un BinHeapC del DualNet dado.
  BinHeapC h = emptyHC();
  Clientes cs = keysMCR(dn->mcr);
  ClientesIterator ics = iniciarRecorridoClientes(cs);
  while (not estaAlFinalDeLosClientes(ics)) {
    InsertHC(lenRuta(lookupMCR(clienteActual(ics), dn->mcr)), clienteActual(ics), h);
    AvanzarASiguienteCliente(ics);
  }
  LiberarClientesIterator(ics);
  LiberarClientes(cs);
  return(h); 
}

void LiberarDN(DualNet dn) {
  LiberarMCR(dn->mcr);
  LiberarSwitch(dn->sw);
  delete dn;
}

void ShowDualNet(DualNet dn) {
  cout << "=======" << endl;
  cout << "DUALNET" << endl;
  cout << "=======" << endl;
  cout << "Map Clientes->Ruta" << endl;
  ShowMapCR(dn->mcr, 2);
  cout << "Switch Clientes" << endl;
  ShowSwitch(dn->sw, 2);
  cout << "=======" << endl;
}
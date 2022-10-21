#include <iostream>
#include "Par.h"
using namespace std;

void cuentaRegresiva(int n){
//Propósito: imprime los números desde n hasta 0, separados por saltos de línea.
    while (n+1) {
        cout<<n<<endl;
        n--;
    }
}

int main() {
    cuentaRegresiva(5);
}
/* 
Ejercicio 1
    1    _______________
        |  ___     ___  |
        | |_0_|   |_2_| |
        |   x  ___  y   |
        |     |_2_|     |               
        |      x+y      |
        |_______________|
    2    _______________
        |  ___     ___  |
        | |_10|   |_5_| |
        |   x       y   |
        |               |               
        |               |
        |_______________|
    3    _______________
        |  _____   ___  |
        | |false| |_1_| |
        |   b       y   |
        |               |               
        |               |
        |_______________|
Ejercicio 2
    1.  Proposito:Dadas dos letras(Char) imprimi las letras desde c1 hasta c2 (ambos incluidos)
        Precondición: c1 < c2
        void printFromTo(char c1, char c2) {
            for(int i = 0; c1 + i <= c2; i++) {
                cout << c1 + i << ", ";
            }
        cout << endl;
        }
        Ejemplo: printFromTo("a", "f") = a, b, c, d, e, f,  
        ConsumoDeMemoria: 1 stack Frame
    2.  Proposito: Imprime el factorial de un número dado. 
        Precondición: n >= 0
        int fc(int n) {
            int x = 1;
            while(n > 0) {
                x = x * n;
                n--;
            }
        return x;
        }
        Ejemplo: fc(5) = 120
        ConsumoDeMemoria: 1 stack Frame
    3.  Proposito: Devuelve el primer valor multiplicado por la diferencia entre el segundo menos el primero.
        Precondición: n <= m
        int ft(int n, int m) {
            if (n == m) {
                return n;
            }
        return n + ft(n+1, m);
        }
        Ejemplo ft(5, 10) = 25
        ConsumoDeMemoria: 1 stack Frame si n == m sino (n-m) stack Frame.
            Se puede mejorar haciendola iterativa y no recursiva. 
Ejercicio 3
    int main() {
        Par par = consPar(2,7);
        cout<<fst(par);
    }
Ejercicio 4
    void printN(int n, string s){
    Propósito: imprime n veces un string s.
        while (n) {
            cout<<s<<endl;
            n--;
        }
    }

    void cuentaRegresiva(int n){
    Propósito: imprime los números desde n hasta 0, separados por saltos de línea.
        while (n+1) {
            cout<<n<<endl;
            n--;
        }
    }

    void desdeCeroHastaN(int n) {
    Propósito: imprime los números de 0 hasta n, separados por saltos de línea.
        for (int i = 0; i<=n; i++) {
            cout<<i<<endl;
        }
    }

    int mult(int n, int m) {
    Propósito: realiza la multiplicación entre dos números (sin utilizar la operación * de C++).
        int x = 0;
        while (m) {
            x += n;
        }
    }

    void primerosN(int n, string s) {
    Propósito: imprime los primeros n char del string s, separados por un salto de línea.
    Precondición: el string tiene al menos n char.
        for (int i = 0;i < n; i++) {
            cout<<s[i]<<endl;
        }
    }

    bool pertenece(char c, string s) {
    Propósito: indica si un char c aparece en el string s.
        int x = 0;
        while (s[x]) {
            x++;
        }
        for (int i= 0; i <= x; i++i) {
            if (c == s[i]) {
                return (True);
            }
        }
        return(False);
    }

    int apariciones(char c, string s){
    Propósito: devuelve la cantidad de apariciones de un char c en el string s
        int x = 0;
        while (s[x]) {
            x++;
        }
        int y = 0;
        for (int i= 0; i <= x; i++i) {
            if (c == s[i]) {
                y++;
            }
        }
        return(y);
    }
*/
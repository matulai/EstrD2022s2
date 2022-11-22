#include <iostream>
#include <string>
#include "Par.h"
#include "Fraccion.h"

using namespace std;

void printFromTo(char, char);
int fc(int);
int ft(int, int);
void printN(int, string);
void printNR(int, string);
void cuentaRegresivaR(int);
void cuentaRegresiva(int);
int aparicionesR(char, string);
bool perteneceR(char, string);

int main() {
    // printFromTo ('A', 'B');
    // cout<<fc(5)<<endl;
    // cout<<ft(5, 10)<<endl;
    // cout<<fst(swap(consPar(3, 7)))<<endl;
    // cout<<10%5<<endl;
    // printN(5,"hola");
    // printNR(5,"holaR");
    // cuentaRegresivaR(4);
    // cuentaRegresiva(4);
    // cout<<aparicionesR('a', "holaquetala")<<endl;
    // cout<<perteneceR('a', "aol")<<endl;
    // cout<<true<<endl;
    // cout<<false<<endl;
    // string x = "holaquetal";
    // cout<<x.substr(1,4)<<endl;
    // cout<<x.length()<<endl;
    // cout<<x.size()<<endl;
    // cout<<x[16]<<endl;
    // cout<<x[17]<<endl;
    // cout<<x[18]<<endl;
    // cout<<x[19]<<endl;
    // cout<<1%1<<endl;
    cout<<numerador(consFraccion(3,3))<<endl;
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
*/

// Ejercicio 2
//     1.  Proposito:Dadas dos letras(Char) imprimi las letras desde c1 hasta c2 (ambos incluidos)
//         Precondición: c1 < c2
        void printFromTo(char c1, char c2) {
            for(int i = 0; c1 + i <= c2; i++) {
                cout << c1 + i << ", ";
            }
        cout << endl;
        }
//         Ejemplo: printFromTo("a", "f") = a, b, c, d, e, f,  
//         ConsumoDeMemoria: 1 stack Frame
//     2.  Proposito: Imprime el factorial de un número dado. 
//         Precondición: n >= 0
        int fc(int n) {
            int x = 1;
            while(n > 0) {
                x = x * n;
                n--;
            }
        return x;
        }
//         Ejemplo: fc(5) = 120
//         ConsumoDeMemoria: 1 stack Frame
//     3.  Proposito: Devuelve la suma de los numeros desde n hasta m, ambos incluidos.
//         Precondición: n <= m
        int ft(int n, int m) {
            if (n == m) {
                return n;
            }
        return n + ft(n+1, m);
        }
//         Ejemplo ft(5, 10) = 45
//         ConsumoDeMemoria:1 stack Frame si n == m sino (n-m) stack Frame.
//                          Se puede mejorar haciendola iterativa y no recursiva. 
// Ejercicio 3
    // int main() {
    //     Par par = consPar(2,7);
    //     cout<<fst(par);
    // }
// Ejercicio 4
    void printN(int n, string s){
//  Propósito: imprime n veces un string s.
        while (n) {
            cout<<s<<endl;
            n--;
        }
    }

    void printNR (int n, string s) {
        if (n) {
            cout<<s<<endl;
            printNR (n-1, s);
        }
    }

    void cuentaRegresiva(int n){
//  Propósito: imprime los números desde n hasta 0, separados por saltos de línea.
        while (n+1) {
            cout<<n<<endl;
            n--;
        }
    }

    void cuentaRegresivaR(int n) {
        cout<<n<<endl;
        if (n) {
            cuentaRegresivaR(n-1);
        }
    }

    void desdeCeroHastaN(int n) {
//  Propósito: imprime los números de 0 hasta n, separados por saltos de línea.
        for (int i = 0; i<=n; i++) {
            cout<<i<<endl;
        }
    }

    // void desdeCeroHastaNR(int n) {
    // int x = n-1;
    //     if (x) {
            
    //     }
    // }

    int mult(int n, int m) {
//  Propósito: realiza la multiplicación entre dos números (sin utilizar la operación * de C++).
        int x = 0;
        while (m) {
            x += n;
        }
        return(x);
    }

    int multR(int n, int m) {
        if (m) {
            n + mult(n, m-1);
        }
        return 0;
    }

    void primerosN(int n, string s) {
//  Propósito: imprime los primeros n char del string s, separados por un salto de línea.
//  Precondición: el string tiene al menos n char.
        for (int i = 0;i < n; i++) {
            cout<<s[i]<<endl;
        }
    }

    // void primerosNR(int n, string s) {
    //     if (n) {
    //         cout<<s[n]<<endl;
    //         primerosNR(n, s);
    //     }
    // }

    bool pertenece(char c, string s) {
//  Propósito: indica si un char c aparece en el string s.
        int x = 0;
        while (s[x]) {
            x++;
        }
        for (int i= 0; i <= x; i++) {
            if (c == s[i]) {
                return (true);
            }
        }
        return(false);
    }

    bool perteneceR(char c, string s) {
        int x = s.length();
        if (x) {
            return s[0] == c || perteneceR(c, s.substr(1,x));
        }
         return false;
         
    }

    int apariciones(char c, string s){
//  Propósito: devuelve la cantidad de apariciones de un char c en el string s
        int x = 0;
        while (s[x]) {
            x++;
        }
        int y = 0;
        for (int i= 0; i <= x; i++) {
            if (c == s[i]) {
                y++;
            }
        }
        return(y);
    }

    int unoSi(bool a) {
        if (a) {
            return 1;
        } 
        return 0;
    }

    int aparicionesR(char c, string s) {
        int x = s.size();
        if (x) {
            return unoSi(c == s[0]) + aparicionesR(c, s.substr(1,x)); 
        } 
        return 0;
    }
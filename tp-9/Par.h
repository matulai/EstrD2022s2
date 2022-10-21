#include <iostream>
using namespace std;

struct ImpPar{
    int x;
    int y;
};

typedef struct ImpPar Par;

Par consPar(int x, int y);
int fst(Par p);
int snd(Par p);
int maxDelPar(Par p);
Par swap(Par p);
Par divisionYResto(int n, int m);

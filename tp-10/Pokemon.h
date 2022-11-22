using namespace std;

typedef string TipoDePokemon;

struct PokeSt {
    TipoDePokemon tipo;
    int vida;
};

typedef PokeSt* Pokemon;

Pokemon consPokemon(TipoDePokemon tipo);
TipoDePokemon tipoDePokemon(Pokemon p);
int energia(Pokemon p);
void perderEnergia(int energia, Pokemon p);
bool superaA(Pokemon p1, Pokemon p2);

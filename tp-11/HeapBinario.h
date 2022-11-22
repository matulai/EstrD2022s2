using namespace std;

struct BinHeapHeaderSt{
    int maxSize; // INV.REP.: curSize < maxSize
    int curSize;
    int* elems;
};
typedef BinHeapHeaderSt* BinHeap; // INV.REP.: el puntero NO es NULL

BinHeap emptyHeap();
void InsertH(int x, BinHeap h);
bool isEmptyHeap(BinHeap h);
int findMin(BinHeap h);
void deleteMin(BinHeap h);
BinHeap crearHeap(int* elements, int cant);
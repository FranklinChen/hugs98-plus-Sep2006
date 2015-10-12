
#include "header.h"
#include "LinkedList.h"

LinkedList* NewLinkedList(void* Data, LinkedList* Next)
{
	LinkedList* l = new LinkedList;
	l->Data = Data;
	l->Next = Next;
	return l;
}

void** LinkedListToArray(LinkedList* Data, int* Size)
{
	//first count them up
	int n = 0;
	for (LinkedList* i = Data; i != NULL; i = i->Next)
		n++;
	*Size = n;
	void** Res = new void*[n];
	int j = 0;
	for (LinkedList* i = Data; i != NULL; i = i->Next)
		Res[j++] = i->Data;
	return Res;
}




struct LinkedList
{
	void* Data;
	LinkedList* Next;
};

LinkedList* NewLinkedList(void* Data, LinkedList* Next);
void** LinkedListToArray(LinkedList* Data, int* Size);

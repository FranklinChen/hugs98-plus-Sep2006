#include "Header.h"

// The history code
// Add to the history, get from the history with a delta
// pushing below the bottom one should return blank
// pushing above top should return top again

// the oldest history is at the lowest number
#define HistoryMax 25

LPSTR History[HistoryMax];
int HistoryPos = 0;
int HistoryCount = 0;

LPCSTR Blank = "";

void AddHistory(LPCSTR Item)
{
    if (HistoryCount != 0 &&
	strcmp(Item, History[HistoryCount-1]) == 0) {
	HistoryPos = HistoryCount;
	return; //duplicate, eat it
    }

    if (HistoryCount == HistoryMax) {
	int i;
	free(History[0]);
	for (i = 1; i < HistoryCount; i++)
	    History[i-1] = History[i];
	HistoryCount--;
    }

    // there is now space for it
    History[HistoryCount] = strdup(Item);
    HistoryCount++;
    HistoryPos = HistoryCount;
}

LPCSTR GetHistory(int delta)
{
    // set a new value, with sanity checks
    HistoryPos += delta;
    if (HistoryPos > HistoryCount)
	HistoryPos = HistoryCount;
    if (HistoryPos < 0)
	HistoryPos = 0;

    if (HistoryPos == HistoryCount)
	return Blank;
    else
	return History[HistoryPos];
}

/*

void AddHistory(LPCSTR Item)
{
    int i;
    InitHistory();

    if (History[0] != NULL)
	free(History[0]);
    for (i = 1; i < HistoryN; i++)
	History[i-1] = History[i];
    History[HistoryN-1] = strdup(Item);
    HistoryPos = HistoryN;
}

LPCSTR GetHistory(int delta)
{
    InitHistory();

    HistoryPos += delta;
    if (HistoryPos >= HistoryN) {
	HistoryPos = HistoryN-1;
	return Blank;
    } else if (History[HistoryPos] == NULL)
	HistoryPos++;

    return History[HistoryPos];
}
*/

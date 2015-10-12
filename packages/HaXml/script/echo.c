#include <stdio.h>

int main (int argc, char** argv) {
  int i=0;
  if (argc>1) {
    if (strcmp(argv[1],"-n")==0) {
      for (i=2; i<argc; i++) {
        fputs(argv[i],stdout);
        if (i+1!=argc) putchar(' ');
      }
    } else {
      for (i=1; i<argc; i++) {
        fputs(argv[i],stdout);
        if (i+1!=argc) putchar(' ');
      }
      putchar('\n');
    }
  } else putchar('\n');
  exit(0);
}

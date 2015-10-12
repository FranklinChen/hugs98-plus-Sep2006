#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <signal.h>
#include <string.h>

/*
 * Simplistic input routines for the default EPOC
 * console.
 *
 * Glenn Strong <Glenn.Strong@cs.tcd.ie>
 *
*/

#define NUM_RECORDED_LINES 10

#define KEY_ESC   27
#define KEY_DEL    8
#define KEY_RET   10
#define KEY_SPACE 32
#define ERASE_CHAR() printf("\b \b")

/* I presume this is a CONIO.H function, which does an
 * unbuffered, unechoed getchar()
*/
char
getch(){
        char c = (char)getchar();
        if (c!=KEY_DEL) 
		ERASE_CHAR();
	else
		printf(" "); /* best we can hope for */
        return c;
};

static char *old_lines[NUM_RECORDED_LINES];
static current_history = 0;
static history_started_flag = 0;

static void history_cleanup(void);

/*
 * Take string s and add it to the current command line history.
 * s is subject to being freed, so we must take a copy of it.
*/
void
add_history(char *s){
	int i;
	/* check exit behaviour */
	if(!history_started_flag){
		history_started_flag=1;
		atexit(history_cleanup);
	}
	/* cycle the buffer */
	if((current_history == (NUM_RECORDED_LINES-1))&&
	   (old_lines[current_history]!=NULL)){
		free(old_lines[0]);
		for(i=0; i<current_history; i++){
			old_lines[i] = old_lines[i+1];
		}
		old_lines[current_history] = NULL;
	}
	old_lines[current_history] = malloc(strlen(s)+1);
	if(old_lines[current_history]==NULL)
		return;
	old_lines[current_history] = strcpy(old_lines[current_history], s);
	if(current_history < (NUM_RECORDED_LINES-1))
		current_history++;
}

#define MAX_BUFSIZE 1024


static void erase_chars(int);
static void editline(const char *op, char *b, int *cursor_pos);

/* declarations */
static void erase_chars(int num);
static void back_up(int num);
static void print_spaces(int num);


/*
 * Get a line of input, using p as the prompt string
 * return a pointer to the buffer containing the string
 * which will be free()d by the client.
 *
*/
char *
readline(char *p){
        char 	*b,   /* buffer to read into */
	     	c;    /* recently read character */
	int 	done=0; /* Finished with this line? */
	int	i;
	int 	cursor_pos, /* location to insert character at */
		last_pos;   /* location of terminating NULL */

	cursor_pos=0; last_pos=0;
        b = malloc(MAX_BUFSIZE);
	*b='\0';

	printf("%s",p);
	do{
		c = getch();
		switch(c){
			case KEY_ESC : 	
					editline(p,b,&cursor_pos);
					last_pos = strlen(b);
					printf("%s%s",p,b);
					back_up(last_pos-cursor_pos);
					break;
			case KEY_RET : done = 1; break;
			case KEY_DEL : 
				if(cursor_pos==0){
					printf("\b%c",p[strlen(p)-1]);
					if(last_pos != 0)
						printf("%c\b",b[0]);
				}else{
					for(i=(cursor_pos-1);i<=(last_pos-1);i++){
						b[i]=b[i+1];
					}
					last_pos--; cursor_pos--;
					printf("\b%s \b",b+cursor_pos);
					back_up(last_pos-cursor_pos);
				}
				break;
			default : if(last_pos==(MAX_BUFSIZE-2)) break;
				for(i=last_pos; i>=cursor_pos; i--){
					b[i+1]=b[i]; /* shuffle up */
				}
				b[cursor_pos]=c; 
				printf("%s",b+cursor_pos);
				back_up(last_pos-cursor_pos);
				cursor_pos++; last_pos++;
				break;
		}
	}while(!done);
	printf("\n");
        return b;
}

static void
erase_chars(int num){
	back_up(num);
	print_spaces(num);
	back_up(num);
}

static void
back_up(int num){
	for(;num>0;num--) printf("\b");
}

static void
print_spaces(int num){
	for(;num>0;num--) printf(" ");
}

/*
 *
 * Line editing mode: select lines from history buffer and edit them.
 *
*/

#define EDIT_PROMPT "Edit> "
static void
editline(const char *p, char *b, int *cursor_pos){
	char store[MAX_BUFSIZE];
	char c;
	int n, i, done=0, old_cursor_pos = (*cursor_pos);
	printf("%s",b+(*cursor_pos));
	erase_chars(strlen(b)+strlen(p)-1);
	printf("%s%s",EDIT_PROMPT,b);
	print_spaces( (strlen(p) - strlen(EDIT_PROMPT) ));
	back_up( (strlen(p) - strlen(EDIT_PROMPT) ));
	back_up(strlen(b)-(*cursor_pos));
	strcpy(store,b);
	do{
		c = (char)getchar();
		ERASE_CHAR();
		switch(c){
			case KEY_SPACE: 
				if(current_history==0) break;
				c = (char)('0' + current_history);
				if(current_history!=(NUM_RECORDED_LINES-1)) 
					c--;
			case '0': 
			case '1':
			case '2':
			case '3': 
			case '4':
			case '5':
			case '6': 
			case '7': 
			case '8':
			case '9': n = c-'0';
				  i = strlen(b);
				  if ((n<=current_history)&&(old_lines[n]!=NULL)){
					strcpy(b,old_lines[c-'0']); 
				  };
				  erase_chars(strlen(EDIT_PROMPT)+(*cursor_pos));
				  printf("%s%s",EDIT_PROMPT,b);
				  print_spaces( i-(strlen(b) ));
				  back_up( i - strlen(b) );
				  (*cursor_pos) = strlen(b);
				break;
			case KEY_ESC: 
				printf("\n");
				for(i=0;i<=current_history;i++)
					if(old_lines[i]!=NULL)
						printf("%d %s\n",i,old_lines[i]);
				printf("%s%s","Edit>",b);
				back_up( strlen(b)-(*cursor_pos) );
				break;
			case 'h':
				if((*cursor_pos) > 0){
					printf("%s",(b+(*cursor_pos)));
					(*cursor_pos)--;
					back_up(strlen(b)-(*cursor_pos));
				}
				break;
			case 'l':
				if((*cursor_pos) < strlen(b)){
					printf("%s",(b+(*cursor_pos)));
					(*cursor_pos)++;
					back_up(strlen(b)-(*cursor_pos));
				}
				break;
			case '$':
				printf("%s",(b+(*cursor_pos)));
				(*cursor_pos) = strlen(b);
				break;
			case '^':
				printf("%s",(b+(*cursor_pos)));
				(*cursor_pos)=0;
				back_up(strlen(b)-(*cursor_pos));
				break;
			case 'D':
				print_spaces(strlen(b)-(*cursor_pos));
				back_up(strlen(b)-(*cursor_pos));
				b[*cursor_pos] = '\0';
				(*cursor_pos) = strlen(b);
				break;	
			case 'r':
				printf("%s",b+(*cursor_pos));
				back_up(strlen(b)-(*cursor_pos));
				c=getchar();
				if(c==KEY_ESC){
					printf("%s",b+(*cursor_pos));
					back_up(strlen(b)-(*cursor_pos));
				}else{
					if((*cursor_pos)==strlen(b))
						b[(*cursor_pos)+1] = '\0';
					b[(*cursor_pos)] = c;
					back_up(1);
					printf("%s",b+(*cursor_pos));
					back_up(strlen(b)-(*cursor_pos));
				}
				break;
			case 'q':
				erase_chars(*cursor_pos);
				strcpy(b,store);
				printf("%s\n",b);
				print_spaces( (*cursor_pos) - (strlen(p)+strlen(b)) );
				back_up( (*cursor_pos) - (strlen(p)+strlen(b)+1) );
				(*cursor_pos) = old_cursor_pos;
				done = 1;
				break;
			case KEY_RET : done = 1; printf("\n"); break;
			case '?' : 
				printf("\nCheat sheet for edit mode hack\n");
				printf("\tescape\tShow history buffer\n");
				printf("\tdigit\tSelect history line\n");
				printf("\tspace\tSelect most recent\n");
				printf("\treturn\tBack to input mode\n");
				printf("\tq     \tRevert to input mode\n");
				printf("\th     \tBack one character\n");
				printf("\tl     \tForward one character\n");
				printf("\t^     \tStart of line\n");
				printf("\t$     \tEnd of line\n");
				printf("\tD     \tDelete to end of line\n");
				printf("\tr     \tChange current character\n");
				printf("%s%s",p,b);
				break;
			default : 
				printf("%s",b+(*cursor_pos));
				back_up(strlen(b)-(*cursor_pos));
				break;
		}
	}while(!done);
}

/*
 * We allocate some heap space for dealing with the history.
 * although it's probably not necessary, we want to clean it up
*/
static void
history_cleanup(){
	int i;
	for(i=0;i<NUM_RECORDED_LINES;i++)
		if(old_lines[i]!=NULL)
			free(old_lines[i]);
}


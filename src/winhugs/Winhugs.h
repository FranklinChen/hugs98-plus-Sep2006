/* external interface of winhugs files */

#ifndef __WINHUGS_H__
#define __WINHUGS_H__

#include <stdio.h>

// standard definitions for communicating with WinHugs deliberately
// pop up the message in an error box
extern void	ErrorBox(const char* Msg);
extern void	InfoBox(const char* Msg);

extern char*	WinHugsPickDefaultEditor();

extern void	WinHugsExit(void);

extern int	InAutoReloadFiles;
extern void	SetWorkingDir(const char* Src);
extern void	startEvaluatorThread(void);
extern void	stopEvaluatorThread(void);
extern void	loopInBackground(void);

// support for Most Recently Used Files
extern void	WinHugsAddMruFile(const char* file);

extern void	WinHugsMessagePump(void);

// Used for trapping console output and GUI'ifying it
// fprintf (stdstr, ...) is used to direct output to the string stdstrbuff
extern FILE	*stdstr;
extern char	stdstrbuff[];

// Colours

// do not use Windows RGB so you don't introduce a dependancy on Windows.h
#define rgb(r,g,b) ((r) | ((g) << 8) | ((b) << 16))

#define BLACK	rgb(0,0,0)
#define BLUE	rgb(0,0,175)
#define GREEN	rgb(0,135,0)
#define CYAN	rgb(0,175,175)
#define RED	rgb(175,0,0)
#define MAGENTA	rgb(150,0,150)
#define YELLOW	rgb(175,175,0)
#define WHITE	rgb(255,255,255)

extern int	WinHugsColor(int Color);

// Redirection of console I/O

extern void	WinHugsPutS(FILE* f, char* Buffer);
extern int	WinHugsPrintf(const char* format, ...);
extern int	WinHugsFPrintf(FILE* f, const char* format, ...);
extern int	WinHugsPutC(FILE* f, int c);
extern int	WinHugsGetC(FILE* f);
extern void	WinHugsHyperlink(const char* msg);
extern void	WinHugsFilename(const char* FileName, int LineNo);

// undefine everything that is a macro already
#undef getc
#undef getchar
#undef putchar
#undef putc

// output with formatting buffers
#define printf          WinHugsPrintf
#define fprintf         WinHugsFPrintf

// standard output
#define putchar(ch)     WinHugsPutC(stdout, ch)
#define putc(ch,file)   WinHugsPutC(file, ch)
#define fputc(ch,file)  WinHugsPutC(file, ch)

// standard input
#define getc(file)      WinHugsGetC(file)
#define getchar()       WinHugsGetC(stdin)
#define getch()         WinHugsGetC(stdin)

#endif /* __WINHUGS_H__ */

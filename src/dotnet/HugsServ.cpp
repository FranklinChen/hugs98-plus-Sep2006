//
// Managed C++ wrapper class around the Hugs server API.
//
#using <mscorlib.dll>
extern "C" {
#include "prelude.h"
#include "storage.h"
#include "machdep.h"
#include "connect.h"
};
#include "prim.h"
#include "HugsServ.h"

#define ToCharString(str) \
  static_cast<char*>(System::Runtime::InteropServices::Marshal::StringToHGlobalAnsi(str).ToPointer())
#define FreeCharString(pstr) System::Runtime::InteropServices::Marshal::FreeHGlobal(pstr)

extern "C" {
extern char* lastError;
extern char* ClearError();
extern Void  setError  (char*);
extern Bool  safeEval  (Cell c);
extern Void  startEval (Void);
};

/* All server entry points set CStackBase for the benefit of the (conservative)
 * GC and do error catching.  Any calls to Hugs functions should be "protected"
 * by being placed inside this macro.
 *
 *   void entryPoint(arg1, arg2, result)
 *   T1 arg1;
 *   T2 arg2;
 *   T3 *result;
 *   {
 *       protect(doNothing(),
 *           ...
 *       );
 *   }
 *
 * Macro decomposed into BEGIN_PROTECT and END_PROTECT pieces so that i
 * can be used on some compilers (Mac?) that have limits on the size of
 * macro arguments.
 */
#define BEGIN_PROTECT \
  if (NULL == lastError) { \
      Cell dummy; \
      CStackBase = &dummy;              /* Save stack base for use in gc  */ \
      consGC = TRUE;                    /* conservative GC is the default */ \
      if (1) {
#define END_PROTECT \
      } else { \
	setError("Error occurred"); \
	normalTerminal(); \
      }	\
  }
#define protect(s)	BEGIN_PROTECT s; END_PROTECT

static Void    MkObject    Args((System::Object*));
static Object* EvalObject  Args((Void));
static Int     DoIO_Object Args((Object* __gc&));

/* Push an Object/DotNetPtr onto the stack */
static Void MkObject(Object* a) 
{
#ifndef NO_DYNAMIC_TYPES
    Cell d = getTypeableDict(type);
    if (isNull(d)) {
      setError("MkObject: can't create Typeable instance");
      return 0;
    }
    protect(push(ap(ap(nameToDynamic,d),mkDotNetPtr(a,freeNetPtr))));
#else
    protect(push(mkDotNetPtr(a,freeNetPtr)));
#endif
}

static Object* EvalObject()          /* Evaluate a cell (:: Object)    */
{
    Cell d;
    BEGIN_PROTECT
	startEval();
#ifndef NO_DYNAMIC_TYPES
	d = getTypeableDict(type);
	if (isNull(d)) {
	    setError("EvalObject: can't create Typeable instance");
	    return 0;
	}
	safeEval(ap(ap(nameToDynamic,d),pop()));
#else
	safeEval(pop());
#endif
	normalTerminal();
	return getNP(whnfHead);
    END_PROTECT
    return 0;
}

/* 
 * Evaluate a cell (:: IO DotNetPtr) return exit status
 */
static Int DoIO_Object(Object* __gc& phval)
{
    BEGIN_PROTECT
        Int exitCode = 0;
        Bool ok;
        StackPtr oldsp = sp;
        startEval();
#ifndef NO_DYNAMIC_TYPES
        ok = safeEval(ap(nameIORun,ap(nameRunDyn,pop())));
#else
        ok = safeEval(ap(nameIORun,pop()));
#endif
        if (!ok)
        {
            sp = oldsp-1;   
            exitCode = 1;
	} else if (whnfHead == nameLeft) { 
            safeEval(pop());
            exitCode = whnfInt;
        } else {   
	    if (phval) {
	      safeEval(pop());
	      phval = getNP(whnfHead);
	    } else {
	      drop();
	    }
            exitCode = 0; 
        }
        normalTerminal();
        if (sp != oldsp-1) {
            setError("doIO: unbalanced stack");
            return 1;
        }
        return exitCode;
    END_PROTECT;
    return -1; /* error code */
}

namespace Hugs {

System::String* Server::ClearError() {
  char* s = m_server->clearError();
  return new System::String(s);
}

//
// Method:  SetHugsArgs(String* argv[])
//
// Purpose: Configure the argument vector which
//          H98's System.getArgs returns.
//

void Server::SetHugsArgs(System::String* argv[]) {
  char __nogc* __nogc* args = new char*[argv.Length];
  int len = argv.Length;
  
  for (int i=0; i < len; i++) {
    args[i] = new char[argv[i]->Length + 1];
    args[i] = ToCharString(argv[i]);
  }
  m_server->setHugsArgs(len,args);
  
  /* Looks kind of silly; a better way? */
  for (int i=0; i < len; i++) {
    delete args[i];
  }
  delete args;

  return;
}

int    Server::GetNumScripts() {
  return m_server->getNumScripts();
}

void   Server::Reset(int i) {
  m_server->reset(i);
  return;
}

void   Server::SetOutputEnable (int i) {
  m_server->setOutputEnable(i);
  return;
}

void   Server::ChangeDir(System::String* dir) {
  char __nogc* dirStr = ToCharString(dir);
  m_server->changeDir(dirStr);
  FreeCharString(dirStr);
  return;
}

void   Server::LoadProject(System::String* proj) {
  char __nogc* projStr = ToCharString(proj);
  m_server->loadProject(projStr);
  FreeCharString(projStr);
  return;
}

void   Server::LoadFile(System::String* fname) {
  char __nogc* fnameStr = ToCharString(fname);
  m_server->loadProject(fnameStr);
  FreeCharString(fnameStr);
  return;
}

void   Server::LoadFromBuffer(System::String* haskMod) {
  char __nogc* hStr = ToCharString(haskMod);
  m_server->loadFromBuffer(hStr);
  FreeCharString(hStr);
  return;
}

void   Server::SetOptions(System::String* opts) {
  char __nogc* hStr = ToCharString(opts);
  m_server->setOptions(hStr);
  FreeCharString(hStr);
  return;
}

System::String* Server::GetOptions() {
  char* r = m_server->getOptions();
  return System::Runtime::InteropServices::Marshal::PtrToStringAnsi(r);
}

HVal Server::CompileExpr(System::String* mo,System::String* v) {
  char __nogc* moStr = ToCharString(mo);
  char __nogc* vStr = ToCharString(v);
  HVal res = m_server->compileExpr(moStr,vStr);
  FreeCharString(moStr);
  FreeCharString(vStr);
  return res;
}

void Server::GarbageCollect() {
  m_server->garbageCollect();
  return;
}

void  Server::LookupName(System::String* mo,System::String* v) {
  char __nogc* moStr = ToCharString(mo);
  char __nogc* vStr = ToCharString(v);
  m_server->lookupName(moStr,vStr);
  FreeCharString(moStr);
  FreeCharString(vStr);

  return;
}

void Server::mkInt(int i) {
  m_server->mkInt(i);
  return;
}

void Server::mkAddr(void* ptr) {
  m_server->mkAddr(ptr);
  return;
}

void Server::mkObject(Object* obj) {
  MkObject(obj);
  return;
}

void  Server::mkString(System::String* s) {
  char* str = ToCharString(s);
  m_server->mkString(str);
  FreeCharString(str);
  return;
}

void   Server::apply() {
  m_server->apply();
  return;
}

int Server::evalInt() {
  return m_server->evalInt();
}

void* Server::evalAddr() {
  return m_server->evalAddr();
}

Object* Server::evalObject() {
  return EvalObject();
}

System::String* Server::evalString() {
  char* str = m_server->evalString();
  return System::Runtime::InteropServices::Marshal::PtrToStringAnsi(str);
}

int Server::doIO() {
  return m_server->doIO();
}

int Server::doIO_Int(int* pRes) {
  return 0;
}

int Server::doIO_Addr(void** pRes)  {
  return 0;
}
int Server::doIO_Object(Object* __gc& pRes)  {
  return DoIO_Object(pRes);
}

HVal Server::popHVal() {
  HVal h = m_server->popHVal();
  return h;
}

void Server::pushHVal(HVal arg) {
  m_server->pushHVal(arg);
  return;
}

void Server::freeHVal(HVal arg) {
  m_server->freeHVal(arg);
  return;
}
};

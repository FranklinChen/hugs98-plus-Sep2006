#using <mscorlib.dll>
extern "C" {
#include "prelude.h"
#include "storage.h"
#include "connect.h"
#include "builtin.h"
#include "errors.h"
#include "evaluator.h"
#include "prim.h"
#include "Invoker.h"
};

/* Utility macro for converting a System::String to a char* (and later on, free it.) */
#define ToCharString(str) \
  (char*)System::Runtime::InteropServices::Marshal::StringToHGlobalAnsi(str).ToPointer()
#define FreeCharString(pstr) System::Runtime::InteropServices::Marshal::FreeHGlobal(pstr)

/* --------------------------------------------------------------------------
 * .NET Ptrs: like mallocPtrs, but store managed pointers instead.
 * ------------------------------------------------------------------------*/
__gc struct strDotNetPtr {      /* .NET pointer description            */
    Cell npcell;		/* Back pointer to NPCELL              */
    System::Object* ptr;        /* Pointer into managed heap           */
    Int  refCount;              /* Reference count                     */
    Void (*cleanup) Args((System::Object*)); 
                                /* Code to free the .NET pointer */
};

/*
 * Encoding a global variable in Managed C++;
 */
__gc class DotNetPtrTable {
private:
  static System::Array __gc* m_table;
public:
  static DotNetPtrTable() {
    int i;
    m_table = Array::CreateInstance(__typeof(__gc struct strDotNetPtr), NUM_DOTNETPTRS); 
    for (i=0; i < NUM_DOTNETPTRS; i++) {
      __gc struct strDotNetPtr* rec = new __gc struct strDotNetPtr;
      rec->ptr      = 0;
      rec->cleanup  = 0;
      rec->refCount = 0;
      rec->npcell   = NIL;
      m_table->SetValue(rec,i);
    }
  }
  static int hw_dotnetptrs = 1;
  static void setVal(int idx, __gc struct strDotNetPtr* rec) {
#if 0
    Console::WriteLine("setVal({0}) = {1}", __box(idx), rec->ptr->ToString());
#endif
    m_table->SetValue(rec,idx);
  }

  static __gc struct strDotNetPtr* indexDotNetPtrs (int i) {
    __gc struct strDotNetPtr* res = static_cast<__gc struct strDotNetPtr*>(m_table->GetValue(i));
#if 0
    if (res->ptr == 0) {
      ;
    } else {
      Console::WriteLine("indexDotNetPtrs({0}) = {1}", __box(i), res->ptr->ToString());
    }
#endif
    return res;
  }
};

__gc struct strDotNetPtr* idxDotNetPtr(int i) {
  return DotNetPtrTable::indexDotNetPtrs(i);
}

#define dotNetPtrOf(c)   snd(c)
#define derefNP(c)       (idxDotNetPtr((Int)dotNetPtrOf(c))->ptr)

static Cell mkDotNetPtr Args((System::Object *, Void (*)(System::Object *)));

extern "C" {
System::Object __gc* getNP(Cell c) {
  return derefNP(c);
}

static Void incDotNetPtrRefCnt  Args((Int, Int));

static Void local pushWideString(wchar_t s __gc[], int l) {  /* push pointer to string onto stack */
    Int  j;
    char arr[10];
    int len;

    push(nameNil);
    while (--l >= 0) {
      len = wctomb(arr,s[l]);
      for (j=len ; j >= 1; j--) {
	topfun(consChar(arr[len-j]));
      }
    }
}

Void zeroDotNetTable() {
  Int i;

  for (i=0; i<DotNetPtrTable::hw_dotnetptrs; i++)
    idxDotNetPtr(i)->npcell = NIL;
}

Void markDotNetPtrs(Int* marks) {
  Int i;
  
  int max = DotNetPtrTable::hw_dotnetptrs-1;
  for (i=max; i>0; i--) { /* release any unused dotnet ptrs   */
    __gc struct strDotNetPtr* rec = DotNetPtrTable::indexDotNetPtrs(i);
    if (isPair(rec->npcell)) {
      int place = placeInSet(rec->npcell);
      int mask  = maskInSet(rec->npcell);
      if ((marks[place]&mask)==0) {
	incDotNetPtrRefCnt(i,-1);
      }
    }
  }
}

/*
 * Allocate .NET object reference on the heap, tagging it with
 * an NPCELL.
 */
static
Cell
mkDotNetPtr(Object* ptr,void (*cleanup)(Object*)) { 
  int i;
  Cell c = 0;
  __gc struct strDotNetPtr* rec = new __gc struct strDotNetPtr;

  for (i=0; i<DotNetPtrTable::hw_dotnetptrs && 
	    idxDotNetPtr(i)->refCount!=0; ++i)
    ;					    /* Search for unused entry */
  if (i>= NUM_DOTNETPTRS) {                 /* If at first we don't    */
    garbageCollect();			    /* succeed, garbage collect*/
    for (i=0; i<NUM_DOTNETPTRS && 
	      idxDotNetPtr(i)->refCount!=0; ++i)
      ;					/* and try again ...	   */
  }
  if (i>=NUM_DOTNETPTRS) {			/* ... before we give up   */
    ERRMSG(0) "Too many DotNetPtrs open"
      EEND;
  }
  if ((i+1) >= DotNetPtrTable::hw_dotnetptrs) {
    DotNetPtrTable::hw_dotnetptrs = i+1;
  }
  c = ap(NPCELL,i);

  rec->ptr      = ptr;
  rec->cleanup  = cleanup;
  rec->refCount = 1;
  rec->npcell   = c;

#if 0
  Console::WriteLine("Created {0} at idx {1} {2}", ptr->ToString(), __box(i), __box(snd(c)));
#endif
  DotNetPtrTable::setVal(i,rec);

  return c;
}

static Void
incDotNetPtrRefCnt(Int n, Int i) { /* change ref count of MallocPtr */
  if (!(0<=n && n<DotNetPtrTable::hw_dotnetptrs && 
	idxDotNetPtr(n)->refCount > 0))
    internal("incDotNetPtrRefCnt");
  __gc struct strDotNetPtr* rec = idxDotNetPtr(n);

  rec->refCount += i;
  if (rec->refCount <= 0) {
    rec->cleanup(rec->ptr);
    rec->ptr      = 0;
    rec->cleanup  = 0;
    rec->refCount = 0;
    rec->npcell   = NIL;
#if 0
    Console::WriteLine("adjusting hw {0} {1}", 
		       __box(n),
		       __box(DotNetPtrTable::hw_dotnetptrs));
#endif
    if ((n+1) == DotNetPtrTable::hw_dotnetptrs) {
      DotNetPtrTable::hw_dotnetptrs = n;
    }
  }
}
  

void freeNetPtr (Object* x) { 
  return;
}

/* ------------------------------------------------------------------------
 * The .NET Primops: 
 * ------------------------------------------------------------------------*/

/*
 * Function: primInvoker()
 *
 * Performs invocation of .NET methods / access fields using metadata provided
 * via a FFI declaration. 
 */
Void primInvoker(StackPtr root,Name n) {
   ::Text  methName;
   ::Text  fieldSpec;
   ::Text  libName;
   Int   ffiFlags;
   Bool  isIO;
   Int   resultType;
   List  paramTys;
   Cell  c;
   Int   primArity;
   
   if (name(n).foreignInfo == NIL) {
     Console::WriteLine("primInvoker: no ForeignInfo!");
     return;
   }
   /* unravel the foreignInfo metadata; beautiful. */
   c = name(n).foreignInfo;
   methName   = fst(c);
   libName    = fst(snd(c));
   ffiFlags   = intOf(fst(snd(snd(c))));
   isIO       = intOf(fst(snd(snd(snd(c)))));
   resultType = intOf(fst(snd(snd(snd(snd(c))))));
   paramTys   = snd(snd(snd(snd(snd(c)))));
   
   primArity = (isIO ? IOArity : 0);
#define PrimReturn(r) if (isIO) { IOReturn(r) ; } else if (len == 0) { push(r); return; } else { updateRoot(r); return; }
   
   System::String *methNameStr = new System::String(textToStr(methName));
   System::String *libNameStr  = ( (libName != -1) ? new System::String(textToStr(libName)) : "");
   
   int len = length(paramTys);
   List ps = paramTys;
   Bool hasThis = 
      ((ffiFlags & FFI_DOTNET_STATIC) |
       (ffiFlags & FFI_DOTNET_CTOR)) == 0;
   int i = 0;
   int noArgs = (hasThis ? len - 1 : len);
   
   System::Array *args = Array::CreateInstance(__typeof(Object), noArgs);
   System::Object *thisPtr;
   
   /* NOTE: the 'this' pointer is assumed to be passed *last* */
   if (hasThis) {
     eval(primArg(1+primArity));
     thisPtr = derefNP(whnfHead);
   }
   
   /* Fill in the parameter array */
   for (i=0; i<noArgs;i++,ps=tl(ps)) {
     switch (intOf(hd(ps))) {
     case FFI_TYPE_UNIT:
       break;
     case FFI_TYPE_CHAR:
       eval(primArg((len-i)+primArity));
       checkInt();
       args->SetValue(__box((char)whnfInt),i);
       break;
     case FFI_TYPE_INT:
       eval(primArg((len-i)+primArity));
       checkInt();
       args->SetValue(__box(whnfInt),i);
       break;
     case FFI_TYPE_INT8:
       eval(primArg((len-i)+primArity));
       checkInt();
       args->SetValue(__box((signed char)whnfInt),i);
       break;
     case FFI_TYPE_INT16:
       eval(primArg((len-i)+primArity));
       checkInt();
       args->SetValue(__box(System::Convert::ToInt16(whnfInt)),i);
       break;
     case FFI_TYPE_INT32:
       eval(primArg((len-i)+primArity));
       checkInt();
       args->SetValue(__box(whnfInt),i);
       break;
     case FFI_TYPE_INT64:
       eval(primArg((len-i)+primArity));
       args->SetValue(__box(int64FromParts(intOf(fst(snd(whnfHead))),
					   intOf(snd(snd(whnfHead))))),
		      i);
       break;
     case FFI_TYPE_WORD8:
       eval(primArg((len-i)+primArity));
       checkInt();
       args->SetValue(__box((unsigned char)whnfInt),i);
       break;
     case FFI_TYPE_WORD16:
       eval(primArg((len-i)+primArity));
       checkInt();
       args->SetValue(__box(System::Convert::ToUInt16(whnfInt)),i);
       break;
     case FFI_TYPE_WORD32:
       eval(primArg((len-i)+primArity));
       checkInt();
       args->SetValue(__box((unsigned int)whnfInt),i);
       break;
     case FFI_TYPE_WORD64:
       eval(primArg((len-i)+primArity));
       args->SetValue(__box(int64FromParts(intOf(fst(snd(whnfHead))),
					   intOf(snd(snd(whnfHead))))),
		      i);
       break;
     case FFI_TYPE_FLOAT:
       eval(primArg((len-i)+primArity));
       checkFloat();
       args->SetValue(__box((float)whnfFloat), i);
       break;
     case FFI_TYPE_DOUBLE:
       eval(primArg((len-i)+primArity));
       checkDouble();
       args->SetValue(__box((float)whnfDouble), i);
       break;
     case FFI_TYPE_BOOL:
       eval(primArg((len-i)+primArity));
       checkBool();
       args->SetValue(__box((int)whnfHead==nameTrue), i);
       break;
     case FFI_TYPE_ADDR:
     case FFI_TYPE_PTR:
     case FFI_TYPE_FUNPTR:
       eval(primArg((len-i)+primArity));
       checkPtr();
       args->SetValue(__box((System::IntPtr)ptrOf(whnfHead)), i);
       break;
     case FFI_TYPE_FOREIGN:
       eval(primArg((len-i)+primArity));
       args->SetValue(__box((System::IntPtr)derefMP(whnfHead)), i);
       break;
     case FFI_TYPE_STABLE:
       eval(primArg((len-i)+primArity));
       args->SetValue(__box((int)intOf(whnfHead)), i);
       break;
     case FFI_TYPE_OBJECT:
       eval(primArg((len-i)+primArity));
       args->SetValue(derefNP(whnfHead), i);
       break;
     case FFI_TYPE_STRING:
       {
       char* r = (char*)evalName(primArg((len-i)+primArity));
       args->SetValue(new System::String(r),i);
       break;}
     default:
       break;
     }
   }
   
   /* Make the call / access the field. */
   try {
     Object __pin* res;
     if ( ffiFlags & FFI_DOTNET_CTOR ) {
       res = DynInvoke::InvokeBridge::CreateObject(libNameStr, methNameStr, args);
       PrimReturn(mkDotNetPtr(res,freeNetPtr));
     } else if ( ffiFlags & FFI_DOTNET_METHOD ) {
       if ( ffiFlags & FFI_DOTNET_STATIC ) {
	 res = DynInvoke::InvokeBridge::InvokeStaticMethod(libNameStr, methNameStr, args);
       } else {
         res = DynInvoke::InvokeBridge::InvokeMethod(thisPtr, methNameStr, args);
       }
     } else if ( ffiFlags & FFI_DOTNET_FIELD ) {
       Bool setter = (resultType == FFI_TYPE_UNIT);
       if ( ffiFlags & FFI_DOTNET_STATIC ) {
	 /* ToDo: split up methNameStr into class and field components */
	 System::String *fieldName;
	 System::String *clsName;
	 int idx = methNameStr->LastIndexOf('.');
	 
	 if (idx != (-1) ) {
	   fieldName = methNameStr->Substring(idx+1);
	   clsName   = methNameStr->Substring(0,idx);
	 }
	 if (setter) {
	   DynInvoke::InvokeBridge::SetStaticField(clsName, 
						   fieldName,
						   args->GetValue(0));
	   IOReturn(nameUnit);
	 } else {
	   res = DynInvoke::InvokeBridge::GetStaticField(clsName,
							 fieldName);
	 }
       } else {
	 if (setter) {
	   DynInvoke::InvokeBridge::SetField(thisPtr, methNameStr, args->GetValue(0));
	   IOReturn(nameUnit);
	 } else {
	   res = DynInvoke::InvokeBridge::GetField(thisPtr, methNameStr);
	 }
       }
     }
     /* With the result in hand, return back to Haskell. */
     switch (resultType) {
     case FFI_TYPE_UNIT:   PrimReturn(nameUnit);
     case FFI_TYPE_CHAR:   PrimReturn(mkChar(System::Convert::ToByte(res)));
     case FFI_TYPE_INT:    PrimReturn(mkInt(System::Convert::ToInt32(res)));
     case FFI_TYPE_INT8:   PrimReturn(mkInt((signed char)res));
     case FFI_TYPE_INT16:  PrimReturn(mkInt((System::Int16)res));
     case FFI_TYPE_INT32:  PrimReturn(mkInt((System::Int32)res));
     case FFI_TYPE_WORD8:  PrimReturn(mkInt((unsigned char)res));
     case FFI_TYPE_WORD16: PrimReturn(mkInt((System::UInt16)res));
     case FFI_TYPE_WORD32: PrimReturn(mkInt((System::UInt32)res));
     case FFI_TYPE_INT64: 
       { __int64 r = (__int64)res;
       
       PrimReturn(pair(I64CELL,pair(part1Int64(r),part2Int64(r))));
       }
     case FFI_TYPE_WORD64: 
       { __int64 r = (__int64)res;
       
       PrimReturn(pair(I64CELL,pair(part1Int64(r),part2Int64(r))));
       }
     case FFI_TYPE_FLOAT:   PrimReturn(mkFloat((float)System::Convert::ToDouble(res)));
     case FFI_TYPE_DOUBLE:  PrimReturn(mkDouble(System::Convert::ToDouble(res)));
     case FFI_TYPE_BOOL:    PrimReturn((System::Convert::ToBoolean(res)) ? nameTrue : nameFalse);
     case FFI_TYPE_ADDR:    PrimReturn(mkPtr((::Pointer)(int)res));
     case FFI_TYPE_PTR:     PrimReturn(mkPtr((::Pointer)(int)res));
     case FFI_TYPE_FUNPTR:  PrimReturn(mkPtr((::Pointer)(int)res));
     case FFI_TYPE_FOREIGN: PrimReturn(mkMallocPtr((::Pointer)(int)res,NULL));
     case FFI_TYPE_STABLE:  PrimReturn(derefStablePtr((int)res));
     case FFI_TYPE_OBJECT:  PrimReturn(mkDotNetPtr(res,freeNetPtr));
     case FFI_TYPE_STRING:  
	 if (res == 0) {
	   push(nameNil);
	 } else {
	   //	   char* r = ToCharString(res->ToString());
	   //	   pushString(r);
	   wchar_t wRes __gc[] = res->ToString()->ToCharArray();
	   pushWideString(wRes, res->ToString()->Length);
	 }
	 PrimReturn(pop());
     default:
      IOFail(mkIOError(NIL,
		       nameNetException,
		       textToStr(methName),
		       "unknown result type",
		       NIL));
       
     }
  } catch (Exception* e) {
      IOFail(mkIOError(NIL,
		       nameNetException,
		       textToStr(methName),
		       ToCharString(e->ToString()),
		       NIL));
  }
  PrimReturn(nameUnit);
}

extPrimFun(primCreateObject) { /* String -> DotNetPtr -> IO DotNetPtr */
  char* s = evalName(IOArg(2));

  eval(IOArg(1));
  try {
      Object __pin* res = 
	  DynInvoke::InvokeBridge::CreateObject(0,
						new System::String(s),
						static_cast<System::Array*>(derefNP(whnfHead)));
      IOReturn(mkDotNetPtr(res,freeNetPtr));
  } catch (Exception* e) {
      IOFail(mkIOError(NIL,
		       nameNetException,
		       "Dotnet.createObject",
		       ToCharString(e->ToString()),
		       NIL));
  }
}

extPrimFun(primInvokeMethod) { /* DotNetPtr -> String -> DotNetPtr -> IO DotNetPtr */
  Object* arg1;
  System::Array* arg3;
  Object* res;
  
  eval(IOArg(1));
  arg3 = static_cast<System::Array*>(derefNP(whnfHead));

  char* s = evalName(IOArg(2));

  eval(IOArg(3));
  arg1 = derefNP(whnfHead);

  try {
      res = DynInvoke::InvokeBridge::InvokeMethod(arg1, new System::String(s), arg3);
  } catch (Exception* e) {
      IOFail(mkIOError(NIL,
		       nameNetException,
		       s,
		       ToCharString(e->GetType()->ToString()),
		       NIL));
  }
  IOReturn(mkDotNetPtr(res,freeNetPtr));
}

extPrimFun(primInvokeStaticMethod) { /* String -> DotNetPtr -> IO DotNetPtr */
  System::Array* arg2;
  Object* res;
  
  eval(IOArg(1));
  arg2 = static_cast<System::Array*>(derefNP(whnfHead));

  char* s = evalName(IOArg(2));

  try {
      res = DynInvoke::InvokeBridge::InvokeStaticMethod(0, new System::String(s), arg2);
  } catch (Exception* e) {
      IOFail(mkIOError(NIL,
		       nameNetException,
		       s,
		       ToCharString(e->GetType()->ToString()),
		       NIL));
  }
  IOReturn(mkDotNetPtr(res,freeNetPtr));
}

extPrimFun(primGetField) { /* DotNetPtr -> String -> IO DotNetPtr */
  System::Object* arg1;
  Object* res;
  
  char* s = evalName(IOArg(1));

  eval(IOArg(2));
  arg1 = derefNP(whnfHead);

  try {
      res = DynInvoke::InvokeBridge::GetField(arg1, new System::String(s));
  } catch (Exception* e) {
      IOFail(mkIOError(NIL,
		       nameNetException,
		       s,
		       ToCharString(e->GetType()->ToString()),
		       NIL));
  }
  IOReturn(mkDotNetPtr(res,freeNetPtr));
}

extPrimFun(primGetStaticField) { /* String -> String -> IO DotNetPtr */
  System::Object* arg1;
  Object* res;
  
  char* fName = evalName(IOArg(1));
  char* cName = evalName(IOArg(2));

  try {
    res = DynInvoke::InvokeBridge::GetStaticField(new System::String(cName),new System::String(fName));
  } catch (Exception* e) {
      IOFail(mkIOError(NIL,
		       nameNetException,
		       fName,
		       ToCharString(e->GetType()->ToString()),
		       NIL));
  }
  IOReturn(mkDotNetPtr(res,freeNetPtr));
}

extPrimFun(primSetField) { /* DotNetPtr -> String -> DotNetPtr -> IO () */
  System::Object* arg1;
  System::Object* arg3;
  
  eval(IOArg(1));
  arg3 = derefNP(whnfHead);

  char* s = evalName(IOArg(2));

  eval(IOArg(3));
  arg1 = derefNP(whnfHead);

  try{
      DynInvoke::InvokeBridge::SetField(arg1, new System::String(s),arg3);
  } catch (Exception* e) {
      IOFail(mkIOError(NIL,
		       nameNetException,
		       s,
		       ToCharString(e->GetType()->ToString()),
		       NIL));
  }
  IOReturn(nameUnit);
}

extPrimFun(primSetStaticField) { /* String -> String -> DotNetPtr -> IO () */
  System::Object* arg3;
  
  eval(IOArg(1));
  arg3 = derefNP(whnfHead);

  char* fName   = evalName(IOArg(2));
  char* clsName = evalName(IOArg(3));

  try{
      DynInvoke::InvokeBridge::SetField(new System::String(clsName),new System::String(fName),arg3);
  } catch (Exception* e) {
      IOFail(mkIOError(NIL,
		       nameNetException,
		       fName,
		       ToCharString(e->GetType()->ToString()),
		       NIL));
  }
  IOReturn(nameUnit);
}

extPrimFun(primNewString) { /* String -> IO DotNetPtr */
  Object* res;
  char* s = evalName(IOArg(1));

  try {
      res = new System::String(s);
  } catch (Exception* e) {
      IOFail(mkIOError(NIL,
		       nameNetException,
		       "Dotnet.newString",
		       ToCharString(e->GetType()->ToString()),
		       NIL));
  }
  IOReturn(mkDotNetPtr(res,freeNetPtr));
}

extPrimFun(primToHsString) { /* DotNetPtr -> IO String */
  Object* arg1;
  int count;
  
  eval(IOArg(1));
  arg1 = derefNP(whnfHead);
  
  if (arg1) {
  
    wchar_t wRes __gc[] = arg1->ToString()->ToCharArray();
    pushWideString(wRes, arg1->ToString()->Length);
  } else {
    push(nameNil);
  }
  IOReturn(pop());
}

extPrimFun(primNewArgArray) { /* Int -> IO DotNetPtr */
  Object* res;
  int sz;

  IntArg(sz,1+IOArity);

  try {
      res = DynInvoke::InvokeBridge::NewArgArray(sz);
  } catch (Exception* e) {
      IOFail(mkIOError(NIL,
		       nameNetException,
		       "Dotnet.newArgArray",
		       ToCharString(e->GetType()->ToString()),
		       NIL));
  }
  IOReturn(mkDotNetPtr(res,freeNetPtr));
}

extPrimFun(primSetArg) { /* DotNetPtr -> DotNetPtr -> Int -> IO () */
  System::Array* arg1;
  Object* arg2;
  int idx;

  IntArg(idx,1+IOArity);

  eval(IOArg(2));
  arg2 = derefNP(whnfHead);

  eval(IOArg(3));
  arg1 = static_cast<System::Array*>(derefNP(whnfHead));


  try {
      DynInvoke::InvokeBridge::SetArg(arg1,arg2,idx);
  } catch (Exception* e) {
      IOFail(mkIOError(NIL,
		       nameNetException,
		       "Dotnet.setArgArray",
		       ToCharString(e->GetType()->ToString()),
		       NIL));
  }
 
  IOReturn(nameUnit);
}

extPrimFun(primGetArg) { /* DotNetPtr -> Int -> IO DotNetPtr */
  System::Array* arg1;
  Object* res;
  int idx;

  IntArg(idx,1+IOArity);

  eval(IOArg(2));
  arg1 = static_cast<System::Array*>(derefNP(whnfHead));

  try {
      res = DynInvoke::InvokeBridge::GetArg(arg1,idx);
  } catch (Exception* e) {
      IOFail(mkIOError(NIL,
		       nameNetException,
		       "Dotnet.getArg",
		       ToCharString(e->GetType()->ToString()),
		       NIL));
  }
  IOReturn(mkDotNetPtr(res,freeNetPtr));
}

extPrimFun(primIsNullPtr) { /* DotNetPtr -> IO Bool */
  Object* arg1;

  eval(IOArg(1));
  arg1 = static_cast<System::Object*>(derefNP(whnfHead));

  IOReturn((arg1 == 0) ? nameTrue:nameFalse);
}

extPrimFun(primMkPrimVector) { /* Int -> Int -> IO DotNetPtr */
  System::Int32 ty;
  System::Int32 sz;

  IntArg(ty,2+IOArity);
  IntArg(sz,1+IOArity);
  
  switch (ty) {
  case 0: // Byte
    IOReturn(mkDotNetPtr(new Byte[sz],freeNetPtr));
  case 1: // Boolean
    IOReturn(mkDotNetPtr(new Boolean[sz],freeNetPtr));
  case 2: // Char
    IOReturn(mkDotNetPtr(new System::Char[sz],freeNetPtr));
  case 3: // Double
    IOReturn(mkDotNetPtr(new System::Double[sz],freeNetPtr));
  case 4: // Int16
    IOReturn(mkDotNetPtr(new System::Int16[sz],freeNetPtr));
  case 5: // Int32
    IOReturn(mkDotNetPtr(new System::Int32[sz],freeNetPtr));
  case 6: // Int64
    IOReturn(mkDotNetPtr(new System::Int64[sz],freeNetPtr));
  case 7: // SByte
    IOReturn(mkDotNetPtr(new System::SByte[sz],freeNetPtr));
  case 8: // Single
    IOReturn(mkDotNetPtr(new System::Single[sz],freeNetPtr));
  case 9: // UInt16
    IOReturn(mkDotNetPtr(new System::UInt16[sz],freeNetPtr));
  case 10: // UInt32
    IOReturn(mkDotNetPtr(new System::UInt32[sz],freeNetPtr));
  case 11: // UInt64
    IOReturn(mkDotNetPtr(new System::UInt64[sz],freeNetPtr));
  default:
    IOFail(mkIOError(NIL,
		     nameIllegal,
		     "Dotnet.mkPrimVector",
		     "",
		     NIL));
  }
}

};

//
// (c) 2002-2003, sof.
//
// Hugs Server API exposed as a .NET type (via Managed C++).
//
#pragma once
#using <mscorlib.dll>
extern "C" {
#include "prelude.h"
#include "server.h"
extern struct _HugsServerAPI* getHugsAPI();
};

using namespace System;

namespace Hugs {

typedef int HVal;

public __gc class Server {
 private:
  static struct _HugsServerAPI* m_server;
public:
  static Server() {
    m_server = getHugsAPI();
  }
  
  static System::String* ClearError();
  static void   SetHugsArgs (System::String* args[]);
  static int    GetNumScripts();
  static void   Reset(int i);
  static void   SetOutputEnable (int i);
  static void   ChangeDir(System::String* dir);
  static void   LoadProject(System::String* proj);
  static void   LoadFile(System::String* fname);
  static void   LoadFromBuffer(System::String* haskMod);
  static void   SetOptions(System::String* opts);
  static System::String* GetOptions();
  
  static HVal   CompileExpr(System::String* a1, System::String* a2);
  static void   GarbageCollect();
  static void   LookupName(System::String* mod,System::String* nm);

  static void   mkInt(int i);
  static void   mkAddr(void* ptr);
  static void   mkObject(Object* obj);
  static void   mkString(System::String* s);

  static void   apply();
  
  static int     evalInt();
  static void*   evalAddr();
  static Object* evalObject();
  static System::String* evalString();
  
  static int     doIO();
  static int     doIO_Int(int* pRes);
  static int     doIO_Addr(void** pRes);
  static int     doIO_Object(Object* __gc& pRes);
  
  static HVal    popHVal();
  static void    pushHVal(HVal arg);
  static void    freeHVal(HVal arg);
};
};

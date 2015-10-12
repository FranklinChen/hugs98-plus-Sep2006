//
// Helper class for dynamically creating types/classes
// wrapping up Hugs functions/actions.
//
// (c) 2002-2003, sof.
//
#pragma once
#using <mscorlib.dll>
extern "C" {
#include "prelude.h"
#include "storage.h"
#include "machdep.h"
#include "connect.h"
};
#include "dotnet/HugsServ.h"
#include "dotnet/Invoker.h"

using namespace System;
using namespace System::Reflection;
using namespace System::Reflection::Emit;
using namespace Hugs;

namespace Hugs {

public __gc class Wrapper {
private:
  //  Class:   FunctionInfo
  // 
  //  Packages up the information in a 'method string' that's
  //  passed from Haskell.
  //
  __gc class FunctionInfo {
  public:
    System::String* methodName;
    System::String* moduleName;
    System::String* functionName;
    System::String* argTys;
    System::String* resTy;
    
    FunctionInfo(System::String* str) {
      // The format of the string is as follows:
      //
      //    MethodName#[Module.]function|<type>+|<type>
      //
      // where <type> is a type tag:
      //     I - int
      //     S - string
      //     O - object pointer.
      //
      int idx;
      
      idx = str->IndexOf('#');
      methodName = str->Substring(0,Math::Max(0,idx));
      
      str = str->Substring(idx+1);
      idx = str->IndexOf('.');
      moduleName = str->Substring(0,Math::Max(0,idx));
    
      str = str->Substring(idx+1);
    
      idx = str->IndexOf('|');
      functionName = str->Substring(0,Math::Max(0,idx));
    
      str = str->Substring(idx+1);
      
      idx    = str->IndexOf('|');
      argTys = str->Substring(0,Math::Max(0,idx));
    
      resTy  = str->Substring(idx+1);
    }
  
    static System::Type* tagToType(char ch) {
      switch (ch) {
      case 'I': return System::Type::GetType("System.Int32");
      case 'S': return System::Type::GetType("System.System::String");
      case 'O': return System::Type::GetType("System.Object");
      default:  return System::Type::GetType("System.Void");
      }
    }
  
    static System::Type* tagsToType(System::String* s) [] {
      System::Type* tyVec[] = new System::Type*[s->Length];
      
      for ( int i=0; i<s->Length; i++ ) {
	tyVec[i] = tagToType(s->get_Chars(i));
      }
      return tyVec;
    }
  }; /* class FunctionInfo */

public:
  static Wrapper() {
   AssemblyName* assemblyName = new AssemblyName();
   assemblyName->Name = "HugsAssembly";
   
   AppDomain* currentDomain = AppDomain::CurrentDomain;

   currentDomain->AssemblyResolve += new ResolveEventHandler(0, Resolver);

   assemblyBuilder = 
     currentDomain->DefineDynamicAssembly(assemblyName, 
					 AssemblyBuilderAccess::Run,
					  //AssemblyBuilderAccess::RunAndSave,
					 (System::String*)0);
   moduleBuilder = 
         assemblyBuilder->DefineDynamicModule("HugsModule");
   //         assemblyBuilder->DefineDynamicModule("HugsModule", "test.dll");

   hugs = new Server();
   uniq = 0;
  }
private:
  static Hugs::Server* hugs;
  static AssemblyBuilder* assemblyBuilder;
  static ModuleBuilder*   moduleBuilder;
  static int uniq;

public:
  //
  // Method: InvokeFunction()
  //
  // Given a 'method string' and an object array holding the arguments,
  // construct a (Hugs) function application and then perform it.
  //
  static Object* InvokeFunction (System::String* str, Object* args[]) {
    int i = 0;
    FunctionInfo* fi = new FunctionInfo(str);
    
    Server::LookupName(fi->moduleName,fi->functionName);
    while (i < fi->argTys->Length) {
      switch (fi->argTys->get_Chars(i)) {
      case 'I':
       Server::mkInt(Convert::ToInt32(args[i]));
       break;
      case 'H':
	Server::pushHVal(Convert::ToInt32(args[i]));
	break;
     case 'S':     
       Server::mkString(__try_cast<System::String*>(args[i]));
       break;
     case 'O':
       Server::mkObject(args[i]);
       break;
     default:
       Console::WriteLine("bogus type tag {0};ignoring.", __box(fi->argTys->get_Chars(i)));
       break;
     }
     i++;
     if (i % 2 == 1) {
       Server::apply();
     }
   }
   if (i > 0 && i%2 == 0) {
     // make sure the last argument is hooked up to an @ node too.
     Server::apply();
   }

   if (fi->resTy->Length > 0) {
     switch (fi->resTy->get_Chars(0)) {
     case 'I':
       {
	 int res;
	 Server::doIO_Int(&res);
	 return __box(res);
       }
     case 'S':
       {
	 Object* res = new Object();
	 try {
	   Server::doIO_Object(res);
	 } catch (Exception* e) {
	   Console::WriteLine("{0} {1}", e, e->InnerException);
	   throw(e);
	 }
	 return __try_cast<System::String*>(res);
       }
     case 'O':
       {
	 Object* res = new Object();
	 Server::doIO_Object(res);
	 return res;
       }
     default:
       Server::doIO();
       return 0;
     }
   }
   return 0;
  }

  //
  // Method: InvokeStablePtr()
  //
  // Like InvokeFunction, but instead of via a 'method string', the Haskell
  // function value to call is given as a stable ptr.
  // 
  static Object __gc* InvokeStablePtr(int stablePtr,
				      System::String* argTys,
				      System::String* resTy,
				      Object __gc* args[]) {
    int i = 0;

    Server::pushHVal(stablePtr);
    while (i < argTys->Length) {
      switch (argTys->get_Chars(i)) {
      case 'I':
	Server::mkInt(Convert::ToInt32(args[i]));
	break;
      case 'H':
	Server::pushHVal(Convert::ToInt32(args[i]));
	break;
      case 'S':     
	Server::mkString(__try_cast<System::String*>(args[i]));
	break;
      case 'O':
	Server::mkObject(args[i]);
       break;
      default:
       Console::WriteLine("bogus type tag {0};ignoring.", argTys);
       break;
      }
      i++;
      if (i % 2 == 1) {
	Server::apply();
      }
    }
    if (i > 0) {
      // make sure the last argument is hooked up to an @ node too.
      Server::apply();
    }

    if (resTy->Length > 0) {
      switch (resTy->get_Chars(0)) {
      case 'I':
	{
	 int res;
	 Server::doIO_Int(&res);
	 return __box(res);
	}
     case 'S':
       {
	 Object* res = new Object();
	 Server::doIO_Object(res);
	 return __try_cast<System::String*>(res);
       }
     case 'O':
       {
	 Object* res = new Object();
	 Server::doIO_Object(res);
	 return res;
       }
     default:
       Server::doIO();
       return 0;
      }
    }
    return 0;
  }

  static Assembly* Resolver(Object* sender, ResolveEventArgs* args) {
    return assemblyBuilder;
  }

  //
  // Method:  DefineType
  //
  // Given a type name and a 'method string', construct a new
  // type in a dynamic module in a dynamic assembly. The new type's
  // function in life is to wrap up Hugs functions, so that when
  // its methods are invoked, they will delegate the call to their
  // corresponding Hugs function. 
  //
 static System::String* DefineType(System::String* typeName,
				   System::String* super,
				   System::String* methodSpecs) {
   int count   = 0;
   int idx     = 0;
   int i       = 0;
   int len     = methodSpecs->Length;
   MethodBuilder* methodBuilder;

   // Ideally, we want to pass the method strings in an array, but
   // I'm running into issues passing arrays via the Hugs .NET primops
   // (specifically, how to make Convert.ChangeType() deal with the
   // arrays.)
   //
   // So, temporarily, we separate the method strings by '/'.
   // 
   while (idx >= 0 ) {
     idx = methodSpecs->IndexOf('/');
     count++;
     methodSpecs = methodSpecs->Substring(idx+1);
   }

   System::String* methods[] = new System::String*[count];
   idx = 0; count = 0;
   while (idx >= 0 ) {
     idx = methodSpecs->IndexOf('/');
     if (idx == (-1)) {
       methods[count] = methodSpecs->Substring(0);
     } else {
       methods[count] = methodSpecs->Substring(0,Math::Max(0,idx));
     }
     count++;
   }
   int no = methods->Length;
   System::String* theTypeName;
   TypeBuilder* typeBuilder;
   
   theTypeName = typeName;
   while (true) {
     try {
       if (super != 0 && super->Length > 0) {
	 System::Type* supTy = DynInvoke::InvokeBridge::GetType(super);
	 typeBuilder = moduleBuilder->DefineType(theTypeName, TypeAttributes::Public, supTy);
#if 0
	 Console::WriteLine("Succeeded creating {0} type", supTy);
#endif
       } else {
	 typeBuilder = moduleBuilder->DefineType(theTypeName, TypeAttributes::Public);
       }
       break;
     } catch (ArgumentException*) {
       uniq++;
       theTypeName = String::Format("{0}{1}",typeName, __box(uniq));
     }
   }
#if 0
   Console::WriteLine("Succeeded creating {0} type", theTypeName);
#endif

   ConstructorBuilder* constructorBuilder = 
     typeBuilder->DefineConstructor(MethodAttributes::Public,
				   CallingConventions::Standard,
				   0);
   ILGenerator*  ilGenerator = constructorBuilder->GetILGenerator();
   FieldBuilder* fieldBuilder;

   // Call the base constructor -- required?
   ilGenerator->Emit(OpCodes::Ldarg_0);
   System::Type* ctor_args[] = new System::Type*[0];
   ilGenerator->Emit(OpCodes::Call,
		     System::Type::GetType("System.Object")->GetConstructor(ctor_args));

   for (i=0;i < no; i++) {
     fieldBuilder = 
       typeBuilder->DefineField("spec_" + i, 
				__typeof(System::String), 
				FieldAttributes::Private);
     // assign the field.
     ilGenerator->Emit(OpCodes::Ldarg_0);
     ilGenerator->Emit(OpCodes::Ldstr,methods[i]);
     ilGenerator->Emit(OpCodes::Stfld, fieldBuilder);
     
     FunctionInfo* fi = new FunctionInfo(methods[i]);
   
     methodBuilder =
       typeBuilder->DefineMethod(fi->methodName,
				 (MethodAttributes)(
				 MethodAttributes::Public | MethodAttributes::Virtual),
				CallingConventions::Standard,
				(fi->resTy->Length == 0 ? 
				 System::Type::GetType("System.Void") : 
				 FunctionInfo::tagToType(fi->resTy->get_Chars(0))),
				 FunctionInfo::tagsToType(fi->argTys));
				
     ILGenerator* ilGen = methodBuilder->GetILGenerator();
     
     ilGen->DeclareLocal(__typeof(Object*[]));
     ilGen->Emit(OpCodes::Ldc_I4,fi->argTys->Length);
     ilGen->Emit(OpCodes::Newarr,__typeof(Object));
     ilGen->Emit(OpCodes::Stloc_0);
     
     for (int j=0; j < fi->argTys->Length; j++) {
       ilGen->Emit(OpCodes::Ldloc_0);
       ilGen->Emit(OpCodes::Ldc_I4,j);
       switch (j+1) {
       case 1 : ilGen->Emit(OpCodes::Ldarg_1); break;
       case 2 : ilGen->Emit(OpCodes::Ldarg_2); break;
       case 3 : ilGen->Emit(OpCodes::Ldarg_3); break;
       default: ilGen->Emit(OpCodes::Ldarg,j+1); break;
       }
       if (fi->argTys->get_Chars(j) == 'I') {
	 ilGen->Emit(OpCodes::Box,Type::GetType("System.Int32"));
       }
       ilGen->Emit(OpCodes::Stelem_Ref);
     }
     ilGen->Emit(OpCodes::Ldarg_0);
     ilGen->Emit(OpCodes::Ldfld,fieldBuilder);
     ilGen->Emit(OpCodes::Ldloc_0);
     ilGen->Emit(OpCodes::Call,
		 System::Type::GetType("Hugs.Wrapper")->GetMethod("InvokeFunction"));

     // pop the result off of the stack when returning void.
     if (fi->resTy->Length > 0 && fi->resTy->get_Chars(0) == 'V') {
       ilGen->Emit(OpCodes::Pop);
     }
     ilGen->Emit(OpCodes::Ret);
     
     // of 'delegator' shape?
     if ((fi->resTy->Length  == 1 && fi->resTy->get_Chars(0) == 'V') &&
	 (fi->argTys->Length == 2 && fi->argTys->get_Chars(1) == 'O' && fi->argTys->get_Chars(1) == 'O')) {
       System::Type* del_args[] = new System::Type*[2];
       del_args[0] = __typeof(System::Object);
       del_args[1] = System::Type::GetType("System.EventArgs");
       MethodBuilder* delegator = 
	 typeBuilder->DefineMethod(String::Concat(fi->methodName, "_delegator"),
				  MethodAttributes::Public,
				  CallingConventions::Standard,
				  __typeof(void),
				  del_args);
       // Simple stuff - just delegate the call (do we really need this meth?)
       ILGenerator* delIl = delegator->GetILGenerator();
       delIl->Emit(OpCodes::Ldarg_0);
       delIl->Emit(OpCodes::Ldarg_1);
       delIl->Emit(OpCodes::Ldarg_2);
       delIl->Emit(OpCodes::Call,methodBuilder);
       delIl->Emit(OpCodes::Ret);
       
       // System::Type compatible with a delegator, create impedance matchers for free.
       fieldBuilder =
	 typeBuilder->DefineField(String::Concat(fi->methodName, "_handler"),
				 System::Type::GetType("System.EventHandler"),
				 FieldAttributes::Public);

       ilGenerator->Emit(OpCodes::Ldarg_0);
       ilGenerator->Emit(OpCodes::Ldarg_0);
       ilGenerator->Emit(OpCodes::Ldftn,delegator);
       System::Type* eh_args[] = new System::Type*[2];
       eh_args[0] = __typeof(System::Object);
       eh_args[1] = System::Type::GetType("System.IntPtr");
       ConstructorInfo* ci = System::Type::GetType("System.EventHandler")->GetConstructor(eh_args);
       ilGenerator->Emit(OpCodes::Newobj,ci);
       ilGenerator->Emit(OpCodes::Stfld,fieldBuilder);
     }
   }
   ilGenerator->Emit(OpCodes::Ret);

   System::Type* res = typeBuilder->CreateType();
   
#if 0
   Console::WriteLine("Succeeded creating {0} type..", res);
#endif
   // For debugging purposes, persist the generated assembly.
   // (this goes hand-in-hand with the dynamic, persistable module
   // we created above).
   //   assemblyBuilder->Save("foo.dll");
   return theTypeName;
 }
 
 static System::String* DefineDelegator(System::String* methodName, int stablePtr) {
   System::String* theTypeName;
   TypeBuilder* typeBuilder;
   
   theTypeName = "DynDelegator";
   while (true) {
     try {
#if 0
       Console::WriteLine("Attempting to create type {0}..",theTypeName);
#endif
       typeBuilder = moduleBuilder->DefineType(theTypeName, TypeAttributes::Public);
       break;
     } catch (ArgumentException*) {
       uniq++;
       theTypeName = String::Format("DynDelegator{0}",__box(uniq));
     }
   }
#if 0
   Console::WriteLine("Succeeded creating {0} type", theTypeName);
#endif
   ConstructorBuilder* constructorBuilder = 
     typeBuilder->DefineConstructor(MethodAttributes::Public,
				   CallingConventions::Standard,
				   0);
   ILGenerator* ilGenerator = constructorBuilder->GetILGenerator();
   
   // Call the base constructor -- required?
   ilGenerator->Emit(OpCodes::Ldarg_0);
   System::Type* ctor_args[] = new System::Type*[0];
   ilGenerator->Emit(OpCodes::Call,
		     System::Type::GetType("System.Object")->GetConstructor(ctor_args));

   // Build the delegator method which calls back into Haskell.
   System::Type* del_args[] = new System::Type*[2];
   del_args[0] = __typeof(System::Object);
   del_args[1] = System::Type::GetType("System.EventArgs");
   MethodBuilder* delegator =
     typeBuilder->DefineMethod(methodName,
			       MethodAttributes::Public,
			       CallingConventions::Standard,
			       System::Type::GetType("System.Void"),
			       del_args);

   ILGenerator* delIl = delegator->GetILGenerator();

   // construct the object array expected by InvokeFunction().
   delIl->DeclareLocal(__typeof(Object*[]));
   delIl->Emit(OpCodes::Ldc_I4,2);
   delIl->Emit(OpCodes::Newarr,__typeof(System::Object));
   delIl->Emit(OpCodes::Stloc_0);
   
   // args[0] = arg1;
   delIl->Emit(OpCodes::Ldloc_0);
   delIl->Emit(OpCodes::Ldc_I4,0);
   delIl->Emit(OpCodes::Ldarg_1);
   delIl->Emit(OpCodes::Stelem_Ref);

   // args[0] = arg2;
   delIl->Emit(OpCodes::Ldloc_0);
   delIl->Emit(OpCodes::Ldc_I4,1);
   delIl->Emit(OpCodes::Ldarg_2);
   delIl->Emit(OpCodes::Stelem_Ref);

   // call InvokeFunction, passing it the method name 
   //   delIl.Emit(OpCodes.Ldarg_0);
   delIl->Emit(OpCodes::Ldc_I4,stablePtr);
   delIl->Emit(OpCodes::Ldstr,"OO");
   delIl->Emit(OpCodes::Ldstr,"V");
   delIl->Emit(OpCodes::Ldloc_0);
   delIl->Emit(OpCodes::Call, System::Type::GetType("Hugs.Wrapper")->GetMethod("InvokeStablePtr"));

   delIl->Emit(OpCodes::Pop);
   delIl->Emit(OpCodes::Ret);
       
   // System::Type compatible with a delegator, create impedance matchers for free.
   FieldBuilder* fieldBuilder =
     typeBuilder->DefineField(String::Concat(methodName,"_handler"),
			     System::Type::GetType("System.EventHandler"),
			      FieldAttributes::Public);

   ilGenerator->Emit(OpCodes::Ldarg_0);
   ilGenerator->Emit(OpCodes::Ldarg_0);
   ilGenerator->Emit(OpCodes::Ldftn,delegator);
   System::Type* eh_args[] = new System::Type*[2];
   eh_args[0] = __typeof(Object);
   eh_args[1] = System::Type::GetType("System.IntPtr");
   ConstructorInfo* ci = System::Type::GetType("System.EventHandler")->GetConstructor(eh_args);
   ilGenerator->Emit(OpCodes::Newobj,ci);
   ilGenerator->Emit(OpCodes::Stfld,fieldBuilder);
   
   ilGenerator->Emit(OpCodes::Ret);

   System::Type* res = typeBuilder->CreateType();
   //   assemblyBuilder.Save("foo.dll");   
   return theTypeName;
 }
}; /* class Wrapper */

}; /* namespace Hugs */

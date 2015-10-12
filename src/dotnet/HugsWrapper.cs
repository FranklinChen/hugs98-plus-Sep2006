//
// Helper class for dynamically creating types/classes
// wrapping up Hugs functions/actions.
//
//  (c) 2002, sof.
// 
using System;
using System.Reflection;
using System.Reflection.Emit;
using Hugs;

//
// Class:  HugsWrapper
// 
public class HugsWrapper {

 //  Class:   FunctionInfo
 // 
 //  Packages up the information in a 'method string' that's
 //  passed from Haskell.
 //
 private class FunctionInfo {
   public String methodName;
   public String moduleName;
   public String functionName;
   public String argTys;
   public String resTy;
   
   internal FunctionInfo(String str) {
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
     
     idx = str.IndexOf('#');
     methodName = str.Substring(0,Math.Max(0,idx));
   
     str = str.Substring(idx+1);
   
     idx = str.IndexOf('.');
     moduleName = str.Substring(0,Math.Max(0,idx));
   
     str = str.Substring(idx+1);
   
     idx = str.IndexOf('|');
     functionName = str.Substring(0,Math.Max(0,idx));
   
     str = str.Substring(idx+1);

     idx    = str.IndexOf('|');
     argTys = str.Substring(0,Math.Max(0,idx));
   
     resTy  = str.Substring(idx+1);
   }
   
   public static Type tagToType(char ch) {
     switch (ch) {
     case 'I': return Type.GetType("System.Int32");
     case 'S': return Type.GetType("System.String");
     case 'O': return Type.GetType("System.Object");
     default:  return Type.GetType("System.Void");
     }
   }
   
   public static Type[] tagsToType(String s) {
     Type[] tyVec = new Type[s.Length];
     
     for ( int i=0; i<s.Length; i++ ) {
       tyVec[i] = tagToType(s[i]);
     }
     return tyVec;
   }
   
 } 

 static HugsWrapper() {
   AssemblyName assemblyName = new AssemblyName();
   assemblyName.Name = "HugsAssembly";
   
   AppDomain currentDomain = AppDomain.CurrentDomain;

   currentDomain.AssemblyResolve += new ResolveEventHandler(Resolver);

   assemblyBuilder = 
     currentDomain.DefineDynamicAssembly(assemblyName, 
					 AssemblyBuilderAccess.Run,//AssemblyBuilderAccess.RunAndSave,
					 (String)null);
   moduleBuilder = 
         assemblyBuilder.DefineDynamicModule("HugsModule");
   //     assemblyBuilder.DefineDynamicModule("HugsModule", "test.dll");

   hugs = new Server();
   uniq = 0;
 }

 static private Server hugs;
 static private AssemblyBuilder assemblyBuilder;
 static private ModuleBuilder   moduleBuilder;
 static private Int32 uniq;
 
 //
 // Method: InvokeFunction()
 //
 // Given a 'method string' and an object array holding the arguments,
 // construct a (Hugs) function application and then perform it.
 // 
 public static Object InvokeFunction(String str,Object[] args) {
   Int32 i = 0;
   FunctionInfo fi = new FunctionInfo(str);

   Server.LookupName(fi.moduleName,fi.functionName);
   Console.WriteLine("lookupName({0},{1},{2});", fi.moduleName,fi.functionName,fi.argTys);
   while (i < fi.argTys.Length) {
     switch (fi.argTys[i]) {
     case 'I':
       Server.mkInt((int)args[i]);
       break;
     case 'H':
       Server.pushHVal((int)args[i]);
       break;
     case 'S':     
       Server.mkString((String)args[i]);
       break;
     case 'O':
       Server.mkObject(args[i]);
       break;
     default:
       Console.WriteLine("bogus type tag {0};ignoring.", fi.argTys[i]);
       break;
     }
     i++;
     if (i % 2 == 1) {
	 Console.WriteLine("apply");
       Server.apply();
     }
   }
   if (i > 0 && i%2 == 0) {
     // make sure the last argument is hooked up to an @ node too.
     Server.apply();
     Console.WriteLine("apply");
   }

   if (fi.resTy.Length > 0) {
     switch (fi.resTy[0]) {
     case 'I':
       unsafe {
	 int res;
	 Server.doIO_Int(&res);
	 return (Object)res;
       }
     case 'S':
       unsafe {
	 Object res = new Object();
	 Server.doIO_Object(ref res);
	 return (String)res;
       }
     case 'O':
       unsafe {
	 Object res = new Object();
	 Server.doIO_Object(ref res);
	 return res;
       }
     default:
       Server.doIO();
       return null;
     }
   }
   return null;
 }

 //
 // Method: InvokeStablePtr()
 //
 // Like InvokeFunction, but instead of via a 'method string', the Haskell
 // function value to call is given as a stable ptr.
 // 
 public static Object InvokeStablePtr(Int32 stablePtr,
				      String argTys,
				      String resTy,
				      Object[] args) {
   Int32 i = 0;

   Server.pushHVal(stablePtr);
   while (i < argTys.Length) {
     switch (argTys[i]) {
     case 'I':
       Server.mkInt((int)args[i]);
       break;
     case 'H':
       Server.pushHVal((int)args[i]);
       break;
     case 'S':     
       Server.mkString((String)args[i]);
       break;
     case 'O':
       Server.mkObject(args[i]);
       break;
     default:
       Console.WriteLine("bogus type tag {0};ignoring.", argTys[i]);
       break;
     }
     i++;
     if (i % 2 == 1) {
       Server.apply();
     }
   }
   if (i > 0) {
     // make sure the last argument is hooked up to an @ node too.
     Server.apply();
   }

   if (resTy.Length > 0) {
     switch (resTy[0]) {
     case 'I':
       unsafe {
	 int res;
	 Server.doIO_Int(&res);
	 return (Object)res;
       }
     case 'S':
       unsafe {
	 Object res = new Object();
	 Server.doIO_Object(ref res);
	 return (String)res;
       }
     case 'O':
       unsafe {
	 Object res = new Object();
	 Server.doIO_Object(ref res);
	 return res;
       }
     default:
       Server.doIO();
       return null;
     }
   }
   return null;
 }

 static Assembly Resolver(Object sender, ResolveEventArgs args) {
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
 static public String DefineType(String typeName, String super, String methodSpecs) {
   Int32 count   = 0;
   Int32 idx     = 0;
   Int32 i       = 0;
   Int32 len     = methodSpecs.Length;
   MethodBuilder   methodBuilder;

   // Ideally, we want to pass the method strings in an array, but
   // I'm running into issues passing arrays via the Hugs .NET primops
   // (specifically, how to make Convert.ChangeType() deal with the
   // arrays.)
   //
   // So, temporarily, we separate the method strings by '/'.
   // 
   while (idx >= 0 ) {
     idx = methodSpecs.IndexOf('/');
     count++;
     methodSpecs = methodSpecs.Substring(idx+1);
   }

   String[] methods = new String[count];
   idx = 0; count = 0;
   while (idx >= 0 ) {
     idx = methodSpecs.IndexOf('/');
     if (idx == (-1)) {
       methods[count] = methodSpecs.Substring(0);
     } else {
       methods[count] = methodSpecs.Substring(0,Math.Max(0,idx));
     }
     count++;
   }
   Int32 no = methods.Length;
   String theTypeName;
   TypeBuilder typeBuilder = null;
   
   theTypeName = typeName;
   while (true) {
     try {
       if (super != null && super.Length > 0) {
	 Type supTy = DynInvoke.InvokeBridge.GetType(super);
	 typeBuilder = moduleBuilder.DefineType(theTypeName, TypeAttributes.Public, supTy);
	 Console.WriteLine("Succeeded creating {0} type", supTy);
       } else {
	 typeBuilder = moduleBuilder.DefineType(theTypeName, TypeAttributes.Public);
       }
       break;
     } catch (ArgumentException) {
       uniq++;
       theTypeName = typeName + uniq;
     }
   }
   Console.WriteLine("Succeeded creating {0} type", theTypeName);

   ConstructorBuilder constructorBuilder = 
     typeBuilder.DefineConstructor(MethodAttributes.Public,
				   CallingConventions.Standard,
				   null);
   ILGenerator ilGenerator = constructorBuilder.GetILGenerator();
   FieldBuilder fieldBuilder;

   // Call the base constructor -- required?
   ilGenerator.Emit(OpCodes.Ldarg_0);
   Type[] ctor_args = new Type[0];
   ilGenerator.Emit(OpCodes.Call,Type.GetType("System.Object").GetConstructor(ctor_args));

   for (i=0;i < no; i++) {
     fieldBuilder = 
       typeBuilder.DefineField("spec_" + i, 
			       typeof(String), 
			       FieldAttributes.Private);
     // assign the field.
     ilGenerator.Emit(OpCodes.Ldarg_0);
     ilGenerator.Emit(OpCodes.Ldstr,methods[i]);
     ilGenerator.Emit(OpCodes.Stfld, fieldBuilder);
     
     FunctionInfo fi = new FunctionInfo(methods[i]);
   
     methodBuilder =
       typeBuilder.DefineMethod(fi.methodName,
				MethodAttributes.Public | MethodAttributes.Virtual,
				CallingConventions.Standard,
				(fi.resTy.Length == 0 ? Type.GetType("System.Void") : FunctionInfo.tagToType(fi.resTy[0])),
				FunctionInfo.tagsToType(fi.argTys));
				
     ILGenerator ilGen = methodBuilder.GetILGenerator();
     
     ilGen.DeclareLocal(typeof(Object[]));
     ilGen.Emit(OpCodes.Ldc_I4,fi.argTys.Length);
     ilGen.Emit(OpCodes.Newarr,typeof(Object));
     ilGen.Emit(OpCodes.Stloc_0);
     
     for (Int32 j=0; j < fi.argTys.Length; j++) {
       ilGen.Emit(OpCodes.Ldloc_0);
       ilGen.Emit(OpCodes.Ldc_I4,j);
       switch (j+1) {
       case 1 : ilGen.Emit(OpCodes.Ldarg_1); break;
       case 2 : ilGen.Emit(OpCodes.Ldarg_2); break;
       case 3 : ilGen.Emit(OpCodes.Ldarg_3); break;
       default: ilGen.Emit(OpCodes.Ldarg,j+1); break;
       }
       if (fi.argTys[j] == 'I') {
	 ilGen.Emit(OpCodes.Box,Type.GetType("System.Int32"));
       }
       ilGen.Emit(OpCodes.Stelem_Ref);
     }
     ilGen.Emit(OpCodes.Ldarg_0);
     ilGen.Emit(OpCodes.Ldfld,fieldBuilder);
     ilGen.Emit(OpCodes.Ldloc_0);
     ilGen.Emit(OpCodes.Call, Type.GetType("HugsWrapper").GetMethod("InvokeFunction"));

     // pop the result off of the stack when returning void.
     if (fi.resTy.Length > 0 && fi.resTy[0] == 'V') {
       ilGen.Emit(OpCodes.Pop);
     }
     ilGen.Emit(OpCodes.Ret);
     
     // of 'delegator' shape?
     if ((fi.resTy.Length  == 1 && fi.resTy[0] == 'V') &&
	 (fi.argTys.Length == 2 && fi.argTys[1] == 'O' && fi.argTys[1] == 'O')) {
       Type[] del_args = new Type[2];
       del_args[0] = typeof(Object);
       del_args[1] = Type.GetType("System.EventArgs");
       MethodBuilder delegator = 
	 typeBuilder.DefineMethod(fi.methodName + "_delegator",
				  MethodAttributes.Public,
				  CallingConventions.Standard,
				  typeof(void),
				  del_args);
       // Simple stuff - just delegate the call (do we really need this meth?)
       ILGenerator delIl = delegator.GetILGenerator();
       delIl.Emit(OpCodes.Ldarg_0);
       delIl.Emit(OpCodes.Ldarg_1);
       delIl.Emit(OpCodes.Ldarg_2);
       delIl.Emit(OpCodes.Call,methodBuilder);
       delIl.Emit(OpCodes.Ret);
       
       // Type compatible with a delegator, create impedance matchers for free.
       fieldBuilder =
	 typeBuilder.DefineField(fi.methodName + "_handler",
				 Type.GetType("System.EventHandler"),
				 FieldAttributes.Public);

       ilGenerator.Emit(OpCodes.Ldarg_0);
       ilGenerator.Emit(OpCodes.Ldarg_0);
       ilGenerator.Emit(OpCodes.Ldftn,delegator);
       Type[] eh_args = new Type[2];
       eh_args[0] = typeof(Object);
       eh_args[1] = Type.GetType("System.IntPtr");
       ConstructorInfo ci = Type.GetType("System.EventHandler").GetConstructor(eh_args);
       ilGenerator.Emit(OpCodes.Newobj,ci);
       ilGenerator.Emit(OpCodes.Stfld,fieldBuilder);
     }
   }
   ilGenerator.Emit(OpCodes.Ret);

   Type res = typeBuilder.CreateType();
   
   Console.WriteLine("Succeeded creating {0} type..", res);

   // For debugging purposes, persist the generated assembly.
   // (this goes hand-in-hand with the dynamic, persistable module
   // we created above).
   //   assemblyBuilder.Save("foo.dll");
   return theTypeName;
 }
 
 static public String DefineDelegator(String methodName, Int32 stablePtr) {
   String theTypeName;
   TypeBuilder typeBuilder = null;
   
   theTypeName = "DynDelegator";
   while (true) {
     try {
       Console.WriteLine("Attempting to create type {0}..",theTypeName);
       typeBuilder = moduleBuilder.DefineType(theTypeName, TypeAttributes.Public);
       break;
     } catch (ArgumentException) {
       uniq++;
       theTypeName = "DynDelegator" + uniq;
     }
   }
   Console.WriteLine("Succeeded creating {0} type", theTypeName);
   ConstructorBuilder constructorBuilder = 
     typeBuilder.DefineConstructor(MethodAttributes.Public,
				   CallingConventions.Standard,
				   null);
   ILGenerator ilGenerator = constructorBuilder.GetILGenerator();
   
   // Call the base constructor -- required?
   ilGenerator.Emit(OpCodes.Ldarg_0);
   Type[] ctor_args = new Type[0];
   ilGenerator.Emit(OpCodes.Call,Type.GetType("System.Object").GetConstructor(ctor_args));

   // Build the delegator method which calls back into Haskell.
   Type[] del_args = new Type[2];
   del_args[0] = typeof(Object);
   del_args[1] = Type.GetType("System.EventArgs");
   MethodBuilder delegator =
     typeBuilder.DefineMethod(methodName,
			      MethodAttributes.Public,
			      CallingConventions.Standard,
			      Type.GetType("System.Void"),
			      del_args);

   ILGenerator delIl = delegator.GetILGenerator();

   // construct the object array expected by InvokeFunction().
   delIl.DeclareLocal(typeof(Object[]));
   delIl.Emit(OpCodes.Ldc_I4,2);
   delIl.Emit(OpCodes.Newarr,typeof(Object));
   delIl.Emit(OpCodes.Stloc_0);
   
   // args[0] = arg1;
   delIl.Emit(OpCodes.Ldloc_0);
   delIl.Emit(OpCodes.Ldc_I4,0);
   delIl.Emit(OpCodes.Ldarg_1);
   delIl.Emit(OpCodes.Stelem_Ref);

   // args[0] = arg2;
   delIl.Emit(OpCodes.Ldloc_0);
   delIl.Emit(OpCodes.Ldc_I4,1);
   delIl.Emit(OpCodes.Ldarg_2);
   delIl.Emit(OpCodes.Stelem_Ref);

   // call InvokeFunction, passing it the method name 
   //   delIl.Emit(OpCodes.Ldarg_0);
   delIl.Emit(OpCodes.Ldc_I4,stablePtr);
   delIl.Emit(OpCodes.Ldstr,"OO");
   delIl.Emit(OpCodes.Ldstr,"V");
   delIl.Emit(OpCodes.Ldloc_0);
   delIl.Emit(OpCodes.Call, Type.GetType("HugsWrapper").GetMethod("InvokeStablePtr"));

 
   delIl.Emit(OpCodes.Pop);
   delIl.Emit(OpCodes.Ret);
       
   // Type compatible with a delegator, create impedance matchers for free.
   FieldBuilder fieldBuilder =
     typeBuilder.DefineField(methodName + "_handler",
			     Type.GetType("System.EventHandler"),
			     FieldAttributes.Public);

   ilGenerator.Emit(OpCodes.Ldarg_0);
   ilGenerator.Emit(OpCodes.Ldarg_0);
   ilGenerator.Emit(OpCodes.Ldftn,delegator);
   Type[] eh_args = new Type[2];
   eh_args[0] = typeof(Object);
   eh_args[1] = Type.GetType("System.IntPtr");
   ConstructorInfo ci = Type.GetType("System.EventHandler").GetConstructor(eh_args);
   ilGenerator.Emit(OpCodes.Newobj,ci);
   ilGenerator.Emit(OpCodes.Stfld,fieldBuilder);
   
   ilGenerator.Emit(OpCodes.Ret);

   Type res = typeBuilder.CreateType();
//   assemblyBuilder.Save("foo.dll");   
   return theTypeName;
 }

#if WANT_MAIN
 public static void Main() {
   AppDomain currentDomain = AppDomain.CurrentDomain;
   currentDomain.AssemblyResolve += new ResolveEventHandler(Resolver);

   try {
     DefineType("HugsTest", "Clicked#DotNet.hello|OO|O");
     currentDomain.CreateInstance("HugsAssembly", "HugsTest");
   } catch (Exception e) {
     Console.WriteLine(e.Message);
   }
   InvokeFunction("Clicked#DotNet.hello|OO|O",null);

//   assemblyBuilder.Save("foo.dll");
 } 
#endif

} /* class HugsWrapper */

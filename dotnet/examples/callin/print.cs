/*
 * Test harness for Hugs.NET - compile with
 *
 *    csc /t:library print.cs /r:hugs.exe
 */
using System;
using Hugs;

public class Print {
 public static void p(Server o) { 
   Console.WriteLine("In C#: Being passed a {0}", o);
   /* Looks odd; the entire Server interface is static */
   Server.LookupName("CallIn", "greeting");
   Server.doIO();
   Console.WriteLine("Finished in C#-land;returning.");
 }
}

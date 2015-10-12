//
// (c) sof, 2002-2003
//
using System;

namespace HsWrapGen
{
	/// <summary>
	/// Toplevel Main wrapper for HsWrapGen tool.
	/// </summary>
	class App
	{
		/// <summary>
		/// Throw-away tool for generating Haskell .NET class wrappers.
		/// </summary>
		[STAThread]
		static void Main(string[] args)
		{
            if (args.Length > 0) {
                TypeInfo ti = new TypeInfo(args[0]);
		if (ti.Type == null) {
		  Console.WriteLine("Unknown type: {0}", args[0]);
		} else {
		  HsOutput hs = new HsOutput(ti.Type,ti.Members);
		  String outFile;
		  if (args.Length > 1) {
                    outFile = args[1];
		  } else {
                    Int32 idx = args[0].LastIndexOf('.');
                    if (idx >= 0) {
                        outFile = String.Concat(args[0].Substring(idx+1), ".hs");
                    } else {
		      outFile = String.Concat(args[0], ".hs");
                    }
		  }
		  Console.WriteLine(outFile);
		  hs.OutputToFile(outFile);
		}
            } else {
                Console.WriteLine("Usage: hswrapgen classname [outfile]");
            }

		}
	}
}

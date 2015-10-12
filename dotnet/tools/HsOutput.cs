//
// (c) sof, 2002-2003
//
using System;

namespace HsWrapGen
{
	/// <summary>
	/// 
	/// </summary>
	public class HsOutput
	{
        private System.Type m_type;
	private System.Reflection.MemberInfo[] m_members;
        private System.Collections.Specialized.StringCollection m_names;
        private System.Collections.Specialized.StringCollection m_imports;
	private System.String m_modname;

	public HsOutput(System.Type ty,System.Reflection.MemberInfo[] mems) {
	  m_type = ty;
	  m_members = mems;
	  m_names   = new System.Collections.Specialized.StringCollection();
	  m_imports = new System.Collections.Specialized.StringCollection();
	  m_modname = "Dotnet." + m_type.FullName;
	}
	

        protected void OutputHeader(System.IO.StreamWriter st) {
	  String supTy    = (m_type.IsInterface ? "System.Object" : m_type.BaseType.FullName);
	  String supTyCls = (m_type.IsInterface ? "Object" : m_type.BaseType.Name);
	  st.WriteLine("module Dotnet.{0} where", m_type.FullName);
	  st.WriteLine("");
	  st.WriteLine("import Dotnet");
	  AddImport("Dotnet."+supTy);
	  foreach (String s in m_imports) {
	    st.WriteLine("import qualified {0}", s);
	  }
	  st.WriteLine("");
	  // ToDo: provide the option of stashing this away in a separate
	  //       module.
	  st.WriteLine("data {0}_ a", m_type.Name);
	  st.WriteLine("type {0} a = Dotnet.{1}.{2} ({0}_ a)",
		       m_type.Name,
		       supTy,
		       supTyCls);
	  st.WriteLine("");
        }

        private String ToHaskellName(String x) {
            System.String candName, candNameOrig;
            System.Int32 uniq = 1;
            if (System.Char.IsUpper(x[0])) {
                candName = 
                    String.Concat(System.Char.ToLower(x[0]),
                    x.Substring(1));
            } else {
                candName = x;
            }
            candNameOrig = candName;
            while (m_names.Contains(candName)) {
                candName = String.Concat(candNameOrig,"_",uniq.ToString());
                uniq++;
            }
            m_names.Add(candName);

            return candName;
        }

        private String ToHaskellConName(String x) {
            System.String candName, candNameOrig;
            System.Int32 uniq = 1;
            if (System.Char.IsLower(x[0])) {
                candName = 
                    String.Concat(System.Char.ToUpper(x[0]),
                    x.Substring(1));
            } else {
                candName = x;
            }
            candNameOrig = candName;
            while (m_names.Contains(candName)) {
                candName = String.Concat(candNameOrig,"_",uniq.ToString());
                uniq++;
            }
            m_names.Add(candName);

            return candName;
        }

        private void AddImport(System.String nm) {

            if (!m_imports.Contains(nm) && String.Compare(nm, m_modname) != 0) {
                m_imports.Add(nm);
            }
        }

        protected void OutputHaskellType(System.Text.StringBuilder sb,
                                         System.Type ty,
                                         System.Int32 idx) {
	  /* Curiously, &-versions of prim types are showing up (cf. System.Uri.HexUnescape).
	   * Just ignore them.
	   */
            if (ty.FullName == "System.Boolean" || ty.FullName == "System.Boolean&" ) {
              sb.Append("Bool"); return;
            }
            if (ty.FullName == "System.String") {
                sb.Append("String"); return;
            }
            if (ty.FullName == "System.Char" || ty.FullName == "System.Char&") {
              sb.Append("Char"); return;
            }
            if (ty.FullName == "System.Double" || ty.FullName == "System.Double&") {
              sb.Append("Double"); return;
            }
            if (ty.FullName == "System.Single" || ty.FullName == "System.Single&") {
              sb.Append("Double"); return;
            }
            if (ty.FullName == "System.SByte" || ty.FullName == "System.SByte&") {
	      AddImport("Data.Int");
              sb.Append("Data.Int.Int8"); return;
            }
            if (ty.FullName == "System.Int16" || ty.FullName == "System.Int16&") {
	      AddImport("Data.Int");
              sb.Append("Data.Int.Int16"); return;
            }
            if (ty.FullName == "System.Int32" || ty.FullName == "System.Int32&") {
              sb.Append("Int"); return;
            }
            if (ty.FullName == "System.Int64" || ty.FullName == "System.Int64&") {
	      AddImport("Data.Int");
              sb.Append("Data.Int.Int64"); return;
            }
            if (ty.FullName == "System.Byte" || ty.FullName == "System.Byte&") {
	      AddImport("Data.Word");
              sb.Append("Data.Word.Word8"); return;
            }
            if (ty.FullName == "System.UInt16" || ty.FullName == "System.UInt16&") {
	      AddImport("Data.Word");
              sb.Append("Data.Word.Word16"); return;
            }
            if (ty.FullName == "System.UInt32" || ty.FullName == "System.UInt32&") {
	      AddImport("Data.Word");
              sb.Append("Data.Word.Word32"); return;
            }
            if (ty.FullName == "System.UInt64" || ty.FullName == "System.UInt64&") {
	      AddImport("Data.Word");
              sb.Append("Data.Word.Word64"); return;
            }
            if (ty.FullName == "System.Void") {
              sb.Append("()"); return;
            }
            if (ty.FullName == "System.Object") {
	        AddImport("Dotnet.System.Object");
                sb.AppendFormat("Dotnet.System.Object.Object a{0}",idx); return;
            }

            if (ty.IsArray) {
                AddImport("Dotnet.System.Array");
                sb.Append("Dotnet.System.Array.Array (");
		OutputHaskellType(sb, ty.GetElementType(), idx);
		sb.Append(")");
            } else {
                AddImport("Dotnet." + ty.FullName);
                sb.AppendFormat("Dotnet.{0}.{1} a{2}", ty.FullName, ty.Name, idx);
            }       
        }

        protected void OutputMethodSig(System.Text.StringBuilder sb,
                                       System.Reflection.MemberInfo mi) {
            System.Reflection.MethodInfo m = (System.Reflection.MethodInfo)mi;
            System.Reflection.ParameterInfo[] ps = m.GetParameters();
            int i;

            for (i=0; i < ps.Length; i++) {
                OutputHaskellType(sb,ps[i].ParameterType,i);
                sb.Append(" -> ");
            }
	    if (m.IsStatic) {
	      sb.Append("IO (");
	    } else {
	      sb.AppendFormat("{0} obj -> IO (", mi.DeclaringType.Name);
	    }
            OutputHaskellType(sb,m.ReturnType,i);
            sb.AppendFormat("){0}",System.Environment.NewLine);
        }

        protected void OutputCtorSig(System.Text.StringBuilder sb,
				     System.Reflection.ConstructorInfo ci) {
            System.Reflection.ParameterInfo[] ps = ci.GetParameters();
            int i;

            for (i=0; i < ps.Length; i++) {
                OutputHaskellType(sb,ps[i].ParameterType,i);
                sb.Append(" -> ");
            }
	    sb.AppendFormat("IO ({0} ())", ci.DeclaringType.Name);
            sb.Append(System.Environment.NewLine);
        }

        protected void OutputFieldSig(System.Text.StringBuilder sb,
				      System.Reflection.FieldInfo fi,
				      bool isSetter) {

	    /* Note: indexed values are provided via properties */
	    if (isSetter) {
	      OutputHaskellType(sb,fi.FieldType,0);
	      if (!fi.IsStatic) {
		sb.AppendFormat(" -> {0} obj", fi.DeclaringType.Name);
	      }
	      sb.AppendFormat(" -> IO (){0}",System.Environment.NewLine);
	    } else {
	      if (fi.IsStatic) {
		sb.Append("IO (");
	      } else {
		sb.AppendFormat("{0} obj -> IO (", fi.DeclaringType.Name);
	      }
	      OutputHaskellType(sb,fi.FieldType,0);
	      sb.AppendFormat("){0}",System.Environment.NewLine);
	    }
	}

        protected void OutputArgs(System.Text.StringBuilder sb,
                                  System.Reflection.MemberInfo mi,
                                  System.Boolean isTupled) {
            System.Reflection.MethodInfo m = (System.Reflection.MethodInfo)mi;
            Int32 i = 0;
            System.Reflection.ParameterInfo[] ps = m.GetParameters();

            if (isTupled && ps.Length != 1) sb.Append("(");

            for (i=0; i < ps.Length; i++) {
                sb.AppendFormat("arg{0}",i); 
                if (isTupled && (i+1) < ps.Length) {
                    sb.Append(",");
                } else {
                    if (!isTupled) sb.Append(" ");
                }
            }
            if (isTupled && ps.Length != 1) sb.Append(")");
        }

        protected void OutputMember(System.Text.StringBuilder sb,
				    System.Reflection.MemberInfo mi) {
            switch (mi.MemberType) {
	    case System.Reflection.MemberTypes.Method:
	      System.String methName = ToHaskellName(mi.Name);
	      System.Reflection.MethodInfo m = (System.Reflection.MethodInfo)mi;
	      sb.Append("foreign import dotnet"); sb.Append(System.Environment.NewLine);
	      // the 'method' bit is really optional.
	      sb.AppendFormat("  \"{0}method {1}.{2}\"", (m.IsStatic ? "static " : ""), mi.DeclaringType, mi.Name);
	      sb.Append(System.Environment.NewLine);
	      sb.AppendFormat("  {0} :: ", methName);
	      OutputMethodSig(sb,mi);
	      // the mind boggles, System.Environment ?
	      sb.Append(System.Environment.NewLine);
	      /* old habit ;) */
	      break;
	    case System.Reflection.MemberTypes.Constructor:
	      OutputCtor(sb,(System.Reflection.ConstructorInfo)mi);
	      break;

	    case System.Reflection.MemberTypes.Field:
	      System.String fieldName = mi.Name;
	      System.Reflection.FieldInfo f = (System.Reflection.FieldInfo)mi;
	      System.String staticPrefix = (f.IsStatic ? "static " : "");
	      sb.Append("foreign import dotnet");
	      sb.Append(System.Environment.NewLine);
	      sb.AppendFormat("  \"{0}field {1}.{2}\"", staticPrefix, mi.DeclaringType, mi.Name);
	      sb.Append(System.Environment.NewLine);
	      sb.AppendFormat("  get_{0} :: ", fieldName);
	      OutputFieldSig(sb,f,false);
	      sb.Append(System.Environment.NewLine);
	      if (!f.IsInitOnly) {
		sb.Append("foreign import dotnet");
		sb.Append(System.Environment.NewLine);
		sb.AppendFormat("  \"{0}field {1}.{2}\"", staticPrefix, mi.DeclaringType, mi.Name);
		sb.Append(System.Environment.NewLine);
		sb.AppendFormat("  set_{0} :: ", fieldName);
		OutputFieldSig(sb,f,true);
		sb.Append(System.Environment.NewLine);
	      }
	      break;
	    default:
	      break;
            }
        }
	
	protected void OutputCtor(System.Text.StringBuilder sb,
				  System.Reflection.ConstructorInfo ci) {
	  System.String ctorName = ToHaskellName("new"+ci.DeclaringType.Name);
	  sb.Append("foreign import dotnet");
	  sb.Append(System.Environment.NewLine);
	  sb.AppendFormat("  \"ctor {0}\"", ci.DeclaringType);
	  sb.AppendFormat("{0}",System.Environment.NewLine);
	  sb.AppendFormat("  {0} :: ", ctorName);
	  OutputCtorSig(sb,ci);
	  sb.Append(System.Environment.NewLine);
	}
        
        protected void OutputField(System.Text.StringBuilder sb,
				   System.Reflection.MemberInfo mi) {
            switch (mi.MemberType) {
                case System.Reflection.MemberTypes.Field:
                    System.String fieldName = ToHaskellConName(mi.Name);
		    sb.Append(fieldName);
                    break;
                default:
                    break;
            }
        }

        public void OutputToFile(String fn) {
            System.IO.FileStream fs = new System.IO.FileStream(fn,System.IO.FileMode.Create);
            System.IO.StreamWriter st = new System.IO.StreamWriter(fs,System.Text.Encoding.ASCII);
            System.Text.StringBuilder sb = new System.Text.StringBuilder();

	    if (!m_type.IsInterface && m_type.BaseType.FullName == "System.Enum") {
	      /* enumerations are mapped onto Haskell data types. */
	      System.String sep = " = ";
	      sb.AppendFormat("data {0}Ty", m_type.Name);
	      sb.Append(System.Environment.NewLine);
	      foreach (System.Reflection.MemberInfo mem in m_members) {
		if (mem.Name != "value__") {
		  sb.Append(sep);
		  OutputField(sb,mem);
		  sb.Append(System.Environment.NewLine);
		  sep = " | ";
		}
	      }
	      sb.AppendFormat("  deriving ( Enum, Show, Read ){0}",System.Environment.NewLine);
	      // Emit functions for converting betw alg type and object type.
	      AddImport("IOExts");
	      AddImport("Dotnet.System.Type");
	      AddImport("Dotnet.System.Enum");
	      sb.AppendFormat("to{0} :: {0}Ty -> {0} (){1}", m_type.Name, System.Environment.NewLine);
	      sb.AppendFormat("to{0} tag = IOExts.unsafePerformIO (Dotnet.System.Enum.parse (IOExts.unsafePerformIO (Dotnet.System.Type.getType \"{1}\")) (show tag)){2}", m_type.Name, m_type.AssemblyQualifiedName,System.Environment.NewLine);
	      sb.Append(System.Environment.NewLine);
	      sb.AppendFormat("from{0} :: {0} () -> {0}Ty{1}", m_type.Name, System.Environment.NewLine);
	      sb.AppendFormat("from{0} obj = IOExts.unsafePerformIO (toString obj >>= return.read)", m_type.Name);
	      sb.Append(System.Environment.NewLine);
	    } else {
	      foreach (System.Reflection.MemberInfo mem in m_members) {
                OutputMember(sb,mem);
	      }
	      foreach (System.Reflection.ConstructorInfo ci in m_type.GetConstructors()) {
                OutputCtor(sb,ci);
	      }
	      
	    }
            OutputHeader(st);
            st.WriteLine(sb.ToString());
            st.Flush();
            st.Close();
            fs.Close();
	    }
	}
}

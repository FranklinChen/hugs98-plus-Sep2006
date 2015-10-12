//
// (c) sof, 2002-2003
//
using System;
using System.Reflection;

namespace HsWrapGen
{
	/// <summary>
	/// Given a type name, locate the metainfo needed to generate
	/// Haskell wrappers.
	/// </summary>
    public class TypeInfo {
        protected Type m_type;
        protected System.Reflection.MemberInfo[] m_members;
	
	protected static System.Collections.ArrayList m_assemblies;
	
	static TypeInfo() {
	  Assembly      corAss = Assembly.Load("mscorlib.dll");
	  System.String corDir = System.IO.Path.GetDirectoryName(corAss.Location);
	  
	  m_assemblies = new System.Collections.ArrayList();
	  
	  System.String[] fs = System.IO.Directory.GetFiles(corDir, "*.dll");
	  for (int i=0; i < fs.Length; i++) {
	    try {
	      Assembly tA = Assembly.LoadFrom(fs[i]);
	      m_assemblies.Add(tA.FullName);
	    } catch (Exception) {
	      continue;
	    }
	      
	  }
	}
	
	public static Type GetType(System.String tyName) {

	  try {
	    Type t = Type.GetType(tyName);
	    if (t != null) return t;
	  } catch (Exception) {
	    ;
	  }
	  for (int i=0; i < m_assemblies.Count; i++) {
	    try {
	      String s = String.Format("{0},{1}", tyName, 
				       m_assemblies[i].ToString());
	      //	      Console.WriteLine(s);
	      Type t = Type.GetType(s);
	      if (t != null) return t;
	    } catch (Exception) {
	      continue;
	    }
	  }
	  return null;
	}

        public System.Type Type {
            get { return (m_type); }
        }

        public System.Reflection.MemberInfo[] Members {
            get { return (m_members); }
        }   

		private bool myFilter(System.Reflection.MemberInfo m,
							  System.Object filterCrit)
		{
			return
			   (m.MemberType == System.Reflection.MemberTypes.Method ||
				m.MemberType == System.Reflection.MemberTypes.Property ||
				m.MemberType == System.Reflection.MemberTypes.Field);
		}

		public TypeInfo(System.String tyName)
		{
			m_type = TypeInfo.GetType(tyName);
			
			if (m_type != null) {
			  if (m_type.IsInterface) {
			    m_members = m_type.GetMethods();
			  } else {
			    m_members = 
			      m_type.FindMembers(
						 System.Reflection.MemberTypes.All,
						 System.Reflection.BindingFlags.DeclaredOnly |
						 System.Reflection.BindingFlags.Instance |
						 System.Reflection.BindingFlags.Public |
						 System.Reflection.BindingFlags.Static,
						 new System.Reflection.MemberFilter(myFilter),
						 null);
			  }
			}
		}
	}
}

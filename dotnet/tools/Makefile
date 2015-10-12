#
# Makefile for hswrapgen, basic tool for generating
# Haskell .NET declarations from a .NET type.
#

CSC=csc
CSC_OPTS=
SRCS=App.cs AssemblyInfo.cs HsOutput.cs TypeInfo.cs

all : hswrapgen.exe

hswrapgen.exe : $(SRCS)
	$(CSC) $(CSC_OPTS) /out:hswrapgen.exe $(SRCS)

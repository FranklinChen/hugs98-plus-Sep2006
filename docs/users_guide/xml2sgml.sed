/<?xml/d
/^<!ENTITY %/,/>/d
/^%/d
/^<!ENTITY/ s/\.xml/.sgml/
/^<!DOCTYPE/,/\[/ {
	s/ XML / /
	s/xml/sgml/
	s/docbookx/docbook/
}
s:/>:>:g

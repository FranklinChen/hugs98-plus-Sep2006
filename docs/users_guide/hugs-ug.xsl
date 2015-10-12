<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">

<xsl:param name="section.autolabel" select="'1'"></xsl:param>
<xsl:param name="section.label.includes.component.label" select="'1'"></xsl:param>
<xsl:param name="xref.with.number.and.title" select="'0'"></xsl:param>
<xsl:param name="generate.toc">
book	toc,title
chapter	title
qandaset toc
</xsl:param>

<xsl:param name="base.dir" select="'users_guide/'"></xsl:param>
<xsl:param name="use.id.as.filename" select="'1'"></xsl:param>
<xsl:param name="html.ext" select="'.html'"></xsl:param>
<xsl:param name="html.stylesheet" select="'hugs-ug.css'"></xsl:param>

<xsl:include href="http://docbook.sourceforge.net/release/xsl/current/html/chunk.xsl"/>

</xsl:stylesheet>

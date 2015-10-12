<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">

<xsl:param name="chapter.autolabel" select="'0'"></xsl:param>
<xsl:param name="generate.toc">
book    toc,title
chapter title
qandaset toc
</xsl:param>

<xsl:param name="base.dir" select="'users_guide-htmlhelp/'"></xsl:param>
<xsl:param name="use.id.as.filename" select="'1'"></xsl:param>
<xsl:param name="manifest.in.base.dir" select="'1'"></xsl:param>

<xsl:param name="htmlhelp.chm" select="'..\users_guide.chm'"></xsl:param>

<xsl:include href="http://docbook.sourceforge.net/release/xsl/current/htmlhelp/htmlhelp.xsl"/>

</xsl:stylesheet>

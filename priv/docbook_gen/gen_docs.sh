#!/bin/bash
cd ./docbook
xsltproc http://docbook.sourceforge.net/release/xsl/current/html/docbook.xsl --output book.html book.xml
xsltproc http://docbook.sourceforge.net/release/xsl/current/html/docbook.xsl --output gui.html gui.xml
# Super Shonky! from GG the shonkmaster....
sed 's/<\/head>/<\/head><link rel="stylesheet" type="text\/css" href="..\/css\/book.css" \/>/' book.html > ../html/book.html
sed 's/<\/head>/<\/head><link rel="stylesheet" type="text\/css" href="..\/css\/book.css" \/>/' gui.html > ../html/gui.html
cd ..
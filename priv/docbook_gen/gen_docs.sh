#!/bin/bash
cd ./docbook
xsltproc http://docbook.sourceforge.net/release/xsl/current/html/docbook.xsl --output book.html book.xml
mv book.html ../html/
cd ..
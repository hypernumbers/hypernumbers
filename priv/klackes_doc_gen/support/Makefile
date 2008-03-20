all: HEAD

HEAD: HEAD.src ../vsn.mk
	sed -e "s/@INMVSN@/`sed s/INMVSN=// ../vsn.mk`/g" \
	    -e "s/@DATE@/`date +'%B %d, %Y'`/g" HEAD.src > HEAD

clean: 
	rm -f HEAD

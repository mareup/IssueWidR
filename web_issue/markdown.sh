#!/bin/sh

a=1

for txt in $@
do
	sed -f ../websedscr ${txt} > ${txt}$a.rmd
	mv ${txt}$a.rmd ../Rmd/

	a=`expr $a + 1`
done

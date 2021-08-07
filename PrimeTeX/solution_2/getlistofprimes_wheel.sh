#!/bin/sh
cd "$(dirname $0)"
wheeltimings="wheel_primestofile_timings.txt"
pleasewait="Please wait..."
pleasewaitalot="Please wait (a lot)..."
rm -f $wheeltimings


echo "Constructing $wheeltimings"
cat <<\EOF >>$wheeltimings
=============
N = 1,000,000
=============

$ time tex wheel_primestofile
EOF
echo $pleasewait
{ time tex wheel_primestofile.tex ; } >>$wheeltimings 2>&1

cat <<\EOF >>$wheeltimings

$ time pdftex wheel_primestofile
EOF
echo $pleasewait
{ time pdftex wheel_primestofile.tex ; } >>$wheeltimings 2>&1

cat <<\EOF >>$wheeltimings

==============
N = 10,000,000
==============

$ time tex \\def\\Range{10000000}\\input wheel_primestofile
EOF
echo $pleasewait
{ time tex \\def\\Range{10000000}\\input wheel_primestofile.tex ; } >>$wheeltimings 2>&1

cat <<\EOF >>$wheeltimings

$ time pdftex \\def\\Range{10000000}\\input wheel_primestofile
EOF
echo $pleasewait
{ time pdftex \\def\\Range{10000000}\\input wheel_primestofile.tex ; } >>$wheeltimings 2>&1

cat <<\EOF >>$wheeltimings

$ time luatex \\def\\Range{10000000}\\input wheel_primestofile
EOF
echo $pleasewait
{ time luatex \\def\\Range{10000000}\\input wheel_primestofile.tex ; } >>$wheeltimings 2>&1

cat <<\EOF >>$wheeltimings

===============
N = 100,000,000 
===============

$ time luatex \\def\\Range{100000000}\\input wheel_primestofile
EOF
echo $pleasewait
{ time luatex \\def\\Range{100000000}\\input wheel_primestofile.tex ; } >>$wheeltimings 2>&1

cat <<\EOF >>$wheeltimings

===============
N = 999,999,999
===============

$ time luatex \\def\\Range{999999999}\\input wheel_primestofile
EOF
echo $pleasewaitalot
{ time luatex \\def\\Range{999999999}\\input wheel_primestofile.tex ; } >>$wheeltimings 2>&1
echo "Done"


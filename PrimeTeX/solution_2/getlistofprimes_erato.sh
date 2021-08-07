#!/bin/sh
cd "$(dirname $0)"
eratotimings="erato_primestofile_timings.txt"
pleasewait="Please wait..."
pleasewaitalot="Please wait (a lot)..."
rm -f $eratotimings

echo "Constructing $eratotimings"
echo $pleasewait
cat <<\EOF >>$eratotimings
=============
N = 1,000,000
=============

$ time tex erato_primestofile
EOF
echo $pleasewait
{ time tex erato_primestofile.tex ; } >>$eratotimings 2>&1

cat <<\EOF >>$eratotimings

$ time pdftex erato_primestofile
EOF
echo $pleasewait
{ time pdftex erato_primestofile.tex ; } >>$eratotimings 2>&1

cat <<\EOF >>$eratotimings

==============
N = 10,000,000
==============

$ time tex \\def\\Range{10000000}\\input erato_primestofile
EOF
echo $pleasewait
{ time tex \\def\\Range{10000000}\\input erato_primestofile.tex ; } >>$eratotimings 2>&1

cat <<\EOF >>$eratotimings

$ time pdftex \\def\\Range{10000000}\\input erato_primestofile
EOF
echo $pleasewait
{ time pdftex \\def\\Range{10000000}\\input erato_primestofile.tex ; } >>$eratotimings 2>&1

cat <<\EOF >>$eratotimings

$ time luatex \\def\\Range{10000000}\\input erato_primestofile
EOF
echo $pleasewait
{ time luatex \\def\\Range{10000000}\\input erato_primestofile.tex ; } >>$eratotimings 2>&1

cat <<\EOF >>$eratotimings

===============
N = 100,000,000 
===============

$ time luatex \\def\\Range{100000000}\\input erato_primestofile
EOF
echo $pleasewait
{ time luatex \\def\\Range{100000000}\\input erato_primestofile.tex ; } >>$eratotimings 2>&1

cat <<\EOF >>$eratotimings

===============
N = 999,999,999
===============

$ time luatex \\def\\Range{999999999}\\input erato_primestofile
EOF
echo $pleasewaitalot
{ time luatex \\def\\Range{999999999}\\input erato_primestofile.tex ; } >>$eratotimings 2>&1
echo "Done"


#!/bin/sh
cd "$(dirname $0)"
algo="${1:?'missing algo name'}"
timings="${algo}_primestofile_timings.txt"

pleasewait="Please wait..."
pleasewaitalot="Please wait (a lot)..."

echo "Constructing (or overwriting) $timings"
rm -f $timings

cat <<\EOF >>$timings
=============
N = 1,000,000
=============

EOF
echo "$ time tex ${algo}_primestofile" >>$timings
echo "$pleasewait (tex 1,000,000)"
{ time tex ${algo}_primestofile.tex ; } >>$timings 2>&1

echo '' >>$timings
echo "$ time pdftex ${algo}_primestofile" >>$timings
echo "$pleasewait (pdftex 1,000,000)"
{ time pdftex ${algo}_primestofile.tex ; } >>$timings 2>&1

cat <<\EOF >>$timings

==============
N = 10,000,000
==============

EOF
echo '$ time tex "\def\Range{10000000}\input '"${algo}"'_primestofile"' >>$timings
echo "$pleasewait (tex 10,000,000)"
{ time tex \\def\\Range{10000000}\\input ${algo}_primestofile.tex ; } >>$timings 2>&1

echo '' >>$timings
echo '$ time pdftex "\def\Range{10000000}\input '"${algo}"'_primestofile"' >>$timings
echo "$pleasewait (pdftex 10,000,000)"
{ time pdftex \\def\\Range{10000000}\\input ${algo}_primestofile.tex ; } >>$timings 2>&1

cat <<\EOF >>$timings

EOF
echo '$ time luatex "\def\Range{10000000}\input '"${algo}"'_primestofile"' >>$timings
echo "$pleasewait (luatex 10,000,000)"
{ time luatex \\def\\Range{10000000}\\input ${algo}_primestofile.tex ; } >>$timings 2>&1

cat <<\EOF >>$timings

===============
N = 100,000,000 
===============

EOF
echo '$ time pdftex -cnf-line font_mem_size=51000000 -cnf-line extra_mem_top=10000000 "\def\Range{100000000}\input '"${algo}"'_primestofile"' >>$timings
echo "$pleasewait (pdftex 100,000,000) (enlarged font and main memory)"
{ time pdftex -cnf-line font_mem_size=51000000 -cnf-line extra_mem_top=10000000 \\def\\Range{100000000}\\input ${algo}_primestofile.tex ; } >>$timings 2>&1

echo '' >>$timings
echo '$ time luatex "\def\Range{100000000}\input '"${algo}"'_primestofile"' >>$timings
echo "$pleasewait (luatex 100,000,000)"
{ time luatex \\def\\Range{100000000}\\input ${algo}_primestofile.tex ; } >>$timings 2>&1

cat <<\EOF >>$timings

===============
N = 999,999,999
===============

EOF
echo '$ time luatex "\def\Range{999999999}\input '"${algo}"'_primestofile"' >>$timings
echo "$pleasewaitalot (luatex 999,999,999)"
{ time luatex \\def\\Range{999999999}\\input ${algo}_primestofile.tex ; } >>$timings 2>&1
echo "Done"



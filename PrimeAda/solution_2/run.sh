#!/bin/bash

gprclean && gprbuild -p -XMODE=debug0 && echo "# debug0" > ./results.txt && ./sieves >> ./results.txt
gprclean && gprbuild -p -XMODE=debug1 && echo "# debug1" >> ./results.txt &&  ./sieves >> ./results.txt
gprclean && gprbuild -p -XMODE=debug2 && echo "# debug2" >> ./results.txt &&  ./sieves >> ./results.txt
gprclean && gprbuild -p -XMODE=debug3 && echo "# debug3" >> ./results.txt &&  ./sieves >> ./results.txt
gprclean && gprbuild -p -XMODE=release && echo "# release" >> ./results.txt &&  ./sieves >> ./results.txt
gprclean && gprbuild -p -XMODE=optimised && echo "# optimised" >> ./results.txt &&  ./sieves >> ./results.txt

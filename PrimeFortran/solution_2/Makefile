.PHONY: all run clean

ALL=prime-bitarray prime-8bit prime-logical-array

FORTRAN=gfortran
#  -march=native just slows it down (?!)
FORTRANFLAGS=-Ofast

all: $(ALL)

%: %.f08
	$(FORTRAN) $(FORTRANFLAGS) -o $@ $<

run: all
	@for program in $(ALL); do ./$${program}; done

clean:
	rm -f $(ALL) *.mod

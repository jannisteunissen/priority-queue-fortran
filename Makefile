FC := gfortran
FFLAGS := -Wall -O2 -g
PROGS := test

.phony: all clean

all: $(PROGS)

clean:
	$(RM) $(PROGS) *.o *.mod

%: %.f90
	$(FC) -o $@ $^ $(FFLAGS)
%.o: %.f90
	$(FC) -c -o $@ $< $(FFLAGS)

$(PROGS): m_pq.o

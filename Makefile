BIN = mergesort
OBJ = mergesort_module.o random_module.o dump_module.o
MOD = mergesort_module.mod random_module.mod dump_module.mod
FC = gfortran
FCFLAGS = -Werror -g
FLFLAGS = -Werror

$(BIN): mergesort.o $(OBJ)
	$(FC) -o $@ $^ $(FLFLAGS)

mergesort.o: mergesort.f95 $(MOD)
	$(FC) -c $< $(FCFLAGS)

$(MOD): $(OBJ)

mergesort_module.o: mergesort_module.f95
	$(FC) -c $< $(FCFLAGS) -std=f95

random_module.o: random_module.f95
	$(FC) -c $< $(FCFLAGS) -std=f95

dump_module.o: dump_module.f95
	$(FC) -c $< $(FCFLAGS) -std=f95

clean:
	$(RM) $(BIN) $(OBJ) $(MOD)

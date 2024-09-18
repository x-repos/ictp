.PHONY: int, sys, ode

int:
	gfortran -o e integral.f90
sys:
	gfortran -o e system-ode.f90
ode:
	gfortran -o e ode.f90
test:
	gfortran -o e test.f90
run:
	./e
mod:
	rm *mod
dat:
	rm *dat
# Sien make bl. 110

FOR_OBJS = 

F95_OBJS = main.o

OBJS = $(F95_OBJS) $(FOR_OBJS)  

FC = gfortran
PROG = xfig2poly
BIN = 
LIB = ../
OPT = -O3 -I include

all : $(PROG)

install :
	cp $(PROG) $(LIB)

$(PROG) : $(OBJS) 
	$(FC) -o $(PROG) $(OBJS)

$(FOR_OBJS) : %.o : %.for
	$(FC) $(OPT) -c $< -o $@

$(F95_OBJS) : %.o : %.f95
	$(FC) $(OPT) -c $< -o $@

clean :
	rm *.o
	rm *.exe
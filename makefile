CC = gfortran
CFLAGS = 
LIB = 

OBJECTS = graph.o          \
	  jump.o          \
	  density.o  \
	  LineTest.o \
	  Dran.o
	  
 %.o : %.F90
	$(CC) $(CFLAGS) -c $<

Dran: $(OBJECTS)
	$(CC) $(CFLAGS) -o $@ $(OBJECTS) $(LIB)
	
	
graph.o:
jump.o:
density.o:
LineTest.o: density.o
Dran.o:LineTest.o density.o jump.o graph.o

clean: 
	rm Dran *.mod *.o
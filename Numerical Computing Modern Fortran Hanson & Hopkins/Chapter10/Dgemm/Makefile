CC = cc
F90 = sunf95
LIBS = /home/cur/trh/./CVSROOT/Libs/SunOptF90/lapack.a
CC = gcc
F90 = nagf95
LIBS = /home/cur/trh/./CVSROOT/Libs/NagOptF90/lapack.a

driver_dgemm: dgemm.o dgemm_main.o
	$(F90) -o driver_dgemm dgemm.o dgemm_main.o $(LIBS)

dgemm.o: dgemm.f
	$(F90) -c dgemm.f

dgemm_main.o: dgemm_main.c
	$(CC) -c dgemm_main.c

clean:
	rm -rf driver* *.o

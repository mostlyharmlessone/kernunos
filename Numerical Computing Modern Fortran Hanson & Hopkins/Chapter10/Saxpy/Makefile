CC = cc
F90 = sunf95
LIBS = /home/cur/trh/./CVSROOT/Libs/SunOptF90/lapack.a
CC = gcc
F90 = nagf95
LIBS = /home/cur/trh/./CVSROOT/Libs/NagOptF90/lapack.a

driver_saxpy: saxpy.o saxpy_main.o
	$(F90) -o driver_saxpy saxpy.o saxpy_main.o

saxpy.o: saxpy.f
	$(F90) -c saxpy.f

saxpy_main.o: saxpy_main.c
	$(CC) -c saxpy_main.c

clean:
	rm -rf driver* *.o

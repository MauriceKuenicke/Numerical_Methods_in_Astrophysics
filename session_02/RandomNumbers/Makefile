#
# Makefile: simply run make to compile the program RandomNumbers
#
all: RandomNumbers

RandomNumbers: 
	cd src; $(MAKE) $@; cp $@ ..

clean:
	rm -f *~
	cd src; make clean

cleaner: clean
	rm -f RandomNumbers src/RandomNumbers
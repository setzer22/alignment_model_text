
FLDIR=/home/padro/Software/install/FL-4.0-master

kbest : kbest.cc
	g++ -std=c++0x -o kbest kbest.cc -L$(FLDIR)/lib/ -I$(FLDIR)/include -lfreeling -lboost_system

clean :
	rm -f kbest


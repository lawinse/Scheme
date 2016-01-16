.PHONY: all
all: main
main: main.o Longint.o Exception.o
	g++ -std=c++11 -Ofast -flto -o main main.o Longint.o Exception.o
main.o: main.cpp 
	g++ -std=c++11 -Ofast -flto -c main.cpp -lstdc++ -lm -o main.o
Longint.o: Longint.cpp
	g++ -std=c++11 -Ofast -flto -c Longint.cpp -lstdc++ -lm -o Longint.o
Exception.o: Exception.cpp
	g++ -std=c++11 -Ofast -flto -c Exception.cpp -lstdc++ -lm -o Exception.o
.PHONY: clean
clean:
	-rm main.o main Longint.o Exception.o

libguilepoll.so: ext.c
	gcc -fPIC `pkg-config --cflags --libs guile-2.0` -shared -o $@ $^

run: libguilepoll.so
	LD_LIBRARY_PATH=`pwd` guile example.scm

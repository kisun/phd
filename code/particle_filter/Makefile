CC := gcc
CCFLAGS := -Wall
SRCDIR := src
BUILDDIR := build
INCLUDE := -I $(shell pg_config --includedir) -I include
LIBS := -lpq -lm
SOURCES := $(shell find $(SRCDIR) -type f -name *.c)
OBJECTS := $(patsubst $(SRCDIR)/%, $(BUILDDIR)/%, $(SOURCES:.c=.o))
EXEC = shape_distance stop_distance pf.so

default: $(EXEC)

shape_distance: bin/shape_distance

stop_distance: bin/stop_distance

pf.so:
	MAKEFLAGS="PKG_CPPFLAGS=-Iinclude"  R CMD SHLIB -o bin/pf.so src/truncated_normal.c src/pf.c

bin/%: build/%.o
	$(CC) $(CCFLAGS) $^ -o $@ $(LIBS)

$(BUILDDIR)/%.o: $(SRCDIR)/%.c include/*.h
	mkdir -p $(BUILDDIR)
	$(CC) $(CCFLAGS) $(INCLUDE) -c $< -o $@

.PHONY: clean

clean:
	-rm -r $(BUILDDIR)
	-rm -f bin/*
	-rm src/*.o

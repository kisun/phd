CC := gcc
SRCDIR := src
BUILDDIR := build
INCLUDE := -I $(shell pg_config --includedir) -I include
LIBS := -lpq -lm
SOURCES := $(shell find $(SRCDIR) -type f -name *.c)
OBJECTS := $(patsubst $(SRCDIR)/%, $(BUILDDIR)/%, $(SOURCES:.c=.o))
EXEC = shape_distance stop_distance

default: $(EXEC)

shape_distance: bin/shape_distance

stop_distance: bin/stop_distance

bin/%: build/%.o
	$(CC) $^ -o $@ $(LIBS)

$(BUILDDIR)/%.o: $(SRCDIR)/%.c include/*.h
	mkdir -p $(BUILDDIR)
	$(CC) $(INCLUDE) -c $< -o $@

.PHONY: clean

clean:
	-rm -r $(BUILDDIR)
	-rm -f bin/*

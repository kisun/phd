CC := gcc
SRCDIR := src
BUILDDIR := build
INCLUDE := -I $(shell pg_config --includedir)
LIBS := -lpq -lm
TARGET := bin/shape_distance
SOURCES := $(shell find $(SRCDIR) -type f -name *.c)
OBJECTS := $(patsubst $(SRCDIR)/%, $(BUILDDIR)/%, $(SOURCES:.c=.o))

default: shape_distance

shape_distance: $(OBJECTS)
	$(CC) $^ -o $(TARGET) $(LIBS)

$(BUILDDIR)/%.o: $(SRCDIR)/%.c
	mkdir -p $(BUILDDIR)
	$(CC) $(INCLUDE) -c $< -o $@

.PHONY: clean

clean:
	-rm -r $(BUILDDIR)
	-rm -f $(TARGET)

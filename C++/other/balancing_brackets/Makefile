BIN = main
CPPFLAGS = -g -Wall -Wextra -pedantic -std=c++20
LDFLAGS =

CPP = g++

SRCDIR = src
OBJDIR = obj

SRCFILES = $(wildcard $(SRCDIR)/*.cpp)
OBJFILES = $(patsubst $(SRCDIR)/%.cpp, $(OBJDIR)/%.o, $(SRCFILES))

.PHONY: all
all: $(BIN) $(OBJDIR)

.PHONY: run
run: $(BIN)
	./$(BIN)

$(OBJDIR):
	mkdir -p $@

$(BIN): $(OBJFILES)
	$(CPP) $(CPPFLAGS) $^ -o $@ $(LDFLAGS)

$(OBJDIR)/%.o: $(SRCDIR)/%.cpp $(OBJDIR)
	$(CPP) $(CPPFLAGS) -c $< -o $@

.PHONY: clean
clean:
	@rm -rf $(OBJDIR) $(BIN)

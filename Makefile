SRC=common.ml syntax.ml parser.mly lexer.mll eval.ml type.ml main.ml
COMPONENT=common.ml syntax.ml parser.mli parser.ml lexer.ml eval.ml type.ml main.ml
TARGET=miniocaml

all: $(TARGET)

$(TARGET): $(COMPONENT)
	ocamlmktop $(COMPONENT) -w -31 -o $(TARGET)

parser.mli: parser.mly
	ocamlyacc parser.mly

parser.ml: parser.mly
	ocamlyacc parser.mly

lexer.ml: lexer.mll
	ocamllex lexer.mll

backup:
	cp -f Makefile $(SRC) back

clean:
	rm -f parser.ml parser.mli lexer.ml $(TARGET) *.cmi *.cmo *.mli
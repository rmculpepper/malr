SCRIBARGS=--redirect-main https://docs.racket-lang.org/ +m

all: 
	raco scribble --htmls --dest build $(SCRIBARGS) malr.scrbl

onepage:
	raco scribble --html --dest build/onepage $(SCRIBARGS) malr.scrbl

pdf: 
	raco scribble --pdf --dest build $(SCRIBARGS) malr.scrbl

SCRIBARGS=--dest build --redirect-main http://docs.racket-lang.org/ +m

all: 
	raco scribble --htmls $(SCRIBARGS) malr.scrbl

pdf: 
	raco scribble --pdf $(SCRIBARGS) malr.scrbl

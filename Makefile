all: 
	scribble --htmls --dest build ++xref-in setup/xref load-collections-xref --redirect-main http://docs.racket-lang.org/ main.scrbl

latex: 
	scribble --latex --dest build ++xref-in setup/xref load-collections-xref main.scrbl

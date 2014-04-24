all: 
	scribble --htmls --dest build ++xref-in setup/xref load-collections-xref --redirect-main http://docs.racket-lang.org/ malr.scrbl

latex: 
	scribble --latex --dest build ++xref-in setup/xref load-collections-xref malr.scrbl

pub:
	scp -r build/malr/* ccs:tree/web/malr-draft

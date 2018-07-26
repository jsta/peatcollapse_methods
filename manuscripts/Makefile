all: stachelek_peatcollapse.tex
	pdflatex $<
	bibtex stachelek_peatcollapse
	pdflatex $<

Rmd: stachelek_peatcollapse.Rmd
	Rscript -e 'rmarkdown::render("$<")'
	
clean: 
	-rm skeleton.tex skeleton.pdf
	-rm -rf skeleton_cache figures
	

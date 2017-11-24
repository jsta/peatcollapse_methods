construct-images:
	cd images && make construct-images

construct-figures:
	cd data && make construct-figures

all: construct-images construct-figures
	echo "make complete"

clean:
	rm *.log
	rm *.spl
	rm *.synctex.gz

reviewer_comments.pdf: reviewer_comments.md
	pandoc -s reviewer_comments.md -o reviewer_comments.pdf

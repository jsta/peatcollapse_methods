construct-figures:
	Rscript -e 'source("../scripts/chem-timeseries.R")'
	Rscript -e 'source("../scripts/chamber-leakage.R")'
	Rscript -e 'source("../scripts/pilot-dilution-profiles.R")'

montage-figures:
	convert ../figures/chem-ts_bw-sal.png -trim ../figures/chem-ts_bw-sal.png
	convert ../figures/chem-ts_fw-sal.png -trim ../figures/chem-ts_fw-sal.png
	montage ../figures/chem-ts_fw-sal.png ../figures/chem-ts_bw-sal.png -geometry +2+2 -tile x2 ../figures/chem-ts.png
	convert ../figures/site-diagrams.png -resample 300 ../figures/site-diagrams_small.png
	convert ../figures/site-diagrams_small.png -resize 50% ../figures/site-diagrams_small.png




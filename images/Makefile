construct-images:
	montage boardwalk_plus_supports.jpg chamber_upclose.jpg field_mesocosm.jpg \
		-border 2 -bordercolor black -geometry x400+5+2 -tile 3x - | \
		convert - -pointsize 30 -gravity south -annotate -490+0 'A' \
		-fill White -annotate -156+0 'B' \
		-fill White -annotate +225+0 'C' photos.png 
	#convert ../figures/tech_diagram/tech_diagram.svg -crop 80x100%+0+0 tech_diagram.png
	#montage Chamber.jpg Chamber2.jpg -border 2 -bordercolor black -geometry x627+5+2 -tile 2x chambers.png
	#montage boardwalk_plus_supports.jpg mixing_chambers_brackish.png -border 2 -bordercolor black -geometry x627+5+2 -tile 2x photos.png
	#montage warehouse.jpg -trim field_mesocosm.jpg -border 2 -bordercolor black -geometry x600+5+2 -tile 2x chambers.png


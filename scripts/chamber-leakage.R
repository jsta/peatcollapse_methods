datapath <- "/home/jose/Documents/Science/Data/peatcollapse/fieldallv8.csv"
outpath <- file.path("../figures", "chamber-leakage.png")

cfieldall <- read.csv(datapath, stringsAsFactors = FALSE)
cfieldall$collect_date <- as.POSIXct(cfieldall$collect_date)
#cfieldall$fac <- paste(cfieldall$trt, cfieldall$site, cfieldall$inout, sep = "\n")
#cfieldall$fac <- factor(cfieldall$fac, levels = c("control\nBW\nout", "control\nBW\nin", "treatment\nBW\nout", "treatment\nBW\nin", "control\nFW\nout", "control\nFW\nin", "treatment\nFW\nout", "treatment\nFW\nin"))

library(ggplot2)

theme_opts <- list(ggplot2::theme(
	panel.grid.minor = ggplot2::element_blank(),
	panel.grid.major = ggplot2::element_blank(),
	panel.background = ggplot2::element_blank(),
	plot.background = ggplot2::element_rect(fill="white"),
	panel.border = ggplot2::element_blank(),
	axis.line = ggplot2::element_line(),
	axis.text.x = ggplot2::element_text(size = 14),
	axis.text.y = ggplot2::element_blank(),
	axis.ticks = ggplot2::element_line(),
	axis.title.x = ggplot2::element_text(size = 14),
	axis.title.y = ggplot2::element_blank(),
	plot.title = ggplot2::element_text(size = 22),
	strip.background = ggplot2::element_rect(fill = 'white'),
	legend.position = "none",
	plot.margin = unit(c(1, 1, 1, 9), "lines")))

gg <- ggplot2::ggplot(data = cfieldall[cfieldall$pwsw == "PW",], ggplot2::aes(x = interaction(inout, trt, site), y = salinity))
gg <- gg + geom_boxplot(notch = TRUE, outlier.colour = "white") + coord_flip(ylim = c(0, 21))

gg <- gg + annotate(geom = "text", x = c(2.5, 6.5), y = -7, label = unique(cfieldall$site)[1:2])
gg <- gg + annotate(geom = "text", x = c(1.5, 3.5, 5.5, 7.5), y = -4.5, label = c("control", "treatment", "control", "treatment"))
gg <- gg + annotate(geom = "text", x = c(1:8), y = -3.2, label = c(rep(c("inside", "outside"), 4)))

gg <- gg + ggplot2::ylab("Salinity") + theme_opts 

gg <- ggplot_gtable(ggplot_build(gg))
gg$layout$clip[gg$layout$name == "panel"] <- "off"
grid::grid.draw(gg)
gg <- gridExtra::arrangeGrob(gg)

ggplot2::ggsave(filename = outpath, plot = gg, width = 8, height = 7)

dt <- cfieldall[cfieldall$pwsw == "PW",]
x <- interaction(dt$inout, dt$trt, dt$site)
y <- dt$salinity
# par(mar = c(7, 4, 0, 0))
# 
# png(outpath)
# boxplot(salinity ~ inout + trt + site,data=cfieldall[cfieldall$pwsw == "PW",], outline = FALSE, ylab = "Salinity", las = 2, axes = FALSE)
# axis(1, at = 1:8, labels = c("BW. in. cont.", "BW. out. cont.", "BW. in. trt.", "BW. out. trt.", "FW. in. cont.", "FW. out. cont.", "FW. in. trt.", "FW. out. trt."), las = 2)
# axis(2, at = seq(0, 20, 5), labels = seq(0, 20, 5))
# dev.off()
datapath <- "../data/fieldallv9.csv"
# datapath <- "data/fieldallv9.csv"
outpath <- file.path("../figures", "chamber-leakage.png")

cfieldall <- read.csv(datapath, stringsAsFactors = FALSE)
cfieldall$collect_date <- as.POSIXct(cfieldall$collect_date)

library(ggplot2)
library(cowplot)

theme_opts <- list(ggplot2::theme(
	panel.grid.minor = ggplot2::element_blank(),
	panel.grid.major = ggplot2::element_blank(),
	panel.background = ggplot2::element_blank(),
	plot.background = ggplot2::element_rect(fill="white"),
	panel.border = ggplot2::element_blank(),
	axis.line = ggplot2::element_line(size = 1.2),
	axis.text.x = ggplot2::element_text(size = 14),
	axis.text.y = ggplot2::element_blank(),
	axis.ticks = ggplot2::element_line(size = 1.2),
	axis.title.x = ggplot2::element_text(size = 14),
	axis.title.y = ggplot2::element_blank(),
	plot.title = ggplot2::element_text(size = 22),
	strip.background = ggplot2::element_rect(fill = 'white'),
	legend.position = "none",
	plot.margin = unit(c(1, 1, 1, 9), "lines")))

cfieldall$site_long <- NA
cfieldall$site_long[cfieldall$site == "FW"] <- "freshwater"
cfieldall$site_long[cfieldall$site == "BW"] <- "brackish"

cfieldall$trt <- factor(cfieldall$trt, levels = c("treatment", "control"))
cfieldall$trt_x <- interaction(cfieldall$inout, cfieldall$trt, cfieldall$site_long)
cfieldall$trt_x <- factor(cfieldall$trt_x, levels = rev(c("out.treatment.freshwater", 
																											"in.treatment.freshwater", 
																											"out.control.freshwater", 
																											"in.control.freshwater", 
																											"out.treatment.brackish",
																											"in.treatment.brackish",
																											"out.control.brackish", 
																											"in.control.brackish")))

pal <- scales::hue_pal()(3)

gg_key <- ggplot(data = subset(cfieldall, !is.na(trt)), 
								 aes(x = temp.degC., y = ph, color = trt)) + 
	geom_point() + scale_color_manual(values = pal)

legend <- get_legend(gg_key + theme(legend.position = "bottom", 
																		legend.title = element_blank()))
legend <- plot_grid(NULL, legend, ncol = 2, nrow = 1, rel_widths = c(0.3, 0.7))

gg <- ggplot(data = cfieldall[cfieldall$pwsw == "PW",], 
											aes(x = trt_x, y = salinity, fill = trt_x)) + 
	geom_boxplot(notch = TRUE, outlier.colour = "white", lwd = 1.1) + 
	coord_flip(ylim = c(0, 21)) + 
	scale_fill_manual(values = rep(c(pal[2], "white", pal[1], "white"), 2))

gg <- gg + annotate(geom = "text", x = c(2.5, 6.5), y = -10, 
										label = unique(cfieldall$site_long)[1:2])
gg <- gg + annotate(geom = "text", x = c(1.5, 3.5, 5.5, 7.5), y = -6.5, 
										label = c("control", "treatment", "control", "treatment"))
gg <- gg + annotate(geom = "text", x = c(1:8), y = -4, 
										label = c(rep(c("inside", "outside"), 4)))

gg <- gg + ggplot2::ylab("Salinity") + theme_opts 

gg <- ggplot_gtable(ggplot_build(gg))
gg$layout$clip[gg$layout$name == "panel"] <- "off"
grid::grid.draw(gg)
gg <- gridExtra::arrangeGrob(gg)

gg_res <- cowplot::plot_grid(legend, gg, ncol = 1, rel_heights = c(0.1, 1), align = "horizontal")

ggplot2::ggsave(filename = outpath, plot = gg_res, width = 5.5, height = 5)

# dt <- cfieldall[cfieldall$pwsw == "PW",]
# x <- interaction(dt$inout, dt$trt, dt$site)
# y <- dt$salinity
# par(mar = c(7, 4, 0, 0))
# 
# png(outpath)
# boxplot(salinity ~ inout + trt + site,data=cfieldall[cfieldall$pwsw == "PW",], outline = FALSE, ylab = "Salinity", las = 2, axes = FALSE)
# axis(1, at = 1:8, labels = c("BW. in. cont.", "BW. out. cont.", "BW. in. trt.", "BW. out. trt.", "FW. in. cont.", "FW. out. cont.", "FW. in. trt.", "FW. out. trt."), las = 2)
# axis(2, at = seq(0, 20, 5), labels = seq(0, 20, 5))
# dev.off()
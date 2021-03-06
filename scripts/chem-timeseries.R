library(ggplot2)
library(dplyr)
# library(peatcollapse)
library(cowplot)

cfieldall <- read.csv("../data/fieldallv9.csv", stringsAsFactors = FALSE)
# cfieldall <- read.csv("data/fieldallv9.csv", stringsAsFactors = FALSE)
cfieldall$collect_date <- as.POSIXct(cfieldall$collect_date)
cfieldall$collect_date <- strftime(cfieldall$collect_date, format = "%Y-%m")


nonchamber_fw <- c(2, 4, 6, 9)
nonchamber_bw <- c(1, 4, 5, 7)

cfieldall <- mutate(cfieldall, trt = case_when(
	site == "FW" & chamber %in% nonchamber_fw ~ "nonchamber", 
	site == "BW" & chamber %in% nonchamber_bw ~ "nonchamber", 
	TRUE ~ trt))

cfieldall$trt <- factor(cfieldall$trt, levels = c("treatment", "control", "nonchamber"))

cfieldall <- group_by(cfieldall, collect_date, trt, site, pwsw, inout)
cfieldall <- select(cfieldall, site:inout, trt, 
										           temp.degC.:salinity, 
										           ALKA.mgL.:DOC.mgL, 
															 NH4.mgL:TDN.mgl,
															 srp.um.l:tdp.ppb)

pc_plot <- function(dt, x, y, site){
	x <- enquo(x)
	y <- enquo(y)
	site <- enquo(site)
	
	dt <- filter(dt, site == !!site, inout == "in", pwsw == "PW")
	dt <- summarize(dt, y = mean(!!y, na.rm = TRUE), 
									upper = y + (2 * sd(!!y, na.rm = TRUE) / sqrt(n())),
									lower = y - (2 * sd(!!y, na.rm = TRUE) / sqrt(n())))
	
	ggplot(data = dt) + 
		geom_point(aes_string(x = quo_name(x), y = "y", color = "trt")) + 
		geom_errorbar(data = dt, aes_string(x = quo_name(x), 
											ymin = "lower", ymax = "upper", color = "trt"), width = 0.2) + 
		theme(axis.text.x = element_text(angle = 90), 
					legend.justification = "top", 
					axis.line.x = element_line(colour = "black", size = 1.1), 
					axis.line.y = element_line(colour = "black", size = 1.1), 
					axis.ticks = element_line(colour = "black", size = 1.1)) + 
		xlab("") + ylab(y) + labs(color = "")
}

(gg_fw <- pc_plot(dt = cfieldall, x = collect_date, y = salinity, site = "FW"))

gg_bw <- pc_plot(dt = cfieldall, x = collect_date, y = salinity, site = "BW")

gg <- cowplot::plot_grid(gg_fw + theme(legend.position="none"), 
												 gg_bw + theme(legend.position="none"), 
												 nrow = 2, ncol = 1, labels = c("A", "B"), hjust = -29)

legend <- get_legend(gg_fw + theme(legend.position = "bottom"))
saveRDS(legend, "../figures/legend.rds")

gg_res <- cowplot::plot_grid(gg, legend, ncol = 1, rel_heights = c(1, 0.1))


ggsave(filename = "../figures/03_chem-ts.png", height = 6, width = 4.6)

# par(mar = c(4, 5, 2, 1))
# 
# png("../figures/chem-ts_fw-sal.png", res = 200, width = 1000, height = 800)
# tsplot(cfieldall, params = "salinity", bwfw = "fw", pwsw = "pw", tofile = FALSE, inout = "in", inclegend = TRUE, print_xaxis = FALSE, print_main = FALSE)
# dev.off()
# 
# png("../figures/chem-ts_bw-sal.png", res = 200, width = 1000, height = 800)
# tsplot(cfieldall, params = "salinity", bwfw = "bw", pwsw = "pw", tofile = FALSE, inout = "in", inclegend = FALSE, print_main = FALSE)
# dev.off()
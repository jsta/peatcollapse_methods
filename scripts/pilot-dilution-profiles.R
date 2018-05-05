#' @name pilot_dilution_profiles
#' @title Plot dilution profiles from the pilot experiments
#' @param fpath file.path to data file
#' @param outpath file.path to save plot
#' @param dim numeric of length 2 specifying figure height and width
#' @details Ignores surface water data
#' @export
#' @import readxl
#' @import magrittr
#' @import ggplot2
#' @import dplyr
#' @examples
#' fpath <- list.files("./data", "*.xlsx", full.names = TRUE, include.dirs = TRUE)
#' outpath <- file.path("./figures", "dilution-profile.png")
#' pilot_dilution_profiles(fpath, outpath)

pilot_dilution_profiles <- function(fpath, outpath, dim = NA){
	
	# format incoming data ####
	dt <- readxl::read_excel(fpath)
	names(dt) <- make.names(names(dt)) %>% gsub("\\.", "", .) %>% gsub(" ", "", .)
	dt[4:ncol(dt)] <- suppressWarnings(apply(dt[4:ncol(dt)], 2, 
																					 function(x) as.numeric(x)))
	dt$chamber <- as.factor(dt$chamber)
	dt <- dt[nchar(dt$quad) == 1,] # remove surface water samples
	dt$days <- as.numeric((dt$date - min(dt$date)) / (60 * 60 * 24))
	
	# compute with-in chamber means ####
	dt <- dplyr::group_by(dt, days, chamber)
	dt <- dplyr::summarise(dt, mean_sal = mean(sal, na.rm = TRUE), 
												 sd_sal = sd(sal, na.rm = TRUE), n = length(sal))
	
	dt$se <- 2 * (dt$sd_sal / sqrt(dt$n))
	dt$upper <- dt$mean_sal + dt$se
	dt$lower <- dt$mean_sal - dt$se
	dt$lower[dt$lower < 0] <- 0
	
	dt <- dplyr::filter(dt, chamber != "wet")
	
	#make plots
	theme_opts <- list(ggplot2::theme(
		panel.grid.minor = ggplot2::element_blank(),
		panel.grid.major = ggplot2::element_blank(),
		panel.background = ggplot2::element_blank(),
		plot.background = ggplot2::element_rect(fill="white"),
		panel.border = ggplot2::element_blank(),
		axis.line = ggplot2::element_line(size = 1.5),
		axis.text.x = ggplot2::element_text(size = 12),
		axis.text.y = ggplot2::element_text(size = 12),
		axis.ticks = ggplot2::element_line(size = 1.5),
		axis.title.x = ggplot2::element_text(size = 12),
		axis.title.y = ggplot2::element_text(size = 12),
		plot.title = ggplot2::element_text(size = 22),
		strip.background = ggplot2::element_rect(fill = 'white'),
		legend.position = "none"))
	
	gg <- ggplot2::ggplot(data = dt, ggplot2::aes(x = days, y = mean_sal))
	gg <- gg + ggplot2::geom_smooth(se = FALSE, color = "gray")
	gg <- gg + ggplot2::geom_point(size = 3) + 
		ggplot2::geom_errorbar(ggplot2::aes(ymin = lower, ymax = upper), 
													 width = 0.3, color = "black")
	
	gg + ggplot2::ylab("salinity") +	ggplot2::xlab("days post dosing") + 
		theme_opts
	
	ggplot2::ggsave(filename = outpath, width = 5, height = 4)
}

library(magrittr)
fpath <- list.files(pattern = "xlsx", full.names = TRUE, include.dirs = TRUE)
outpath <- file.path("../figures", "04_dilution-profile.png")
#fpath <- list.files("data", "*.xlsx", full.names = TRUE, include.dirs = TRUE) 
#outpath <- file.path("figures", "dilution-profile.png")
pilot_dilution_profiles(fpath, outpath)
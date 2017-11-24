library(peatcollapse)

cfieldall <- read.csv("/home/jose/Documents/Science/Data/peatcollapse/fieldallv9.csv", stringsAsFactors = FALSE)
cfieldall$collect_date <- as.POSIXct(cfieldall$collect_date)

par(mar = c(4, 5, 2, 1))

png("../figures/chem-ts_fw-sal.png", res = 200, width = 1000, height = 800)
tsplot(cfieldall, params = "salinity", bwfw = "fw", pwsw = "pw", tofile = FALSE, inout = "in", inclegend = TRUE, print_xaxis = FALSE, print_main = FALSE)
dev.off()

png("../figures/chem-ts_bw-sal.png", res = 200, width = 1000, height = 800)
tsplot(cfieldall, params = "salinity", bwfw = "bw", pwsw = "pw", tofile = FALSE, inout = "in", inclegend = FALSE, print_main = FALSE)
dev.off()
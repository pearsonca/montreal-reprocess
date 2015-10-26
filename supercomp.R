#!/apps/R/3.2.0/bin/Rscript
# TODO receive arg(s)
#  - a config file
#  - a series of files that provides lists to combine
#  - a series of string / range arguments

lifetime <- c("low","mid","high")
variability <- c("lo","med","hi")
peakTime <- c("early","middle","late")
size <- c(seq(5, 20, 5),30)
k <- 2
meetFrequency <- c(7, 14, 28)
matchin <- c("matched","unmatched")
likin <- c("like","unlike")

write.table(expand.grid(lifetime, variability, peakTime, size, k, meetFrequency, matchin, likin), "sim-src.pars",
  row.names = F, col.names = F, quote = F)

## script

#!/usr/bin/env Rscript

rm(list=ls())
require(data.table)
require(parallel)
args <- commandArgs(trailingOnly = T)

covertcovertfiles <- grep("cc.csv", args, value = T)
covertbackgroundfiles <- grep("cu.csv", args, value = T)

# in cc files, user.a and user.b are both covert
# in cu files, user.a is covert, user.b is (raw) background
# in both, location ids are raw

locLimits <- readRDS("input/raw-location-lifetimes.rds")

reader <- function(fname, locLims) locLims[setkey(fread(fname, col.names = c("user.a","user.b","location_id","login","logout","type")), location_id)]

mcmapply(function(ccfile, cufile, rawLocLimits) {
    cc.dt <- reader(ccfile, locLimits)
    cu.dt <- reader(cufile, locLimits)
    saveRDS(cc.dt, file = sub("csv","rds",ccfile))
    saveRDS(cu.dt, file = sub("csv","rds",cufile))
  },
  covertcovertfiles,
  covertbackgroundfiles,
  MoreArgs = list(rawLocLimits=locLimits)
)
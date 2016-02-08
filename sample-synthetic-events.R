#!/usr/bin/env Rscript

rm(list=ls())

set.seed(42)

args <- commandArgs(trailingOnly = T)

list.files(paste0(args[2],".."))

# require(data.table)
# 
# src <- readRDS(args[1])
# p <- as.numeric(paste0("0.", args[2]))
# 
# cat("sample fraction: ",p,"\n")
# 
# saveRDS(src[runif(.N) < p], args[3])
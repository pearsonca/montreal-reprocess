require(data.table)
# should be run in output/src dir
loclimits <- readRDS("../../input/locLimits.RData") # data.table, 
cufiles <- list.files(pattern = "-cu")
ccfiles <- list.files(pattern = "-cc")
readIn <- function(f) setkey(setnames(fread(f), c("userA", "userB", "location_id", "login", "logout", "reason")), location_id)
trim <- function(dt) merge(dt, loclimits, by="location_id")[!(login < start | end < logout), list(userA, userB, login, logout, reason), keyby=location_id]
combos <- function(cu, cc) {
  cu.dt <- trim(readIn(cu))
  cc.dt <- trim(readIn(cc))
  res <- setkey(rbind(cu.dt, cc.dt), login)
  newfile <- sub("cu.csv","combos.RData", cu)
  saveRDS(res, newfile)
}
res <- mapply(combos, cufiles, ccfiles)

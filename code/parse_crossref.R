
library(dplyr)

outdir <- "output"
rdsfs <- dir(outdir, "^[0-9].*.rds$")


# nr <- sapply(rdsfs, 
#              function(u) nrow(readRDS(file.path(outdir,u))))


# split reading in into n chunks
n <- 100
st <- floor(seq(1,length(rdsfs), length.out=n+1))
en <- st[-1]-1
en[n] <- en[n]+1
st <- st[-(n+1)]

tbls <- lookups <- list()

for(i in 1:length(st)) {
  cat(st[i],"..", en[i],"\n")
  z <- lapply(rdsfs[st[i]:en[i]], function(u) readRDS(file.path(outdir,u)))
  br <- bind_rows(z)
  tbls[[i]] <- br[,1:2]
  lookups[[i]] <- unique(br[,-1])
}


tbl <- bind_rows(tbls)
rm(tbls); gc()

saveRDS(tbl, file=file.path(outdir, "tbl.rds"))

lookup <- bind_rows(lookups)
lookup <- unique(lookup)
rm(lookups); gc()

saveRDS(lookup, file=file.path(outdir, "lookup.rds"))



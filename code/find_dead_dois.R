# find bad ZORA DOIs

library(dplyr)
library(rcrossref)

outdir <- "output"
tbl_eprints <- readRDS(file.path(outdir, "tbl_eprints.rds"))

dois <- unique(na.omit(tbl_eprints$doi))
dois <- tolower(dois)


st <- seq(1,length(dois), by=100)
en <- c(st[-1]-1, length(dois))
hundreds <- cbind(st,en)

cns <- vector("list", length(dois))
names(cns) <- cns


# for(i in 1:nrow(hundreds)) {
for(i in 1:nrow(hundreds)) {
    inds <- hundreds[i,1]:hundreds[i,2]
  cns[inds] <- cr_cn(dois[inds])
  cat(i," ")
}


n <- sapply(cns,length)


n <- sapply(cns,length)

sink("big.bib")
cat(paste0(unlist(cns[n==1]), collapse = "\n"))
sink()


library(bib2df)
d <- bib2df("big.bib")
dim(d)
head(d)


ra <- cr_agency(dois = "10.19227/jzar.v4i4.194")

agency <- na.omit(m$doi)
cras <- sapply(cra, function(u) u$agency$id[1])







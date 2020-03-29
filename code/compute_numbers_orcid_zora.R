
library(googlesheets4)
library(rorcid)

tbl_orcid <- readRDS(file.path(outdir, "tbl_orcid.rds"))
tbl_orcid$orcid <- gsub("https://orcid.org/","",tbl_orcid$orcid)


orcs <- unique(tbl_orcid$orcid)
employs <- lapply(orcs, function(u) {
  cat(".")
  rorcid::orcid_employments(u)
})

# z <- read_sheet("https://docs.google.com/spreadsheets/d/1Pn8JgQVd1qzJmfDJU6EO9Do_RwBc3aAi2rjIkS3w4Fo/edit#gid=0")
# orcs <- setNames(z$orcid, z$orcid)

# data.frame(orcid=names(n_orcs), num_records=n_orcs)

n_orcs <- rep(NA, length(orcs))
for(i in 1:length(n_orcs)) {
  cat(".")
  works <- tryCatch(rorcid::orcid_works(orcs[i]), error=function(e) return(-1))
  if(class(works) != "list") {n_orcs[i] <- -1; next}
  works <- works[[1]]$works
  if(nrow(works)==0) {n_orcs[i] <- -1; next}
  n_orcs[i] <- sum(works$type=="journal-article")
}
saveRDS(n_orcs, file="n_orcs.rds")


zz <- table(tbl_orcid$orcid)
m <- match(names(zz), orcs)

qplot(x=as.numeric(zz), n_orcs[m]) + geom_point() + 
  scale_x_sqrt() + scale_y_sqrt() + 
  xlab("Number of ZORA records") + 
  ylab("Number of ORCID Publications") + 
  geom_abline() + 
  geom_density_2d(colour="yellow")


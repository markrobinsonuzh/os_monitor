

# for use in docker, with second docker container containing the mongodb
library(mongolite)

oadoi_fetch_local <- function(dois,collection="unpaywall", db="oa", url="mongodb://192.168.16.3/20"){
  con <- mongo(collection=collection, db=db, url=url)
  con$find(paste0('{"doi": { "$in": ["',paste0(doi,collapse = '","'),'"]}}'), fields = '{"doi":1, "oa_status":1}')
}

doi <- c("10.2217/14750708.2.5.745","10.1192/bjp.89.375.270")
oadoi_fetch_local(doi)

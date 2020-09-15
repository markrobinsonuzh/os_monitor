## Generating ZORA JSON files from API
setwd("/srv/shiny-server/os_monitor")
stub <- "wget -O data/zora_YEAR.js --no-check-certificate https://www.zora.uzh.ch/cgi/exportview/yearnew/YEAR/JSON/YEAR.js"

z <- 2020:2000

for(i in z) {
  cmd <- gsub("YEAR",i,stub)
  cat(cmd,"\n")
  system(cmd, show.output.on.console = FALSE)
}
system("gzip -f data/*.js")

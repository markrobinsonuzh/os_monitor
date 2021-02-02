

library(readxl)

ch_rpdeals <- "
https://consortium.ch/wp_live/wp-content/uploads/2021/01/CUP_Journals_titlelist_publish.xlsx
https://consortium.ch/wp_live/wp-content/uploads/2020/05/Elsevier_titlelist_publication.xlsx
https://consortium.ch/wp_live/wp-content/uploads/2021/01/Karger_titlelist_publlish.xlsx
https://consortium.ch/wp_live/wp-content/uploads/2021/01/RSC_Journals_titlelist_publish.xlsx
https://consortium.ch/wp_live/wp-content/uploads/2021/01/Sage_Journals_titlelist_publish.xlsx
https://consortium.ch/wp_live/wp-content/uploads/2020/06/Springer_titlelist_publication.xlsx
https://consortium.ch/wp_live/wp-content/uploads/2021/01/TandF_titlelist_publish.xlsx
"
ch_rpdeals <- strsplit(ch_rpdeals, "\n")[[1]]
ch_rpdeals <- ch_rpdeals[nchar(ch_rpdeals)>0]

dummy <- lapply(ch_rpdeals, function(u) download.file(u, destfile = basename(u)))
xlsx <- lapply(ch_rpdeals, function(u) {
  rx <- read_xlsx(basename(u), skip = 7)
  colnames(rx)[colnames(rx)=="e-ISSN"] <- "ISSN"
  rx
})

rp_journals <- bind_rows(xlsx)

saveRDS(rp_journals, file="rp_journals.rds")


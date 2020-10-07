
qrows <- df_scholar[is.na(df_scholar$doi),]
i <- 1
Sys.setenv(crossref_email="reto.gerber@uzh.ch")
system.time({
# seq_len(dim(qrows)[1])
out <- lapply(1:10,function(i){
  # i <- 7
  sq <- cr_works(flq=list(query.bibliographic=paste(qrows$title[i],qrows$year[i]),
                          query.author=qrows$author[i]),limit = 3)
  scores <- as.numeric(sq$data$score)
  if (scores[1]/scores[2]>1.5 & scores[1]>70 & !is.null(sq)){
    sq$data[1,] %>% select(doi,container.title,published.print,title)
  } else{
    scores
  }
})
})
out

pull(out$data[1,"link"])[[1]]$URL


oadoi_fetch_local(out[[8]]$doi,unpaywall)

out <- cr_works(dois="10.1186/s13059-020-01967-8")
out$data$score

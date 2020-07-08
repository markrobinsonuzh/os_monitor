
# knitr::spin("code/department_tracking.R")

library(dplyr)
library(ggplot2)
library(ggrepel)


outdir <- here::here("output")
tbl_subjects <- readRDS(file.path(outdir, "tbl_subjects.rds"))
tbl_eprints <- readRDS(file.path(outdir, "tbl_eprints.rds"))
tbl_authorkeys <- readRDS(file.path(outdir, "tbl_authorkeys.rds"))

# View(tbl_subjects %>% group_by(subjects, name) %>% tally())
# 
# # dmls <- tbl_subjects %>% filter(subjects==10124) %>% left_join(tbl_eprints)
# geo <- tbl_subjects %>% filter(subjects==10122) %>% left_join(tbl_eprints) %>%
#   left_join(tbl_authorkeys)
# tg <- table(geo$authorkey)
# head(sort(tg, decreasing = TRUE), 50)
# 
# 
# tg[grep("schmid b", names(tg))]
# 
# ieu <- tbl_subjects %>% filter(subjects==10127) %>% left_join(tbl_eprints) %>%
#   left_join(tbl_authorkeys)
# 
# tg <- table(ieu$authorkey)
# head(sort(tg, decreasing = TRUE), 50)
# 
# author <- c("altermatt f 0000 0002 4831 6958", "altermatt f")
# orcid <- "0000-0002-4831-6958"
# 
# tbl_author <- tbl_authorkeys %>% filter(authorkey %in% author) %>%
#   left_join(tbl_eprints) # %>%  left_join(tbl_subjects)
# 

# tg <- table(geo$authorkey)
# head(sort(tg, decreasing = TRUE), 50)
# 
# grep("robinson m", names(tg), value=TRUE)
# 
# mr <- grep("robinson m", names(tg))

# paste0(names(nm[]),collapse=",")

# tbl_authorkeys <- readRDS(file.path("output", "tbl_authorkeys.rds")) %>%
#   filter(authorkey %in% zora_key)
# tbl_subjects <- readRDS(file.path("output", "tbl_subjects.rds")) %>%
#   filter(subjects %in% 10124)
# tbl_eprints_filtered <- tbl_eprints %>% select(-date,-institution) %>%
#   filter(eprintid %in% intersect(tbl_authorkeys$eprintid,tbl_subjects$eprintid))


all <- tbl_subjects %>% #filter(subjects==10122) %>% 
  left_join(tbl_eprints) %>%
  left_join(tbl_authorkeys)

authors_open <- all %>% select(authorkey, eprintid, doi, 
                               oa_status, published_doc, date) %>%
  unique() %>%
  group_by(authorkey) %>% 
  summarize(open_pct = mean(oa_status != "closed")*100,
            open_blue_pct = mean(oa_status != "closed" | published_doc)*100,
            last = max(date)) %>%
  filter(last >= 2019)

# authors_open %>% filter(grepl("robinson m", authorkey))
# 
# all %>% filter(grepl("robinson m 0000 0002", authorkey), 
#                oa_status == "closed", !published_doc) %>%
#   select(authorkey, eprintid, doi, oa_status, published_doc, title) %>%
#   unique()

z <- all %>% filter(authorkey != "et al", 
                    !grepl("collaboration", authorkey),
                    !grepl("commission", authorkey),
                    authorkey %in% authors_open$authorkey) %>%
  group_by(authorkey, name) %>% tally() %>% arrange(desc(n)) %>%
  group_by(name) %>% top_n(10)

depts <- z %>% summarize(n_m = mean(n)) %>% arrange(desc(n_m))

faculty_lookup <- all %>% select(name, parent_name) %>% unique()

uzh_summary <- z %>% left_join(depts) %>% filter(n > 10) %>% 
  arrange(desc(n_m), name) %>% select(-n_m) %>%
  left_join(authors_open)

dept_summary <- uzh_summary %>% group_by(name) %>% 
  summarise(mean_oa = mean(open_pct),
            mad_oa = mad(open_pct),
            mean_oa_blue = mean(open_blue_pct),
            mad_oa_blue = mad(open_blue_pct)) %>% 
  arrange(desc(mean_oa)) %>%
  left_join(faculty_lookup)

uzh_summary <- uzh_summary %>% left_join(dept_summary) %>%
  mutate(z_oa = (open_pct-mean_oa)/mad_oa,
        z_oa_blue = (open_blue_pct-mean_oa_blue)/mad_oa_blue) %>%
  select(-mad_oa, -mad_oa_blue)



dept_summary$name <- factor(dept_summary$name,
                            levels=dept_summary$name[!duplicated(dept_summary$name)])
# dept_summary$parent_name <- factor(dept_summary$parent_name,
#                                    levels=dept_summary$parent_name[!duplicated(dept_summary$parent_name)])


p1 <- ggplot(dept_summary %>% filter(grepl("^0[1-8]", parent_name)) %>% 
         mutate(parent_name = gsub("Faculty of ","",parent_name)), 
       aes(name, mean_oa)) + 
  geom_point() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  facet_grid(~parent_name, scales = "free_x", space = "free_x")
p2 <- ggplot(dept_summary %>% filter(!grepl("^0[1-8]", parent_name)) %>% 
               mutate(parent_name = gsub("Faculty of ","",parent_name)), 
             aes(name, mean_oa)) + 
  geom_point() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  facet_grid(~parent_name, scales = "free_x", space = "free_x")

## ----plot1, fig.height = 10, fig.width=20----
p1

pdf("depts.pdf", width=20, height=7)
p1
p2
dev.off()

uzh_summary$name <- factor(uzh_summary$name,
                           levels=dept_summary$name[!duplicated(dept_summary$name)])

uzh_summary$os_delegates <- FALSE
uzh_summary$os_delegates[grepl("robinson m 0000 0002", uzh_summary$authorkey)] <- TRUE
uzh_summary$os_delegates[grepl("thommen m", uzh_summary$authorkey)] <- TRUE


uzh_summary$z_oa[uzh_summary$z_oa > 10] <- 10.5
uzh_summary$z_oa[uzh_summary$z_oa < -10] <- -10.5
uzh_summary$z_oa_blue[uzh_summary$z_oa_blue > 7] <- 7.5

## ----plot2, fig.height = 12, fig.width=12----

p <- ggplot(uzh_summary, aes(n, open_pct, label = authorkey)) + 
  geom_point() + 
  geom_text_repel(data = uzh_summary %>% filter(n>200), size=2) +
  scale_x_sqrt()
p
ggsave("individuals_n.pdf", p, width=15, height=10)


p <- ggplot(uzh_summary, aes(n, open_blue_pct, label = authorkey)) + 
  geom_point(col="blue") +
  geom_text_repel(data = uzh_summary %>% filter(n>200), size=2) +
  scale_x_sqrt()
p
ggsave("individuals_n_blue.pdf", p, width=15, height=10)

p <- ggplot(uzh_summary, aes(n, z_oa, label = authorkey)) + 
  geom_point() +
  geom_text_repel(data = uzh_summary %>% filter(z_oa>3), size=3) +
  scale_x_log10()
p

ggsave("individuals_z.pdf", p, width=15, height=10)

ggplot(uzh_summary, aes(n, z_oa_blue, label = authorkey)) + 
  geom_point(col="blue") +
  geom_text_repel(data = uzh_summary %>% filter(z_oa_blue>3), size=3) +
  scale_x_log10()

write.table(as.data.frame(uzh_summary %>% arrange(desc(mean_oa))), file = "uzh_summary.csv",
            sep = ",", row.names = FALSE)


top <- head(levels(uzh_summary$name), 40)
bot <- tail(levels(uzh_summary$name), 40)

## ----plot3, fig.height = 12, fig.width=20----
p <- ggplot(uzh_summary %>% filter(name %in% c(top,bot)), 
            aes(name, open_pct, label = authorkey, 
                color = os_delegates, size = os_delegates)) + 
  geom_point() + 
  #geom_text_repel(data = uzh_summary %>% filter(n>200, open_pct>75)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
p

ggsave("depts_top10.pdf", p, width=20, height = 8)


# ggsave("depts.pdf", p, width=30, height=8)
# ggsave("depts.pdf", p, width=50, height=8, limitsize = FALSE)


# p <- ggplot(dept_summary %>% #filter(grepl("Faculty", parent_name)) %>% 
#               mutate(parent_name = gsub("Faculty of ","",parent_name)), 
#             aes(mean, name)) + 
#   geom_point() + 
#   theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
#   facet_grid(parent_name~., scales = "free_y", space = "free_y")



# tg <- table(all$authorkey)
# 
# grep("clauss m", names(tg), value=TRUE)




library(readxl)
library(dplyr)
library(ggplot2)

d <- read_excel("data/CWTS Leiden Ranking 2020.xlsx", sheet = "Results")
d <- d %>% filter(Frac_counting==1)
d$PP_OA <- d$PP_OA*100
d$PP_top10 <- d$PP_top10*100
z <- d %>% filter(Country=="Switzerland")


# set some colours
t_uni <- table(z$University)
cols <- c("gray48", "darkorange1", "chartreuse4", "gold",
          "red", "darkgoldenrod4", "blue", "black")
names(cols) <- names(t_uni)
my_scale <- scale_colour_manual(values=cols)


pdf("cwts_mark.pdf", width=11, height=6)

ggplot(d, aes(x=Period, y=PP_OA, group=University, colour=Field)) +
  facet_wrap(~Field, nrow=2, scales = "free_y") +
  geom_line() +
  geom_line(data = d %>% filter(University == "University of Zurich"), 
            colour="black", size=2) +
  theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("Worldwide")

ggplot(z, aes(x=Period, y=PP_OA, group=University, colour=University)) +
  facet_wrap(~Field, nrow=2, scales = "free_y") +
  geom_line(size=2) +
  theme(axis.text.x = element_text(angle = 90)) +
  my_scale +
  ggtitle("Switzerland")


ggplot(d, aes(x=Period, y=PP_top10, group=University, colour=Field)) +
  facet_wrap(~Field, nrow=2, scales = "free_y") +
  geom_line() +
  geom_line(data = d %>% filter(University == "University of Zurich"), 
            colour="black", size=2) +
  theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("Worldwide")
  
ggplot(z, aes(x=Period, y=PP_top10, group=University, colour=University)) +
  facet_wrap(~Field, nrow=2, scales = "free_y") +
  geom_line(size=2) +
  theme(axis.text.x = element_text(angle = 90)) +
  my_scale +
  ggtitle("Switzerland")

dev.off()



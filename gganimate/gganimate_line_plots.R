setwd("C:/Users/test1/OneDrive/Documents 201617/R/gganimate")
install.packages("tidyverse")
library(tidyverse)
install.packages("ggplot2")
library(ggplot2)
install.packages("gganimate")
library(gganimate)
install.packages("gifski")
library(gifski)
install.packages("png")
library(png)
install.packages("ggthemes")
library(ggthemes)
BP7genus=read.table("Wang_genus_top20_bp7.csv",header=T,sep=",", strip.white = TRUE)
head(as.data.frame(BP7genus), n = 12)
BP7genus_long=gather(BP7genus, "Genus", "Counts", 4:23)
head(as.data.frame(BP7genus_long), n = 12)

#try and make static line plot
p <- ggplot(
  BP7genus_long,
  aes(Months.since.start.of.biopile, Counts, group = Genus, color = factor(Genus))
) +
  geom_line() +
  labs(x = "Months since start of biopile", y = "Abundance") +
  theme(legend.position = "right")

#facet wrap
p_by_sample= p + facet_wrap(~Sample)

p_by_sample

#sqrt y axis

p_sqrt= p_by_sample + scale_y_continuous(trans='sqrt')

p_sqrt

#log y axis

p_log <- ggplot(
  BP7genus_long,
  aes(Months.since.start.of.biopile, log10(Counts), group = Genus, color = factor(Genus))
) + 
  geom_line() +
  labs(x = "Months since start of biopile", y = "Abundance") +
  theme(legend.position = "right") +
  facet_wrap(~Sample)

p_log

#annimate
p_facet_animate= p_log + transition_reveal(Months.since.start.of.biopile)

p_facet_animate

#save

animate(p_facet_animate, height = 800, width =1000)
anim_save("Bp7_genus.gif")

#try and slow it down

transition_length = 2, state_length = 5



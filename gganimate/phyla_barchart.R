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
BP7phyla=read.table("BP7_phyla.csv",header=T,sep=",", strip.white = TRUE)
head(as.data.frame(BP7phyla), n = 12)
BP7phyla_long=gather(BP7phyla, "Phyla", "Counts", 4:15)
head(as.data.frame(BP7phyla_long), n = 12)
theme_set(theme_classic())

#filter for month
just_0_A= filter(BP7phyla_long, Months.since.start.of.biopile == 0, Sample == A)

#trying pie chart

df <- as.data.frame(table(just_0_A$Phyla))
colnames(df) <- c("Phyla", "Counts")
pie <- ggplot(df, aes(x = "", y=Counts, fill = factor(Phyla))) + 
  geom_bar(width = 1, stat = "identity") +
  theme(axis.line = element_blank(), 
        plot.title = element_text(hjust=0.5)) + 
  labs(fill="Phyla", 
       x=NULL, 
       y=NULL, 
       title="Pie Chart of Phyla", 
       caption="Fingers crossed")

attempt= pie + coord_polar(theta = "y", start=0)
attempt


#giveup, try bar plot

barplot= ggplot(just_0) +
  facet_wrap(~Sample) +
  geom_bar(aes(Phyla, log10(Counts), fill = Phyla), stat = "identity")
barplot

#try annimating

barplot_animate= ggplot(BP7phyla_long) +
  facet_wrap(~Sample) +
  geom_bar(aes(Phyla, log10(Counts), fill = Phyla), stat = "identity")+
transition_states(Months.since.start.of.biopile, transition_length = 2, state_length = 0)
animate(barplot_animate, height = 800, width =1000)
anim_save("Bp7_phyla.gif")

#try increasing state length and doing mp4

barplot_animate= ggplot(BP7phyla_long) +
  facet_wrap(~Sample) +
  geom_bar(aes(Phyla, log10(Counts), fill = Phyla), stat = "identity")+
  transition_states(Months.since.start.of.biopile, transition_length = 2, state_length = 5)
animate(barplot_animate, height = 800, width =1000)
anim_save("Bp7_phyla_slower.gif")

#trying to turn it into time
barplot_animate= ggplot(BP7phyla_long) +
  facet_wrap(~Sample) +
  geom_bar(aes(Phyla, log10(Counts), fill = Phyla), stat = "identity")+
  transition_time(Months.since.start.of.biopile)+
  labs(title = "Months: {frame_time}")
animate(barplot_animate, height = 800, width =1000)
anim_save("Bp7_phyla_time.gif")





#current script for creating the microcosm scatter plots
#based on my previous "qPCRscatter" script

####setting the scene####
setwd("C:/Users/sally/OneDrive/Documents 201617/R/Nitritepaper/20200402_Figure3")
rm(list=ls())
Sys.setenv(PATH = paste(Sys.getenv("PATH"), "*InstallDirectory*/Rtools/bin/",
                        "*InstallDirectory*/Rtools/mingw_64/bin", sep = ";")) #for 64 bit version
Sys.setenv(BINPREF = "*InstallDirectory*/Rtools/mingw_64/bin")

#####all packages needed for this script, just install once####
install.packages("ggplot2")
install.packages("ggsci")
install.packages("tidyr")
install.packages("reshape2")
install.packages("ggpubr")
library(devtools)
devtools::install_github("thomasp85/patchwork")
devtools::install_github("tomwenseleers/export")
install.packages("export")
devtools::install_github("johannesbjork/LaCroixColoR")

####open all packages needed for this script, do every time #### 
library(ggplot2)
library(ggsci)
library(tidyr)
library(patchwork)
library(tidyverse)
library(lubridate)
library(export)
library(reshape2)
library(ggpubr)
library(LaCroixColoR)

####Getting the data in the right format ####

qPCRdf <- read_csv(
  "20200402_microcosm_qPCR.csv",
  col_types = "cnnnfff")

qPCRlongish <- qPCRdf %>% pivot_longer(c("AOB", "Nitrospira", "Nitrobacter"), names_to = "Nitrifier", values_to = "copies")

#haven't figured out how to double check factors/rows etc after pivoting
#write.csv(qPCRlongish, "qPCRlongish.csv")
#manually editted to remove row names

qPCRlong <- read_csv(
  "qPCRlongish.csv",
  col_types = "cffffn")

qPCRlong$Weeks<-factor(qPCRlong$Weeks, levels=c('0','3','6','9'))
qPCRdf$Weeks<-factor(qPCRdf$Weeks, levels=c('0','3','6','9'))

####### Figure3A #######

Figure3A <- ggplot(data = qPCRlong) +
  aes(x = qPCRlong$Weeks) +
  labs(x = "Weeks") +
  aes(y = copies) +
  labs(y = "log10 copies/g soil") +
  geom_point(aes(col=Nitrifier), alpha = 0.5, size = 4) +
  theme_bw(base_size = 12, base_family = "Arial") +
  facet_grid(rows=vars(Treatment), cols=vars(Temperature)) +
  scale_colour_manual(name = "", values = c("#88005F","#009779", "#B48F00")) +
  theme(legend.position = c(0.91,0.085)) +
  scale_y_continuous(limits = c(3, 8)) +
  labs(title = "A")

plot(Figure3A)

#graph2doc(Figure3A,file="20200402_Figure3A.docx")

graph2tif(Figure3A,file="2020402_Figure3A.tif", pointsize = 10, width = 7, height = 8)

####### Figure3B #######

Figure3B <- ggplot(data = qPCRdf) +
  aes(x = AOB) +
  labs(x = "AOB (log10 copies/g soil)") +
  aes(y = Nitrobacter) +
  labs(y = "Nitrobacter (log10 copies/g soil)") +
  geom_point(aes(col = interaction(Temperature, Treatment)), size = 4) +
  geom_smooth(se = FALSE, method = lm, color = "#777777", size = 1) +
  scale_color_manual(values = lacroix_palette("paired", n = 6, type = "paired")) +
  theme_bw(base_size = 12, base_family = "Arial") +
  theme(legend.position = "bottom") +
  theme(legend.title = element_blank()) +
  labs(title = "B")

plot(Figure3B)

#graph2doc(Figure3B,file="20200402_Figure3B.docx")

graph2tif(Figure3B,file="2020402_Figure3B.tif", pointsize = 10, width = 7, height = 8)

####### Figure3Balt_nitrospira&AOB #######

Figure3Balt <- ggplot(data = qPCRdf) +
  aes(x = AOB) +
  labs(x = "AOB (log10 copies/g soil)") +
  aes(y = Nitrospira) +
  labs(y = "Nitrospira (log10 copies/g soil)") +
  geom_point(aes(col = interaction(Temperature, Treatment)), size = 4) +
  geom_smooth(se = FALSE, method = lm, color = "#777777", size = 1) +
  scale_color_manual(values = lacroix_palette("paired", n = 6, type = "paired")) +
  theme_bw(base_size = 12, base_family = "Arial") +
  theme(legend.position = "bottom") +
  theme(legend.title = element_blank()) +
  labs(title = "Balt")

plot(Figure3Balt)

#graph2doc(Figure3Balt,file="20200402_Figure3Balt.docx")

graph2tif(Figure3Balt,file="2020402_Figure3Balt.tif", pointsize = 10, width = 7, height = 8)

####### Figure3sup_facets #######

Figure3sup <- ggplot(data = qPCRdf) +
  aes(x = AOB) +
  labs(x = "AOB log10 copies/g soil") +
  aes(y = Nitrobacter) +
  labs(y = "Nitrobacter log10 copies/g soil") +
  geom_point(aes(col=Weeks), alpha = 0.5, size = 4) +
  geom_smooth(se = FALSE, method = lm, color = "#777777", size = 0.5) +
  theme_bw(base_size = 12, base_family = "Arial") +
  facet_grid(rows=vars(Treatment), cols=vars(Temperature)) +
  theme(legend.position = c(0.91,0.1)) +
  labs(title = "S3")

plot(Figure3sup)

#graph2doc(Figure3sup,file="20200402_Figure3sup.docx")

graph2tif(Figure3sup,file="2020402_Figure3sup.tif", pointsize = 10, width = 7, height = 8)


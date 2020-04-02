#this is the current script for generating nitrite scatter plot for all biopiles
#based on my previous 'scatterfacet' script

####setting the scene####
setwd("C:/Users/sally/OneDrive/Documents 201617/R/Nitritepaper/20200331_Figure2")
rm(list=ls())
Sys.setenv(PATH = paste(Sys.getenv("PATH"), "*InstallDirectory*/Rtools/bin/",
                        "*InstallDirectory*/Rtools/mingw_64/bin", sep = ";")) #for 64 bit version
Sys.setenv(BINPREF = "*InstallDirectory*/Rtools/mingw_64/bin")

#####all packages needed for this script, just install once####
install.packages("ggplot2")
install.packages("ggsci")
install.packages("tidyr")
install.packages("export")
install.packages("reshape2")
install.packages("ggpubr")
library(devtools)
devtools::install_github("thomasp85/patchwork")
devtools::install_github("tomwenseleers/export")
install.packages("export")

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

####Getting the data in the right format ####

biopileNutrient <- read_csv(
  "20200331_AllBiopile_Simplenutrients.csv",
  col_types = "cfnfnnnnnnnnnnn")

biopilelongish <- biopileNutrient %>% pivot_longer(c("Nitrite", "Nitrate", "Ammonium", "Ammonium_KCl", "Ammonium_Borate", "Phosphate", "Phosphate_bicarb"), names_to = "Nutrient", values_to = "ppm")
  
justnitrite <- filter(biopilelongish, Nutrient == "Nitrite")

#write.csv(justnitrite, "jutnitrite.csv")

#edited it manually in excel, removed nitrite column and row label, and the other non-biopiles.

nitrite_df <- read_csv(
  "justnitrite.csv",
  col_types = "cfnfnnnnn"
)

####### Figure2 #######

Figure2 <- ggplot(data = nitrite_df) +
  aes(x = Months) +
  labs(x = "Months") +
  aes(y = ppm) +
  labs(y = "Nitrite (mg/kg)") +
  geom_point(col= "#88005F", alpha = 0.5, size = 4) +
  theme_bw(base_size = 12, base_family = "Arial") +
  facet_grid(rows=vars(BiopileNumber), scales = "free", switch = "y") +
  scale_x_continuous(limits = c(0, 60)) +
  scale_y_continuous(limits = c(0, 200)) +
  labs(title = "Figure 2")

plot(Figure2)

#graph2doc(Figure2,file="20200331_Figure2.docx")

graph2tif(Figure2,file="20200331_Figure2.tif", pointsize = 10, width = 5, height = 20)


####### Figure2_notBP7 #######

not7 <- filter(nitrite_df, BiopileNumber != "7" )

Figure2not7 <- ggplot(data = not7) +
  aes(x = Months) +
  labs(x = "Months") +
  aes(y = ppm) +
  labs(y = "Nitrite (mg/kg)") +
  geom_point(col= "#88005F", alpha = 0.5, size = 4) +
  theme_bw(base_size = 12, base_family = "Arial") +
  facet_wrap("BiopileNumber") +
  scale_x_continuous(limits = c(0, 60)) +
  scale_y_continuous(limits = c(0, 200)) +
  labs(title = "Figure 2")

plot(Figure2not7)

#graph2doc(Figure2not7,file="20200331_Figure2.docx")

graph2tif(Figure2not7,file="20200331_Figure2not7.tif", pointsize = 10, aspectr=1.3)

####### Figure2_colorbymonth #######

#colour by month

Figure2month <- ggplot(data = nitrite_df) +
  aes(x = Months) +
  labs(x = "Months") +
  aes(y = ppm) +
  labs(y = "Nitrite (mg/kg)") +
  geom_point(aes(col= Month), alpha = 0.5, size = 4) +
  theme_bw(base_size = 12, base_family = "Arial") +
  facet_grid(nitrite_df$BiopileNumber) +
  scale_x_continuous(limits = c(0, 60)) +
  scale_y_continuous(limits = c(0, 200)) +
  labs(title = "Figure 2 by month")

plot(Figure2month)

#graph2doc(Figure2,file="20200331_Figure2month.docx")

graph2tif(Figure2month,file="20200331_Figure2month.tif", pointsize = 10, width = 10, height = 20)


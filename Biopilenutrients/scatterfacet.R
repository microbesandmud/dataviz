
setwd("C:/Users/test1/OneDrive/Documents 201617/R/Nitritepaper/BiopileNutrients")
rm(list=ls())

install.packages("glue")
library(glue)
install.packages("tidyr")
library(tidyr)

library(tidyverse)
library(lubridate)
library(extrafont)
library(ggdark)

#only once font_import()
loadfonts(device = "win", quiet = TRUE)

install.packages("rcartocolor")
library(rcartocolor)


install.packages("ggpubr")
library(ggpubr)

Sys.setenv(PATH = paste(Sys.getenv("PATH"), "*InstallDirectory*/Rtools/bin/",
                        "*InstallDirectory*/Rtools/mingw_64/bin", sep = ";")) #for 64 bit version
Sys.setenv(BINPREF = "*InstallDirectory*/Rtools/mingw_64/bin")
library(devtools)
devtools::install_github("thomasp85/patchwork")
library(patchwork)
library(tidyr)
install.packages("reshape2")
library(reshape2)

install.packages("wesanderson")
library(wesanderson)

devtools::install_github("tidyverse/tidyr")

devtools::install_github("johannesbjork/LaCroixColoR")
library(LaCroixColoR)

biopileNutrient <- read_csv(
  "20191023_biopileNutrients.csv",
  col_types = "ccnnfnnnnnnnnnnnnnnn")


biopilelongish <-
  melt(biopileNutrient, id.vars = c("Barcode", "UserCode", "Height", "Depth", "BiopileNumber", "Months"),
    cols = c("Nitrite", "Ammonium", "Nitrate"),
    variable.name = "Nutrient",
    value.names = "ppm",
  )

biopilelongish

somelongish <- filter(biopilelongish, (Nutrient == "Nitrite")|(Nutrient == "Ammonium")|(Nutrient == "Nitrate") )

somelongish


plotn <- (ggplot(somelongish, aes(x=Months, y=value)) +
  geom_point(aes(col=Nutrient)) +
    scale_color_manual(values = lacroix_palette("PassionFruit", n = 3)) +
  facet_grid(rows=vars(BiopileNumber), cols=vars(Nutrient)) +
  xlim(c(0, 60)) + 
  ylim(c(0, 2000)) + 
  labs(subtitle="Months Vs Nitrite", 
       y="ppm", 
       x="Months", 
       title="Figure1", 
       caption = "Source: Biopile soil Nutrients mastersheet") +
  theme(
    text = element_text(family = "Calibri", size = 12), 
    plot.title = element_text(size = 14),
    plot.caption = element_text(size = 8)))


plot(plotn)

plotall <- (ggplot(biopilelongish, aes(x=Months, y=value)) +
            geom_point(aes(col=Nutrient)) + 
              scale_color_manual(values = lacroix_palette("PassionFruit", n = 14, type = "continuous")) +
            facet_grid(rows=vars(BiopileNumber), cols=vars(Nutrient)) +
            xlim(c(0, 60)) + 
            ylim(c(0, 2000)) + 
            labs(subtitle="Months Vs Nitrite", 
                 y="Nitrite", 
                 x="Months", 
                 title="Scatterplot", 
                 caption = "Source: BiopileNutrients") +
  theme(
    text = element_text(family = "Calibri", size = 12), 
    plot.title = element_text(size = 14),
    plot.caption = element_text(size = 8)))


plot(plotall)


plotalllog2 <- (ggplot(biopilelongish, aes(x=Months, y=value)) +
              geom_point(aes(col=Nutrient)) + 
              scale_color_manual(values = lacroix_palette("PassionFruit", n = 14, type = "continuous")) +
              facet_grid(rows=vars(BiopileNumber), cols=vars(Nutrient)) +
              xlim(c(0, 60)) + 
              scale_y_continuous(trans='log2') +
              labs(subtitle="Months Vs Nitrite", 
                   y="Nitrite", 
                   x="Months", 
                   title="Scatterplot", 
                   caption = "Source: BiopileNutrients") +
  theme(
    text = element_text(family = "Calibri", size = 12), 
    plot.title = element_text(size = 14),
    plot.caption = element_text(size = 8)))

plot(plotalllog2)

plotalllog10 <- (ggplot(biopilelongish, aes(x=Months, y=value)) +
                  geom_point(aes(col=Nutrient)) + 
                  scale_color_manual(values = lacroix_palette("PassionFruit", n = 14, type = "continuous")) +
                  facet_grid(rows=vars(BiopileNumber), cols=vars(Nutrient)) +
                  xlim(c(0, 60)) + 
                  scale_y_continuous(trans='log10') +
                  labs(subtitle="Months Vs Nitrite", 
                       y="Nitrite", 
                       x="Months", 
                       title="Scatterplot", 
                       caption = "Source: BiopileNutrients") +
  theme(
    text = element_text(family = "Calibri", size = 12), 
    plot.title = element_text(size = 14),
    plot.caption = element_text(size = 8)))

plot(plotalllog10)

plotnlog10 <- (ggplot(somelongish, aes(x=Months, y=value)) +
            geom_point(aes(col=Nutrient)) +
            scale_color_manual(values = lacroix_palette("PassionFruit", n = 3)) +
            facet_grid(rows=vars(BiopileNumber), cols=vars(Nutrient)) +
            xlim(c(0, 60)) + 
              scale_y_continuous(trans='log10') +
            labs(subtitle="Months Vs Nitrite", 
                 y="ppm", 
                 x="Months", 
                 title="Figure1", 
                 caption = "Source: Biopile soil Nutrients mastersheet") +
              theme(
                text = element_text(family = "Calibri", size = 12), 
                plot.title = element_text(size = 14),
                plot.caption = element_text(size = 8)))


plot(plotnlog10)

plotnlog2 <- (ggplot(somelongish, aes(x=Months, y=value)) +
                 geom_point(aes(col=Nutrient)) +
                 scale_color_manual(values = lacroix_palette("PassionFruit", n = 3)) +
                 facet_grid(rows=vars(BiopileNumber), cols=vars(Nutrient)) +
                 xlim(c(0, 60)) + 
                 scale_y_continuous(trans='log2') +
                 labs(subtitle="Months Vs Nitrite", 
                      y="ppm", 
                      x="Months", 
                      title="Figure1", 
                      caption = "Source: Biopile soil Nutrients mastersheet") +
                theme(
                  text = element_text(family = "Calibri", size = 12), 
                  plot.title = element_text(size = 14),
                  plot.caption = element_text(size = 8)))


plot(plotnlog2)

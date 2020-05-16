#Using Wild_TRH data taken from "20191017_AA_BP7_Sample_Mapping_Mastersheets_Inc_dups_Oct2019_SC_JL.xlsx"
#Only selecting the four NEPM values and total area
#Removed all entries with no Wild_TRH data
#Removed all duplicates using find & replace on barcodes with "*.1"
#Renamed columns and saved as csv
#exploring some theme packages and the gghighlight

setwd("C:/Users/test1/OneDrive/Documents 201617/R/Nitritepaper/TRH_areaplot")
rm(list=ls())

library(tidyverse)
library(ggplot2)
library(tidyr)

#install.packages("tvthemes")
library(tvthemes)
library(extrafont)
loadfonts(quiet = TRUE)
library(devtools)
library(reshape2)
#install.packages("gghighlight")
library(gghighlight)

#devtools::install_github('cttobin/ggthemr')
library(ggthemr)
ggthemr('grape')
ggthemr_reset()

library(LaCroixColoR)


#install.packages("officer")
#install.packages("rvg")
#install.packages("openxlsx")
#install.packages("flextable")
#install.packages("xtable")
#install.packages("rgl")
#install.packages("stargazer")
#install.packages("tikzDevice")
#install.packages("xml2")
#install.packages("broom")
#devtools::install_github("tomwenseleers/export")

library(officer)
library(rvg)
library(openxlsx)
library(flextable)
library(xtable)
library(rgl)
library(stargazer)
library(tikzDevice)
library(xml2)
library(broom)
library(export)

WildTRH <- read_csv(
  "Wild_NEPM_TRH_Oct2019.csv",
  col_types = "cncfnnnnn")


#make it long format
TRHlongish <-
  melt(WildTRH, id.vars = c("Barcode","Months","Cut","Sample"),
       cols = c("C8toC10", "C10toC16", "C16toC34", "C34toC40", "TotalareaC10toC40"),
       variable.name = "SizeRange",
       value.names = "mg/kg",
  )

TRHlongish

#summarise into means
TRHmeans <- TRHlongish %>%
  group_by(Months, SizeRange) %>%
  summarize(mean_value = mean(value, na.rm = TRUE))

TRHmeans


#get rid of the total
Nottotal <- filter(TRHmeans, SizeRange != "TotalareaC10toC40" )

Nottotal
  
#plot area graph
TRHarea <- ggplot(data = Nottotal, aes(x = Months, y = mean_value, fill = SizeRange)) +
  geom_area(alpha = 0.9) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(title = stringr::str_wrap("Biopile 7 Hydrocarbons", width = 50),
       subtitle = "Hydrocarbon reductions largely due to degradation of C10 - C16 alkanes",
       caption = "WildTRH data, 20191017AAMastersheet",
       x = "Months", y = "Total area in range (mg/kg)")

TRHarea


#export area graph to ppt
   
graph2ppt(x=TRHarea,file="20191104_BP7_NEPM_TRH_areaplot_dark.pptx")



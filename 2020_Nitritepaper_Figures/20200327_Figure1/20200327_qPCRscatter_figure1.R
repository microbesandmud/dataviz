#this is the current script for creating biopile 7 scatter plots
#based on my previous "qPCRscatter" script

####setting the scene#####

setwd("C:/Users/sally/OneDrive/Documents 201617/R/Nitritepaper/20200327_Figure1")
rm(list=ls())
Sys.setenv(PATH = paste(Sys.getenv("PATH"), "*InstallDirectory*/Rtools/bin/",
                        "*InstallDirectory*/Rtools/mingw_64/bin", sep = ";")) #for 64 bit version
Sys.setenv(BINPREF = "*InstallDirectory*/Rtools/mingw_64/bin")

#####all packages needed for this script, just install once####
install.packages("ggplot2")
install.packages("ggsci")
install.packages("tidyr")
install.packages("export")
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

####Getting the data in the right format ####
qPCRdf <- read_csv("20200327_Figure1data.csv",
  col_types = "cnnnn")

#qPCRlongish <- qPCRdf %>% pivot_longer(c("AOB", "Nitrospira"), names_to = "Nitrifier", values_to = "copies/g soil")
#qPCRlongish$"log10 copies/g soil" <- log10(qPCRlongish$"copies/g soil")
#write.csv(qPCRlongish, "qPCRlongish.csv")
#got frustrated and exported, then manually edited longish

qPCRlongish <- read_csv("qPCRlongish.csv",
                   col_types = "cnnfnn")
####### Figure1A #######

Figure1A <- ggplot(data = qPCRlongish) +
  aes(x = Months) +
  labs(x = "Months") +
  aes(y = log10copies) +
  labs(y = "log10 copies/g soil") +
  geom_point(aes(col=Nitrifier), alpha = 0.5, size = 4) +
  theme_bw(base_size = 12, base_family = "Arial") +
  theme(legend.position = c(0.85,0.2)) +
  scale_colour_manual(name = "", values = c("#88005F","#009779")) +
  scale_x_continuous(limits = c(0, 60)) +
  labs(title = "A")
  
plot(Figure1A)

#graph2doc(Figure1A,file="20200328_Figure1.docx")

graph2tif(Figure1A,file="20200328_Figure1A.tif", pointsize = 10, aspectr = 1.3)

####### Figure1B #######
Figure1B <- ggplot(data = qPCRdf) +
  aes(x = Months) +
  labs(x = "Months") +
  aes(y = (Nitrospira/AOB)) +
  labs(y = "ratio Nitrospira:AOB (copies/g)") +
  geom_point(col= "#88005F", alpha = 0.5, size = 4) +
  theme_bw(base_size = 12, base_family = "Arial") +
  scale_y_log10(limits = c(0.01, 100), breaks = c(0.01, 0.1, 1, 10, 100), labels = c(0.01,0.1,1,10,100)) +
  geom_hline(yintercept = c(1), lty = 2, col = "grey") +
  scale_x_continuous(limits = c(0, 60)) +
  labs(title = "B")

plot(Figure1B)

#graph2doc(Figure1B,file="20200328_Figure1.docx", width=9, aspectr=sqrt(2), append=TRUE)

graph2tif(Figure1B,file="20200328_Figure1B.tif", pointsize = 10, aspectr = 1.3)

####### Figure1C #######

Figure1C <- ggplot(data = qPCRdf) +
  aes(x = Months) +
  labs(x = "Months") +
  aes(y = Nitrite) +
  labs(y = "Nitrite (mg/kg)") +
  geom_point(col= "#88005F", alpha = 0.5, size = 4) +
  theme_bw(base_size = 12, base_family = "Arial") +
  scale_y_continuous(limits = c(0, 200)) +
  scale_x_continuous(limits = c(0, 60)) +
  labs(title = "C")

plot(Figure1C)

#graph2doc(Figure1C,file="20200328_Figure1C.docx")

graph2tif(Figure1C,file="20200328_Figure1C.tif", pointsize = 10, aspectr = 1.3)

####### Figure1D #######

Figure1D <- ggplot(data = qPCRdf) +
  aes(x = Nitrite) +
  labs(x = "Nitrite (mg/kg)") +
  aes(y = (Nitrospira/AOB)) +
  labs(y = "ratio Nitrospira:AOB (copies/g)") +
  geom_point(col= "#88005F", alpha = 0.5, size = 4) +
  theme_bw(base_size = 12, base_family = "Arial") +
  geom_hline(yintercept = c(1), lty = 2, col = "grey") +
  scale_y_log10(limits = c(0.01, 100), breaks = c(0.01, 0.1, 1, 10, 100), labels = c(0.01,0.1,1,10,100)) +
  scale_x_continuous(limits = c(0, 200)) +
  labs(title = "D")

plot(Figure1D)

#graph2doc(Figure1D,file="20200328_Figure1D.docx")

graph2tif(Figure1D,file="20200328_Figure1D.tif", pointsize = 10, aspectr = 1.3)

####### Figure1D_notlog #######

Figure1D_notlog <- ggplot(data = qPCRdf) +
  aes(x = Nitrite) +
  labs(x = "Nitrite (mg/kg)") +
  aes(y = (Nitrospira/AOB)) +
  labs(y = "ratio Nitrospira:AOB (copies/g)") +
  geom_point(col= "#88005F", alpha = 0.5, size = 4) +
  theme_bw(base_size = 12, base_family = "Arial") +
  geom_hline(yintercept = c(1), lty = 2, col = "grey") +
  scale_x_continuous(limits = c(0, 200)) +
  labs(title = "Figure 1D (alternative, not log)")

plot(Figure1D_notlog)

#graph2doc(Figure1D_notlog,file="20200328_Figure1D_notlog.docx")

graph2tif(Figure1D_notlog,file="20200328_Figure1D_notlog.tif", pointsize = 10, aspectr = 1.3)

####### Composite Figure1 #######

Figure1 <- ((Figure1A + Figure1B)/(Figure1C + Figure1D)) 

plot (Figure1, width = 7)


graph2tif(Figure1,file="20200328_Figure1.tif", pointsize = 8, width = 10, height = 7)



###### discards #####
monthsasfactors <- read_csv("qPCRlongish.csv",
                            col_types = "cfnfnn")
Figure1Afactors <- ggplot(data = monthsasfactors) +
  aes(x = Months) +
  labs(x = "Months") +
  aes(y = log10copies) +
  labs(y = "log10 copies/g soil") +
  aes(col = fct_inorder(Nitrifier)) +
  geom_jitter(width = .01, height = 0, size = 4, alpha = 0.5) +
  theme_bw(base_size = 12, base_family = "Times") +
  scale_colour_manual(name = "", values = c("darkblue", "goldenrod3")) +
  scale_x_discrete(limits = c(0, 60)) +
  labs(title = "Figure 1A")
plot(Figure1Afactors)








aes(col = fct_inorder(value_type)) +
  aes(alpha = fct_inorder(value_type)) +
  aes(shape = fct_inorder(value_type)) +
  geom_hline(yintercept = c(0, 100), col = "grey") +
  geom_hline(yintercept = c(50), lty = 2, col = "grey") +
  
  
figure1A <- (ggplot(qPCRlongish, aes(x=Months, y=log10copies)) +
               geom_point(aes(col=Nitrifier), alpha = 0.5, size = 3) +
               xlim(c(0, 60)) + 
               ylim(c(3.25, 8.5)) + 
               labs(subtitle="Nitrifier Abundances", 
                    y="log10 copies/g soil (w/w)", 
                    x="Months", 
                    title="Figure3 - Biopile", 
                    caption = "Source:  AA Sample Mapping Mastersheet") +
               theme(
                 text = element_text(family = "Calibri", size = 12), 
                 plot.title = element_text(size = 14),
                 plot.caption = element_text(size = 8)))
plot(figure1A)

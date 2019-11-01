setwd("C:/Users/test1/OneDrive/Documents 201617/R/Nitritepaper/qPCR_scatter")
rm(list=ls())

install.packages("ggsci")
library(ggsci)

Sys.setenv(PATH = paste(Sys.getenv("PATH"), "*InstallDirectory*/Rtools/bin/",
                        "*InstallDirectory*/Rtools/mingw_64/bin", sep = ";")) #for 64 bit version
Sys.setenv(BINPREF = "*InstallDirectory*/Rtools/mingw_64/bin")
library(devtools)
devtools::install_github("thomasp85/patchwork")
library(patchwork)


qPCRdf <- read_csv(
  "all_qPCR.csv",
  col_types = "cnnnfffn")

qPCRnitrite <- read_csv(
  "qPCR_biopile_notlog_nitrite.csv",
  col_types = "cncnnn")


qPCRlongish <-
  melt(qPCRdf, id.vars = c("Sample","Treatment","Temperature","Experiment","Weeks"),
       cols = c("AOB", "Nitrospira", "Nitrobacter"),
       variable.name = "Nitrifier",
       value.names = "copies/g soil",
  )

qPCRlongish

biopilelongish <- filter(qPCRlongish, (Experiment == "Biopile")& ((Nitrifier == "AOB")|(Nitrifier == "Nitrospira")) )

biopilelongish

microcosmlongish <- filter(qPCRlongish, (Experiment == "Microcosm") )

microcosmlongish

microcomss <- filter(qPCRdf, (Experiment == "Microcosm") )



plotm <- (ggplot(microcosmlongish, aes(x=Weeks, y=value)) +
            geom_point(aes(col=Nitrifier), alpha = 0.3, size = 3) +
            scale_color_manual(values = lacroix_palette("PassionFruit", n = 3)) +
            facet_grid(rows=vars(Treatment), cols=vars(Temperature)) +
            xlim(c(0, 9)) + 
            ylim(c(3.5, 8)) + 
            labs(subtitle="Nitrifier Abundances", 
                 y="log10 copies/g soil (w/w)", 
                 x="Weeks", 
                 title="Figure4 - Microcosm", 
                 caption = "Source: Microcosm qPCR results") +
            theme(
              text = element_text(family = "Calibri", size = 12), 
              plot.title = element_text(size = 14),
              plot.caption = element_text(size = 8)))


plot(plotm)



plotlin <- (ggplot(microcomss, aes(x=AOB, y=Nitrobacter)) +
              geom_point(aes(colour = interaction(Temperature, Treatment), shape = Temperature, size = Weeks), alpha = 0.7) +
              scale_color_manual(values = lacroix_palette("paired", n = 6, type = "paired")) +
              geom_smooth(se = FALSE, method = loess, color = '#172869') +
            xlim(c(5, 8)) + 
            ylim(c(3, 7)) + 
            labs(subtitle="Nitrifiers", 
                 y="Nitrobacter log10 copies/g soil (w/w)", 
                 x="AOB log10 copies/g soil (w/w)", 
                 title="Figure5 - Microcosm ratio", 
                 caption = "Source: Microcosm qPCR results") +
            theme(
              text = element_text(family = "Calibri", size = 12), 
              plot.title = element_text(size = 14),
              plot.caption = element_text(size = 8)))


plot(plotlin)


plotb <- (ggplot(biopilelongish, aes(x=(Weeks/4), y=value)) +
            geom_point(aes(col=Nitrifier), alpha = 0.5, size = 3) +
            scale_color_manual(values = lacroix_palette("PassionFruit", n = 3)) +
            xlim(c(0, 60)) + 
            ylim(c(3, 8)) + 
            labs(subtitle="Nitrifier Abundances", 
                 y="log10 copies/g soil (w/w)", 
                 x="Months", 
                 title="Figure3 - Biopile", 
                 caption = "Source:  AA Sample Mapping Mastersheet") +
            theme(
              text = element_text(family = "Calibri", size = 12), 
              plot.title = element_text(size = 14),
              plot.caption = element_text(size = 8)))


plot(plotb)


plotr <- (ggplot(qPCRnitrite, aes(x=(Months), y=(Nitrospira/AOB))) +
            geom_point(color = '#C70E7B',alpha = 0.5, size = 3) +
            xlim(c(0, 60)) + 
            scale_y_continuous(trans='log10') +
            labs(subtitle="Nitrifier Abundances", 
                 y="Nitrospira:AOB ratio", 
                 x="Months", 
                 title="Figure3 - Biopile", 
                 caption = "Source:  AA Sample Mapping Mastersheet") +
            theme(
              text = element_text(family = "Calibri", size = 12), 
              plot.title = element_text(size = 14),
              plot.caption = element_text(size = 8)))


plot(plotr)

plotn <- (ggplot(qPCRnitrite, aes(x=(Nitrite), y=(Nitrospira/AOB))) +
            geom_point(color = '#C70E7B',alpha = 0.5, size = 3) +
            xlim(c(0, 60)) + 
            scale_y_continuous(trans='log10') +
            labs(subtitle="Nitrifier Abundances", 
                 y="Nitrospira:AOB ratio", 
                 x="Nitrite N mg/kg", 
                 title="Figure3 - Biopile", 
                 caption = "Source:  AA Sample Mapping Mastersheet") +
            theme(
              text = element_text(family = "Calibri", size = 12), 
              plot.title = element_text(size = 14),
              plot.caption = element_text(size = 8)))


plot(plotn)


plotb + plotm + plotn

plotx <- (plotb/plotn)

plotx | plotm



#ternary plot script
#adapted from jkaupp's tidy tuesday post 19th July 2019
#see https://github.com/jkaupp/tidytuesdays/blob/master/2019/week29/R/analysis.R

setwd("C:/Users/test1/OneDrive/Documents 201617/R/Ternaryplot")
rm(list=ls())

#bug fix for Rtools
  
  # Set path of Rtools
  Sys.setenv(PATH = paste(Sys.getenv("PATH"), "*InstallDirectory*/Rtools/bin/",
                          "*InstallDirectory*/Rtools/mingw_64/bin", sep = ";")) #for 64 bit version
  Sys.setenv(BINPREF = "*InstallDirectory*/Rtools/mingw_64/bin")
  library(devtools)

  #Manually "force" version to be accepted 

  assignInNamespace("version_info", c(devtools:::version_info, list("3.5" = list(version_min = "3.3.0", version_max = "99.99.99", path = "bin"))), "devtools")
  find_rtools() # is TRUE now


#jkmisc personal package, so it 

library(devtools)
install.packages("stringi")
library(stringi)
install_github("clauswilke/gridtext")
install_github("clauswilke/ggtext")
install_github("jkaupp/jkmisc")

install.packages("tricolore")
install.packages("ggtern")
install.packages("here")
install.packages("jkmisc")
install.packages("magick")


library(tidyverse)
library(tricolore)
library(ggtern)
library(here)
library(jkmisc)
library(magick)



givitago <- read_csv("TPH_nitrite_ammonium_nodupes_nomissing.csv")

normalizeData <- function(givitago,norm100=TRUE){
  # input a data.frame object with named columns, e.g. df <- data.frame(a=c(1,2),b=c(2,3))
  # if norm100 is TRUE, a base 100 [%] normalized dataframe will be retured.
  # Otherwise the data will be normalized to 1.0
  if(norm100){
    normFactor = 100
  } else {
    normFactor = 1.0
  }
  fracs <- data.frame()
  for(colNr in seq(givitago)){
    # normalizes each column and add data inplace, so all colnames stay the same
    givitago[colNr] = givitago[colNr]/sum(givitago[colNr]) * normFactor
  }
  return(givitago)
}

# 0. Normalize Data
# normalizeData(df,norm100=TRUE)

normalizeData(givitago,norm100=TRUE)

tern_plot <- Tricolore(givitago, "TPH",
                       "Nitrite",
                       "Ammonium", breaks = 5, show_data = FALSE)
tern_plot

legend <- tern_plot$key +
  labs(title = "Color Legend",
       x       = "TPH",
       y       = "Nitrite",
       z       = "Ammonium") +
  theme_hidetitles() +
  theme_hidelabels() +
  theme_hideticks() +
  theme(plot.title = element_text(hjust = 0.5, family = "Scope One", size = 40),
        axis.text = element_text(family = "Scope One"),
        axis.title = element_text(family = "Scope One"))

png("legend.png") 
legend
dev.off()

legend <- image_read("legend.png")

plot <- givitago %>% 
  mutate(color = tern_plot$rgb) %>% 
  ggtern(aes(x = TPH, y = Nitrite, z = Ammonium, color = color)) +
  geom_point(size = 3) +
  scale_color_identity() +
  labs(title = str_to_title("The Dialogue in the R4DS Slack indicates an Open and Inclusive Learning Community"),
       subtitle = str_wrap("Below is a ternary digram presenting the message composition in public channels, private channels and direct messages as a percentage.  Each day is represented by a point with the composition represented by position relative to each axes.  Composition is additionally encoded by color as illustrated on the inset legend.", 100),
       x       = "TPH",
       xarrow  = "More TPH",
       y       = "Nitrite",
       yarrow  = "More Nitrite",
       z       = "Ammonium",
       zarrow  = "More Ammonium",
       caption = "Data: R4DS Community | Graphic: @jakekaupp") +
  theme(panel.background = element_rect(fill = "#2E3440"),
        panel.grid = element_line(color = "#ffffff", size = 0.1),
        panel.grid.minor = element_blank(),
        text = element_text(family = "Oswald"),
        plot.subtitle = element_text(family = "Scope One"),
        axis.text = element_text(family = "Scope One"),
        axis.title = element_text(family = "Scope One")) +
  theme_showarrows() +
  theme_arrowlong() 

plot


png("tw29_plot.png", width = 10, height = 8, units = "in", res = 200)
grid::grid.newpage()
plot
grid::grid.raster(legend, width = 0.18, height = 0.2, x = 0.75, y = 0.7)
dev.off()

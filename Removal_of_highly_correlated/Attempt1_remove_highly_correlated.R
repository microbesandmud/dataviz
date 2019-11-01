setwd("C:/Users/test1/OneDrive/Documents 201617/R/Removal_of_highly_correlated")
rm(list=ls())
install.packages("ggplot2")
install.packages("corrplot")
install.packages("car")
install.packages("gridExtra")
install.packages("caret")
install.packages("dplyr")
library ("ggplot2") 
library ("corrplot")
library ("car")
library ("gridExtra")
library ("caret")
library ("dplyr")
'%!in%' <- function(x,y)!('%in%'(x,y))
data <- read.csv("./justtg_ncyc_simple.csv",header=T,row.names=1)
class(data)
rc <- sapply(data,is.numeric) %>% data[,.]
zv <- apply(rc, 2, function(x) length(unique(x)) == 1)
sum(zv)
rc <- rc[, !zv]
cor_rc <- cor(rc)
cor_rc_plot <- cor_rc
hcr_rc <- findCorrelation(cor_rc,cutoff=0.999,names=TRUE)
if(length(hcr_rc)!= 0){
  rc <- rc[,names(rc) %!in% hcr_rc]
  cor_rc_plot <- cor_rc_plot[which(rownames(cor_rc_plot) %!in% hcr_rc),
                             which(colnames(cor_rc_plot) %!in% hcr_rc)]
}
scatterplotMatrix(rc)
cex.before <- par("cex")
par(cex=0.5)
corrpl <- corrplot.mixed(cor_rc_plot,lower="number",upper="circle",
                         order="AOE",tl.cex=0.7,tl.col="black",tl.srt=60,cl.cex=1/par("cex"))
save.image('./rmc.rdata')


library(foreign)
library(ooc)
library(ggplot2)
library(viridis)
library(weights)
library(xtable)
library(nnet)
library(effects)
library(corrplot)
library(ggrepel)
library(scales)
library(reshape)
library(MCMCpack)
library(apsrtable)
library(gplots)
library(statar)
library(gridExtra)
library(dplyr)

set.seed(1985)

UTAS2012 <- read.csv("./utas12_ooc.csv", header=TRUE)

UTAS2012 <- UTAS2012[UTAS2012$cv=="voter",]

UTAS2012 <- UTAS2012[,c(4,8:12,15:54)]

partyid <- vector()
for(i in 1:nrow(UTAS2012)){
  if(UTAS2012$psup_short[i]=="LDP"){
    partyid[i] <- "LDP"
  }else if(UTAS2012$psup_short[i]=="DPJ"){
    partyid[i] <- "DPJ"
  }else if(UTAS2012$psup_short[i]=="JRP"){
    partyid[i] <- "JRP"
  }
}


result.issuesvalues.dim2 <- ooc(UTAS2012[,7:42], dims=2, minvotes=10, lop=0.001, polarity=c(194,194), iter=25, nv.method="svm.reg", cost=1)

idealpoints.issuesvalues <- result.issuesvalues.dim2$respondents[,grepl("coord", colnames(result.issuesvalues.dim2$respondents))]

idealpoints.issuesvalues <- data.frame(idealpoints.issuesvalues, UTAS2012$psup_short)

utas_id <- idealpoints.issuesvalues %>% filter(UTAS2012.psup_short == "LDP" | UTAS2012.psup_short == "DPJ" | UTAS2012.psup_short == "JRP")
colnames(utas_id) <- c("coord1D","coord2D","party")

pdf("utas2012_ooc.pdf", width=5.8, height=4.6)
ggplot(data=utas_id, aes(x=coord1D, y=coord2D, color=party)) + geom_point(size=2, alpha=0.6) +
  xlim(-1,1) +
  ylim(-1,1) +
  xlab("\nPC1") +
  ylab("PC2\n") +
  ggtitle("OOC Result of UTAS 2012") +
  scale_colour_manual(values = c( '#F08E39','#DF585C','#507AA6'), labels=c("DPJ","JRP","LDP")) +
  theme(plot.title = element_text(hjust = 0.5), legend.position = c(0.97, 0.97),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.background = element_rect(colour="#DCDCDC", fill = NA),
        legend.key = element_rect(colour = "transparent", fill = NA),
        legend.margin = margin(1, 1, 1, 1),
        legend.title=element_blank())
dev.off()

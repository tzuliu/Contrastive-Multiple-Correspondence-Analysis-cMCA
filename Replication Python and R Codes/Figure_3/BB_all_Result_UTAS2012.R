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
library(basicspace)

set.seed(1985)

UTAS2012 <- read.csv("./data/utas12_ooc.csv", header=TRUE)

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
  } else if(!is.na(UTAS2012$psup_short[i])){
    partyid[i] <- "OTH"
  }
}

dt <- UTAS2012[,7:46]

for(i in 1:ncol(dt)){
  for(j in 1:nrow(dt)){
    if(is.na(dt[j,i])){
      dt[j,i] <- 99
    }
  }
}

for(i in 1:ncol(dt)){
    dt[,i] <- as.numeric(dt[,i])
}

testr <- blackbox(dt, missing=99, dims=2, minscale=5, verbose=TRUE)
xx <- testr$individuals[[2]][,1]
yy <- testr$individuals[[2]][,2]

bbdt <- cbind.data.frame(xx,yy,partyid)
colnames(bbdt) <- c("coord1D", "coord2D", "party")

bbdt_n <- bbdt %>% filter(party == "LDP" | party == "DPJ" | party == "JRP")

pdf("utas2012_bb.pdf", width=5.8, height=4.6)
ggplot(data=bbdt_n, aes(x=coord1D, y=coord2D, color=party)) + geom_point(size=2, alpha=0.6) +
  xlim(-1,1) +
  ylim(-1,1) +
  xlab("\nPC1") +
  ylab("PC2\n") +
  ggtitle("BlackBox Result of UTAS 2012") +
  scale_colour_manual(values = c( '#F08E39','#DF585C','#507AA6'), labels=c("DPJ","JRP","LDP")) +
  theme(plot.title = element_text(hjust = 0.5), legend.position = c(0.97, 0.97),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.background = element_rect(colour="gray85", fill = "gray93"),
        legend.key = element_rect(colour = "transparent", fill = NA),
        legend.margin = margin(1, 6, 1, 1),
        legend.title=element_blank())
dev.off()

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

ESS2018 <- read.csv("./uk2018.csv", header=TRUE)

ESS2018 <- ESS2018[,c(2:24,26)]

partyid <- vector()
for(i in 1:nrow(ESS2018)){
    if(!is.na(ESS2018$prtclcgb[i])){
        if(ESS2018$prtclcgb[i]==1){
            partyid[i] <- "Con"
        }else if(ESS2018$prtclcgb[i]==2){
            partyid[i] <- "Lab"
        }else if(ESS2018$prtclcgb[i]==3){
            partyid[i] <- "LD"
        }else if(ESS2018$prtclcgb[i]==4){
            partyid[i] <- "SNP"
        }else if(ESS2018$prtclcgb[i]==6){
            partyid[i] <- "Green"
        }else if(ESS2018$prtclcgb[i]==7){
            partyid[i] <- "UKIP"
        }else if(!is.na(ESS2018$prtclcgb[i])){
            partyid[i] <- "Other"
        }
    }
}

dt <- ESS2018[,1:23]

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

bbdt_n <- bbdt %>% filter(party == "Con" | party == "Lab" | party == "LD" | party == "SNP" | party == "Green" | party == "UKIP" | party == "Other")

pdf("ess2018_bb.pdf", width=5.8, height=4.6)
ggplot(data=bbdt_n, aes(x=coord1D, y=coord2D, color=party)) + geom_point(size=2, alpha=0.6) +
  xlim(-1,1) +
  ylim(-1,1) +
  xlab("\nPC1") +
  ylab("PC2\n") +
  ggtitle("BlackBox Result of ESS 2018") +
  scale_colour_manual(values = c('#507AA6','#F08E39','#DF585C','#5BA053','#AF7BA1','#ECC854','#9A7460'), labels=c("Con","Lab","LD","SNP","Green","UKIP","Other")) +
  theme(plot.title = element_text(hjust = 0.5), legend.position = c(0.98, 0.98),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.background = element_rect(colour="gray85", fill = "gray93"),
        legend.key = element_rect(colour = "transparent", fill = NA),
        legend.margin = margin(1, 6, 1, 1),
        legend.title=element_blank())
dev.off()

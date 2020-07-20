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

result.issuesvalues.dim2 <- ooc(ESS2018[,1:23], dims=2, minvotes=10, lop=0.001, polarity=c(1131,1131), iter=25, nv.method="svm.reg", cost=1)

idealpoints.issuesvalues <- result.issuesvalues.dim2$respondents[,grepl("coord", colnames(result.issuesvalues.dim2$respondents))]

idealpoints.issuesvalues <- data.frame(idealpoints.issuesvalues, partyid)

ess_id <- idealpoints.issuesvalues %>% filter(partyid == "Con" | partyid == "Lab" | partyid == "LD" | partyid == "SNP" | partyid == "Green" | partyid == "UKIP" | partyid == "Other")
colnames(ess_id) <- c("coord1D","coord2D","party")

pdf("ess2018_occ.pdf", width=5.8, height=4.6)
ggplot(data=ess_id, aes(x=coord1D, y=coord2D, color=party)) + geom_point(size=2, alpha=0.6) +
  xlim(-1,1.4) +
  ylim(-1,1) +
  xlab("\nPC1") +
  ylab("PC2\n") +
  ggtitle("OOC Result of ESS 2018") +
  scale_colour_manual(values = c('#507AA6','#F08E39','#DF585C','#5BA053','#AF7BA1','#ECC854','#9A7460'), labels=c("Con","Lab","LD","SNP","Green","UKIP","Other")) +
  theme(plot.title = element_text(hjust = 0.5), legend.position = c(0.98, 0.05),
        legend.justification = c("right", "bottom"),
        legend.box.just = "right",
        legend.background = element_rect(colour="#DCDCDC", fill = NA),
        legend.key = element_rect(colour = "transparent", fill = NA),
        legend.margin = margin(1, 1, 1, 1),
        legend.title=element_blank())
dev.off()

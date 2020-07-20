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

CCES2015 <- read.dta("./CCES15_UGA_OUTPUT_Jan2016.dta",convert.factors = FALSE)

nresp.CCES2015 <- nrow(CCES2015)
partyid <- CCES2015$pid7
#partyid[partyid < 1 | partyid > 7] <- NA
partyid[CCES2015$pid7==1] <- 1
partyid[CCES2015$pid7==2] <- 1
partyid[CCES2015$pid7==3] <- 1
partyid[CCES2015$pid7==4] <- 2
partyid[CCES2015$pid7==5] <- 2
partyid[CCES2015$pid7==6] <- 2
partyid[partyid < 1 | partyid > 2] <- NA
partyid <- factor(partyid, levels=c(1, 2), labels=c("DEM", "REP"))
ideology <- CCES2015$CC15_340a
ideology[ideology < 1 | ideology > 7] <- NA
socialideology <- CCES2015$UGA310A
socialideology[socialideology < 1 | socialideology > 7] <- NA
economicideology <- CCES2015$UGA310B
economicideology[economicideology < 1 | economicideology > 7] <- NA
natsecurityideology <- CCES2015$UGA310C
natsecurityideology[natsecurityideology < 1 | natsecurityideology > 7] <- NA
voted2012 <- CCES2015$CC15_315
voted2012[CCES2015$CC15_315==4] <- 1
voted2012[CCES2015$CC15_315==5] <- 1
voted2012[CCES2015$CC15_315==1] <- 2
voted2012[CCES2015$CC15_315==2] <- 2
voted2012[CCES2015$CC15_315==3] <- 2
voted2012[voted2012 < 1 | voted2012 > 2] <- NA
romneyvote <- CCES2015$CC15_315
romneyvote[romneyvote > 2] <- NA
teapartypositive <- CCES2015$UGA305E
teapartypositive[teapartypositive > 100] <- NA
teapartypositive[teapartypositive >= 0 & teapartypositive <= 15] <- 1
teapartypositive[teapartypositive >= 16 & teapartypositive <= 35] <- 2
teapartypositive[teapartypositive >= 36 & teapartypositive <= 55] <- 3
teapartypositive[teapartypositive >= 56 & teapartypositive <= 85] <- 4
teapartypositive[teapartypositive >= 86 & teapartypositive <= 100] <- 5
news_interest <- CCES2015$newsint
news_interest[news_interest > 4] <- NA
news_interest <- -1 * (news_interest - 5)
knowledge_correcthouse <- CCES2015$CC15_310a
knowledge_correcthouse <- knowledge_correcthouse + 1
knowledge_correcthouse[knowledge_correcthouse!=2 | is.na(knowledge_correcthouse)] <- 1
knowledge_correctsenate <- CCES2015$CC15_310b
knowledge_correctsenate <- knowledge_correctsenate + 1
knowledge_correctsenate[knowledge_correctsenate!=2 | is.na(knowledge_correctsenate)] <- 1
knowledge_correcthousesenate <- NA
knowledge_correcthousesenate[knowledge_correcthouse==2 & knowledge_correctsenate==2] <- 2
knowledge_correcthousesenate[is.na(knowledge_correcthousesenate)] <- 1
dimensionsalience <- CCES2015$UGA311
dimensionsalience[dimensionsalience < 1 | dimensionsalience > 7] <- NA
age <- 2015 - CCES2015$birthyr
age[age>=18 & age <=29] <- 1
age[age>=30 & age <=44] <- 2
age[age>=45 & age <=59] <- 3
age[age>=60] <- 4
female <- CCES2015$gender
female[female > 2] <- NA
education <- CCES2015$educ
education[education==1 | education==2] <- 1
education[education==3 | education==4] <- 2
education[education==5 | education==6] <- 3
black <- CCES2015$race
black[black!=2] <- 1
black[black==2] <- 2
hispanic <- CCES2015$race
hispanic[hispanic!=3] <- 1
hispanic[hispanic==3] <- 2
georegion <- CCES2015$region #NMSW
georegion[georegion > 4] <- NA
income <- CCES2015$faminc
income[CCES2015$faminc>=1 & CCES2015$faminc <=3] <- 1
income[CCES2015$faminc>=4 & CCES2015$faminc <=6] <- 2
income[CCES2015$faminc>=7 & CCES2015$faminc <=9] <- 3
income[CCES2015$faminc>=10 & CCES2015$faminc <=16] <- 4
income[income > 4] <- NA
homeownership <- CCES2015$ownhome
homeownership[homeownership > 1] <- 0
homeownership <- homeownership + 1
stockownership <- CCES2015$investor
stockownership <- -1 * (stockownership - 3)
stockownership[stockownership > 2] <- NA
unionmember <- NA
unionmember[CCES2015$union==1 | CCES2015$union==2 | CCES2015$unionhh==1 | CCES2015$unionhh==2] <- 2
unionmember[CCES2015$union==3 & CCES2015$unionhh==3] <- 1
unionmember[unionmember > 2] <- NA
bornagain <- CCES2015$pew_bornagain
bornagain <- -1 * (bornagain - 3)
bornagain[bornagain > 2] <- NA
bornagain <- (bornagain-min(bornagain, na.rm=TRUE))/(max(bornagain, na.rm=TRUE)-min(bornagain, na.rm=TRUE))
importancereligion <- CCES2015$pew_religimp
importancereligion <- -1 * (importancereligion - 5)
importancereligion[importancereligion > 4] <- NA
importancereligion <- (importancereligion-min(importancereligion, na.rm=TRUE))/(max(importancereligion, na.rm=TRUE)-min(importancereligion, na.rm=TRUE))
churchattendance <- CCES2015$pew_churatd
churchattendance[churchattendance > 6] <- NA
churchattendance <- -1 * (churchattendance - 7)
churchattendance <- (churchattendance-min(churchattendance, na.rm=TRUE))/(max(churchattendance, na.rm=TRUE)-min(churchattendance, na.rm=TRUE))
frequencyprayer <- CCES2015$pew_prayer
frequencyprayer[frequencyprayer > 7] <- NA
frequencyprayer <- -1 * (frequencyprayer - 8)
frequencyprayer <- (frequencyprayer-min(frequencyprayer, na.rm=TRUE))/(max(frequencyprayer, na.rm=TRUE)-min(frequencyprayer, na.rm=TRUE))
religiositymat <- cbind(bornagain, importancereligion, churchattendance, frequencyprayer)
#alpha(religiositymat)
religiosity.scores <- rowMeans(religiositymat)
Q <- quantile(religiosity.scores, na.rm=TRUE)
religiosity.Qs <- NA
religiosity.Qs[religiosity.scores >= Q[1] & religiosity.scores < Q[2]] <- 1
religiosity.Qs[religiosity.scores >= Q[2] & religiosity.scores < Q[3]] <- 2
religiosity.Qs[religiosity.scores >= Q[3] & religiosity.scores < Q[4]] <- 3
religiosity.Qs[religiosity.scores >= Q[4] & religiosity.scores <= Q[5]] <- 4
repealACA <- CCES2015$CC15_327A
repealACA <- -1 * (repealACA - 3)
repealACA[repealACA > 2] <- NA
approvekeystone <- CCES2015$CC15_327B
approvekeystone <- -1 * (approvekeystone - 3)
approvekeystone[approvekeystone > 2] <- NA
iransanctions <- CCES2015$CC15_327C
iransanctions <- -1 * (iransanctions - 3)
iransanctions[iransanctions > 2] <- NA
transpacificpartnershipact <- CCES2015$CC15_327D
transpacificpartnershipact <- -1 * (transpacificpartnershipact - 3)
transpacificpartnershipact[transpacificpartnershipact > 2] <- NA
normalizecubarelations <- CCES2015$CC15_327E
normalizecubarelations <- -1 * (normalizecubarelations - 3)
normalizecubarelations[normalizecubarelations > 2] <- NA
renewpatriotact <- CCES2015$CC15_327F1
renewpatriotact[renewpatriotact > 2] <- NA
renewpatriotact <- (renewpatriotact - 3) * -1
usafreedomact <- CCES2015$CC15_327F2
usafreedomact[usafreedomact > 2] <- NA
usafreedomact <- (usafreedomact - 3) * -1
tradeadjustmentact <- CCES2015$CC15_327G
tradeadjustmentact[tradeadjustmentact > 2] <- NA
tradeadjustmentact <- (tradeadjustmentact - 3) * -1
VAWA <- CCES2015$CC15_327H
VAWA[VAWA > 2] <- NA
VAWA <- (VAWA - 3) * -1
cutmedicaremedicaid <- CCES2015$CC15_330a
cutmedicaremedicaid <- -1 * (cutmedicaremedicaid - 3)
cutmedicaremedicaid[cutmedicaremedicaid > 2] <- NA
budgetcutsacrossboard <- CCES2015$CC15_330b
budgetcutsacrossboard <- -1 * (budgetcutsacrossboard - 3)
budgetcutsacrossboard[budgetcutsacrossboard > 2] <- NA
middleclasstaxcut <- CCES2015$CC15_330c
middleclasstaxcut <- -1 * (middleclasstaxcut - 3)
middleclasstaxcut[middleclasstaxcut > 2] <- NA
taxhikepreventionact <- CCES2015$CC15_330d
taxhikepreventionact <- -1 * (taxhikepreventionact - 3)
taxhikepreventionact[taxhikepreventionact > 2] <- NA
balancebudget.firstchoice.cutdomesticspending <- NA
balancebudget.firstchoice.cutdomesticspending[CCES2015$CC15_333a==1] <- 1
balancebudget.firstchoice.cutdomesticspending[CCES2015$CC15_333a==2] <- 2
balancebudget.firstchoice.cutdomesticspending[CCES2015$CC15_333a==3] <- 1
balancebudget.firstchoice.cutdefensespending <- NA
balancebudget.firstchoice.cutdefensespending[CCES2015$CC15_333a==1] <- 2
balancebudget.firstchoice.cutdefensespending[CCES2015$CC15_333a==2] <- 1
balancebudget.firstchoice.cutdefensespending[CCES2015$CC15_333a==3] <- 1
balancebudget.firstchoice.raisetaxes <- NA
balancebudget.firstchoice.raisetaxes[CCES2015$CC15_333a==1] <- 1
balancebudget.firstchoice.raisetaxes[CCES2015$CC15_333a==2] <- 1
balancebudget.firstchoice.raisetaxes[CCES2015$CC15_333a==3] <- 2
environment_regulateCO2 <- CCES2015$CC15_323_1
environment_regulateCO2[environment_regulateCO2 > 2] <- NA
environment_raisefuelefficiency <- CCES2015$CC15_323_2
environment_raisefuelefficiency[environment_raisefuelefficiency > 2] <- NA
environment_requirerenewablefuels <- CCES2015$CC15_323_3
environment_requirerenewablefuels[environment_requirerenewablefuels > 2] <- NA
environment_strengthenenforcementcleanair <- CCES2015$CC15_323_4
environment_strengthenenforcementcleanair[environment_strengthenenforcementcleanair > 2] <- NA
guncontrol_backchecks <- CCES2015$CC15_320a
guncontrol_backchecks[guncontrol_backchecks > 2] <- NA
guncontrol_prohibitpublishingnames <- CCES2015$CC15_320b
guncontrol_prohibitpublishingnames <- -1 * (guncontrol_prohibitpublishingnames - 3)
guncontrol_prohibitpublishingnames[guncontrol_prohibitpublishingnames > 2] <- NA
guncontrol_banassaultrifles <- CCES2015$CC15_320c
guncontrol_banassaultrifles[guncontrol_banassaultrifles > 2] <- NA
guncontrol_easierconcealcarry <- CCES2015$CC15_320d
guncontrol_easierconcealcarry <- -1 * (guncontrol_easierconcealcarry - 3)
guncontrol_easierconcealcarry[guncontrol_easierconcealcarry > 2] <- NA
immigration_grantlegalstatus <- CCES2015$CC15_321_1
immigration_grantlegalstatus[immigration_grantlegalstatus > 2] <- NA
immigration_increasepatrols <- CCES2015$CC15_321_2
immigration_increasepatrols <- -1 * (immigration_increasepatrols - 3)
immigration_increasepatrols[immigration_increasepatrols > 2] <- NA
immigration_allowpolicequestion <- CCES2015$CC15_321_3
immigration_allowpolicequestion <- -1 * (immigration_allowpolicequestion - 3)
immigration_allowpolicequestion[immigration_allowpolicequestion > 2] <- NA
immigration_finebusinesseshire <- CCES2015$CC15_321_4
immigration_finebusinesseshire <- -1 * (immigration_finebusinesseshire - 3)
immigration_finebusinesseshire[immigration_finebusinesseshire > 2] <- NA
immigration_deportillegalimmigrants <- CCES2015$CC15_321_5
immigration_deportillegalimmigrants <- -1 * (immigration_deportillegalimmigrants - 3)
immigration_deportillegalimmigrants[immigration_deportillegalimmigrants > 2] <- NA
alwaysallowabortion <- CCES2015$CC15_322a
alwaysallowabortion[alwaysallowabortion > 2] <- NA
prohibitabortiontwentyweeks <- CCES2015$CC15_322c
prohibitabortiontwentyweeks[prohibitabortiontwentyweeks > 2] <- NA
prohibitabortiontwentyweeks <- (prohibitabortiontwentyweeks - 3) * -1
allowemployersdeclineabortion <- CCES2015$CC15_322d
allowemployersdeclineabortion[allowemployersdeclineabortion > 2] <- NA
allowemployersdeclineabortion <- (allowemployersdeclineabortion - 3) * -1
prohibitfederalfundingabortions <- CCES2015$CC15_322e
prohibitfederalfundingabortions[prohibitfederalfundingabortions > 2] <- NA
prohibitfederalfundingabortions <- (prohibitfederalfundingabortions - 3) * -1
gaymarriage <- CCES2015$CC15_325
gaymarriage[gaymarriage > 2] <- NA
affirmativeaction <- CCES2015$CC15_326a
affirmativeaction[affirmativeaction > 2] <- NA
syria_significantforce <- CCES2015$CC15_303_8
syria_significantforce[syria_significantforce > 2] <- NA
syria_significantforce <- (syria_significantforce - 3) * -1
moraltrad.adjust <- CCES2015$UGA306A_1
moraltrad.adjust[moraltrad.adjust < 1] <- NA
moraltrad.adjust[moraltrad.adjust > 5] <- NA
moraltrad.lifestyle <- CCES2015$UGA306B_1
moraltrad.lifestyle[moraltrad.lifestyle < 1] <- NA
moraltrad.lifestyle[moraltrad.lifestyle > 5] <- NA
moraltrad.lifestyle <- (-1 * moraltrad.lifestyle) + 6
moraltrad.tolerant <- CCES2015$UGA306C_1
moraltrad.tolerant[moraltrad.tolerant < 1] <- NA
moraltrad.tolerant[moraltrad.tolerant > 5] <- NA
moraltrad.familyvalues <- CCES2015$UGA306D_1
moraltrad.familyvalues[moraltrad.familyvalues < 1] <- NA
moraltrad.familyvalues[moraltrad.familyvalues > 5] <- NA
moraltrad.familyvalues <- (-1 * moraltrad.familyvalues) + 6
egalitarianism.equal <- CCES2015$UGA307A_1
egalitarianism.equal[egalitarianism.equal < 1] <- NA
egalitarianism.equal[egalitarianism.equal > 5] <- NA
egalitarianism.toofar <- CCES2015$UGA307B_1
egalitarianism.toofar[egalitarianism.toofar < 1] <- NA
egalitarianism.toofar[egalitarianism.toofar > 5] <- NA
egalitarianism.toofar <- (-1 * egalitarianism.toofar) + 6
egalitarianism.bigprob <- CCES2015$UGA307C_1
egalitarianism.bigprob[egalitarianism.bigprob < 1] <- NA
egalitarianism.bigprob[egalitarianism.bigprob > 5] <- NA
egalitarianism.worryless <- CCES2015$UGA307D_1
egalitarianism.worryless[egalitarianism.worryless < 1] <- NA
egalitarianism.worryless[egalitarianism.worryless > 5] <- NA
egalitarianism.worryless <- (-1 * egalitarianism.worryless) + 6
egalitarianism.notbigprob <- CCES2015$UGA307E_1
egalitarianism.notbigprob[egalitarianism.notbigprob < 1] <- NA
egalitarianism.notbigprob[egalitarianism.notbigprob > 5] <- NA
egalitarianism.notbigprob <- (-1 * egalitarianism.notbigprob) + 6
egalitarianism.fewerprobs <- CCES2015$UGA307F_1
egalitarianism.fewerprobs[egalitarianism.fewerprobs < 1] <- NA
egalitarianism.fewerprobs[egalitarianism.fewerprobs > 5] <- NA
militarism.strength <- CCES2015$UGA312_1
militarism.strength[militarism.strength < 1] <- NA
militarism.strength[militarism.strength > 7] <- NA
militarism.diplomacy <- CCES2015$UGA313_1
militarism.diplomacy[militarism.diplomacy < 1] <- NA
militarism.diplomacy[militarism.diplomacy > 7] <- NA

issues <- cbind(ideology,
                socialideology, economicideology, natsecurityideology,
                repealACA,
                approvekeystone,
                iransanctions,
                transpacificpartnershipact,
                normalizecubarelations,
                renewpatriotact,
                usafreedomact,
                tradeadjustmentact,
                VAWA,
                cutmedicaremedicaid,
                budgetcutsacrossboard,
                middleclasstaxcut,
                taxhikepreventionact,
                balancebudget.firstchoice.cutdefensespending,
                balancebudget.firstchoice.cutdomesticspending,
                balancebudget.firstchoice.raisetaxes,
                environment_regulateCO2, environment_raisefuelefficiency,
                environment_requirerenewablefuels, environment_strengthenenforcementcleanair,
                guncontrol_backchecks, guncontrol_prohibitpublishingnames,
                guncontrol_banassaultrifles, guncontrol_easierconcealcarry,
                immigration_grantlegalstatus, immigration_increasepatrols,
                immigration_allowpolicequestion, immigration_finebusinesseshire,
                immigration_deportillegalimmigrants,
                alwaysallowabortion, prohibitabortiontwentyweeks,
                allowemployersdeclineabortion, prohibitfederalfundingabortions,
                gaymarriage, affirmativeaction, syria_significantforce)

values <- cbind(moraltrad.adjust, moraltrad.lifestyle,
                moraltrad.tolerant, moraltrad.familyvalues,
                egalitarianism.equal, egalitarianism.toofar,
                egalitarianism.bigprob, egalitarianism.worryless,
                egalitarianism.notbigprob, egalitarianism.fewerprobs,
                militarism.strength, militarism.diplomacy)

issuesvalues <- cbind(issues, values)
issuesvalues <- as.matrix(issuesvalues)
for(i in 1:ncol(issuesvalues)){
  for(j in 1:nrow(issuesvalues)){
    if(is.na(issuesvalues[j,i])){
      issuesvalues[j,i] <- 99
    }
  }
}

bbresult <- blackbox(issuesvalues, missing=99, dims=2, minscale=5, verbose=TRUE)

xx <- bbresult$individuals[[2]][,1]
yy <- bbresult$individuals[[2]][,2]

bbresultdt <- cbind.data.frame(xx,yy,partyid)
colnames(bbresultdt) <- c("coord1D", "coord2D", "party")
bbresultdt_n <- bbresultdt %>% filter(party == "DEM" | party == "REP")

pdf("cces2015_bb.pdf", width=5.8, height=4.6)
ggplot(data=bbresultdt_n, aes(x=coord1D, y=coord2D, color=as.factor(party))) + geom_point(size=2, alpha=0.6) +
  xlim(-1,1) +
  ylim(-1,1) +
  xlab("\nPC1") +
  ylab("PC2\n") +
  ggtitle("BlackBox Result of CCES 2015") +
  scale_colour_manual(values = c("#507AA6", "#DF585C"), labels=c("Dem","Rep")) +
  theme(plot.title = element_text(hjust = 0.5), legend.position = c(0.97, 0.97),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.background = element_rect(colour="gray85", fill = "gray93"),
        legend.key = element_rect(colour = "transparent", fill = NA),
        legend.margin = margin(1, 6, 1, 1),
        legend.title=element_blank())
dev.off()

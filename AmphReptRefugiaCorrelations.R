############
############ Correlations
############
rm(list=ls(all=TRUE))

setwd("D:/2023_CurrentNM_Work/R_NM/Tables")

library("Hmisc")
library("corrplot")
library("rstatix")

AmphRept<-read.csv("AmphReptJoin.csv")
AmphRept=subset(AmphRept, select=-c(HUC12,NAME, HUC8))
names(AmphRept)
AmphReptClim<-subset(AmphRept, select=c(PerWatHerpCount,WatBodAre,Diff_Bio8_50_cur_MEAN_H8,Diff_Bio9_50_cur_MEAN_H8,Diff_Bio5_50_cur_H8,
                                        Diff_Bio1_50_cur_H8, PctChange_Bio18_MEAN_H8, PctChange_Bio19_MEAN_H8,
                                        PctChange_Bio12_MEAN_H8,PctChange_Bio13_MEAN_H8,PctChange_Bio14_MEAN_H8,
                                    Back_rcp45_95_55_MEAN, MeanDifETH8,MeanDifHMIH8,MeanDifPNVH8,MeanDifSWEH8,
                                    MeanDifSMCH8,MeanJunFloPctCh2040))

AmphReptHydro<-subset(AmphRept, select=c(PerWatHerpCount,WatBodAre,MeanCanHei,PctMarWetMeaH12,PctNatSemNatWoo12,PctNatCovSB,PctTreCanBuf,PctFPShr,
                                         PctFPWet,mean_avwatstr,sbd250m_100bd_mean,cti_mean,curve_mean,dem_mean,tpi_mean,twi_mean,
                                         hli_mean,aspect_sh3_mean,geomorph_sh3_std,slope_stdev9_mean,PctCarbKarstH8))


AmphReptVeg<-subset(AmphRept, select=c(PerWatHerpCount,WatBodAre,MeanCanHei,PctMarWetMeaH12,PctNatSemNatWoo12,PctNatCovSB,PctTreCanBuf,PctFPShr,
                                       PctFPWet))


AmphReptTopo<-subset(AmphRept, select=c(PerWatHerpCount,WatBodAre,mean_avwatstr,sbd250m_100bd_mean,cti_mean,curve_mean,dem_mean,tpi_mean,twi_mean,
                                    hli_mean,aspect_sh3_mean,geomorph_sh3_std,slope_stdev9_mean,PctCarbKarstH8))


MacroZ<-read.csv("AmphRep_Z_clim.csv")
MacroZ=subset(MacroZ, select=-HUC12)
HydroZ<-read.csv("AmphRep_Z_topoveg.csv")
HydroZ=subset(HydroZ, select=-PerWatHerpCount)
# computing correlation matrix. default is Pearson.

cor_AmphReptMacroP= cor(AmphReptClim, use="complete.obs")
cor_AmphReptHydroP= cor(AmphReptHydro, use="complete.obs")
cor_AmphReptMacroS= cor(AmphReptClim, use="complete.obs",method = 'spearman')
cor_AmphReptHydroS= cor(AmphReptHydro, use="complete.obs",method = 'spearman')

write.csv(cor_AmphReptMacroP,"AmphReptMacroP.csv")
write.csv(cor_AmphReptHydroP,"AmphReptHydroP.csv")
write.csv(cor_AmphReptMacroS,"AmphReptMacroS.csv")
write.csv(cor_AmphReptHydroS,"AmphReptHydroS.csv")

cor_MacroZP = cor(MacroZ, use="complete.obs")
cor_HydroZP = cor(HydroZ, use="complete.obs")
cor_MacroZS = cor(MacroZ, use="complete.obs",method = 'spearman')
cor_HydroZS = cor(HydroZ, use="complete.obs",method = 'spearman')

write.csv(cor_MacroZP,"AmphReptMacroZP.csv")
write.csv(cor_HydroZP,"AmphReptHydroZP.csv")
write.csv(cor_MacroZS,"AmphReptMacroZS.csv")
write.csv(cor_HydroZS,"AmphReptHydroZS.csv")


corrplot(cor_AmphReptMacroP, type = "lower", tl.cex=0.5)
corrplot(cor_AmphReptHydroP, type = "lower", tl.cex=0.5)
corrplot(cor_AmphReptMacroS, type = "lower", tl.cex=0.5)
corrplot(cor_AmphReptHydroS, type = "lower", tl.cex=0.5)

corrplot(cor_MacroZP, type = "lower", tl.cex=0.5)
corrplot(cor_HydroZP, type = "lower", tl.cex=0.5)
corrplot(cor_MacroZS, type = "lower", tl.cex=0.5)
corrplot(cor_HydroZS, type = "lower", tl.cex=0.5)


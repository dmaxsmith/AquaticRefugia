############
############ Correlations
############
rm(list=ls(all=TRUE))

setwd("D:/2023_CurrentNM_Work/R_NM/Tables")

library("Hmisc")
library("corrplot")
library("rstatix")

#creating versions without HUC12, which we don't want in correlation. I do not have lines for all the tables above so modify according to what you use. Also, 
#good time to remove other variables you don't want in the correlation. Alternatively, use subset.
##Hydrology and climate indicators##


ColWat<-read.csv("ColdWaterJoin.csv")
ColWat=subset(ColWat, select=-c(HUC12,NAME, HUC8))
names(ColWat)
ColWatClim<-subset(ColWat, select=c(FishCount,SpringDens,Pct10deg,Diff_Bio8_50_cur_MEAN,Diff_Bio9_50_cur_MEAN,Diff_Bio5_50_curr_MEAN,
                                    Diff_Bio1_Curr_50_MEAN, PctChange_Bio18_MEAN, PctChange_Bio19_MEAN,
                                    PctChg_Bio12_MEAN,PctChg_Bio13_MEAN,PctChg_Bio14_MEAN,
                                     Back_rcp45_95_55_MEAN, MeanDifETH12,MeanDifHMIH12,MeanDifPNVH12,MeanDifSWEH12,
                                     MeanDifSMCH12,MeanJunFloPctCh2040,StrTemCha2040))

ColWatHydro<-subset(ColWat, select=c(FishCount,SpringDens,Pct10deg,mean_avwatstr,sbd250m_100bd_mean,cti_mean,curve_mean,dem_mean,tpi_mean,twi_mean,
                                     hli_mean,aspect_sh3_mean,geomorph_sh3_std,slope_stdev9_mean,PctCarbKarstH8,PctVolcKarstH8,MeanCanHei,PctMarWetMeaH12,PctNatSemNatWoo12,PctNatCovSB,PctTreCanBuf,PctFPShr,
                                     PctFPWet))


ColWatVeg<-subset(ColWat, select=c(FishCount,SpringDens,Pct10deg,MeanCanHei,PctMarWetMeaH12,PctNatSemNatWoo12,PctNatCovSB,PctTreCanBuf,PctFPShr,
                                     PctFPWet))
ColWatTopo<-subset(ColWat, select=c(FishCount,SpringDens,Pct10deg,mean_avwatstr,sbd250m_100bd_mean,cti_mean,curve_mean,dem_mean,tpi_mean,twi_mean,
                                     hli_mean,aspect_sh3_mean,geomorph_sh3_std,slope_stdev9_mean,PctCarbKarstH8,PctVolcKarstH8))


MacroZ<-read.csv("ColdWater_Z_clim.csv")
MacroZ=subset(MacroZ, select=-HUC12)
HydroZ<-read.csv("ColdWater_Z_topoveg.csv")
HydroZ=subset(HydroZ, select=-HUC12)

# computing correlation matrix. default is Pearson.


cor_ColWatMacroP= cor(ColWatClim, use="complete.obs")
cor_ColWatHydroP= cor(ColWatHydro, use="complete.obs")
cor_ColWatMacroS= cor(ColWatClim, use="complete.obs",method = 'spearman')
cor_ColWatHydroS= cor(ColWatHydro, use="complete.obs",method = 'spearman')

write.csv(cor_ColWatMacroP,"ColdwaterMacroP.csv")
write.csv(cor_ColWatHydroP,"ColdwaterHydroP.csv")
write.csv(cor_ColWatMacroS,"ColdwaterMacroS.csv")
write.csv(cor_ColWatHydroS,"ColdwaterHydroS.csv")

cor_MacroZP = cor(MacroZ, use="complete.obs")
cor_HydroZP = cor(HydroZ, use="complete.obs")
cor_MacroZS = cor(MacroZ, use="complete.obs",method = 'spearman')
cor_HydroZS = cor(HydroZ, use="complete.obs",method = 'spearman')


write.csv(cor_MacroZP,"ColdwaterMacroZP.csv")
write.csv(cor_HydroZP,"ColdwaterHydroZP.csv")
write.csv(cor_MacroZS,"ColdwaterMacroZS.csv")
write.csv(cor_HydroZS,"ColdwaterHydroZS.csv")


corrplot(cor_ColWatMacroP, type = "lower", tl.cex=0.5)
corrplot(cor_ColWatHydroP, type = "lower", tl.cex=0.5)
corrplot(cor_ColWatMacroS, type = "lower", tl.cex=0.5)
corrplot(cor_ColWatHydroS, type = "lower", tl.cex=0.5)

corrplot(cor_MacroZP, type = "lower", tl.cex=0.5)
corrplot(cor_HydroZP, type = "lower", tl.cex=0.5)
corrplot(cor_MacroZS, type = "lower", tl.cex=0.5)
corrplot(cor_HydroZS, type = "lower", tl.cex=0.5)










# visualizing correlogram
corrplot(cor_ColWat,tl.cex=0.5, method="circle")


corrplot(cor_climz,tl.cex=0.5, method="circle")
corrplot(M, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)

corrplot(cor_topoz, tl.cex=0.8, method="circle")
corrplot(cor_vegz, tl.cex=0.8, method="circle")

corrplot(cor_topovegz, tl.cex=0.8, method="circle")

#With P values
library("rstatix")

Cor_DivAll2 = cor_mat(Hydroclim, method="pearson", conf.level=0.95)

#specify variable of interest mydata%>% cor_mat(mgh, ht wt) or remove cor_mat(-mpg, -hp))

#visualize
Cor_DivAll2 %>%
  cor_reorder() %>%
  pull_lower_triangle() %>%
  cor_plot(label = TRUE, cex.lab=0.1, cex.axis=0.1, cex.sub=0.1)


#changing font size...
number.cex=7/ncol(Cor_DivAll2)
# visualizing correlogram
# as circle

############Lithology and soils#####

Lithsoil<-read.csv("LithSoilJoin.csv")
Lithsoil=subset(Lithsoil, select=-c(HUC12,NAME,HUC8))

# computing correlation matrix. default is Pearson.

cor_lithsoil = cor(Lithsoil, use="complete.obs")

print("Correlation matrix")
print(cor_lithsoil)

#Set up parameter for visualization
cor_lithsoil %>%
  cor_reorder() %>%
  pull_lower_triangle() %>%
  cor_plot(label = TRUE, number.cex=7/ncol(Lithsoil))


# visualizing correlogram

corrplot(cor_lithsoil, method="circle")
corrplot(cor_lithsoil, method="pie")
corrplot(cor_lithsoil, method="color")
corrplot(cor_lithsoil, method="number")
corrplot(cor_lithsoil, method="ellipse")
corrplot(cor_lithsoil, method="shade")

#With P values
library("rstatix")

Cor_DivAll2 = cor_mat(coldpercorr, method="pearson", conf.level=0.95)

#specify variable of interest mydata%>% cor_mat(mgh, ht wt) or remove cor_mat(-mpg, -hp))

#visualize
Cor_DivAll2 %>%
  cor_reorder() %>%
  pull_lower_triangle() %>%
  cor_plot(label = TRUE, cex.lab=0.1, cex.axis=0.1, cex.sub=0.1)


#changing font size...
number.cex=7/ncol(Cor_DivAll2)
# visualizing correlogram
# as circle

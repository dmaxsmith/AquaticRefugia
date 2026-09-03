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


springs<-read.csv("SpringsJoin20250407.csv")
springs=subset(springs, select=-c(HUC12,NAME, HUC8))
names(springs)
springsClim<-subset(springs, select=c(TotalAquaticSpeciesRich,SpringDens,Diff_Bio8_50_cur_MEAN_H8,Diff_Bio9_50_cur_MEAN_H8,Diff_Bio5_50_cur_H8,
                                    Diff_Bio1_50_cur_H8, PctChange_Bio18_MEAN_H8, PctChange_Bio19_MEAN_H8,
                                    PctChange_Bio12_MEAN_H8,PctChange_Bio13_MEAN_H8,PctChange_Bio14_MEAN_H8,
                                    Back_rcp45_95_55_MEAN, MeanDifETH8,MeanDifHMIH8,MeanDifPNVH8,MeanDifSWEH8,
                                    MeanDifSMCH8))

springsHydro<-subset(springs, select=c(TotalAquaticSpeciesRich,SpringDens,MeanSBDH8,PctCarbKarstH8,PctVolcKarstH8,PctUnconsH8,AquiRichH8,aspect_sh3_mean,
                                       dem_sh3_mean,hli_mean,slope_stdev3_mean,tpi_mean,vrm_mean
                                     ))
climz<-read.csv("springs_Z_clim.csv")
hydroz<-read.csv("springs_Z_hydro.csv")
# computing correlation matrix. default is Pearson.



cor_SringsMacroP= cor(springsClim, use="complete.obs")
cor_SringsHydroP= cor(springsHydro, use="complete.obs")
cor_SringsMacroS= cor(springsClim, use="complete.obs",method = 'spearman')
cor_SringsHydroS= cor(springsHydro, use="complete.obs",method = 'spearman')

write.csv(cor_SringsMacroP,"SringsMacroP.csv")
write.csv(cor_SringsHydroP,"SringsHydroP.csv")
write.csv(cor_SringsMacroS,"SringsMacroS.csv")
write.csv(cor_SringsHydroS,"SringsHydroS.csv")


cor_macrozP = cor(climz, use="complete.obs")
cor_hydrozP = cor(hydroz, use="complete.obs")
cor_macrozS = cor(climz, use="complete.obs",method = 'spearman')
cor_hydrozS = cor(hydroz, use="complete.obs",method = 'spearman')

write.csv(cor_macrozP,"SringsMacroZP.csv")
write.csv(cor_hydrozP,"SringsHydroZP.csv")
write.csv(cor_macrozS,"SringsMacroZS.csv")
write.csv(cor_hydrozS,"SringsHydroZS.csv")

corrplot(cor_PerStrClimP, type = "lower", tl.cex=0.5)
corrplot(cor_PerStrHydroP, type = "lower", tl.cex=0.5)
corrplot(cor_PerStrClimS, type = "lower", tl.cex=0.5)
corrplot(cor_PerStrHydroS, type = "lower", tl.cex=0.5)


# visualizing correlogram
corrplot(cor_springs,tl.cex=0.5, method="circle")
corrplot(cor_springsClim, tl.cex=0.5, method="circle")
corrplot(cor_springsHydro, tl.cex=0.5, method="circle")
corrplot(cor_climz,tl.cex=0.8, method="circle")
corrplot(cor_hydroz, tl.cex=0.8, method="circle")

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

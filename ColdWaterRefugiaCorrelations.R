
###Step 3: Testing for correlations among variables in joined file###
############
setwd("E:/2023_CurrentNM_Work/R/Tables")

library("Hmisc")
library("corrplot")
library("rstatix")

##Create versions without HUC12, which we don't want in correlation##
##Hydrology and climate indicators##


ColWat<-read.csv("ColdWaterJoin.csv")
ColWat=subset(ColWat, select=-c(HUC12,NAME, HUC8))
names(ColWat)
ColWatClim<-subset(ColWat, select=c(FishCount,SpringDens,Pct10deg,Diff_Bio8_50_cur_MEAN,Diff_Bio9_50_cur_MEAN,Diff_Bio5_50_curr_MEAN,
                                    Diff_Bio1_Curr_50_MEAN, PctChange_Bio18_MEAN, PctChange_Bio19_MEAN,
                                    PctChg_Bio12_MEAN,PctChg_Bio13_MEAN,PctChg_Bio14_MEAN,
                                     Back_rcp45_95_55_MEAN, MeanDifETH12,MeanDifHMIH12,MeanDifPNVH12,MeanDifSWEH12,
                                     MeanDifSMCH12,MeanJunFloPctCh2040,StrTemCha2040))
ColWatVeg<-subset(ColWat, select=c(FishCount,SpringDens,Pct10deg,MeanCanHei,PctMarWetMeaH12,PctNatSemNatWoo12,PctNatCovSB,PctTreCanBuf,PctFPShr,
                                     PctFPWet))
ColWatTopo<-subset(ColWat, select=c(FishCount,SpringDens,Pct10deg,mean_avwatstr,sbd250m_100bd_mean,cti_mean,curve_mean,dem_mean,tpi_mean,twi_mean,
                                     hli_mean,aspect_sh3_mean,geomorph_sh3_std,slope_stdev9_mean,PctCarbKarstH8,PctVolcKarstH8))


##Read in files from Z score output to test for post-transfomation correlations##
topoz<-read.csv("ColdWater_Z_topo.csv")
climz<-read.csv("ColdWater_Z_clim.csv")
vegz<-read.csv("ColdWater_Z_veg.csv")
topovegz<-read.csv("ColdWater_Z_topoveg.csv")


#Compute correlation matrix. Default is Pearson##
cor_ColWat= cor(ColWat, use="complete.obs")
cor_ColWatClim= cor(ColWatClim, use="complete.obs")
cor_ColWatVeg= cor(ColWatVeg, use="complete.obs")
cor_ColWatTopo= cor(ColWatTopo, use="complete.obs")

cor_topoz = cor(topoz, use="complete.obs")
cor_climz = cor(climz, use="complete.obs")
cor_vegz = cor(vegz, use="complete.obs")
cor_topovegz = cor(topovegz, use="complete.obs")

print(cor_ColWat)
print(cor_ColWatClim)
print(cor_ColWatVeg)
print(cor_ColWatTopo)

print(cor_topoz)
print(cor_climz)


# visualizing correlogram
corrplot(cor_ColWat,tl.cex=0.5, method="circle")
corrplot(cor_ColWatClim, tl.cex=0.5, method="circle")
corrplot(cor_ColWatVeg, tl.cex=0.5, method="circle")
corrplot(cor_ColWatTopo, tl.cex=0.5, method="circle")

corrplot(cor_climz,tl.cex=0.5, method="circle")
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


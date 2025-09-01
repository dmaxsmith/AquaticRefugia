setwd("D:/2023_CurrentNM_Work/R/Tables")
rm(list=ls(all=TRUE))

library(tidyr)
library(ggplot2) 
library(dplyr) 


HUC_indicators<-read.csv("ColdWaterJoin.csv")
names(HUC_indicators)


HUC_indicators = subset(HUC_indicators, select=-c(HUC12,NAME,HUC8,Diff_Bio1_Curr_50_MEAN, PctChg_Bio12_MEAN, PctChg_Bio13_MEAN, PctChg_Bio14_MEAN, MeanDifETH12, MeanDifHMIH12,MeanDifSMCH12, PctFPShr, PctFPWet, mean_avwatstr, cti_mean, curve_mean,tpi_mean))

names(HUC_indicators)



#adjust z scores to match relationship between diversity and richness, adjust Z scores for things we want to minimize (change in conditions and velocity and soil bulk density) 


HUC_indicators$Back_rcp45_rev = HUC_indicators$Back_rcp45_95_55_MEAN*-1 

HUC_indicators$Diff_bio8_rev = HUC_indicators$Diff_Bio8_50_cur_MEAN*-1 

HUC_indicators$Diff_bio9_rev = HUC_indicators$Diff_Bio9_50_cur_MEAN*-1 

HUC_indicators$Diff_bio5_rev = HUC_indicators$Diff_Bio5_50_curr_MEAN*-1 

HUC_indicators$MeanDifPNVH12_rev = HUC_indicators$MeanDifPNVH12*-1 

HUC_indicators$StrTemCha2040_rev = HUC_indicators$StrTemCha2040*-1 

HUC_indicators$hli_mean_rev = HUC_indicators$hli_mean*-1 

HUC_indicators$sbd250m_100bd_mean_rev = HUC_indicators$sbd250m_100bd_mean*-1 
names(HUC_indicators)
 

ColdWater_Z_clim = subset(HUC_indicators, select=c(1:3, 7:8,11:12,28:33)) 
names(ColdWater_Z_clim)

ColdWater_Z_topo = subset(HUC_indicators, select=c(1:3, 20:21,23:27,34:35))
names(ColdWater_Z_topo)

ColdWater_Z_veg = subset(HUC_indicators, select=c(1:3, 14:18))
names(ColdWater_Z_veg)

ColdWater_Z_topoveg = subset(HUC_indicators, select=c(1:3, 14:16,18,20,23:24,26:27,34:35))
names(ColdWater_Z_topoveg)

columns_to_stand1 = c(4:13) #drops richness 
ColdWater_Z_clim [columns_to_stand1]= lapply(ColdWater_Z_clim[columns_to_stand1], scale) 

columns_to_stand1 = c(4:12) #drops richness 
ColdWater_Z_topo [columns_to_stand1]= lapply(ColdWater_Z_topo[columns_to_stand1], scale) 

columns_to_stand1 = c(4:8) #drops richness 
ColdWater_Z_veg [columns_to_stand1]= lapply(ColdWater_Z_veg[columns_to_stand1], scale)

columns_to_stand1 = c(4:14) #drops richness 
ColdWater_Z_topoveg [columns_to_stand1]= lapply(ColdWater_Z_topoveg[columns_to_stand1], scale)


write.csv(ColdWater_Z_clim,"ColdWater_Z_clim.csv", row.names=FALSE) 

write.csv(ColdWater_Z_topo,"ColdWater_Z_topo.csv", row.names=FALSE) 

write.csv(ColdWater_Z_veg,"ColdWater_Z_veg.csv", row.names=FALSE) 

write.csv(ColdWater_Z_topoveg,"ColdWater_Z_topoveg.csv", row.names=FALSE) 

#Assess Z scores 






#reshape for plotting 


z_long1 = tidyr::pivot_longer(ColdWater_Z_clim, cols = c(4:13), names_to = "variable", values_to = "z_score") 

z_long2 = tidyr::pivot_longer(ColdWater_Z_topo, cols = c(4:12), names_to = "variable", values_to = "z_score") 

z_long3 = tidyr::pivot_longer(ColdWater_Z_veg, cols = c(4:8), names_to = "variable", values_to = "z_score") 


#plot Z-scores 

p1 =ggplot(z_long1, aes(x=variable, y=z_score)) +geom_boxplot() +geom_jitter(alpha =0.2) + 
  
  theme_minimal() +labs(title="distribution of climate zscores by variable", x="variable", y = "Z-score") +
  theme(axis.text=element_text(size=3))+
  geom_hline(yintercept = c(-3,3), 
                                                                                                               
                                                                                                               linetype = "dashed", color = "red") 
print (p1) 

p2 =ggplot(z_long2, aes(x=variable, y=z_score)) +geom_boxplot() +geom_jitter(alpha =0.2) + 
  
  theme_minimal() +labs(title="distribution of topo zscores by variable", x="variable", y = "Z-score") + 
  theme(axis.text=element_text(size=5))+
  geom_hline(yintercept = c(-3,3), 
                                                                                                               
                                                                                                               linetype = "dashed", color = "red") 
print (p2) 

p3 =ggplot(z_long3, aes(x=variable, y=z_score)) +geom_boxplot() +geom_jitter(alpha =0.2) + 
  
  theme_minimal() +labs(title="distribution of topo zscores by variable", x="variable", y = "Z-score") + 
  theme(axis.text=element_text(size=5))+
  geom_hline(yintercept = c(-3,3), 
                                                                                                               
                                                                                                               linetype = "dashed", color = "red")
print (p3)

#Identify extreme z-scores (just for first iteration below) 

extrem_z = z_long1 %>% 
  
  filter(abs(z_score) >3) %>% 
  
  arrange(desc(abs(z_score))) 

#print summary stats 

cat("\nsummar of z-scores:\n") 

print(summary(z_long1$z_score)) 

cat("\nExtreme Z scores (X>3):\n") 

print(extrem_z) 

#check for normality (run on original scaled data, code for just first file below) 

cat("\nShapiro_Wilk test for normality:\n") 

for(col in names (AmphRep_Z)){ 
  
  test = shapiro.test(AmphRep_Z[[col]]) 
  
  cat(col, ": W=",  
      
      round(test$statistic, 4), 
      
      ", p-value=", 
      
      round(test$p.value, 4), "/")} 

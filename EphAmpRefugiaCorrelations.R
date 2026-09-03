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


EphAmp<-read.csv("EphCatchmentJoin.csv")
EphAmp=subset(EphAmp, select=-c(HUC12,NAME, HUC8))

topoz<-read.csv("EphAmp_Z_topo.csv")
climz<-read.csv("EphAmp_Z_clim.csv")

# computing correlation matrix. default is Pearson.



cor_ephcatP= cor(EphAmp, use="complete.obs")
cor_ephcatS= cor(EphAmp, use="complete.obs",method = 'spearman')

write.csv(cor_ephcatP,"cor_ephcatP.csv")
write.csv(cor_ephcatS,"cor_ephcatS.csv")

cor_topoz = cor(topoz, use="complete.obs")
cor_climz = cor(climz, use="complete.obs")

write.csv(cor_topoz,"cor_ephcatZP.csv")
write.csv(cor_climz,"cor_ephcatZS.csv")

corrplot(cor_ephcatP, type = "lower", tl.cex=0.5)
corrplot(cor_ephcatS, type = "lower", tl.cex=0.5)




# visualizing correlogram
corrplot(cor_ephamp, method="circle")



corrplot(cor_ephcatP, type = "lower", tl.cex=0.5)
corrplot(cor_ephcatS, type = "lower", tl.cex=0.5)










corrplot(cor_climz, method="circle")


corrplot(cor_topoz, tl.cex=0.8, method="circle")


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

##Step 2: Join data layers to create one table with all variables used to model refugia for the habitat group##

setwd("E:/2023_CurrentNM_Work/R/Tables")

library(tidyverse)

#Value#
colwatjoin1<-full_join(colperhuc12s, HUC12fishes, "HUC12")
colwatjoin1_noNAs<-na.omit(colwatjoin1)

colwatjoin2<-full_join(colwatjoin1_noNAs, huc12sprden, "HUC12")
colwatjoin2_noNAs<-na.omit(colwatjoin2)

colwatjoin3<-full_join(colwatjoin2_noNAs, huc12pct10deg, "HUC12")
colwatjoin3_noNAs<-na.omit(colwatjoin3)

##Climate/hydrology##
colwatjoin4<-full_join(colwatjoin3_noNAs, huc12diftemwetqua, "HUC12") #Bio8
colwatjoin4_noNAs<-na.omit(colwatjoin4)

colwatjoin5<-full_join(colwatjoin4_noNAs, huc12diftemdriqua, "HUC12") #Bio9
colwatjoin5_noNAs<-na.omit(colwatjoin5)

colwatjoin6<-full_join(colwatjoin5_noNAs, huc12difmaxtemwarmon, "HUC12") #Bio5
colwatjoin6_noNAs<-na.omit(colwatjoin6)

colwatjoin7<-full_join(colwatjoin6_noNAs, huc12difanntem, "HUC12") #Bio1
colwatjoin7_noNAs<-na.omit(colwatjoin7)

colwatjoin8<-full_join(colwatjoin7_noNAs, huc12pctchaprewarqua, "HUC12") #Bio18
colwatjoin8_noNAs<-na.omit(colwatjoin8)

colwatjoin9<-full_join(colwatjoin8_noNAs, huc12perchaprecolqua, "HUC12") #Bio19
colwatjoin9_noNAs<-na.omit(colwatjoin9)

colwatjoin10<-full_join(colwatjoin9_noNAs, huc12perchaannpre, "HUC12") #Bio12
colwatjoin10_noNAs<-na.omit(colwatjoin10)

colwatjoin11<-full_join(colwatjoin10_noNAs, huc12perchaprewetmon, "HUC12") #Bio13
colwatjoin11_noNAs <-na.omit(colwatjoin11)

colwatjoin12<-full_join(colwatjoin11_noNAs, huc12perchapredrimon, "HUC12") #Bio14
colwatjoin12_noNAs <-na.omit(colwatjoin12)

colwatjoin13<-full_join(colwatjoin12_noNAs, huc12bacvel, "HUC12")
colwatjoin13_noNAs <-na.omit(colwatjoin13)

colwatjoin14<-full_join(colwatjoin13_noNAs, huc12difet, "HUC12")
colwatjoin14_noNAs <-na.omit(colwatjoin14)

colwatjoin15<-full_join(colwatjoin14_noNAs, huc12difhmi, "HUC12")
colwatjoin15_noNAs <-na.omit(colwatjoin15)

colwatjoin16<-full_join(colwatjoin15_noNAs, huc12difpnv, "HUC12")
colwatjoin16_noNAs <-na.omit(colwatjoin16)

colwatjoin17<-full_join(colwatjoin16_noNAs, huc12difswe, "HUC12")
colwatjoin17_noNAs <-na.omit(colwatjoin17)

colwatjoin18<-full_join(colwatjoin17_noNAs, huc12difsmc, "HUC12")
colwatjoin18_noNAs <-na.omit(colwatjoin18)

colwatjoin19<-full_join(colwatjoin18_noNAs, huc12junflo2040, "HUC12")
colwatjoin19_noNAs<-na.omit(colwatjoin19)

colwatjoin20<-full_join(colwatjoin19_noNAs, huc12strtemcha2040, "HUC12")
colwatjoin20_noNAs<-na.omit(colwatjoin20)

#vegetation#
colwatjoin21<-full_join(colwatjoin20_noNAs, huc12meacanhei, "HUC12")
colwatjoin21$MeanCanHei<-colwatjoin21$MeanCanHei%>% replace(is.na(.),0)
colwatjoin21_noNAs <-na.omit(colwatjoin21)

colwatjoin22<-full_join(colwatjoin21_noNAs, huc12nmripmarwetmear, "HUC12")
colwatjoin22$PctMarWetMeaH12<-colwatjoin22$PctMarWetMeaH12%>% replace(is.na(.),0)
colwatjoin22_noNAs <-na.omit(colwatjoin22)

colwatjoin23<-full_join(colwatjoin22_noNAs, huc12nmripnatsemnatwoo, "HUC12")
colwatjoin23$PctNatSemNatWoo12<-colwatjoin23$PctNatSemNatWoo12%>% replace(is.na(.),0)
colwatjoin23_noNAs <-na.omit(colwatjoin23)

colwatjoin24<-full_join(colwatjoin23_noNAs, huc12easbnatcov, "HUC12")
colwatjoin24$PctNatCovSB<-colwatjoin24$PctNatCovSB%>% replace(is.na(.),0)
colwatjoin24_noNAs <-na.omit(colwatjoin24)

colwatjoin25<-full_join(colwatjoin24_noNAs, huc12easbtrecan, "HUC12")
colwatjoin25$PctTreCanBuf<-colwatjoin25$PctTreCanBuf%>% replace(is.na(.),0)
colwatjoin25_noNAs <-na.omit(colwatjoin25)

colwatjoin26<-full_join(colwatjoin25_noNAs, huc12eafpshr, "HUC12")
colwatjoin26_noNAs<-na.omit(colwatjoin26)

colwatjoin27<-full_join(colwatjoin26_noNAs, huc12eafpwet, "HUC12")
colwatjoin27_noNAs<-na.omit(colwatjoin27)

#Soil/litho/topo#
colwatjoin28<-full_join(colwatjoin27_noNAs, huc12meawatsto, "HUC12")
colwatjoin28_noNAs<-na.omit(colwatjoin28)

colwatjoin29<-full_join(colwatjoin28_noNAs, huc12meansbd, "HUC12")
colwatjoin29_noNAs<-na.omit(colwatjoin29)

colwatjoin30<-full_join(colwatjoin29_noNAs, huc12meancti, "HUC12")
colwatjoin30_noNAs<-na.omit(colwatjoin30)

colwatjoin31<-full_join(colwatjoin30_noNAs, huc12meancurve, "HUC12")
colwatjoin31_noNAs<-na.omit(colwatjoin31)

colwatjoin32<-full_join(colwatjoin31_noNAs, huc12meanelev, "HUC12")
colwatjoin32_noNAs<-na.omit(colwatjoin32)

colwatjoin33<-full_join(colwatjoin32_noNAs, huc12meantpi, "HUC12")
colwatjoin33_noNAs<-na.omit(colwatjoin33)

colwatjoin34<-full_join(colwatjoin33_noNAs, huc12meantwi, "HUC12")
colwatjoin34_noNAs<-na.omit(colwatjoin34)

colwatjoin35<-full_join(colwatjoin34_noNAs, huc12meanhli, "HUC12")
colwatjoin35_noNAs<-na.omit(colwatjoin35)

colwatjoin36<-full_join(colwatjoin35_noNAs, huc12meanaspectdiv, "HUC12")
colwatjoin36_noNAs<-na.omit(colwatjoin36)

colwatjoin37<-full_join(colwatjoin36_noNAs, huc12meangeomdiv, "HUC12")
colwatjoin37_noNAs<-na.omit(colwatjoin37)

colwatjoin38<-full_join(colwatjoin37_noNAs, huc12meanslopestd9, "HUC12")
colwatjoin38_noNAs<-na.omit(colwatjoin38)

colwatjoin39<-full_join(colwatjoin38_noNAs, huc8carbkarst, "HUC8")
colwatjoin39$PctCarbKarstH8<-colwatjoin39$PctCarbKarstH8%>% replace(is.na(.),0)
colwatjoin39_noNAs<-na.omit(colwatjoin39)

colwatjoin40<-full_join(colwatjoin39_noNAs, huc8volckarst, "HUC8")
colwatjoin40$PctVolcKarstH8<-colwatjoin40$PctVolcKarstH8%>% replace(is.na(.),0)
colwatjoin40_noNAs<-na.omit(colwatjoin40)

write.csv(colwatjoin40_noNAs, "ColdWaterJoin.csv", row.names=FALSE )
names(colwatjoin40_noNAs)









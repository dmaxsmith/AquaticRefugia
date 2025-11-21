##1st step: read in data layers prepared in ArcGIS Pro##

rm(list=ls(all=TRUE))
setwd("E:/2023_CurrentNM_Work/R/Tables")
library(tidyverse)


###Base layers###
huc12s<-read.csv("huc12s.csv") ##12th-digit hydrologic unit code boundaries, clipped to the state of New Mexico's borders##


##Biotic value layers##
huc12totaqusperic<-read.csv("huc12totaqusperic.csv") ##Total aquatic species richness - EnviroAtlas data
huc12nataquvulind<-read.csv("huc12nataquvulind.csv") ##Native Aquatic Species Vulnerability Index - EnviroAtlas data
huc12perwatherps<-read.csv("huc12perwatherpcount.csv" ) ##Species richness of snakes, amphs, and turtles that rely on per. water - IUCN and USGS data
huc12ephcatamps<-read.csv("huc12ephcatampcount.csv" ) ##Species richness amphibians that rely on ephemeral catchments - USGS data
HUC12fishes<-read.csv("huc12fishcount.csv" ) ##Species richness of fishes - IUCN data 
#HUC12lakes ##?##
#HUC12Springs ##?##

##Climate layers##
huc8diftemwetqua<-read.csv("huc8diftemwetqua.csv")##mean difference temperature wettest quarter Bio8
huc8diftemdriqua<-read.csv("huc8difftemdriqua.csv")##mean difference temperature driest quarter Bio9
huc8difanntem<-read.csv("huc8difanntem.csv")##Mean difference annual temperature Bio1
huc8difmaxtemwarmon<-read.csv("huc8difmaxtemwarmon.csv")##Mean difference max temperature warmest month Bio5
huc8perchaprewarqua<-read.csv("huc8perchaprewarqua.csv") ##Mean percent change precipitation warmest quarter Bio18
huc8perchaprecolqua<-read.csv("huc8perchaprecolqua.csv")  ##Mean percent change precipitation coldest quarter Bio19
huc8perchaannpre<-read.csv("huc8perchaannpre.csv") ##Mean percent change annual precipitation Bio12
huc8perchaprewetmon<-read.csv("huc8perchaprewetmon.csv") ##Mean percent change precipitation wettest month Bio13
huc8perchapredrimon<-read.csv("huc8perchapredrimon.csv") ##Mean percent change precipitation driest month Bio14
huc8difet<-read.csv("huc8difet.csv")##Mean difference evapotranspiration 
huc8difhmi<-read.csv("huc8difhmi.csv")##Mean difference heat moisture index
huc8difpnv<-read.csv("huc8difpnv.csv")##Mean difference potential evapotranspiration of natural vegetation
huc8difsmc<-read.csv("huc8difsmc.csv") ##Mean difference soil moisture content
huc8difswe<-read.csv("huc8difswe.csv") ##Mean difference snow water equivalent

huc12diftemwetqua<-read.csv("huc12difftempwetqua.csv") ##mean difference temperature wettest quarter Bio8

huc12diftemdriqua<-read.csv("huc12difftempdriqua.csv") ##mean difference temperature driest quarter Bio9

huc12difanntem<-read.csv("huc12diffanntemp.csv") ##Mean difference annual temperature Bio1

huc12difmaxtemwarmon<-read.csv("huc12diffmaxtemwarmon.csv")##Mean difference max temperature warmest month Bio5

huc12diftempran<-read.csv("huc12difftempran.csv")##Mean difference temperature range Bio7

huc12diftemsea<-read.csv("huc12difftemsea.csv")##Mean difference temperature seasonality Bio4

huc12difiso<-read.csv("huc12difiso.csv")##Mean difference isothermality Bio3

huc12difmeadiuran<-read.csv("huc12diffmeadiuran.csv") ##Mean difference diurnal range Bio2

huc12pctchaprewarqua<-read.csv("huc12pctchaprewarqua.csv") ##Mean percent change precipitation warmest quarter Bio18
huc12perchaprecolqua<-read.csv("huc12perchaprecolqua.csv") ##Mean percent change precipitation coldest quarter Bio19
huc12perchaannpre<-read.csv("huc12perchaannpre.csv")  ##Mean percent change annual precipitation Bio12
huc12perchaprewetmon<-read.csv("huc12perchaprewetmon.csv") ##Mean percent change precipitation wettest month Bio13
huc12perchapredrimon<-read.csv("huc12perchapredrimon.csv") ##Mean percent change precipitation driest month Bio14
huc12forvel<-read.csv("huc12forvel.csv")##Mean forward climate velocity
huc12bacvel<-read.csv("huc12bacvel.csv")##Mean backward climate velocity
huc12dissim<-read.csv("huc12dissim.csv") ##Mean climate dissimilarity 
huc12novelty<-read.csv("huc12novelty.csv") ##Mean climate novelty
huc12noveltysd<-read.csv("huc12noveltysd.csv") ##Std climate novelty
huc12difet<-read.csv("huc12difet.csv") ##Difference in evapotranspiration
huc12difhmi<-read.csv("huc12difhmi.csv") ##Difference in heat moisture index
huc12difpnv<-read.csv("huc12difpnv.csv") ##Difference in PET of natural vegetation
huc12difsmc<-read.csv("huc12difsmc.csv") ##Difference in soil moisture content
huc12difswe<-read.csv("huc12difswe.csv") ##Difference snow water equivalent


##Soil layers##
huc12soilhydgroweldra<-read.csv("huc12soilhydgroweldra.csv") ##pct. with well-drained soils
huc8soilhydgroweldra<-read.csv("huc8soilhydgroweldra.csv") ##pct. with well-drained soils
huc12soilhydgrosloinf<-read.csv("huc12soilhydgrosloinf.csv") ##pct. slow-infiltrating soils
huc8soilhydgrosloinf<-read.csv("huc8soilhydgrosloinf.csv") ##pct. slow-infiltrating soils
huc8negsoibulden<-read.csv("huc8negsoibulden.csv") ##soil bulk density *-1
huc12soibulden <-read.csv("huc12soibulden.csv") ##soil bulk density
huc8soibulden<-read.csv("huc8soibulden.csv") ##soil bulk density
huc8soilhydgrorich<-read.csv("huc8soilhydgrorich.csv") ##number of soil hydrogroups
huc12soilhydgrorich<-read.csv("huc12soilhydgrorich.csv") ##number of soil hydrogroups
huc12minwattab<-read.csv("huc12minwattab.csv") ## minimum water table height
huc12meawatsto<-read.csv("huc12meawatsto.csv") ##Mean soil water storage
huc12meansbd<-read.csv("huc12meansbd.csv") ##mean soil bulk density in huc12s

##Lithology layers##
huc8carbkarst<-read.csv("huc8carbkarst.csv") #pct with carbonate karst
huc8volckarst<-read.csv("huc8volckarst.csv") #pct with volcanic karst
huc8uncon<-read.csv("huc8uncon.csv") #pct with unconsolidated aquifers
huc8aquirich<-read.csv("huc8aquirich.csv") #number of underlying aquifers
huc12carbkarst<-read.csv("huc12carbkarst.csv") #pct with carbonate karst
huc12volckarst<-read.csv("huc12volckarst.csv") #pct with volcanic karst
huc12uncon<-read.csv("huc12uncon.csv") #pct with unconsolidated aquifers
huc12aquirich<-read.csv("huc12aquirich.csv") #number of underlying aquifers

##Topography layers##
huc12meanaspectdiv<-read.csv("huc12meanaspectdiv.csv") ##mean aspect diversity
huc12meancti<-read.csv("huc12meancti.csv") ##mean compond topographic index
huc12meancurve<-read.csv("huc12meancurve.csv") ##mean earth curvature##
huc12meanelev<-read.csv("huc12meanelev.csv") ##Mean elevation##
huc12meanelevdiv<-read.csv("huc12meanelevdiv.csv") ##Mean elevation diversity##
huc12meangeomdiv<-read.csv("huc12stdgeomdiv.csv") ##Mean geomorphologic diversity##
huc12meanhli<-read.csv("huc12meanhli.csv") ##Mean heat load index##
huc12pctnorth<-read.csv("huc12pctnorth.csv") ##pct. northness##
huc12meanrough<-read.csv("huc12meanrough.csv") ##mean roughness##
huc12meanslopestd3<-read.csv("huc12meanslopestd3.csv") ##mean slope std 3x3 cell area##
huc12meanslopestd9<-read.csv("huc12meanslopestd9.csv")  ##mean slope std 9x9 cell area##
huc12meantpi<-read.csv("huc12meantpi.csv") ##mean topographic position index##
huc12meantwi<-read.csv("huc12meantwi.csv") ##mean topographic wetness index##
huc12meanvrm<-read.csv("huc12meanvrm.csv") ##mean vector ruggedness index?##

##Hydrology layers##
huc12wateryield<-read.csv("huc12wateryield.csv") ##water yield
huc12watbodare<-read.csv("huc12watbodare.csv") ##water body area
huc12strden<-read.csv("huc12strden.csv") ##stream density
huc12sprden<-read.csv("huc12sprden.csv")  ##spring density
huc12strtem10<-read.csv("huc12strtem10.csv") ##total miles of streams with mean Aug temps 10 C or less
huc12pctper<-read.csv("huc12pctper.csv")##percent of  stream miles that are perennial
huc12pct10deg<-read.csv("huc12pct10deg.csv") ##percent of stream miles with mean august water temps 10 degrees or less
huc12pct17deg<-read.csv("huc12pct17deg.csv") ##percent of stream miles with mean august water temps 10 degrees or less
huc12plaintlak<-read.csv("huc12plaintlak.csv")

##Vegetation layers##
huc12meacanhei<-read.csv("huc12meacanhei.csv") ##mean canopy height
huc8nmripmarwetmear<-read.csv("huc8nmripmarwetmear.csv") ##pct with wet meadow riparian vegetation
huc12nmripmarwetmear<-read.csv("huc12nmripmarwetmear.csv") ##pct with wet meadow riparian vegetation
huc8nmripnatsemnatwoo<-read.csv("huc8nmripnatsemnatwoo.csv") ##pct with woody riparian vegetation
huc12nmripnatsemnatwoo<-read.csv("huc12nmripnatsemnatwoo.csv") ##pct with woody riparian vegetation
huc12f2fnatcov<-read.csv("huc12f2fnatcov.csv") ##pct with natural cover
huc12f2fripnatcov<-read.csv("huc12f2fripnatcov.csv") ##pct with riparian natural cover
huc12eafpnat<-read.csv("huc12eafpnat.csv") ##pct. natural floodplain cover
huc12eafpfor<-read.csv("huc12eafpfor.csv") ##pct. forested floodplain cover
huc12eafpshr<-read.csv("huc12eafpshr.csv") ##pct. shrubby floodplain cover
huc12eafpwet<-read.csv("huc12eafpwet.csv") ##pct. wetland floodplain cover
huc12eawetcov<-read.csv("huc12eawetcov.csv") ##pct. wetland  cover
huc12eaforwoowetcov<-read.csv("huc12eaforwoowetcov.csv") ##pct. forest or wetland cover
huc12eaheremwetcov<-read.csv("huc12eaheremwetcov.csv") ##pct. emergent wetland cover
huc12easbforwoowet<-read.csv("huc12easbforwoowet.csv") ##pct. stream buffer with forest or wetland cover
huc12easbfor<-read.csv("huc12easbfor.csv") ##pct. stream buffer with forest  cover
huc12easbnatcov<-read.csv("huc12easbnatcov.csv") ##pct. stream buffer natural cover
huc12easbtrecan<-read.csv("huc12easbtrecan.csv") ##pct. stream buffer with tree canopy
huc12corridors<-read.csv("huc12corridors.csv") ##total number of corridors##







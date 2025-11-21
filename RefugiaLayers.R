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

##Climate layers; variables in which greater values = negative impacts are multiplied by -1##
huc8diftemwetqua<-read.csv("huc8diftemwetqua.csv")##mean difference temperature wettest quarter Bio8
huc8negdiftemwetqua<-read.csv("huc8negdiftemwetqua.csv") ##mean difference temperature wettest quarter *-1 
huc8diftemdriqua<-read.csv("huc8difftemdriqua.csv")##mean difference temperature driest quarter Bio9
huc8negdiftemdriquaa<-read.csv("huc8negdiftemdriqua.csv") ##mean difference temperature driest quarter *-1 
huc8difanntem<-read.csv("huc8difanntem.csv")##Mean difference annual temperature Bio1
huc8negdifanntem<-read.csv("huc8negdifanntem.csv") ##Mean difference annual temperature *-1
huc8difmaxtemwarmon<-read.csv("huc8difmaxtemwarmon.csv")##Mean difference max temperature warmest month Bio5
huc8negdifmaxtemwarmon<-read.csv("huc8negdifmaxtemwarmon.csv") ##Mean difference max temperature warmest month *-1
huc8perchaprewarqua<-read.csv("huc8perchaprewarqua.csv") ##Mean percent change precipitation warmest quarter Bio18
huc8perchaprecolqua<-read.csv("huc8perchaprecolqua.csv")  ##Mean percent change precipitation coldest quarter Bio19
huc8perchaannpre<-read.csv("huc8perchaannpre.csv") ##Mean percent change annual precipitation Bio12
huc8perchaprewetmon<-read.csv("huc8perchaprewetmon.csv") ##Mean percent change precipitation wettest month Bio13
huc8perchapredrimon<-read.csv("huc8perchapredrimon.csv") ##Mean percent change precipitation driest month Bio14
huc8difet<-read.csv("huc8difet.csv")##Mean difference evapotranspiration 
huc8negdifet<-read.csv("huc8negdifet.csv") ##Mean difference evapotranspiration *-1
huc8difhmi<-read.csv("huc8difhmi.csv")##Mean difference heat moisture index
huc8negdifhmi<-read.csv("huc8negdifhmi.csv") ##Mean difference heat moisture index *-1
huc8difpnv<-read.csv("huc8difpnv.csv")##Mean difference potential evapotranspiration of natural vegetation
huc8negdifpnv<-read.csv("huc8negdifpnv.csv") ##Mean difference potential evapotranspiration of natural vegetation *-1

huc8difsmc<-read.csv("huc8difsmc.csv") ##Mean difference soil moisture content
huc8difswe<-read.csv("huc8difswe.csv") ##Mean difference snow water equivalent

huc12diftemwetqua<-read.csv("huc12difftempwetqua.csv") ##mean difference temperature wettest quarter Bio8
huc12negdiftempwetqua<-read.csv("huc12negdifftempwetqua.csv") ##mean difference temperature wettest quarter *-1
huc12diftemdriqua<-read.csv("huc12difftempdriqua.csv") ##mean difference temperature driest quarter Bio9
huc12negdiftempdriqua<-read.csv("huc12negdifftempdriqua.csv") ##mean difference temperature driest quarter *-1 
huc12difanntem<-read.csv("huc12diffanntemp.csv") ##Mean difference annual temperature Bio1
huc12negdifanntem <-read.csv("huc12negdiffanntemp.csv") ##Mean difference annual temperature *-1
huc12difmaxtemwarmon<-read.csv("huc12diffmaxtemwarmon.csv")##Mean difference max temperature warmest month Bio5
huc12negdifmaxtemwarmon<-read.csv("huc12negdiffmaxtemwarmon.csv") ##Mean difference max temperature warmest month *-1
huc12diftempran<-read.csv("huc12difftempran.csv")##Mean difference temperature range Bio7
huc12negdiftempran<-read.csv("huc12negdifftempran.csv") ##Mean difference temperature range *-1
huc12diftemsea<-read.csv("huc12difftemsea.csv")##Mean difference temperature seasonality Bio4
huc12negdiftemsea<-read.csv("huc12negdifftemsea.csv") ##Mean difference temperature seasonality *-1
huc12difiso<-read.csv("huc12difiso.csv")##Mean difference isothermality Bio3
huc12negdifiso<-read.csv("huc12negdifiso.csv") ##Mean difference isothermality *-1
huc12difmeadiuran<-read.csv("huc12diffmeadiuran.csv") ##Mean difference diurnal range Bio2
huc12negdifmeadiuran<-read.csv("huc12negdiffmeadiuran.csv") ##Mean difference diurnal range *-1
huc12pctchaprewarqua<-read.csv("huc12pctchaprewarqua.csv") ##Mean percent change precipitation warmest quarter Bio18
huc12perchaprecolqua<-read.csv("huc12perchaprecolqua.csv") ##Mean percent change precipitation coldest quarter Bio19
huc12perchaannpre<-read.csv("huc12perchaannpre.csv")  ##Mean percent change annual precipitation Bio12
huc12perchaprewetmon<-read.csv("huc12perchaprewetmon.csv") ##Mean percent change precipitation wettest month Bio13
huc12perchapredrimon<-read.csv("huc12perchapredrimon.csv") ##Mean percent change precipitation driest month Bio14
huc12forvel<-read.csv("huc12forvel.csv")##Mean forward climate velocity
huc12negforvel<-read.csv("huc12negforvel.csv") ##Mean forward climate velocity *-1
huc12bacvel<-read.csv("huc12bacvel.csv")##Mean backward climate velocity
huc12negbacvel<-read.csv("huc12negbacvel.csv") ##Mean backward climate velocity *-1
huc12dissim<-read.csv("huc12dissim.csv") ##Mean climate dissimilarity 
huc12negdissim<-read.csv("huc12negdissim.csv") ##Mean climate dissimilarity *-1
huc12novelty<-read.csv("huc12novelty.csv") ##Mean climate novelty
huc12negnovelty<-read.csv("huc12negnovelty.csv") ##Mean climate novelty *-1
huc12noveltysd<-read.csv("huc12noveltysd.csv") ##Std climate novelty
huc12difet<-read.csv("huc12difet.csv") ##Difference in evapotranspiration
huc12negdifet<-read.csv("huc12difhmi.csv") ##Difference in evapotranspiration *-1
huc12difhmi<-read.csv("huc12difhmi.csv") ##Difference in heat moisture index
huc12negdifhmi<-read.csv("huc12negdifhmi.csv") ##Difference in heat moisture index *-1
huc12difpnv<-read.csv("huc12difpnv.csv") ##Difference in PET of natural vegetation
huc12negdifpnv<-read.csv("huc12negdifpnv.csv") ##Difference in PET of natural vegetation *-1
huc12difsmc<-read.csv("huc12difsmc.csv") ##Difference in soil moisture content
huc12difswe<-read.csv("huc12difswe.csv") ##Difference snow water equivalent


##Soil layers##
huc12soilhydgroweldra<-read.csv("huc12soilhydgroweldra.csv") ##pct. with well-drained soils
huc8soilhydgroweldra<-read.csv("huc8soilhydgroweldra.csv") ##pct. with well-drained soils
huc12soilhydgrosloinf<-read.csv("huc12soilhydgrosloinf.csv") ##pct. slow-infiltrating soils
huc8soilhydgrosloinf<-read.csv("huc8soilhydgrosloinf.csv") ##pct. slow-infiltrating soils
huc8negsoibulden<-read.csv("huc8negsoibulden.csv") ##soil bulk density *-1
huc12negsoibulden<-read.csv("huc12negsoibulden.csv") ##soil bulk density 
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

##Future change layers##
huc12meaanflo2040<-read.csv("huc12meaanflo2040.csv") ##Change in mean annual flow
huc121_5yrflo2040<-read.csv("huc121_5yrflo2040.csv") ##Change in 5 year peak flow
huc1210yrflo2040<-read.csv("huc1210yrflo2040.csv") ##Change in 10 year peak flow
huc1225yrflo2040<-read.csv("huc1225yrflo2040.csv") ##Change in 25 year peak flow
huc12lowflodat2040<-read.csv("huc12lowflodat2040.csv") ##Change in low flow date
huc12lowflovol2040<-read.csv("huc12lowflovol2040.csv") ##change in low flow volume
huc12basfloind2040<-read.csv("huc12basfloind2040.csv") ##Change in base flow index
huc12winflo2040<-read.csv("huc12winflo2040.csv") ##Change in winter low flow
huc12junflo2040<-read.csv("huc12junflo2040.csv") ##Change in mean June flow
huc12sumflo2040<-read.csv("huc12sumflo2040.csv") ##Change in mean Jun, Jul, Aug flow
huc12strtemcha2040<-read.csv("huc12strtemcha2040.csv") ##Mean August stream temperature change
huc12negstrtemcha2040<-read.csv("huc12negstrtemcha2040.csv") ##Negative mean August stream temperature change


##Disturbance layers
huc12agslope3<-read.csv("huc12agslope3.csv") ##Agriculture cover on 3 degree slopes
huc12paslope3<-read.csv("huc12paslope3.csv") ##Pasture cover on 3 degree slopes
huc12crslope3<-read.csv("huc12crslope3.csv") ##Crop cover on 3 degree slopes
huc12agslope9<-read.csv("huc12agslope9.csv") ##Agriculture cover on 9 degree slopes
huc12paslope9<-read.csv("huc12paslope9.csv") ##Pasture cover on 9 degree slopes
huc12crslope9<-read.csv("huc12crslope9.csv") ##Crop cover on 9 degree slopes
huc12eadevcov<-read.csv("huc12eadevcov.csv") ##Developed land cover
huc12eaagcov<-read.csv("huc12eaagcov.csv")  ##Agriculture land cover
huc12eapascov<-read.csv("huc12eapascov.csv") ##Pasture cover
huc12eacrocov<-read.csv("huc12eacrocov.csv") ##Crop cover
huc12insdis<-read.csv("huc12insdis.csv") ##Insect and disease risk
huc12eafpdev<-read.csv("huc12eafpdev.csv") ##Floodplain development
huc12eafpagr<-read.csv("huc12eafpagr.csv") ##Floodplain agriculture cover
huc12eafpdcro<-read.csv("huc12eafpdcro.csv") ##Floodplain crop cover
huc12pctimp<-read.csv("huc12pctimp.csv") ##Pct. stream miles with impairments (pollution, etc.) 
huc12devind4045<-read.csv("huc12devind4045.csv") ##Development index for 2040 under rcp.45 scenario
huc12devind4085<-read.csv("huc12devind4085.csv") ##Development index for 2040 under rcp.85 scenario
huc12watquacha4045<-read.csv("huc12watquacha4045.csv") ##Projected change in water quantity under rcp.45 scenario
huc12watquacha4085<-read.csv("huc12watquacha4085.csv") ##Projected change in water quantity under rcp.85 scenario
huc12wildfirechange<-read.csv("huc12wildfirechange.csv") ##Projected wildfire risk


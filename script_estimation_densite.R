
#devtools::create(path = "R:/Fhabitat/DS_functions\\FormatDataToGDS")

#########################################################""

#############  PREPA DONNEES FAUNE

#########################################################""

### Etape 1 ### chargement données faune
dfRawData = read.csv2("C:/Users/Thomas.Denis/Desktop/FormatDataToGDS/data/ltsampl_data21112017.csv")

### Etape 2 ### Arrangement et Nettoyage
dfRawData$layon[dfRawData$layon == "PitBarJ"] <- "PitBarH"
dfRawData$layon[dfRawData$layon == "PitBarF" & dfRawData$date == "31/10/2001"] <- "PitBarI"
dfRawData$layon[dfRawData$layon == "PitBarF" & dfRawData$date == "05/11/2001"] <- "PitBarI"
dfRawData <- dfRawData[dfRawData$site != "Camo",]
dfRawData$site <- factor(dfRawData$site)

### Etape 3 ###concatener la date et le sens pour pouvoir implementer les numéro de réplicats avec la fonction AddRepNumber ci-dessous
dfRawData$dateSens <- paste(dfRawData$date,dfRawData$sens,sep="_")



### Etape 4 ### Cette fonction permet d'attribuer à chaque observation (et layon vide également) un numéro de réplicat par sample. Les layons (subSample)
### sont regroupés par sample. Les observations sont ordonnées en fonction de la date et le sens (matin/après-midi), et l'effort d'échantillonnage. Ainsi, les
### observations avec une effort d'échantillonnage faible (layon parcouru partiellement) à un numéro de réplicat élevé.


dfRawData2 <- CreateFData(
  species=dfRawData$sp_latin,
  number=dfRawData$nb,
  preciselyCounted=dfRawData$nb_exact,
  distance=dfRawData$dist_per,
  sample=dfRawData$site,
  subSample=dfRawData$layon,
  order=dfRawData$dateSens,
  samplingEffort=dfRawData$long)


### Etape 5 ### nombre de réplicats par layon (subSample)
vNbReplicat <- ReplicatePerSubSample(dfRawData2)
hist(ReplicatePerSubSample(dfRawData2))


### Etape 6 ### Visualiser la perte d'effort d'échantillonnage avec le nombre de sample (dépendant du nombre de réplicats disponibles)
PlotSampleNumberVsSamplingEffort(data=dfRawData2,seqReplicat=6:15)

### Etape 7 ### nombre de réplicats retenus
nbReplicat <- 12

### Etape 8 ### Enlever les layons avec moins de 12 réplicats
dfRawData3 <- dfRawData2[dfRawData2$subSample %in% names(vNbReplicat[vNbReplicat >= nbReplicat]),]

### Etape 9 ### Enlever les réplicats au dessus de 12
dfRawData3 <- dfRawData3[dfRawData3$replicateNumber <= nbReplicat,]

### Etape 10 ### enlever les levels qui n'existent plus suite aux deux précedents étapes
dfRawData3 <- gdata::drop.levels(dfRawData3)

### Etape 11 ### calcul des longueurs effectués pour chaque sample et chaque réplicat
(dfSamplingEffort <- CalculateSamplingEffort(dfRawData3))


### Etape 12 ### nombre d'observations par espèce
ObsNumberPerSpecies(dfRawData3)

### Etape 13 ### regrouper certains factors du vector species (ici Cebus). Le vector species est ecrasé par le nouveau.
### Revenir à l'étape 12 pour voir le résultat
dfRawDataCebus <- dfRawData3
dfRawDataCebus$species <- ReplaceSpeciesName(
  speciesVector=dfRawDataCebus$species,taxonReplacement="Cebus",speciesNames=c("Cebus apella","Cebus olivaceus","Cebus sp.")[1]
)

### Etape 14 ### création d'un objet DataYGDS avec les données brutes utilisés ($data), un objet y (array - 3 dimensions) où sont présents les observations
### un objet unmarkedFrameGDS pour insérer dans la fonction gdistsamp du package unmarked

DataYGDSCebus <- CreateFDataYGDS(data=dfRawDataCebus, ### Données brutes
                                 siteCovs=NULL, ### data.frame si nécessaire (attention à l'ordre des samples)
                                 samplingEffort=dfSamplingEffort, ### effort d'échantillonnage (fonction ci-dessus)
                                 nbReplicat=nbReplicat, ### nombre de réplicats choisi
                                 stripWidth=80, ### truncation (les observations avec des valeurs supérieures sont supprimées)
                                 DistanceClass=5, ### intervalle des classe de distance (ex: "0-5" = c(0,1,2,3,4), "5-10" = c(5,6,7,8,9), etc)
                                 unitsIn="m", ### unité des distances
                                 survey="line", ### méthode de DS utilisé
                                 species="Cebus", ### espèce choisi
                                 groupNumber=TRUE, ### TRUE: l'abondance est calculée en nbre de GROUPES par unité de surface
                                 ### Toutes les valeurs number de data sont remplacées par 1, même les NA
                                 naRmNumber=FALSE, ### TRUE: les lignes où lareplace valeur number de data est NA sont supprimées
                                 naRmDistance=FALSE, ### TRUE: les lignes où la valeur distance de data est NA sont supprimées
                                 missingValuesNumber="mean", ### "mean": les NA de number sont remplacés par la moyenne des number du sample
                                 ###, "random": les NA de number sont remplacés de façon aléatoire parmi les number du sample
                                 missingValuesDistance="mean") ### "mean": les NA de distance sont remplacés par la moyenne des distances du sample
###, "random": les NA de distance sont remplacés de façon aléatoire parmi les distance du sample

### Etape 15.1 ### visualisation des observation en fonction des classe de distance
PlotObservationPerDistanceClass(DataYGDSCebus)

### Etape 16.1 ### différence entre les observations des données brutes ()
TruncatedObservation(DataYGDSCebus)

library(unmarked)
### Etape 17.1 ### inférence du modèle
gdistsampCebus <- gdistsamp(~1,~1,~1, DataYGDSCebus$FrameGDS, ### data: $GDSData de l'objet DataYGDS
                            keyfun ="halfnorm",unitsOut = "kmsq",
                            output="density", mixture = "NB",K=80)

### Etape 18.1 ### estimation de la variable latente: abundance de la superpopulation en nbre de GROUPES
MCebus <- bup(ranef(gdistsampCebus,stat="mean"))

### Etape 19.1 ### estimation de la migration temporaire phi
phiCebus <- 1/(1+exp(-gdistsampCebus@estimates@estimates$phi@estimates))

### Etape 20.1 ### estimation de l'abondance de la population en nbre de GROUPES
NCebus <- MCebus*phiCebus

### Etape 21.1 ### estimation de la taille moyenne des GROUPES
MGSCebus <- CalculateMGS(data=dfRawDataCebus,species="Cebus",threshold = 5,sampleName=rownames(DataYGDSCebus$FrameGDS@y),
                         preciselyCountedValue = "oui")

### Etape 22.1 ### estimation de l'abondance de la population en nbre de INDIVIDUS
NIndCebus <- NCebus*MGSCebus
NIndCebus

### Etape 15.2 ### Fait tout ce qu'il y a entre 15.1 et 21.1
DensityCebus <- SpeciesDensityEstimation(data = DataYGDSCebus)
DensityCebus$Density*MGSCebus


DataYGDSCebus5<- CreateFDataYGDS(data=dfRawDataCebus,samplingEffort=dfSamplingEffort,
                                  species="Cebus",
                                  groupNumber=TRUE,
                                  naRmDistance=TRUE)
DensityCebus5 <- SpeciesDensityEstimation(data = DataYGDSCebus5)
DensityCebus5$Density*MGSCebus

DataYGDSCebus2 <- CreateFDataYGDS(data=dfRawDataCebus,samplingEffort=dfSamplingEffort,
                                  species="Cebus",
                                  groupNumber=FALSE,
                                  naRmNumber=FALSE)
DensityCebus2 <- SpeciesDensityEstimation(data = DataYGDSCebus2)
PlotObservationPerDistanceClass(DataYGDSCebus2)
DensityCebus2$Density

DataYGDSCebus3 <- CreateFDataYGDS(data=dfRawDataCebus,samplingEffort=dfSamplingEffort,
                                  species="Cebus",
                                  groupNumber=FALSE,
                                  naRmNumber=FALSE,
                                  missingValuesDistance="random")
PlotObservationPerDistanceClass(DataYGDSCebus3)
DensityCebus3 <- SpeciesDensityEstimation(data = DataYGDSCebus3)
DensityCebus3$Density




DataYGDSCebus4 <- CreateFDataYGDS(data=dfRawDataCebus,samplingEffort=dfSamplingEffort,
                                  species="Cebus",
                                  groupNumber=FALSE,
                                  naRmDistance=TRUE)
PlotObservationPerDistanceClass(DataYGDSCebus4)
DensityCebus4 <- SpeciesDensityEstimation(data = DataYGDSCebus4)
DensityCebus4$Density


DataYGDSCebus6 <- CreateFDataYGDS(data=dfRawDataCebus,samplingEffort=dfSamplingEffort,
                                  species="Cebus",
                                  groupNumber=TRUE,
                                  naRmDistance=TRUE)
PlotObservationPerDistanceClass(DataYGDSCebus6)
DensityCebus6 <- SpeciesDensityEstimation(data = DataYGDSCebus6)
DensityCebus6$Density
















dfRawData4 <- dfRawData3
dfRawData4$species <- ChangeSpeciesName(
  speciesVector=dfRawData3$species,speciesReplacement="Tinamidae",
  speciesNames=c("Crypturellus sp. Ou Tinamus sp.","Crypturellus sp.","Tinamus major")
)

### Etape 15 ### Choix des espèces: Au moins 20 observations, présentes dans au moins 5 samples
sum(unlist(lapply(1:length(unique(dfRawData4$species)),function(i) {length(unstack(dfRawData4,number~species)[[i]])}))>=20)
vSpObs20 <- names(unstack(dfRawData4,number~species))[unlist(lapply(1:length(unique(dfRawData4$species)),function(i) {length(unstack(dfRawData4,number~species)[[i]])}))>=20]
vSpSample5 <- names(unstack(dfRawData4,sample~species))[unlist(lapply(1:length(unique(dfRawData4$species)),function(i) {length(unique(unstack(dfRawData4,sample~species)[[1]]))}))>=5]
vSpSelec <- vSpObs20[vSpObs20 %in% vSpSample5]
vSpSelec <- vSpSelec[vSpSelec != ""]
vSpeciesName <- vSpSelec
vSpeciesName <- vSpeciesName[order(vSpeciesName)]
vSpeciesName <- gsub(" ","_",vSpeciesName)
vNbSample <- length(unique(dfRawData3$sample))

dfRawData3$species <- gsub(" ","_",dfRawData3$species)


##################      ESTIMATION DENSITE ESPECES          ########################
##################                                          ########################

vLinkHN <- rep("NB",20)
vLinkHN[7] <- "NB"
vLinkHN[10] <- "P"
vLinkHN[14] <- "P"
lmodSpHN[[20]] <- NA

vLinkHR <- rep("NB",20)
vLinkHR[2] <- "P"
vLinkHR[6] <- "P"
vLinkHR[7] <- "NB"
vLinkHR[10] <- "P"
vLinkHR[14] <- "P"




CebusInd <- SpeciesDensityEstimation(data = dfRawDataCebus,
                                     samplingEffort = dfSamplingEffort,
                                     species="Cebus",
                                     groupNumber=FALSE)

CebusIndNaRmNumber <- SpeciesDensityEstimation(data = dfRawDataCebus,
                                               samplingEffort = dfSamplingEffort,
                                               species="Cebus",
                                               groupNumber=FALSE,
                                               naRmNumber=TRUE)

AMacconnelliNBHN <- SpeciesDensityEstimation(vSpeciesName[1],modelGDS="NB",methodGDS="Nelder-Mead",keyfunGDS="halfnorm")
AMacconnelliPHN <- SpeciesDensityEstimation(vSpeciesName[1],modelGDS="P",methodGDS="BFGS",keyfunGDS="halfnorm")
AMacconnelliNBHZ <- SpeciesDensityEstimation(vSpeciesName[1],modelGDS="NB",methodGDS="Nelder-Mead",keyfunGDS="hazard") ###
AMacconnelliPHZ <- SpeciesDensityEstimation(vSpeciesName[1],modelGDS="P",methodGDS="BFGS",keyfunGDS="hazard") ### OK AIC

APaniscusNBHN <- SpeciesDensityEstimation(vSpeciesName[2],modelGDS="NB",methodGDS="BFGS",keyfunGDS="halfnorm") ### OK AIC
APaniscusPHN <- SpeciesDensityEstimation(vSpeciesName[2],modelGDS="P",methodGDS="BFGS",keyfunGDS="halfnorm")
APaniscusNBHZ <- SpeciesDensityEstimation(vSpeciesName[2],modelGDS="NB",methodGDS="BFGS",keyfunGDS="hazard") ### Hessian matrix singular
APaniscusPHZ <- SpeciesDensityEstimation(vSpeciesName[2],modelGDS="P",methodGDS="BFGS",keyfunGDS="hazard")
sqrt(mean((APaniscusNBHN$IndividualDensity -APaniscusPHN$IndividualDensity)^2))

SApellaNBHN <- SpeciesDensityEstimation(vSpeciesName[3],modelGDS="NB",methodGDS="BFGS",keyfunGDS="halfnorm") ### OK AIC
SApellaPHN <- SpeciesDensityEstimation(vSpeciesName[3],modelGDS="P",methodGDS="BFGS",keyfunGDS="halfnorm")
SApellaNBHZ <- SpeciesDensityEstimation(vSpeciesName[3],modelGDS="NB",methodGDS="BFGS",keyfunGDS="hazard")
SApellaPHZ <- SpeciesDensityEstimation(vSpeciesName[3],modelGDS="P",methodGDS="BFGS",keyfunGDS="hazard")


COlivaceusNBHN <- SpeciesDensityEstimation(vSpeciesName[4],modelGDS="NB",methodGDS="BFGS",keyfunGDS="halfnorm") ### OK AIC
COlivaceusPHN <- SpeciesDensityEstimation(vSpeciesName[4],modelGDS="P",methodGDS="BFGS",keyfunGDS="halfnorm")
COlivaceusNBHZ <- SpeciesDensityEstimation(vSpeciesName[4],modelGDS="NB",methodGDS="BFGS",keyfunGDS="hazard")
COlivaceusPHZ <- SpeciesDensityEstimation(vSpeciesName[4],modelGDS="P",methodGDS="BFGS",keyfunGDS="hazard")



AMacconnelli <- SpeciesDensityEstimation(vSpeciesName[5])
AMacconnelli <- SpeciesDensityEstimation(vSpeciesName[6])
AMacconnelli <- SpeciesDensityEstimation(vSpeciesName[7])
AMacconnelli <- SpeciesDensityEstimation(vSpeciesName[8])
AMacconnelli <- SpeciesDensityEstimation(vSpeciesName[9])
AMacconnelli <- SpeciesDensityEstimation(vSpeciesName[10])
AMacconnelli <- SpeciesDensityEstimation(vSpeciesName[11])
AMacconnelli <- SpeciesDensityEstimation(vSpeciesName[12])
AMacconnelli <- SpeciesDensityEstimation(vSpeciesName[13])
AMacconnelli <- SpeciesDensityEstimation(vSpeciesName[14])
AMacconnelli <- SpeciesDensityEstimation(vSpeciesName[15])
AMacconnelli <- SpeciesDensityEstimation(vSpeciesName[16])
AMacconnelli <- SpeciesDensityEstimation(vSpeciesName[17])
AMacconnelli <- SpeciesDensityEstimation(vSpeciesName[18])
AMacconnelli <- SpeciesDensityEstimation(vSpeciesName[19])
AMacconnelli <- SpeciesDensityEstimation(vSpeciesName[20])



par(mfrow=c(5,5))
for (i in 1:20)
{
  plot((rowSums(lSp[[i]]@y)/(transects.length*12))*10000,dfDensDS.Ngr[,i],main=sp[i],xlab="IKA (grp/10km)",ylab="Density (Grp/km2)")
}

write.csv2(dfDensDS.ind,file="species_individuals_abundance_table.csv")









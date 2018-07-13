#Evan Newell 2015, Ming 2016, Etienne Becht 2016, Evan 2017,2018

## PARAMETERS TO EDIT:

#Choose functions to run
RunAlgorithms = T
RunMeaningPlot = T
RunFileHeatplots = T
    #IF TRUE  ... In progress
    DoClusters=T
    clustParam="Phenograph"  ## e.g., "Phenograph"

#Main Parameters
sourceFcsFolder = "WongTraffickGatedTNK" #Folder with samples (even 1 sample ok)
outputSuffix = "out-10k"
useCSV = F   # T if using .csv files instead of fcs files - will make csv output als
CyTOF = T  # T for CyTOF logicle transform settings F for fluorescence flow
TransformOutput = F  # F Puts derived data on linear scale 1-10k - best for viewing in FJ10 otherwise T is setup for FJ9. Talk to Evan about implications for one-sense
ceil = 10000
#number of events to take per sample (unless sample has less)
FNnames="./annot/Trafficknames2.csv" #Parameters to analyze in csv file
OutputSuffix = outputSuffix # Adds this to all new directories and file names
DotSNE = T
tSNEperpelxity = 30 #default = 30; increased perplexity => increased spread
DoOneSENSE = F  #needs a modified names.csv file with column names for each category
Dophenograph = T  #clusters cells using Rphenograpy
kValue = 20  #default = 30; k is low => more clusters and inversely
DoIsomap = F
DoDiffMap = F
DoPCA = F
Do3DtSNE = F #T for running 3D tSNE
DoUMAP =T

## meaningplot paramters:
MeaningTopPercentile = .999
MeaningBotPercentile = .001
Xaxis = "UMAP1"   ## examples: tSNE1 or UMAP1
Yaxis = "UMAP2"  ## examples: tSNE2 or UMAP1
DoOneForEach = F
prefix = paste0(sourceFcsFolder,"_2_",OutputSuffix)
palette=c("black","blue","green","yellow","red")
color.scale.type="relative"  # choose "relative" or "absolute"
resolution=150
cex=0.15
pch=16

# FileHeatplot (medians)
fileHPoutputsuffix = OutputSuffix
HPpalette=c("black","blue","lightblue","green","yellow","darkorange","darkred")










## No edit below here

source('FCStSNEone.R')
sourceFcsForMeaningPlot <- paste0(sourceFcsFolder,"_",outputSuffix)
if (RunAlgorithms) 
  FCStSNEone(LoaderPATH = sourceFcsFolder, 
           useCSV = useCSV, # T if using .csv files instead of fcs files - will make csv output als
           CyTOF = CyTOF, # T for CyTOF logicle transform settings F for fluorescence flow
           TransformOutput = TransformOutput, # F Puts derived data on linear scale 1-10k - best for viewing in FJ10 otherwise T is setup for FJ9. Talk to Evan about implications for one-sense
           ceil = ceil, #number of events to take per sample (unless sample has less)
           FNnames=FNnames,#Parameters to analyze in csv file
           OutputSuffix = outputSuffix, # Adds this to all new directories and file names
           DotSNE = DotSNE,
           tSNEperpelxity = tSNEperpelxity, #default = 30; increased perplexity => increased spread
           DoOneSENSE = DoOneSENSE, #needs a modified names.csv file with column names for each category
           Dophenograph = Dophenograph, #clusters cells using Rphenograpy
           kValue = kValue, #default = 30; k is low => more clusters and inversely
           DoIsomap = DoIsomap,
           DoDiffMap = DoDiffMap,
           DoPCA = DoPCA, 
           Do3DtSNE = Do3DtSNE, #T for running 3D tSNE
           DoUMAP = DoUMAP) #still in prep- will add many parameters for this


## Still in prep:
if (RunMeaningPlot)
   meaningPlot(LoaderPATH =sourceFcsForMeaningPlot,
            useCSV = useCSV, # T if using .csv files instead of fcs files - will make csv output als
            CyTOF = CyTOF,
            FNnames = FNnames,
            ceil = ceil,
            TransformOutput = TransformOutput,
            MeaningTopPercentile = .99,
            MeaningBotPercentile = .01,
            PC1 = Xaxis,
            PC2 = Yaxis,
            DoOneForEach = DoOneForEach,
            prefix = prefix,
            palette=palette,
            color.scale.type=color.scale.type,
            resolution=resolution,
            cex=cex,
            pch=pch)

if(RunFileHeatplots)
  fileHeatplot(LoaderPATH =sourceFcsForMeaningPlot,
                          useCSV = useCSV, # T if using .csv files instead of fcs files - will make csv output als
                          CyTOF = CyTOF,
                          ceil = ceil,
                          TransformOutput = TransformOutput,
                          FNnames = FNnames,
                          OutputSuffix = fileHPoutputsuffix,
                          palette=HPpalette,
                         DoClusters=DoClusters,
                         clustParam=clustParam)



##### Useful script for making names.csv file:
# if (!require(flowCore)) { 
#   source("http://bioconductor.org/biocLite.R")
#   biocLite("flowCore")
# } 
# 
# inFileN <- "test.fcs"        #FCS file to make names file for 
# outNamesCsv <- "names.csv"   #Output csv file name 
# 
# FF <- read.FCS(inFileN)
# colNames <- FF@parameters$desc
# empties <- which(is.na(colNames) | colNames== " ")
# colNames[empties] <- FF@parameters$name[empties]
# write.csv(colNames, outNamesCsv, row.names = F)

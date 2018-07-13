library(RColorBrewer)
source("../utils.R")

## Loading FItSNE - Has to be installed separetely first, see https://github.com/KlugerLab/FIt-SNE
source("../FIt-SNE/fast_tsne.R")

## Loading the data
load("./data/Samusik_all/dataset.RData")
xp=d$xp_src
regular_channels=subset(read.csv("./annots/template_Samusik_01.csv",header=TRUE,stringsAsFactors=FALSE),Select=="y")[,2]

## Saving transformed and column-subsetted dataset for later benchmarking
xp_tmp=xp
xp=xp[,regular_channels]
save(xp,file="./rdata/xp.RData")
xp=xp_tmp
rm(xp_tmp)

## Annotations (Manual gates ; ref Samusik et al, Nature Methods, 2016)
manual_labels=d$xp_src[,"label"]
manual_labels[is.na(manual_labels)]=0
gs=read.table(file="./annots/population_names_Samusik_and_gating.csv",sep=",",header=TRUE,stringsAsFactors=FALSE,check.names=FALSE)
codePops=c("0"="Ungated",setNames(gs[,2],gs[,1]))
populations_manual=codePops[as.character(manual_labels)]
save(populations_manual,file="./rdata/populations_manual.RData")

## Loading the UMAP module
library(reticulate)
use_python("/usr/bin/python3",TRUE)
umap_module=import("umap",convert=TRUE)

## Computing UMAP
umap=umap_module$UMAP(n_neighbors=15L,min_dist=0.2,metric="euclidean",verbose=TRUE,random_state=123L)$fit_transform(xp[,regular_channels])
colnames(umap)=c("UMAP1","UMAP2")
save(umap,file="./rdata/umap.RData")

## Computing FItSNE
fitsne=fftRtsne(xp[,regular_channels],rand_seed=123L)
colnames(fitsne)=c("FItSNE1","FItSNE2")
save(fitsne,file="./rdata/fitsne.RData")

## Computing FItSNE with late-exageration
fitsnele=fftRtsne(xp[,regular_channels],rand_seed=123L,start_late_exag_iter=800L,late_exag_coeff=4)
colnames(fitsnele)=c("FItSNE_le1","FItSNE_le2")
save(fitsnele,file="./rdata/fitsnele.RData")

## Computing Barnes-Hut tSNE
set.seed(123)
library(Rtsne)
tsne=Rtsne(xp[,regular_channels])$Y
colnames(tsne)=c("tSNE1","tSNE2")
save(tsne,file="./tSNE.RData")

####################
## Phenograph
####################

set.seed(123)
library(Rphenograph) ## https://github.com/JinmiaoChenLab/Rphenograph
sink(tempfile())
populations=Rphenograph(xp[,regular_channels])
sink()
populations=membership(populations[[2]])
save(populations,file="./rdata/populations.RData")

####################
## FIt-SNE : Benchmarking of the late exaggeration coefficient
####################

set.seed(123)
w=sort(sample(1:nrow(xp),300000))
late_ex=c(0.5,1,2,4,8,16)
fitsnele_bm=sapply(late_ex,function(x){
    fftRtsne(xp[w,regular_channels],start_late_exag_iter=800L,late_exag_coeff=x,rand_seed=123L,nthreads=0L)
},simplify=FALSE)
names(fitsnele_bm)=late_ex
save(fitsnele_bm,file="./rdata/fitsnele_bm.RData")


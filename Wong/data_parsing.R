## Parsing Wong's dataset for further benchmarking

## Loading and parsing
library(flowCore)
d=read.FCS("./Concat files/10k.fcs",ignore.text.offset=TRUE,truncate_max_range=FALSE)
xp=exprs(d)
targets = d@parameters$desc
targets[is.na(targets)] = d@parameters$name[is.na(targets)]
colnames(xp)=targets

## Column subsetting
regular_channels=read.csv("./annot/trafficknames2.csv",stringsAsFactors=FALSE)
regular_channels=regular_channels[regular_channels[,2]=="y",1]

## Data transformation
lgcl <- logicleTransform(w=0.25, t=16409, m=4.5, a=0)
xp=xp[,regular_channels]
xp=apply(xp,2,lgcl)

## Export
save(xp,file="./RData/xp.RData")

## Exporting DR ran separetely (and unseeded, but reproducibility is shown in supplementary figures for UMAP)
umap=exprs(d)[,c("UMAP1","UMAP2")]
tsne=exprs(d)[,c("tSNE1","tSNE2")]
save(umap,file="./RData/umap.RData")
save(tsne,file="./RData/tsne.RData")


## Running FItSNE
source("../utils.R")
source("../FIt-SNE/fast_tsne.R")
fitsne=fftRtsne(xp,rand_seed=123L)
colnames(fitsne)=c("FItSNE1","FItSNE2")
save(fitsne,file="./RData/fitsne.RData")

## Running FItSNE late exaggeration
fitsnele=fftRtsne(xp,rand_seed=123L,start_late_exag_iter=800L,late_exag_coeff=4)
colnames(fitsne)=c("FItSNE1","FItSNE2")
save(fitsnele,file="./RData/fitsnele.RData")

## Exporting Phenograph
populations=exprs(d[,"Phenograph"])[,1]
save(populations,file="./RData/populations.RData")

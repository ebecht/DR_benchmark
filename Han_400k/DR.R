## Loading the UMAP module
source("../utils.R")

## Loading FItSNE
source("../FIt-SNE/fast_tsne.R")

## Loading t-SNE
library(Rtsne)

####################
## Dimensionality reduction for the full Han dataset (400k scRNAseq)
####################

load("./rdata/pca.RData") ## Computed in ./data_parsing_full_dataset.R

## UMAP
umap=umap_module$UMAP(n_neighbors=30L,min_dist=0.2,metric="euclidean",verbose=FALSE,random_state=123L,verbose=TRUE)$fit_transform(pca)
colnames(umap)=c("UMAP1","UMAP2")
save(umap,file="./rdata/umap.RData")

## tSNE
tsne=Rtsne(pca)$Y
colnames(tsne)=c("tSNE1","tSNE2")
save(tsne,file="./rdata/tsne.RData")

## FItSNE
fitsne=fftRtsne(pca,rand_seed=123L)
colnames(fitsne)=c("FItSNE1","FItSNE2")
save(fitsne,file="./rdata/fitsne.RData")

## FItSNE-le
fitsnele=fftRtsne(pca,rand_seed=123L,start_late_exag_iter=800L,late_exag_coeff=4)
colnames(fitsnele)=c("FItSNE_le1","FItSNE_le2")
save(fitsnele,file="./rdata/fitsnele.RData")

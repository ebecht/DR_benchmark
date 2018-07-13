########################################
####
##    Running dimensionality reduction on the subsetted Han dataset
####
########################################

####################
## Loading the data for the full dataset (created in the data_parsing.RData script)
####################
sapply(c("umap","xp","cells_AUC","samples","g2"),function(x){
    load(file=paste("./BM_BMcKit_PB_RData/",x,".RData",sep=""),envir=.GlobalEnv)
})

####################
## Running  UMAP and t-SNE
####################

## Specifying which cells are filtered-in (using the g2 object loaded from disk and defined interactively from the previous script)
w2=!is.na(g2)

## Compute 100 approximate principal components. Ref: https://stats.stackexchange.com/questions/66227/whats-the-tractable-data-size-for-sparse-pca-or-lasso
library(irlba)
set.seed(123)
w2=!is.na(g2)
svd=irlba(xp[w2,],nv=100,nu=0)
pca=xp[w2,]%*%svd$v
pca=as.matrix(pca)
colnames(pca)=paste("IRLBA",1:ncol(pca),sep="")
save(pca,file="./BM_BMcKit_PB_RData/pca_g2.RData")

library(reticulate)
use_python("/usr/bin/python3",TRUE) ##Change path to python installation with umap-learn installed if necessary
umap_module=import("umap",convert=TRUE)
umap=umap_module$UMAP(n_neighbors=30L,min_dist=0.1,metric="correlation",verbose=FALSE,random_state=123L,verbose=TRUE,n_epochs=200L)$fit_transform(pca)
colnames(umap)=c("UMAP1","UMAP2")
save(umap,file="./BM_BMcKit_PB_RData/umap_g2.RData")

library(Rtsne)
set.seed(123)
tsne=Rtsne(pca,pca=FALSE)$Y
colnames(tsne)=c("tSNE1","tSNE2")
save(tsne,file="./BM_BMcKit_PB_RData/tsne_g2.RData")

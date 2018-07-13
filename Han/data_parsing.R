########################################
####
##     Creating .RData files from tabulated input
####
########################################

####################
## Parsing input data
####################
## Source data is available at https://figshare.com/articles/MCA_DGE_Data/5435866/3 (Use the "MCA_All-Batch-removed.zip" file)

files=c(
    list.files("./MCA/MCA_All-batch-removed",pattern="PeripheralBlood",full.names=TRUE), ##/!\ sample 2 is an outlier, rest seems to overlap nicely. It's because batch 2 hasn't been Ficolled (so it's full of neutrophils)
    list.files("./MCA/MCA_All-batch-removed",pattern="BoneMarrow",full.names=TRUE)
)

## Annotate files
samplesMap=setNames(sub("_rm.batch_dge.txt.gz","",sapply(strsplit(files,"/"),tail,1)),1:length(files))
samplesTypes=substr(samplesMap,1,nchar(samplesMap)-1)
samplesAnnot=data.frame(id=1:length(files),name=samplesMap,type=samplesTypes,file=files)
write.table(samplesAnnot,file="./annots/samplesAnnot.txt",sep="\t",row.names=FALSE)

## Read the data
data=sapply(files,read.table,sep=" ",header=TRUE,row.names=1,simplify=FALSE)
library(Matrix)
data=lapply(data,function(x){Matrix(as.matrix(x),sparse=TRUE)})

data=lapply(data,t)

## Convert counts to read frequencies per cell
data=sapply(data,function(x){
    dn=dimnames(x)
    x=Diagonal(x=1/rowSums(x))%*%x
    dimnames(x)=dn
    x
},simplify=FALSE)

## ##
## Combine data for individual samples to a single bigger matrix
## ##

## Convert from sparse matrix to full matrix to combine
data=lapply(data,as.matrix)

##Mapping cells to samples
samples=sapply(data,nrow)
samples=rep(1:length(samples),samples)

## List all unique genes
genes=sort(unique(do.call(c,lapply(data,colnames))))
## Create empty matrix for all cells with accurate dimensionality
xp=matrix(ncol=length(genes),nrow=sum(sapply(data,nrow)),data=0L,dimnames=list(do.call(c,lapply(data,rownames)),genes))
## Populate it
for(s in 1:length(data)){
    xp[rownames(data[[s]]),colnames(data[[s]])]=data[[s]]
}
## Free-up some memory
rm(data)

## Convert to sparse matrix
xp=Matrix(xp,sparse=TRUE)

## Compute 100 approximate principal components. Ref: https://stats.stackexchange.com/questions/66227/whats-the-tractable-data-size-for-sparse-pca-or-lasso
library(irlba)
set.seed(123) 
svd=irlba(xp,nv=100,nu=0)
pca=xp%*%svd$v
pca=as.matrix(pca)
colnames(pca)=paste("IRLBA",1:ncol(pca),sep="")

## Computing UMAP on the full dataset
library(reticulate)
use_python("/usr/bin/python3",TRUE) ##Change path to python installation with umap-learn installed if necessary
umap_module=import("umap",convert=TRUE)
umap=umap_module$UMAP(n_neighbors=30L,min_dist=0.1,metric="correlation",verbose=TRUE)$fit_transform(pca) ##This was sadly unseeded. The resulting object is however saved
colnames(umap) <- c("UMAP1","UMAP2")

## Cell signatures from haematopedia
annot_hmp=read.table("./annots/haematopedia_mmc6.csv",sep=",",header=TRUE,stringsAsFactors=FALSE)
annot_hmp=subset(annot_hmp,GeneSymbol%in%colnames(xp))

## Compute cell-lineages scores (slow and memory consuming)
library(AUCell) ##Ref: https://bioconductor.org/packages/3.7/bioc/vignettes/AUCell/inst/doc/AUCell.html#calculate-enrichment-for-the-gene-signatures-auc ; paper : SCENIC: single-cell regulatory network inference and clustering
cells_rankings <- AUCell_buildRankings(t(as.matrix(xp)))
geneSets=split(annot_hmp$GeneSymbol,annot_hmp$Lineage)
cells_AUC <- AUCell_calcAUC(geneSets, cells_rankings, aucMaxRank=nrow(cells_rankings)*0.05,nCores=7L)

####################
## Saving the data for the full dataset
####################
dir.create("./BM_BMcKit_PB_RData/",suppressWarnings=TRUE)
sapply(c("umap","xp","cells_AUC","samples"),function(x){
    save(list=x,file=paste("./BM_BMcKit_PB_RData/",x,".RData",sep=""))
})

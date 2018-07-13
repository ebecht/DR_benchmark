####################
## Data parsing for the full Han dataset
####################

## Source : https://satijalab.org/seurat/mca.html which then points to https://www.dropbox.com/s/8d8t4od38oojs6i/MCA.zip?dl=1
library(Matrix)
xp=readRDS("./MCA/MCA_merged_mat.rds")
dn=dimnames(xp)
xp=xp%*%Diagonal(x=1/colSums(xp)) ## Converts counts to cell-wise frequencies
xp=t(xp)
save(xp,file="./rdata/xp.RData")

## Save samples nad tissues for which each cell originate from (parsing the rownames)
labels=rownames(xp)
labels=strsplit(rownames(xp),".",fixed=TRUE)
labels=sapply(labels,function(x){x[1:(length(x)-1)]},simplify=FALSE)
labels=sapply(labels,paste,collapse=" ")
labels=strsplit(labels,"_")
tissues=sapply(labels,"[",1)
w=rownames(xp)==toupper(rownames(xp))&!grepl("_",rownames(xp))
tissues[w]="Unannotated"
samples=sapply(labels,paste,collapse="_")
samples[w]="Unannotated"
save(tissues,file="./rdata/tissues.RData")
save(samples,file="./rdata/samples.RData")

## Compute 100 approximate principal components. Ref: https://stats.stackexchange.com/questions/66227/whats-the-tractable-data-size-for-sparse-pca-or-lasso
load("./rdata/xp.RData")
library(irlba)
set.seed(123) 
svd=irlba(xp,nv=100,nu=0)
pca=xp%*%svd$v
pca=as.matrix(pca)
colnames(pca)=paste("IRLBA",1:ncol(pca),sep="")
save(pca,file="./rdata/pca.RData")

## Check variance explained
total_variance=sum(sapply(split(xp@x,rep(colnames(xp),diff(xp@p))),sd)^2,na.rm=TRUE)
png("./graphs/variance_explained.png",res=300,height=1000,width=1000)
plot(sort(apply(pca,2,sd)^2,decreasing=TRUE),ylab="Variance per PC",xlab="PC index",pch=16,cex=0.2)
legend(x="topright",bty="n",paste("Var. explained: ",signif(100*sum(apply(pca,2,sd)^2)/total_variance,3),"%",sep=""))
dev.off()

####################
## Phenograph for the full Han dataset
####################

load("./rdata/pca.RData")

set.seed(123)
library(Rphenograph) ## https://github.com/JinmiaoChenLab/Rphenograph
sink(tempfile())
populations=Rphenograph(pca)
sink()
save(populations,file="./rdata/populations.RData")

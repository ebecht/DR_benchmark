########################################
####
##    Creating graphs for the main figures of the Han Bone marrow / PBMC dataset
####
########################################

source("../utils.R")
library(RColorBrewer)

####################
## Loading the data for the subsetted dataset (created in the data_parsing.R and DR_on_data_subset.R scripts)
####################
sapply(c("umap_g2","xp","cells_AUC","tsne_g2","samples","pca_g2","g2"),function(x){
    load(file=paste("./BM_BMcKit_PB_RData/",x,".RData",sep=""),envir=.GlobalEnv)
})

samplesAnnot=read.table("./annots/samplesAnnot.txt",sep="\t",header=TRUE,stringsAsFactors=FALSE)

## Specifying which cells are filtered-in (using the g2 object loaded from disk and defined interactively from the previous script)
w2=!is.na(g2)

## Cell lineages defined in the Haemopedia (doi:10.1016/j.stemcr.2016.07.007 ; Supplementary table 6 - Scores computed using the AUCell package in the data_parsing.R script and stored in the object cells_AUC)
lineages=c("Multi Potential Progenitor","Macrophage Lineage","Neutrophil Lineage","Erythrocyte Lineage","B Cell Lineage","T Cell Lineage","NK Cell Lineage")

####################
## Assigning phenotypes to cells
####################

cutoffs=setNames(c(0.04,0.09,0.05,0.045,0.09,0.075,0.04),lineages) ##The cut-offs are somewhat arbitrary but were selected to have a good trade-off between specificity and sensitivity based on the dimensionality reduction plots

labels=sapply(lineages,function(i){
    w=cells_AUC@assays[[1]][i,][w2]>=cutoffs[i]
    w
})
labels=apply(labels,1,which)
labels=sapply(labels,function(x){if(length(x)==1){x}else{0}})
labels[labels!=0]=lineages[labels[labels!=0]]
labels[labels==0]="Ungated"

####################
## Figure 2 panels CDEF
####################

## Assigning colors to cell types
colors=setNames(
    c("#CCCCCC","royalblue4","#FFED6F","#BEBADA","#B3DE69","#BC80BD","#FB8072","#FDB462"),
    c("Ungated",lineages)
)

## Assigning colors to samples
cols_sample=as.character(factor(samplesAnnot$type[samples],levels=c("BoneMarrowcKit","BoneMarrow","PeripheralBlood"),labels=c("firebrick4","black","blue2")))[w2]

## Scrambling the order of cells to avoid overplotting bias
scrbl=sample(1:nrow(umap))

png(width=4000,height=4000,res=300,filename="./graphs/Figure 2 panels CDEF.png")
layout(matrix(nrow=3,ncol=3*2,data=c(rep(1:6,each=2),c(7:10,0,0)),byrow=TRUE),widths=rep(c(2,2,1),each=2),heights=c(2,2,1))
par(mar=c(4,4,1,1),mgp=c(3,2,1),bty="n")
plot(umap[scrbl,],col=cols_sample[scrbl],pch=16,cex=0.2,xlab="UMAP1",ylab="UMAP2")
plot(tsne[scrbl,],col=cols_sample[scrbl],pch=16,cex=0.2,xlab="t-SNE1",ylab="t-SNE2")
plot.new()
par(mar=c(0,0,0,0))
legend(x=par("usr")[1]-0.25,xpd=NA,y=(par("usr")[3]+par("usr")[4])/2,legend=c("Bone Marrow cKit+","Bone Marrow","Peripheral Blood"),col=c("firebrick4","black","blue2"),pch=16,bty="n",cex=1.5)
par(mar=c(4,4,2,1),mgp=c(3,2,1),bty="n")
plot(umap,col=colors[labels],pch=16,cex=ifelse(labels=="Ungated",0.2,0.6),xlab="UMAP1")
plot(tsne,col=colors[labels],pch=16,cex=ifelse(labels=="Ungated",0.2,0.6),xlab="t-SNE1",ylab="t-SNE2")
plot.new()
par(mar=c(0,0,0,0))
legend(x=par("usr")[1]-0.25,xpd=NA,y=(par("usr")[3]+par("usr")[4])/2,legend=sub(" Lineage","",lineages),col=colors[-1],pch=16,bty="n",cex=1.5)
par(mar=c(4,4,2,1),mgp=c(3,2,1),bty="n")
x="Vpreb3"
plot(x=umap[,1],y=umap[,2],col=toColors_continuous(palette=rev(brewer.pal(6,"RdYlBu")),xp[!is.na(g2),x],drop_quantiles=0.02),xlab="UMAP1",ylab="UMAP2",pch=16,cex=0.1,bty="n",main=x)
x="Chchd10"
plot(x=umap[,1],y=umap[,2],col=toColors_continuous(palette=rev(brewer.pal(6,"RdYlBu")),xp[!is.na(g2),x],drop_quantiles=0.02),xlab="UMAP1",ylab="UMAP2",pch=16,cex=0.1,bty="n",main=x)
x="Vpreb3"
plot(x=tsne[,1],y=tsne[,2],col=toColors_continuous(palette=rev(brewer.pal(6,"RdYlBu")),xp[!is.na(g2),x],drop_quantiles=0.02),xlab="tSNE1",ylab="tSNE2",pch=16,cex=0.1,bty="n",main=x)
x="Chchd10"
plot(x=tsne[,1],y=tsne[,2],col=toColors_continuous(palette=rev(brewer.pal(6,"RdYlBu")),xp[!is.na(g2),x],drop_quantiles=0.02),xlab="tSNE1",ylab="tSNE2",pch=16,cex=0.1,bty="n",main=x)
dev.off()

####################
## Fig 2 along with PCA
####################

png(width=4000*7/5,height=4000,res=300,filename="./graphs/Figure 2 panels CDEF with PCA.png")
layout(matrix(nrow=3,ncol=4*2,data=c(rep(1:8,each=2),c(9:14,0,0)),byrow=TRUE),widths=rep(c(2,2,2,1),each=2),heights=c(2,2,1))
par(mar=c(4,4,1,1),mgp=c(3,2,1),bty="n")
plot(umap[scrbl,],col=cols_sample[scrbl],pch=16,cex=0.2,xlab="UMAP1",ylab="UMAP2")
plot(tsne[scrbl,],col=cols_sample[scrbl],pch=16,cex=0.2,xlab="t-SNE1",ylab="t-SNE2")
plot(pca[scrbl,],col=cols_sample[scrbl],pch=16,cex=0.2,xlab="PC1",ylab="PC2")
plot.new()
par(mar=c(0,0,0,0))
legend(x=par("usr")[1]-0.25,xpd=NA,y=(par("usr")[3]+par("usr")[4])/2,legend=c("Bone Marrow cKit+","Bone Marrow","Peripheral Blood"),col=c("firebrick4","black","blue2"),pch=16,bty="n",cex=1.5)
par(mar=c(4,4,2,1),mgp=c(3,2,1),bty="n")
plot(umap,col=colors[labels],pch=16,cex=ifelse(labels=="Ungated",0.2,0.6),xlab="UMAP1")
plot(tsne,col=colors[labels],pch=16,cex=ifelse(labels=="Ungated",0.2,0.6),xlab="t-SNE1",ylab="t-SNE2")
plot(pca[,1:2],col=colors[labels],pch=16,cex=ifelse(labels=="Ungated",0.2,0.6),xlab="PC1",ylab="PC2")
plot.new()
par(mar=c(0,0,0,0))
legend(x=par("usr")[1]-0.25,xpd=NA,y=(par("usr")[3]+par("usr")[4])/2,legend=sub(" Lineage","",lineages),col=colors[-1],pch=16,bty="n",cex=1.5)
par(mar=c(4,4,2,1),mgp=c(3,2,1),bty="n")
x="Vpreb3"
plot(x=umap[,1],y=umap[,2],col=toColors_continuous(palette=rev(brewer.pal(6,"RdYlBu")),xp[!is.na(g2),x],drop_quantiles=0.02),xlab="UMAP1",ylab="UMAP2",pch=16,cex=0.1,bty="n",main=x)
x="Chchd10"
plot(x=umap[,1],y=umap[,2],col=toColors_continuous(palette=rev(brewer.pal(6,"RdYlBu")),xp[!is.na(g2),x],drop_quantiles=0.02),xlab="UMAP1",ylab="UMAP2",pch=16,cex=0.1,bty="n",main=x)
x="Vpreb3"
plot(x=tsne[,1],y=tsne[,2],col=toColors_continuous(palette=rev(brewer.pal(6,"RdYlBu")),xp[!is.na(g2),x],drop_quantiles=0.02),xlab="tSNE1",ylab="tSNE2",pch=16,cex=0.1,bty="n",main=x)
x="Chchd10"
plot(x=tsne[,1],y=tsne[,2],col=toColors_continuous(palette=rev(brewer.pal(6,"RdYlBu")),xp[!is.na(g2),x],drop_quantiles=0.02),xlab="tSNE1",ylab="tSNE2",pch=16,cex=0.1,bty="n",main=x)
x="Vpreb3"
plot(x=pca[,1],y=pca[,2],col=toColors_continuous(palette=rev(brewer.pal(6,"RdYlBu")),xp[!is.na(g2),x],drop_quantiles=0.02),xlab="PC1",ylab="PC2",pch=16,cex=0.1,bty="n",main=x)
x="Chchd10"
plot(x=pca[,1],y=pca[,2],col=toColors_continuous(palette=rev(brewer.pal(6,"RdYlBu")),xp[!is.na(g2),x],drop_quantiles=0.02),xlab="PC1",ylab="PC2",pch=16,cex=0.1,bty="n",main=x)
dev.off()

####################
## Biplots for 5D PCA 
####################

n=5
coordsp=aggregate_df(pca[,1:n],labels,fun=median)
coordsp=coordsp[-match("Ungated",rownames(coordsp)),]
scrmbl=sample(1:nrow(pca))
png(height=n*1000,width=n*1000,res=300,file=paste("./PCA_1to",n,"_celltypes.png",sep=""))
layout(matrix(nrow=n,ncol=n,data=1:n^2,byrow=TRUE))
par(mar=c(0.1,0.1,0.1,0.1),bty="n",xaxt="n",yaxt="n")
for(i in 1:n){
    for(j in 1:n){
        if(i==j){
            plot.new()
            box(bty="o",xpd=NA)
            text(labels=colnames(pca)[i],x=mean(par("usr")[1:2]),y=mean(par("usr")[3:4]),adj=c(0.5,0.5),cex=3)
        } else {
            plot(x=pca[scrmbl,i],y=pca[scrmbl,j],col=colors[labels][scrmbl],pch=16,cex=0.1)
            text(labels=rownames(coordsp),x=coordsp[,i],y=coordsp[,j])
        }
    }
}
dev.off()

png(height=n*1000,width=n*1000,res=300,file=paste("./PCA_1to",n,"_organs.png",sep=""))
layout(matrix(nrow=n,ncol=n,data=1:n^2,byrow=TRUE))
par(mar=c(0.1,0.1,0.1,0.1),bty="n",xaxt="n",yaxt="n")
for(i in 1:n){
    for(j in 1:n){
        if(i==j){
            plot.new()
            box(bty="o",xpd=NA)
            text(labels=colnames(pca)[i],x=mean(par("usr")[1:2]),y=mean(par("usr")[3:4]),adj=c(0.5,0.5),cex=3)
        } else {
            plot(x=pca[scrmbl,i],y=pca[scrmbl,j],col=cols_sample[scrmbl],pch=16,cex=0.1)
            text(labels=rownames(coordsp),x=coordsp[,i],y=coordsp[,j])
        }
    }
}
dev.off()

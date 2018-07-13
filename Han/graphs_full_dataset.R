########################################
####
##    Creating graphs for supplementary figure Filtering_Han
####
########################################

source("../utils.R")

####################
## Loading the data for the full dataset (created in the ./data_parsing.RData script)
####################
sapply(c("umap","xp","cells_AUC","samples","g2"),function(x){
    load(file=paste("./BM_BMcKit_PB_RData/",x,".RData",sep=""),envir=.GlobalEnv)
})
samplesAnnot=read.table("./annots/samplesAnnot.txt",sep="\t",header=TRUE,stringsAsFactors=FALSE)

####################
## Overlaying the expression of specific genes on the UMAP projection for the full dataset
####################

## Specifying the genes
genes=c("Mcpt8","Prss34","Prg2","S100a8","S100a9","Ctss","Lyz2","Car1","Ccl5","Cd79a","Cd79b","Iglc1","Igkc","Hba-a1","Hba-a2","Hbb-bs","Cmtm7","Cox7b")

## Exporting the graphs
sapply(genes,function(x){
    png(paste("./graphs/full_dataset/genes/",x,".png",sep=""),height=480,width=480,res=144)
    par(mar=c(0.5,0.5,2,0))
    plot(
        x=umap[,1],
        y=umap[,2],
        col=toColors_continuous(xp[,x],drop_quantiles=0.02),
        xlab="UMAP1",
        ylab="UMAP2",
        main=x,
        pch=16,
        cex=0.2,
        bty="l",
        xaxt="n",
        yaxt="n"
    )
    dev.off()
})

####################
## Overlaying tissues or samples on the UMAP projection for the full dataset
####################

## Scramble the cells' order to avoid the last ones being drawn on top of the first ones
scrmbl=sample(1:nrow(umap))

png(paste("./graphs/full_dataset/Sample_type.png",sep=""),height=480,width=480,res=144)
par(mar=c(0.5,0.5,2,0))
plot(
    umap[scrmbl, 1],
    umap[scrmbl, 2],
    pch=16,
    xlab="UMAP1",
    ylab="UMAP2",
    cex=0.2,
    col=as.character(factor(samplesAnnot$type[samples],levels=c("BoneMarrowcKit","BoneMarrow","PeripheralBlood"),labels=c("firebrick1","black","lightsteelblue")))[scrmbl],
    xaxt="n",
    yaxt="n",
    main="",
    bty="l"
)
legend(x="bottomleft",pch=16,bty="n",col=c("firebrick1","black","lightsteelblue"),legend=c("cKit+ BM","BM","PB"))
dev.off()

png("./graphs/full_dataset/Samples ID.png",height=480,width=960,res=144)
par(mfrow=c(1,2),mar=c(0.5,0.5,2,0))
library(RColorBrewer)
palette=colorRampPalette(brewer.pal(12,"Set3"))(14)
cells_annot=palette[as.numeric(factor(samplesAnnot$id[samples]))]
plot(
    umap[scrmbl, 1],
    umap[scrmbl, 2],
    pch=16,
    xlab="UMAP1",
    ylab="UMAP2",
    cex=0.2,
    col=cells_annot[scrmbl],
    xaxt="n",
    yaxt="n",
    bty="l"
)
plot.new()
legend(x=par("usr")[1],y=mean(par("usr")[4]),pch=16,col=palette,legend=samplesAnnot$name,bty="n",xpd=NA,cex=0.5)
dev.off()    

## Interactive function to select a subset of the cells from a biplot.
## g2=gate_from_biplot(umap,1,2)
## We loaded the "g2" object from the disk instead (at the beginning of the script)
w2=!is.na(g2)

png("./graphs/full_dataset/Filtering.png",height=480,width=480,res=144)
par(mar=c(0.5,0.5,2,0))
plot(
    umap[, 1],
    umap[, 2],
    pch=16,
    xlab="UMAP1",
    ylab="UMAP2",
    cex=0.2,
    col=ifelse(!w2,"red","black"),
    xaxt="n",
    yaxt="n",
    main="",
    bty="l"
)
legend(x="bottomleft",pch=16,bty="n",col=c("red","black"),legend=c("Removed","Kept"))
dev.off()

library(flowCore)
library(en.lab)

## Loading and parsing the expression data (which is saved as FCS files split across samples. UMAP and t-SNE have been run on concataned versions)
fs_src=read.flowSet(list.files("./WongTraffickGatedTNK_out-10k",full.names=TRUE))
xp_src=as.matrix(do.call(rbind,fsApply(fs_src,exprs,simplify=F)))
rownames(xp_src)=as.character(1:nrow(xp_src))
samples=unlist(fsApply(fs_src,nrow,simplify=F))
samples=rep(names(samples),times=samples)
events_to_samples=setNames(samples,as.character(1:nrow(xp_src)))
targets=fs_src[[1]]@parameters$desc
targets[is.na(targets)]=fs_src[[1]]@parameters$name[is.na(targets)]
colnames_to_channels=setNames(colnames(xp_src),targets)
colnames(xp_src)=targets
d=list(fs_src=fs_src,xp_src=xp_src,channels.code=colnames_to_channels,       events.code=events_to_samples)

## Annotating events (tissues and samples)
a=read.table("./10k_cluster_annotation.csv",sep=",",header=TRUE,row.names=1,stringsAsFactors=FALSE)
samples=sapply(strsplit(d$events.code,"_"),"[",3)
organs=substr(samples,1,nchar(samples)-1)
regular_channels=read.csv("./annot/trafficknames2.csv",sep=",",header=TRUE,stringsAsFactors=FALSE)
regular_channels=regular_channels[regular_channels[,2]=="y",1]

## Transforms
lgcl=logicleTransform(w=0.25, t=16409, m=4.5, a=0)
ilgcl=inverseLogicleTransform(trans=lgcl)

library(viridis)
library(matlab)
library(RColorBrewer)
organs_cols=setNames(brewer.pal(8,"Spectral"),c("CB","PBMC","Liver","Spleen","Tonsil","Lung","Gut","Skin"))
broad_cols=setNames(brewer.pal(length(unique(a$Broad)),"Set1"),c(sort(unique(a$Broad))[3],sort(unique(a$Broad))[-3]))
pheno_cols=sort(unique(d$xp_src[,"Phenograph"]))
pheno_cols=setNames(rep(brewer.pal(12,"Set3"),4),pheno_cols)
pheno_cols[9+0:3*12]="gray40"

####################
## Figure 1
####################

## Scramble the data to avoid overplotting biases
scrmbl=sample(1:nrow(d$xp_src))
png("./graphs/Figure 1_v2.png",height=1000*3,width=2500,res=300)
layout(
    rbind(
        matrix(nrow=2,ncol=5,data=c(rep(1:2,each=2),3,rep(4:5,each=2),6),byrow=TRUE),
        cbind(6+matrix(nrow=2,ncol=4,data=1:8,byrow=FALSE),matrix(ncol=1,data=rep(0,2)))
    ),
    heights=c(2,2,1)
)
par(bty="l",mar=c(1.5,1.5,1.5,1.5),xaxt="n",yaxt="n")
plot(
    x=d$xp_src[scrmbl,"UMAP1"],
    y=d$xp_src[scrmbl,"UMAP2"],
    pch=16,
    cex=0.1,
    col=broad_cols[a[d$xp_src[scrmbl,"Phenograph"],"Broad"]]
)
mtext(side=3,font=2,line=0,"UMAP")
plot(
    x=d$xp_src[scrmbl,"tSNE1"],
    y=d$xp_src[scrmbl,"tSNE2"],
    pch=16,
    cex=0.1,
    col=broad_cols[a[d$xp_src[scrmbl,"Phenograph"],"Broad"]],
)
mtext(side=3,font=2,line=0,"tSNE")
plot.new()
legend(x=par("usr")[1],y=(par("usr")[3]+par("usr")[4])*3/4,legend=c("Cell types",names(broad_cols)),col=c("#00000000",broad_cols),pch=16,bty="n",text.font=c(2,rep(1,length(broad_cols))))
plot(
    x=d$xp_src[scrmbl,"UMAP1"],
    y=d$xp_src[scrmbl,"UMAP2"],
    pch=16,
    cex=0.1,
    col=organs_cols[organs][scrmbl]
)
mtext(side=3,font=2,line=0,"UMAP")
plot(
    x=d$xp_src[scrmbl,"tSNE1"],
    y=d$xp_src[scrmbl,"tSNE2"],
    pch=16,
    cex=0.1,
    col=organs_cols[organs][scrmbl]
)
mtext(side=3,font=2,line=0,"tSNE")
plot.new()
legend(x=par("usr")[1],y=(par("usr")[3]+par("usr")[4])*3/4,legend=c("Sample types",names(organs_cols)),col=c("#00000000",organs_cols),pch=16,bty="n",text.font=c(2,rep(1,length(organs_cols))))
par(mar=c(0,1.5,1.5,0))
plot(
    x=d$xp_src[scrmbl,"UMAP1"],
    y=d$xp_src[scrmbl,"UMAP2"],
    col=toColors_continuous(d$xp_src[scrmbl,"CD69"],drop_quantiles=0.05),
    pch=16,
    cex=0.1,
    xlab="",
    ylab=""
)
mtext(side=2,font=2,"UMAP",line=0)
mtext(side=3,font=2,line=0,"CD69")
plot(
    x=d$xp_src[scrmbl,"tSNE1"],
    y=d$xp_src[scrmbl,"tSNE2"],
    col=toColors_continuous(d$xp_src[scrmbl,"CD69"],drop_quantiles=0.05),
    pch=16,
    cex=0.1,
    xlab="",
    ylab=""
)
mtext(side=2,font=2,"tSNE",line=0)
plot(
    x=d$xp_src[scrmbl,"UMAP1"],
    y=d$xp_src[scrmbl,"UMAP2"],
    col=toColors_continuous(d$xp_src[scrmbl,"CD103"],drop_quantiles=0.05),
    pch=16,
    cex=0.1,
    xlab="",
    ylab=""
)
mtext(side=3,font=2,line=0,"CD103")
plot(
    x=d$xp_src[scrmbl,"tSNE1"],
    y=d$xp_src[scrmbl,"tSNE2"],
    col=toColors_continuous(d$xp_src[scrmbl,"CD103"],drop_quantiles=0.05),
    pch=16,
    cex=0.1,
    xlab="",
    ylab=""
)
plot(
    x=d$xp_src[scrmbl,"UMAP1"],
    y=d$xp_src[scrmbl,"UMAP2"],
    col=toColors_continuous(d$xp_src[scrmbl,"CD45RO"],drop_quantiles=0.05),
    pch=16,
    cex=0.1,
    xlab="",
    ylab=""
)
mtext(side=3,font=2,line=0,"CD45RO")
plot(
    x=d$xp_src[scrmbl,"tSNE1"],
    y=d$xp_src[scrmbl,"tSNE2"],
    col=toColors_continuous(d$xp_src[scrmbl,"CD45RO"],drop_quantiles=0.05),
    pch=16,
    cex=0.1,
    xlab="",
    ylab=""
)
plot(
    x=d$xp_src[scrmbl,"UMAP1"],
    y=d$xp_src[scrmbl,"UMAP2"],
    col=toColors_continuous(d$xp_src[scrmbl,"CCR7"],drop_quantiles=0.05),
    pch=16,
    cex=0.1,
    xlab="",
    ylab=""
)
mtext(side=3,font=2,line=0,"CCR7")
plot(
    x=d$xp_src[scrmbl,"tSNE1"],
    y=d$xp_src[scrmbl,"tSNE2"],
    col=toColors_continuous(d$xp_src[scrmbl,"CCR7"],drop_quantiles=0.05),
    pch=16,
    cex=0.1,
    xlab="",
    ylab=""
)
dev.off()

####################
## Plotting pairwise biplots for 5D PCA
####################

xp_tmp=apply(d$xp_src[,regular_channels],2,lgcl)
scrmbl=sample(1:nrow(xp_tmp))
pca=prcomp(xp_tmp)
d$xp_src=cbind(d$xp_src,PC1=pca$x[,1],PC2=pca$x[,2])

## Annotated by cell populations
png(height=5000,width=5000,res=300,file="./graphs/PCA_broad.png")
plot(as.data.frame(pca$x[scrmbl,1:5]),col=broad_cols[a[d$xp_src[scrmbl,"Phenograph"],"Broad"]],pch=16,cex=0.1)
dev.off()

## Annotated by Phenograph clusters
n=5
coordsp=aggregate_df(pca$x[,1:n],d$xp_src[,"Phenograph"],fun=median)
png(height=5000,width=5000,res=300,file="./graphs/PCA_phenograph.png")
layout(matrix(nrow=n,ncol=n,data=1:n^2,byrow=TRUE))
par(mar=c(0.1,0.1,0.1,0.1),bty="n",xaxt="n",yaxt="n")
for(i in 1:n){
    for(j in 1:n){
        if(i==j){
            plot.new()
            box(bty="o",xpd=NA)
            text(labels=colnames(pca$x)[i],x=mean(par("usr")[1:2]),y=mean(par("usr")[3:4]),adj=c(0.5,0.5),cex=3)
        } else {
            plot(x=pca$x[scrmbl,j],y=pca$x[scrmbl,i],col=pheno_cols[d$xp_src[scrmbl,"Phenograph"]],pch=16,cex=0.1)
            text(labels=rownames(coordsp),x=coordsp[,j],y=coordsp[,i])
        }
    }
}
dev.off()

####################
## Annotate phenograph clusters on UMAP, tSNE, 2D PCA; without overcrowding
####################

coordsu=aggregate_df(d$xp_src[,c("UMAP1","UMAP2")],d$xp_src[,"Phenograph"],fun=median)
coordst=aggregate_df(d$xp_src[,c("tSNE1","tSNE2")],d$xp_src[,"Phenograph"],fun=median)
coordsp=aggregate_df(pca$x[,1:2],d$xp_src[,"Phenograph"],fun=median)
medoids=aggregate_df(apply(d$xp_src[,regular_channels],2,lgcl),d$xp_src[,"Phenograph"],fun=median)
channels_of_interest=c("CD3","CD4","CD8","CD56","TCRgD","CD161","CD19","CD14")
medoids=scale(medoids[,sort(colnames(medoids))])
medoids=medoids[,c(channels_of_interest,setdiff(colnames(medoids),channels_of_interest))]

## Phenograph
png("./graphs/annotation_phenograph1.png",height=4000,width=3000,res=300)
layout(matrix(nrow=4,ncol=3,data=1:12,byrow=TRUE))
for(i in 1:4){
    coi=as.character((i-1)*12+1:12)
    cols=pheno_cols[d$xp_src[scrmbl,"Phenograph"]]
    cols[!d$xp_src[scrmbl,"Phenograph"]%in%as.numeric(coi)]="gray95"
    cex=ifelse(!d$xp_src[scrmbl,"Phenograph"]%in%as.numeric(coi),0.2,0.1)
    par(bty="n")
    plot(
        x=d$xp_src[scrmbl,"UMAP1"],
        y=d$xp_src[scrmbl,"UMAP2"],
        pch=16,
        cex=cex,
        col=cols,
        xlab="UMAP1",
        ylab="UMAP2",
        main=ifelse(i==1,"UMAP","")
    )
    sapply(coi,function(x){text(labels=x,x=coordsu[x,1],y=coordsu[x,2])})
    legend(x="bottomleft",legend=coi,text.col=pheno_cols[coi],bty="n",cex=0.5)
    plot(
        x=d$xp_src[scrmbl,"tSNE1"],
        y=d$xp_src[scrmbl,"tSNE2"],
        pch=16,
        cex=cex,
        col=cols,
        xlab="tSNE1",
        ylab="tSNE2",
        main=ifelse(i==1,"tSNE","")
    )
    sapply(coi,function(x){text(labels=x,x=coordst[x,1],y=coordst[x,2])})
    legend(x="bottomleft",legend=coi,text.col=pheno_cols[coi],bty="n",cex=0.5)
    plot(
        x=d$xp_src[scrmbl,"PC1"],
        y=d$xp_src[scrmbl,"PC2"],
        pch=16,
        cex=cex,
        col=cols,
        xlab="PC1",
        ylab="PC2",
        main=ifelse(i==1,"PCA","")
    )
    sapply(coi,function(x){text(labels=x,x=coordsp[x,1],y=coordsp[x,2])})
    legend(x="bottomleft",legend=coi,text.col=pheno_cols[coi],bty="n",cex=0.5)
}
dev.off()

## Organs
png("./graphs/annotation_organs.png",height=8000,width=3000,res=300)
layout(matrix(nrow=8,ncol=3,data=1:24,byrow=TRUE))
for(i in names(organs_cols)){
    cex=ifelse(organs[scrmbl]==i,0.2,0.1)
    cols=ifelse(organs[scrmbl]==i,"gray40","gray95")
    par(bty="n")
    plot(
        x=d$xp_src[scrmbl,"UMAP1"],
        y=d$xp_src[scrmbl,"UMAP2"],
        pch=16,
        cex=cex,
        col=cols,
        xlab="UMAP1",
        ylab="UMAP2",
        main=ifelse(i==names(organs_cols)[1],"UMAP","")
    )
    legend(x="bottomleft",bty="n",legend=i,text.col="gray40",text.font=2)
    plot(
        x=d$xp_src[scrmbl,"tSNE1"],
        y=d$xp_src[scrmbl,"tSNE2"],
        pch=16,
        cex=cex,
        col=cols,
        xlab="tSNE1",
        ylab="tSNE2",
        main=ifelse(i==names(organs_cols)[1],"tSNE","")
    )
    plot(
        x=d$xp_src[scrmbl,"tSNE1"],
        y=d$xp_src[scrmbl,"tSNE2"],
        pch=16,
        cex=cex,
        col=cols,
        xlab="PC1",
        ylab="PC2",
        main=ifelse(i==names(organs_cols)[1],"PCA","")
    )
}
dev.off()

## Heatmap of manual annotation of Phenograph clusters into broad cell populations
library(pheatmap)
png("./graphs/annotation_phenograph2.png",height=2000,width=2200,res=300)
a_tmp=a[,"Broad",drop=FALSE]
colnames(a_tmp)="Cell type"
pheatmap(
    medoids[order(a$Broad),],
    col=colorRampPalette(c("darkblue","blue","white","red","darkred"))(99),
    breaks=fastBreaks(medoids,100),
    cluster_cols=FALSE,
    cluster_rows=FALSE,
    annotation_row=a_tmp,
    annotation_colors=list("Cell type"=broad_cols)
)
dev.off()

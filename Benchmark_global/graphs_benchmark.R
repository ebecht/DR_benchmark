## Setting colors and point types for graphs throughout the script
colorCode=c(UMAP="#E41A1CCC",tSNE="#377EB8CC",FItSNE="#4DAF4ACC","FItSNE_le"="#00441BCC","scvis"="#984EA3")
pchCode=c(Han_400k=0,Wong=2,Samusik=4)
source("../utils.R") ## Loads some misc functions

####################
## Runtime (plots the graph in ./graphs/Execution timing.png) 
####################

options(scipen=999) ## This avoids 200,000 to be converted to "2+e05" by as.character()

####
## Loading, transforming and reshaping data
####

load("./rdata/timings.RData") ## Loads the timings measured when running dimensionality reduction methods in ./DR.R (UMAP,BH-tSNE, FIt-SNE, FIt-SNE l.e.) and ./scvis.R (scvis)
timings=log10(timings)
n_cells=as.numeric(dimnames(timings)[[1]])
means=apply(timings[,,,,"elapsed"],c(1,2,4),mean) ## Computes mean for each subsampling size, algorithm and dataset across replicates
sds=apply(timings[,,,,"elapsed"],c(1,2,4),sd) ## Computes standard deviation
algs=dimnames(timings)[[2]]
datasets=dimnames(timings)[[4]]

####
## Specifying graph parameters (colors and symbols)
####

pars=expand.grid(datasets=datasets,algs=algs,stringsAsFactors=FALSE)[,c("algs","datasets")]
pars$col=colorCode[pars$algs]
pars$pch=pchCode[pars$datasets]

####
## Plotting
####

pdf("./graphs/Execution timing.pdf",height=3.5,width=3.5)
par("bty"="n",mar=c(4,4,1,1),cex=0.75)
x=log10(n_cells)
## Main plot
plot.new()
plot.window(xlim=range(x),ylim=range(means[,,]))
for(alg in algs){
    for(dn in datasets){
        col=subset(pars,datasets==dn&algs==alg)$col
        pch=subset(pars,datasets==dn&algs==alg)$pch
        lines(x,y=means[,alg,dn],col=col)
        points(x,y=means[,alg,dn],col=col,pch=pch)
        segments(x0=log10(n_cells),y0=means[,alg,dn]-sds[,alg,dn],y1=means[,alg,dn]+sds[,alg,dn],col=col)
    }
}
## Axes
title(ylab="Average time (seconds)",xlab="Number of input data points")
axis(side=1,at=log10(n_cells),labels=n_cells,cex.axis=0.65,las=1)
yrange=10^seq(floor(min(means[,,])),ceiling(max(means[,,])),by=1)
yticks=c(1,2,5)
yticks=expand.grid(yticks,yrange)
yticks=apply(yticks,1,prod)
axis(side=2,cex.axis=0.65,at=log10(yticks),labels=yticks,las=1)
## Legend
legend(x="bottomright",col=c(colorCode,"#00000000",rep("black",length(pchCode))),legend=c(names(colorCode),"",names(pchCode)),pch=c(rep(16,length(colorCode)+1),pchCode),lty=1,bty="n",cex=0.65)
dev.off()

####################
## Correlation of pairwise distances in the dimensionality-reduced space versus the original space
####################

## Algorithms benchmarked
algorithms=c("UMAP","tSNE","FItSNE","FItSNE_le","scvis")

## Datasets benchmarked
datasets=c("Samusik","Wong","Han_400k")

set.seed(123)
pdf("./graphs/Preservation of distances.pdf",height=(length(algorithms)+0.5)*750/2000*7,width=length(datasets)*750/2000*7)
layout(matrix(nrow=length(algorithms)+1,ncol=length(datasets),data=1:((length(algorithms)+1)*length(datasets)),byrow=FALSE),heights=c(rep(1,length(algorithms)),0.5))
par(bty="l",xaxt="n",yaxt="n",mar=c(0,1,1,0),oma=c(6,3,4,1))
rs=sapply(datasets,function(dataset){
    env=environment()

    ## For each dataset, load the DRs on the full dataset
    if(dataset=="Samusik"){
        sapply(c("../Samusik/rdata/umap.RData","../Samusik/rdata/tSNE.RData","../Samusik/rdata/fitsne.RData","../Samusik/rdata/fitsnele.RData","../Samusik/rdata/xp.RData","../Samusik/rdata/scvis.RData"),load,envir=env)
    }
    if(dataset=="Wong"){
        sapply(c("../Wong/RData/umap.RData","../Wong/RData/tsne.RData","../Wong/RData/fitsne.RData","../Wong/RData/fitsnele.RData","../Wong/RData/xp.RData","../Wong/RData/scvis.RData"),load,envir=env)
    }
    if(dataset=="Han_400k"){
        sapply(c("../Han_400k/rdata/umap.RData","../Han_400k/rdata/tsne.RData","../Han_400k/rdata/fitsne.RData","../Han_400k/rdata/fitsnele.RData","../Han_400k/rdata/pca.RData","../Han_400k/rdata/scvis.RData"),load,envir=env)
        xp=pca
    }
    dr_src=list(UMAP=umap,tSNE=tsne,FItSNE=fitsne,FItSNE_le=fitsnele,scvis=scvis)
    
    ## Sample randomly 10000 points to compute pairwise distances
    spl=sample(1:nrow(umap),10000)

    ## Compute pairwise distances on the DR and original space for this subsample
    d=lapply(dr_src,function(x){dist(x[spl,])})
    d_xp=dist(xp[spl,])
    cuts=cut(d_xp,50)
    
    ## For each algorithm
    rs=sapply(names(d),function(alg){
        
        ## Plot distance on the original space vs distance on the DR plot for each pair of point as boxplots
        bp=boxplot(d[[alg]]~cuts,outline=FALSE,xaxt="n",col=colorCode[alg])
        
        ## Report Pearson's correlation coefficient on the graph
        r=cor(d[[alg]],d_xp,method="pearson")
        text(x=par("usr")[1],y=par("usr")[4],labels=paste("r=",signif(r,2),sep=""),pos=4,xpd=NA)

        ## Report algorithms and datasets
        if(alg=="UMAP"){
            title(main=dataset,line=1,xpd=NA)
        }
        if(dataset==datasets[1]){
            title(ylab=alg,cex.lab=par()$cex.main,font.lab=2,xpd=NA,line=1)
        }

        r
    })

    ## Adding histograms in the last row of the plot showing the distribution of distances among the 50 bins
    barplot(table(cuts),col="gray",border=FALSE,names.arg=FALSE)
    par(xaxt="s")
    axis(side=1,line=1,at=seq(par("usr")[1],par("usr")[2],length.out=5),labels=signif(seq(min(d_xp),max(d_xp),length.out=5),2),xpd=NA,las=3)
    par(xaxt="n")
    if(dataset==datasets[1]){
        title(ylab="Original\ndistances",cex.lab=par()$cex.main,font.lab=2,xpd=NA,line=1)
    }
    rs    
})
dev.off()

####################
## Reproducibility of the embedding
####################

## Loading the dimensionality reduction of data subsamples (Computed using ./DR.R and ./scvis.R)
load("./rdata/dr.RData")

## Recovering the sizes of the subsamples
n_cells=as.numeric(names(dr[[1]]))

## Number of subsample replicates
n_replicates=dim(dr[[1]][[1]])[3]

## Algorithms benchmarked
algorithms=c("UMAP","tSNE","FItSNE","FItSNE_le","scvis")

## Datasets benchmarked
datasets=c("Samusik","Wong","Han_400k")

for(dataset in datasets){
    
    ##For each dataset, load the DRs on the full dataset
    if(dataset=="Samusik"){
        sapply(c("../Samusik/rdata/umap.RData","../Samusik/rdata/tSNE.RData","../Samusik/rdata/fitsne.RData","../Samusik/rdata/fitsnele.RData","../Samusik/rdata/scvis.RData"),load,envir=.GlobalEnv)
    }
    if(dataset=="Wong"){
        sapply(c("../Wong/RData/umap.RData","../Wong/RData/tsne.RData","../Wong/RData/fitsne.RData","../Wong/RData/fitsnele.RData","../Wong/RData/scvis.RData"),load,envir=.GlobalEnv)
    }
    if(dataset=="Han_400k"){
        sapply(c("../Han_400k/rdata/umap.RData","../Han_400k/rdata/tsne.RData","../Han_400k/rdata/fitsne.RData","../Han_400k/rdata/fitsnele.RData","../Han_400k/rdata/scvis.RData"),load,envir=.GlobalEnv)
    }
    dr_src=list(UMAP=umap,tSNE=tsne,FItSNE=fitsne,FItSNE_le=fitsnele,scvis=scvis)

    ## Converting the full DR coordinates to ranks then to colors
    orders=lapply(dr_src,function(x){apply(x,2,function(y){order(order(y))})})
    order_to_color=function(o1,o2){
        n=length(o1)
        rgb(1-o1/n,((o1+o2)-min(o1+o2))/(max(o1+o2)-min(o1+o2)),1-o2/n,maxColorValue=1)
    }
    colors=lapply(orders,function(x){order_to_color(x[,1],x[,2])})
        
    ## Graph output
    png(paste("./graphs/reproducibility_",dataset,".png",sep=""),height=3000,width=5000,res=300)
    w=5:10

    ## Seting up the plot layout
    topmat=matrix(nrow=n_replicates,ncol=n_replicates,data=1) ## To plot the top part (full dataset) 
    bottommat=matrix(nrow=length(w),ncol=n_replicates,data=1+1:(length(w)*n_replicates),byrow=TRUE) ## To plot the bottom part (replicates of subsamples)
    layoutmat=rbind(topmat,bottommat) ## The layout for a single dimensionality-reduction method
    M=max(layoutmat)
    layoutmat=do.call(cbind,lapply(1:length(algorithms)-1,function(i){layoutmat+i*M}))
    layoutmat=cbind(cbind(1:nrow(layoutmat)+max(layoutmat),1:nrow(layoutmat)+max(layoutmat)),layoutmat)
    layout(layoutmat) ## Creating the combined layout
    
    par(pch=16,cex=0.1,xaxt="n",yaxt="n",mar=c(1,1,1,1),bty="o")
    for(alg in algorithms){
        ## Top part: plot the embedding of the full dataset
        plot(
            dr_src[[alg]],
            col=makeTransparent(colors[[alg]],100)
        )
        legend(x="topleft",legend=alg,cex=12,text.font=2,bty="n")
        ## Bottom part: plot the embedding of subsamples using the same color as for the full dataset
        for(i in w){
            for(j in 1:n_replicates){
                plot(
                    dr[[dataset]][[as.character(n_cells[i])]][,paste(alg,1:2),j],
                    col=makeTransparent(colors[[alg]][dr[[dataset]][[as.character(n_cells[i])]][,"Sample",j]],ifelse(n_cells[i]>20000,100,255))
                )
            }
        }

        ## Adding vertical lines between algorithms
        if(alg!=tail(algorithms,1)){
            abline(v=line2user(1,4),xpd=NA,lwd=1.5)
        }
    }

    ## Legends
    plot.new()
    legend(x="right",legend=paste("Dataset:\n",dataset,sep=""),cex=12,text.font=2,bty="n")
    plot.new()
    legend(x="right",legend=paste("Full dataset:\n",nrow(dr_src[[1]])," cells",sep=""),cex=10,text.font=2,bty="n")
    plot.new()
    legend(x="bottomright",legend="Random\nsubsamples\nof sizes:",cex=10,text.font=2,bty="n",xpd=NA)
    for(i in 1:length(w)){
        plot.new()
        legend(x="right",legend=n_cells[w][i],cex=9,text.font=2,bty="n")
    }
    dev.off()
}


####################
## Reproducibility of the embedding : statistics
####################


## Loading the dimensionality reduction of data subsamples (Computed using ./DR.R and ./scvis.R)
load("./rdata/dr.RData")
## Recovering the sizes of the subsamples
n_cells=as.numeric(names(dr[[1]]))

## Number of subsample replicates
n_replicates=dim(dr[[1]][[1]])[3]

## Algorithms benchmarked
algorithms=c("UMAP","tSNE","FItSNE","FItSNE_le","scvis")

## Datasets benchmarked
datasets=c("Samusik","Wong","Han_400k")

## Initialize the array to store the statistics we will be computing
replicability=array(dim=c(length(n_cells),length(algorithms),length(datasets),n_replicates),dimnames=list(n_cells,algorithms,datasets,1:n_replicates))

for(dataset in datasets){
    
    ##For each dataset, load the DRs on the full dataset
    if(dataset=="Samusik"){
        sapply(c("../Samusik/rdata/umap.RData","../Samusik/rdata/tSNE.RData","../Samusik/rdata/fitsne.RData","../Samusik/rdata/fitsnele.RData","../Samusik/rdata/scvis.RData"),load,envir=.GlobalEnv)
    }
    if(dataset=="Wong"){
        sapply(c("../Wong/RData/umap.RData","../Wong/RData/tsne.RData","../Wong/RData/fitsne.RData","../Wong/RData/fitsnele.RData","../Wong/RData/scvis.RData"),load,envir=.GlobalEnv)
    }
    if(dataset=="Han_400k"){
        sapply(c("../Han_400k/rdata/umap.RData","../Han_400k/rdata/tsne.RData","../Han_400k/rdata/fitsne.RData","../Han_400k/rdata/fitsnele.RData","../Han_400k/rdata/scvis.RData"),load,envir=.GlobalEnv)
    }
    dr_src=list(UMAP=umap,tSNE=tsne,FItSNE=fitsne,FItSNE_le=fitsnele,scvis=scvis)
    
    for(alg in algorithms){
        for(i in n_cells){
            for(j in 1:n_replicates){
                ## For each algorithm [alg], subsample size [i] and subsample replicate [j]
                dr_original=dr_src[[alg]][dr[[dataset]][[as.character(i)]][,"Sample",j],]
                x=dr_original[,1]
                y=dr_original[,2]
                dr_subsample=dr[[dataset]][[as.character(i)]][,paste(alg,1:2),j]
                X=dr_subsample[,1]
                Y=dr_subsample[,2]
                ## Compute average of the two coordinates unsigned correlation in the embedding of the full dataset versus the embedding of the subsample
                replicability[as.character(i),alg,dataset,j]=mean(c(abs(cor(x,X,method="pearson")),abs(cor(y,Y,method="pearson"))))
            }
        }
    }
}

## Summarizing across replicates
means=apply(replicability[,,,],c(1,2,3),mean) ## Average
sds=apply(replicability[,,,],c(1,2,3),sd) ## Standard deviation

## Graph output
pdf("./graphs/Replicability_of_embeddings.pdf",height=2000/300,width=2000/300)
options(scipen=999)
layout(matrix(nrow=dim(means)[1],ncol=dim(means)[3],data=1:prod(dim(means)[c(1,3)]),byrow=FALSE))
par(mar=c(0,1,0,1),oma=c(8,8,2,0),bty="n")
for(dataset in dimnames(means)[[3]]){
    for(i in dimnames(means)[[1]]){
        ## Barplots
        bp=barplot(means[i,,dataset],col=colorCode[dimnames(means)[[2]]],bty="n",ylim=c(0,1.2),names.arg=FALSE,yaxt="n")

        ## Data points
        for(alg in dimnames(means)[[2]]){
            points(x=rep(bp[match(alg,dimnames(means)[[2]]),],dim(replicability)[4]),y=replicability[i,alg,dataset,],pch=16,col=colorCode[alg],xpd=NA)
            points(x=rep(bp[match(alg,dimnames(means)[[2]]),],dim(replicability)[4]),y=replicability[i,alg,dataset,],pch=1,col="black",xpd=NA)
        }

        ## Error bars
        segments(x0=bp,y0=means[i,,dataset]-sds[i,,dataset],y1=means[i,,dataset]+sds[i,,dataset])
                
        ## Annotations
        if(dataset==dimnames(means)[[3]][1]){
            axis(side=2,at=seq(0,1,by=0.5),labels=c(0,0.5,1),las=1,line=0)
            title(ylab=paste(i,ifelse(i==dimnames(means)[[1]][1]," cells",""),sep=""),xpd=NA,cex.lab=1,line=5)
        }
        if(i==dimnames(means)[[1]][1]){
            title(main=dataset,line=0,cex.main=2,xpd=NA)
        }
        if(i==tail(dimnames(means)[[1]],1)){
            text(x=bp,y=line2user(0.5,1),labels=dimnames(means)[[2]],col=colorCode[dimnames(means)[[2]]],font=2,xpd=NA,cex=1.5,srt=270,adj=c(0,0.5))
        }
    }
}
mtext(side=2,adj=0.5,text="Average absolute Pearson correlation coefficient",xpd=NA,outer=TRUE,line=1.5,cex=1,font=1)
mtext(side=2,adj=0.5,text="Subsample size",xpd=NA,outer=TRUE,line=5.25,cex=1,font=1)
dev.off()

####################
## Reproducibility of the embedding : local structure (clustering)
####################

## Loading the dimensionality reduction of data subsamples (Computed using ../DR.R)
load("./rdata/dr.RData")

## Recovering the sizes of the subsamples
n_cells=as.numeric(names(dr[[1]]))

## Number of subsample replicates
n_replicates=dim(dr[[1]][[1]])[3]

## Algorithms benchmarked
algorithms=c("UMAP","tSNE","FItSNE","FItSNE_le","scvis")

## Datasets benchmarked
datasets=c("Samusik","Wong","Han_400k")


## Computing kmeans on each size 200,000 subsample for each DR algorithm
library(parallel)
set.seed(123, "L'Ecuyer") ## L'Ecuyer for reproducible parallel computation
kmeans=lapply(dr,function(x){
    setNames(lapply("200000",function(n){
        y=x[[n]] 
        n=as.numeric(n)
        n_clust=100
        setNames(mclapply(algorithms,function(alg){
            sapply(1:3,function(i){ ## Looping over replicates
                kmeans(y[,paste(alg,1:2),i],centers=n_clust,nstart=10,iter.max=100)$cluster
            })
        }),algorithms)
    }),"200000")
})

## Computing kmeans on the full embeddings for each DR algorithm
## Takes ~5 mins
set.seed(123)
kmeans_src=list()
for(dataset in datasets){
    ##For each dataset, load the DRs on the full dataset
    if(dataset=="Samusik"){
        sapply(c("../Samusik/rdata/umap.RData","../Samusik/rdata/tSNE.RData","../Samusik/rdata/fitsne.RData","../Samusik/rdata/fitsnele.RData","../Samusik/rdata/scvis.RData"),load,envir=.GlobalEnv)
    }
    if(dataset=="Wong"){
        sapply(c("../Wong/RData/umap.RData","../Wong/RData/tsne.RData","../Wong/RData/fitsne.RData","../Wong/RData/fitsnele.RData","../Wong/RData/scvis.RData"),load,envir=.GlobalEnv)
    }
    if(dataset=="Han_400k"){
        sapply(c("../Han_400k/rdata/umap.RData","../Han_400k/rdata/tsne.RData","../Han_400k/rdata/fitsne.RData","../Han_400k/rdata/fitsnele.RData","../Han_400k/rdata/scvis.RData"),load,envir=.GlobalEnv)
    }
    dr_src=list(UMAP=umap,tSNE=tsne,FItSNE=fitsne,FItSNE_le=fitsnele,scvis=scvis)
    kmeans_src=c(kmeans_src,list(lapply(dr_src,function(y){kmeans(y,centers=100,nstart=10,iter.max=100)$cluster})))
    names(kmeans_src)[length(kmeans_src)]=dataset
}

## Compute normalized-mutual information
library(entropy)
options(scipen=999)
size=as.character(200000)

## Store the results
mi=array(dim=c(length(algorithms),length(datasets),n_replicates),dimnames=list(algorithms,datasets,1:n_replicates))
for(dataset in datasets){
    for(alg in algorithms){
        for(size in "200000"){
            i=1
            for(replicate in 1:n_replicates){
                sampling=dr[[dataset]][[size]][,"Sample",replicate]
                x=kmeans_src[[dataset]][[alg]][sampling]
                y=kmeans[[dataset]][[size]][[alg]][,replicate]
                mi[alg,dataset,i]=mi.empirical(table(x,y),unit="log2")/entropy.empirical(x) ## Ref https://en.wikipedia.org/wiki/Uncertainty_coefficient
                i=i+1
            }
        }
    }
}

means=apply(mi,c(1:2),mean) ## Average across replicates
sds=apply(mi,c(1:2),sd) ## Standard deviation across replicates

## List algorithms and datasets
algs=dimnames(mi)[[2]]
datasets=dimnames(mi)[[2]]

## Graphical parameters
pars=expand.grid(datasets=datasets,algorithms=algorithms,stringsAsFactors=FALSE)[,c("algorithms","datasets")] ## Graphical parameters for lines to be drawn
pars$col=colorCode[pars$algorithms]
pars$pch=pchCode[pars$datasets]

## Graph output
pdf("./graphs/Mutual information of kmeans.pdf",height=1000/300,width=3000/300)
par(mfrow=c(1,3),mar=c(3,4,3,1))
for(dataset in datasets){
    ## Barplots
    bp=barplot(means[,dataset],names.arg=FALSE,ylim=c(0,max(means[,dataset]+sds[,dataset])),main=dataset,col=colorCode[rownames(means)])
    
    ## Data points
    for(alg in dimnames(means)[[1]]){
        points(x=rep(bp[match(alg,dimnames(means)[[1]]),],dim(mi)[3]),y=mi[alg,dataset,],pch=16,col=colorCode[alg],xpd=NA)
        points(x=rep(bp[match(alg,dimnames(means)[[1]]),],dim(mi)[3]),y=mi[alg,dataset,],pch=1,col="black",xpd=NA)
    }
    
    ## Error bars
    segments(x0=bp[,1],y0=means[,dataset]-sds[,dataset],y1=means[,dataset]+sds[,dataset],lwd=2)
                
    ## Axes and nnotations
    text(y=line2user(1,1),x=bp,labels=sub("tSNE","t-SNE",rownames(means)),xpd=NA,pos=1,offset=0,font=2)
    if(dataset==head(datasets,1)){
        title(ylab="Normalized mutual information",font.lab=2)
    }
}
dev.off()
        

####################
## Annotated scatterplots
####################

## Algorithms benchmarked
algorithms=c("PCA","UMAP","tSNE","FItSNE","FItSNE_le","scvis")

## Datasets benchmarked
datasets=c("Samusik","Wong","Han_400k")

library(RColorBrewer)
png("./graphs/Annotated_DimRed.png",width=(1+length(algorithms))*1500,height=length(datasets)*1500,res=150)
par(mfrow=c(length(datasets),length(algorithms)+1),bty="l",xaxt="n",yaxt="n",mar=c(0,1,1,0),oma=c(0,10,10,0))
rs=sapply(datasets,function(dataset){
    env=environment()
    
    ##For each dataset, load the DRs on the full dataset and annotations
    if(dataset=="Samusik"){
        sapply(c("../Samusik/rdata/umap.RData","../Samusik/rdata/tSNE.RData","../Samusik/rdata/fitsne.RData","../Samusik/rdata/fitsnele.RData","../Samusik/rdata/xp.RData","../Samusik/rdata/populations_manual.RData","../Samusik/rdata/scvis.RData"),load,envir=env)
        populations=populations_manual
        tissues=populations
        colors=c("Ungated"="#BEBEBE64",setNames(colorRampPalette(brewer.pal(12,"Set3"))(24),sort(setdiff(populations,"Ungated"))))
        w=populations!="Ungated"
        cex.annot=1.5
    }
    if(dataset=="Wong"){
        sapply(c("../Wong/RData/umap.RData","../Wong/RData/tsne.RData","../Wong/RData/fitsne.RData","../Wong/RData/fitsnele.RData","../Wong/RData/xp.RData","../Wong/RData/populations.RData","../Wong/RData/scvis.RData"),load,envir=env)
        populations_code=read.csv("../Wong/10k_cluster_annotation.csv",stringsAsFactors=FALSE,row.names=1)
        populations=populations_code[as.character(populations),"Broad"]
        colors=setNames(brewer.pal(length(unique(populations_code$Broad)),"Set1"),c(sort(unique(populations_code$Broad))[3],sort(unique(populations_code$Broad))[-3]))
        tissues=populations
        w=rep(TRUE,nrow(xp))
        cex.annot=1.5
    }
    if(dataset=="Han_400k"){
        sapply(c("../Han_400k/rdata/umap.RData","../Han_400k/rdata/tsne.RData","../Han_400k/rdata/fitsne.RData","../Han_400k/rdata/fitsnele.RData","../Han_400k/rdata/pca.RData","../Han_400k/rdata/populations.RData","../Han_400k/rdata/scvis.RData","../Han_400k/rdata/tissues.RData","../Han_400k/rdata/samples.RData"),load,envir=env)
        xp=pca
        populations=samples
        colors=setNames(colorRampPalette(brewer.pal(12,"Set3"))(length(unique(tissues))),unique(tissues))
        w=rep(TRUE,nrow(xp))
        cex.annot=0.75
    }

    ## Compute 2D PCA on the full dataset and add it to the DR list
    pca=prcomp(xp)$x[,1:2]
    dr_src=list(PCA=pca,UMAP=umap,tSNE=tsne,FItSNE=fitsne,FItSNE_le=fitsnele,scvis=scvis)

    ## Compute cell populations medoids
    coords_src=lapply(dr_src,function(x){
        aggregate_df(x[w,],populations[w],median)
    })

    ## For each algorithm, plot the embedding color-coded by "tissues" and text-labeled by "populations"
    for(alg in algorithms){
        plot(
            dr_src[[alg]],
            pch=16,
            cex=0.1,
            col=colors[tissues]
        )
        text(x=coords_src[[alg]][,1],y=coords_src[[alg]][,2],labels=rownames(coords_src[[alg]]),font=2,cex=cex.annot)
        par(cex.main=7.5)
        if(alg==algorithms[1]){
            title(ylab=dataset,font.lab=2,cex.lab=par()$cex.main,xpd=NA)
        }
        if(dataset==datasets[1]){
            title(main=alg,font.main=2,cex.main=par()$cex.main,xpd=NA)
        }
    }

    ## Colored text to legend populations
    plot.new()
    rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col="black")
    ys=seq(par("usr")[4],par("usr")[3],length.out=length(colors)+2)
    ys=ys[c(-1,-length(ys))]
    x=mean(par("usr")[1],par("usr")[2]*9)/10
    points(x=rep(x,length(ys)),y=ys,pch=16,col=colors)
    x=mean(par("usr")[1],par("usr")[2]*4)/5
    text(x=x,y=ys,labels=names(colors),col=colors,cex=2,pos=4)
})
dev.off()


####################
## Accuracies of random forests
####################

## Load the saved objects and return accuracies
acc=sapply(datasets,function(dataset){
    load(paste("./randomForests/rdata/",dataset,".RData",sep=""))
    res$acc
})

## Graph output
pdf("./graphs/Accuracy of Random Forests.pdf",height=2000/300,width=2000/300)
means=rowMeans(acc) ## Compute mean across datasets
sds=apply(acc,1,sd) ## SD across datasets
cols=rep("gray",length(means))
cols[names(means)%in%names(colorCode)]=colorCode[intersect(names(means),names(colorCode))]

## Barplot
bp=barplot(means,ylim=c(0,1),names.arg=FALSE,col=cols)

## Data points
for(alg in names(means)){
    points(x=rep(bp[match(alg,names(means)),],dim(acc)[2]),y=acc[alg,],pch=pchCode[dimnames(acc)[[2]]],xpd=NA)
}

## Error bars
segments(x0=bp[,1],y0=means-sds,y1=means+sds,lwd=2)

## Annotations
text(y=line2user(1,1),x=bp,labels=c("2D","3D","5D","UMAP","t-SNE","no l.e.","l.e.","scvis"),xpd=NA,pos=3,offset=0,font=c(1,1,1,2,2,1,1))
segments(y0=line2user(1.25,1),x0=bp[1],x1=bp[3],xpd=NA)
segments(y0=line2user(1.25,1),x0=bp[6],x1=bp[7],xpd=NA)
text(y=line2user(1.5,1),x=c(bp[2],mean(bp[6:7])),labels=c("PCA","FIt-SNE"),xpd=NA,pos=1,offset=0,font=2)
title(ylab="Accuracy of RF-classifier on held-out data",font.lab=2)
legend(x="topleft",pch=c(NA,pchCode),legend=c("Dataset:",names(pchCode)),bty="n")
dev.off()

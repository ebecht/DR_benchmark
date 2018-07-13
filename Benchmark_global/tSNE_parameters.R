####################
## Benchmarking of t-SNE's perplexity parameter
####################

## ##
## Perplexity
## ##

source("../utils.R")
source("../FIt-SNE/fast_tsne.R")

## Load the Samusik dataset
load("../Samusik/rdata/xp.RData")

## Sample 50,000 cells
set.seed(123)
spl=sample(1:nrow(xp),50000)

library(parallel)

## Set up perplexity values to benchmark
perplexities=c(2,5,10,15,30,50)

## Run t-SNE with given perplexities and constant seed
tsnes=mclapply(
    perplexities,function(perplexity){
        set.seed(123)
        res=Rtsne(xp[spl,],perplexity=perplexity)$Y
        colnames(res)=c("tSNE1","tSNE2")
        res
    },mc.cores=6L)
save(tsnes,file="./rdata/tsne_parameters/Samusik_tSNE_perplexity.RData")

## Run FIt-SNE with given perplexities and constant seed
fitsnes=mclapply(
    perplexities,function(perplexity){
        res=fftRtsne(xp[spl,],perplexity=perplexity,rand_seed=123L)
        colnames(res)=c("FItSNE1","FItSNE2")
        res
    },mc.cores=6L
)
save(fitsnes,file="./rdata/tsne_parameters/Samusik_FItSNE_perplexity.RData")

## Load the results
load("./rdata/tsne_parameters/Samusik_tSNE_perplexity.RData")
load("./rdata/tsne_parameters/Samusik_FItSNE_perplexity.RData")

## Plot the results
png("./graphs/Benchmarking perplexity.png",width=3000,height=1000,res=300)
par(mfrow=c(2,6),oma=c(0,0,1,0),bty="l",xaxt="n",yaxt="n",mar=c(2,2,2,2))
for(i in 1:length(perplexities)){
    plot(tsnes[[i]],pch=16,cex=0.1)
    if(i==1){
        text(x=line2user(1,2),y=mean(par("usr")[3:4]),srt=90,font=2,offset=0,xpd=NA,cex=2,labels="t-SNE")
        text(x=line2user(2,2),y=line2user(2,3),srt=0,font=4,offset=0,xpd=NA,cex=2,labels="Perplexity",pos=4)
    }
    text(y=line2user(0.5,3),x=mean(par("usr")[1:2]),srt=0,font=2,offset=0,xpd=NA,cex=2,labels=perplexities[i])
}
for(i in 1:length(perplexities)){
    plot(fitsnes[[i]],pch=16,cex=0.1)
    if(i==1){
        text(x=line2user(1,2),y=mean(par("usr")[3:4]),srt=90,font=2,offset=0,xpd=NA,cex=2,labels="FIt-SNE")
    }
}
dev.off()

## ##
## Maximum number of iterations
## ##

## Set up iter_max values to benchmark
max_iters=c(100,500,1000,2000)

## Run t-SNE with given iter_max and constant seed
tsnes=sapply(
    max_iters,
    function(m){
        set.seed(123)
        res=Rtsne(xp[spl,],max_iter=m,verbose=TRUE)$Y
        colnames(res)=c("tSNE1","tSNE2")
        res
    },simplify=FALSE)
save(tsnes,file="./rdata/tsne_parameters/Samusik_tSNE_iterations.RData")

## Run FIt-SNE with given iter_max and constant seed
fitsnes=sapply(
    max_iters,
    function(m){
        res=fftRtsne(xp[spl,],max_iter=m,rand_seed=123L)
        colnames(res)=c("FItSNE1","FItSNE2")
        res
    },simplify=FALSE)
save(fitsnes,file="./rdata/tsne_parameters/Samusik_FItSNE_iterations.RData")

## Load the results
load("./rdata/tsne_parameters/Samusik_tSNE_iterations.RData")
load("./rdata/tsne_parameters/Samusik_FItSNE_iterations.RData")

## Plot the results
png("./graphs/Benchmarking iterations.png",width=2000,height=1000,res=300)
par(mfrow=c(2,4),oma=c(0,0,1,0),bty="l",xaxt="n",yaxt="n",mar=c(2,2,2,2))
for(i in 1:length(max_iters)){
    plot(tsnes[[i]],pch=16,cex=0.1)
    if(i==1){
        text(x=line2user(1,2),y=mean(par("usr")[3:4]),srt=90,font=2,offset=0,xpd=NA,cex=2,labels="t-SNE")
        text(x=line2user(1,2),y=line2user(2,3),srt=0,font=4,offset=0,xpd=NA,cex=2,labels="Max. iter.",pos=4)
    }
    text(y=line2user(0.5,3),x=mean(par("usr")[1:2]),srt=0,font=2,offset=0,xpd=NA,cex=2,labels=max_iters[i])
}
for(i in 1:length(max_iters)){
    plot(fitsnes[[i]],pch=16,cex=0.1)
    if(i==1){
        text(x=line2user(1,2),y=mean(par("usr")[3:4]),srt=90,font=2,offset=0,xpd=NA,cex=2,labels="FIt-SNE")
    }
}
dev.off()

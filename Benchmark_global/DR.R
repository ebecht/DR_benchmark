####################
## Loads the three big datasets and perform various subsampling followed by dimensionality reduction
####################

## Total runtime is ~ 24h on an 8 core 3.6Ghz Intel Core i7-4790

## Loading the UMAP module
source("../utils.R")

## Loading FItSNE - Has to be installed separetely first, see https://github.com/KlugerLab/FIt-SNE
source("../FIt-SNE/fast_tsne.R")

## Loading BH-tSNE
library(Rtsne)

## Disable scientific notation for large numbers
opts=options()
options("scipen"=999)

## Size of subsamples
n_cells=c(10^2,2*10^2,5*10^2,10^3,2*10^3,5*10^3,10^4,2*10^4,10^5,2*10^5)

## Number of replicates for each subsample size
n_replicates=3L

## Algorithms benchmarked
algs=c("UMAP","tSNE","FItSNE","FItSNE_le")

## Large single cell datasets benchmarked
datasets=c("../Han_400k/rdata/pca.RData","../Wong/RData/xp.RData","../Samusik/rdata/xp.RData")
names(datasets)=sapply(strsplit(datasets,"/"),"[",2)

## Initializing the resulting object to store outputs
dr=setNames(list()[1:3],names(datasets))
dr=lapply(dr,function(x){setNames(list()[1:length(n_cells)],n_cells)})
for(dn in names(datasets)){
    for(n in as.character(n_cells)){
        dr[[dn]][[n]]=array(dim=c(as.numeric(n),1+2*length(algs),n_replicates),dimnames=list(1:as.numeric(n),c("Sample",paste(rep(algs,each=2)," ",1:2,sep="")),1:n_replicates))
    }
}

## Initializing the object to store timings
timings=array(dim=c(length(n_cells),length(algs),n_replicates,length(datasets),3),dimnames=list(n_cells,algs,1:n_replicates,names(datasets),c("user","system","elapsed")))

## Setting up seeds
seeds=expand.grid(dn=names(datasets),n=n_cells,i=1:n_replicates)
library(plyr)
seeds=arrange(seeds,dn,n,i)
nseeds=as.integer(1:nrow(seeds)-1)
seeds=cbind(seeds,R_seed=123L+nseeds,UMAP_seed=1234L+nseeds,FItSNE_seed=12345L+nseeds,FItSNE_le_seed=123456L+nseeds)

## Running the benchmark
for(dn in names(datasets)){
    
    ## Load the input matrix and rename it to xp (otherwise it would be named pca for Han)
    env=new.env()
    load(datasets[dn],envir=env)
    xp=get(ls(envir=env),envir=env)
    rm(env)
    
    for(n in n_cells){
        for(i in 1:n_replicates){

            ## Given each parameter, set pre-specified seeds for R, Python and FIt-SNE (makes it easy to resume the script in the event of a crash)
            seedvec=unlist(seeds[seeds[,"dn"]==dn&seeds[,"n"]==n&seeds[,"i"]==i,c("R_seed","UMAP_seed","FItSNE_seed","FItSNE_le_seed")])
            seed=seedvec["R_seed"]
            
            print(seed)
            set.seed(seed)

            ## Sample points uniformly with size n (element of the sizes vector n_cells)
            spl=sample(1:nrow(xp),n)
            xp_tmp=xp[spl,]

            ## Saving the sampled events so that we can recover the sampling in further analyses
            dr[[dn]][[as.character(n)]][,"Sample",i]=spl

            ## Set the number of nearest neighbors for UMAP (15 for CYToF and 30 for scRNAseq)
            if(dn=="Han_400k"){
                nn=30L
            }
            if(dn%in%c("Wong","Samusik")){
                nn=15L
            }
            
            ## ####
            ## Running, timing and saving dimensionality reduction algorithms' outputs
            ## ####

            ## UMAP
            t0=proc.time()
            dr[[dn]][[as.character(n)]][,c("UMAP 1","UMAP 2"),i]=umap_module$UMAP(n_neighbors=nn,min_dist=0.2,metric="euclidean",verbose=FALSE,random_state=seedvec["UMAP_seed"])$fit_transform(xp_tmp)
            t1=proc.time()
            timings[as.character(n),"UMAP",i,dn,]=summary(t1-t0)[1:3] ##Summary on a proc.time object allows to combine self and child processes timings
            
            ## tSNE
            t0=proc.time()
            dr[[dn]][[as.character(n)]][,c("tSNE 1","tSNE 2"),i]=Rtsne(xp_tmp)$Y
            t1=proc.time()
            timings[as.character(n),"tSNE",i,dn,]=summary(t1-t0)[1:3]
            
            ## FItSNE vanilla
            t0=proc.time()
            dr[[dn]][[as.character(n)]][,c("FItSNE 1","FItSNE 2"),i]=fftRtsne(xp_tmp,rand_seed=seedvec["FItSNE_seed"])
            t1=proc.time()
            timings[as.character(n),"FItSNE",i,dn,]=summary(t1-t0)[1:3]
            
            ## FItSNE with late exageration
            t0=proc.time()
            dr[[dn]][[as.character(n)]][,c("FItSNE_le 1","FItSNE_le 2"),i]=fftRtsne(xp_tmp,start_late_exag_iter=800L,late_exag_coeff=4,rand_seed=seedvec["FItSNE_le_seed"])
            t1=proc.time()
            timings[as.character(n),"FItSNE_le",i,dn,]=summary(t1-t0)[1:3]

            ## We save each iteration individually in case of a crash.  There's a script in ./rdata/Single seeds/ to combine these individual saves. That script will also add scvis outputs if they have been computed (by ../scvis.R).
            save(timings,file=paste("./rdata/Single seeds/timings_",seed,".RData",sep=""))
            save(dr,file=paste("./rdata/Single seeds/dr_",seed,".RData",sep=""))
        }
    }
}

save(dr,file="./rdata/dr.RData")
save(timings,file="./rdata/timings.RData")

####################
## Combining RData objects for single seeds produced by ../../DR.R
## The script to generate the embeddings will take quite a while to
## run so this is just a way to make it able to resume in case
## something bad happens
####################

options(scipen=999)
library(en.lab)
i=1
for(seed in c(152,182,212)){
    dr_chunck=en.load(paste("./dr_",seed,".RData",sep=""))
    timing_chunck=en.load(paste("./timings_",seed,".RData",sep=""))
    if(i==1){
        dr=dr_chunck
        timings=timing_chunck
    } else {
        dr[[i]]=dr_chunck[[i]]
        timings[,,,i,]=timing_chunck[,,,i,]
    }
    i=i+1
}

library(abind)
ndim=dim(timings)
ndim[2]=1
dn=dimnames(timings)
dn[[2]]=c("scvis")
timings=abind(timings,array(dim=ndim,dimnames=dn),along=2)

## Adding scvis
for(dataset in names(dr)){
    for(n_cells in names(dr[[1]])){
        for(replicate in 1:dim(dr[[1]][[1]])[3]){
            if(replicate==1){
                dr[[dataset]][[n_cells]]=abind(dr[[dataset]][[n_cells]],array(dim=c(dim(dr[[dataset]][[n_cells]])[1],2,dim(dr[[dataset]][[n_cells]])[3]),dimnames=list(dimnames(dr[[dataset]][[n_cells]])[[1]],c("scvis 1","scvis 2"),dimnames(dr[[dataset]][[n_cells]])[[3]])),along=2)
            }
            subsample=paste(dataset,n_cells,replicate,sep="_")
            input=list.files(paste("../../scvis/outputs/",subsample,"/",sep=""),full.names=TRUE)
            input=grep("[0-9]\\.tsv",input,value=TRUE)
            scvis=read.table(input,sep="\t",header=TRUE,row.names=1)
            colnames(scvis)=c("scvis 1","scvis 2")
            scvis=as.matrix(scvis)
            dr[[dataset]][[n_cells]][,c("scvis 1","scvis 2"),replicate]=scvis

            time=readLines(paste("../../scvis/sinks/",subsample,".txt",sep=""))
            time=grep("Time used for training: ",time,value=TRUE)
            time=sub("Time used for training: ","",time)
            time=strsplit(time,":")[[1]]
            time=sum(as.numeric(time)*c(3600,60,1))
            timings[n_cells,"scvis",replicate,dataset,"elapsed"]=time
        }
    }
}

save(timings,file="../timings.RData")
save(dr,file="../dr.RData")

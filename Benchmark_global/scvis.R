########################################
####
## Benchmarking scvis
####
########################################

source("../utils.R")

####
## Running scvis
####

## Datasets benchmarked
datasets=c("Samusik","Wong","Han_400k")

for(dataset in datasets){
    
    ## Loading input data
    if(dataset=="Samusik"){
        load("../Samusik/rdata/xp.RData")
    }
    if(dataset=="Wong"){
        load("../Wong/RData/xp.RData")
    }
    if(dataset=="Han_400k"){
        load("../Han_400k/rdata/pca.RData")
        xp=pca
    }

    ## Running scvis
    input=paste("./scvis/inputs/",dataset,".tsv",sep="") ## Input file
    write.table(xp,file=input,sep="\t",row.names=FALSE,col.names=TRUE)
    args=paste("train --data_matrix_file",input,"--out_dir",paste("./scvis/outputs/",dataset,sep=""),"--verbose --verbose_interval 50") ## Args to scvis system command
    system2("scvis",args=args,stdout=paste("./scvis/sinks/",dataset,".txt",sep="")) ## Execute scvis. Note that we redirect prints to a txt file so that we later retrieve the runtime
}

## Saving the output from txt to RData
for(dataset in datasets){
    scvis=read.table(file=paste("./scvis/outputs/",dataset,"/perplexity_10_regularizer_0.001_batch_size_512_learning_rate_0.01_latent_dimension_2_activation_ELU_seed_1_iter_30000.tsv",sep=""),sep="\t",header=TRUE,row.names=1,colClasses=c("numeric","numeric"))
    colnames(scvis)=c("scvis 1","scvis 2")
    if(dataset=="Samusik"){
        file="../Samusik/rdata/scvis.RData"
    }
    if(dataset=="Wong"){
        file="../Wong/RData/scvis.RData"
    }
    if(dataset=="Han_400k"){
        file="../Han_400k/rdata/scvis.RData"
    }
    save(scvis,file=file)
}

#### 
## Running scvis on subsamples of the datasets
####

## Datasets benchmarked
datasets=c("Samusik","Wong","Han_400k")

## Write input files to disk as tabulated txt files
for(dataset in datasets){
    ## Loading input data
    if(dataset=="Samusik"){
        load("../Samusik/rdata/xp.RData")
    }
    if(dataset=="Wong"){
        load("../Wong/RData/xp.RData")
    }
    if(dataset=="Han_400k"){
        load("../Han_400k/rdata/pca.RData")
        xp=pca
    }
    load("./rdata/dr.RData")
    dr=dr[[dataset]]
    n_size=names(dr)
    n_replicates=dim(dr[[1]])[3]

    ## Running scvis
    for(size in n_size){
        for(replicate in 1:n_replicates){
            name=paste(dataset,size,replicate,sep="_")
            input=paste("./scvis/inputs/",name,".tsv",sep="")
            sampling=dr[[size]][,"Sample",replicate]
            write.table(xp[sampling,],file=input,sep="\t",row.names=FALSE,col.names=TRUE)
        }
    }
}

## Run scvis on each input file
for(dataset in datasets){   
    ## Loading input data (for size and replicates)
    load("./rdata/dr.RData")
    dr=dr[[dataset]]
    n_size=names(dr)
    n_replicates=dim(dr[[1]])[3]

    ## Running scvis
    for(size in n_size){
        for(replicate in 1:n_replicates){
            name=paste(dataset,size,replicate,sep="_")
            print(name)
            input=paste("./scvis/inputs/",name,".tsv",sep="") ## Input file
            args=paste("train --data_matrix_file",input,"--out_dir",paste("./scvis/outputs/",name,sep=""),"--verbose --verbose_interval 50") ## Args to scvis system command
            system2("scvis",args=args,stdout=paste("./scvis/sinks/",name,".txt",sep="")) ## Execute scvis. Note that we redirect prints to a txt file so that we later retrieve the runtime
        }
    }
}

## The parsing  of scvis on subsamples from txt to .RData is performed in "./rdata/Single seeds/combine_seeds.R"

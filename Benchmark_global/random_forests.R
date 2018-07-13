####################
## Training Random Forests to assess each DR ability to distinguish cell populations. The corresponding plot is to be found in ./graphs_benchmark.R
####################

library(randomForest)
source("../utils.R")
colorCode=c(UMAP="#E41A1CCC",tSNE="#377EB8CC",FItSNE="#4DAF4ACC","FItSNE_le"="#00441BCC","scvis"="#984EA3")

## Datasets benchmarked
datasets=c("Samusik","Wong","Han_400k")

for(dataset in datasets){
    
    ##For each dataset, load the DRs on the full dataset
    if(dataset=="Samusik"){
        sapply(c("../Samusik/rdata/umap.RData","../Samusik/rdata/tSNE.RData","../Samusik/rdata/fitsne.RData","../Samusik/rdata/fitsnele.RData","../Samusik/rdata/populations.RData","../Samusik/rdata/xp.RData","../Samusik/rdata/scvis.RData"),load,envir=.GlobalEnv)
    }
    if(dataset=="Wong"){
        sapply(c("../Wong/RData/umap.RData","../Wong/RData/tsne.RData","../Wong/RData/fitsne.RData","../Wong/RData/fitsnele.RData","../Wong/RData/populations.RData","../Wong/RData/xp.RData","../Wong/RData/scvis.RData"),load,envir=.GlobalEnv)
    }
    if(dataset=="Han_400k"){
        sapply(c("../Han_400k/rdata/umap.RData","../Han_400k/rdata/tsne.RData","../Han_400k/rdata/fitsne.RData","../Han_400k/rdata/fitsnele.RData","../Han_400k/rdata/populations.RData","../Han_400k/rdata/pca.RData","../Han_400k/rdata/scvis.RData"),load,envir=.GlobalEnv)
        xp=pca
    }
    dr_src=list(UMAP=umap,tSNE=tsne,FItSNE=fitsne,FItSNE_le=fitsnele,scvis=scvis)

    ## Add PCA of various sizes to the list of DRs
    pca=prcomp(xp)
    dr_src=c(list(pca2=pca$x[,1:2],pca3=pca$x[,1:3],pca5=pca$x[,1:5]),dr_src)

    ## Sample 50,000 cells for the training set and a distinct sample of 50,000 for the test set
    set.seed(123)
    sampling=sample(1:nrow(dr_src[[1]]),50000)
    testset=sample(setdiff(1:nrow(dr_src[[1]]),sampling),50000)
    populations=factor(populations)

    ## Train random-forests to classify populations from embedding coordinates
    rf=lapply(
        dr_src,
        function(x){
            model=randomForest(
                x=x[sampling,],
                y=populations[sampling]
            )
            predictions=predict(model,newdata=x[testset,])
            list(predictions=predictions,model=model)
        }
    )

    ## Compute accuracies on test-set
    acc=lapply(rf,function(x){
        table(populations[testset],x$predictions)
    })
    acc=sapply(acc,function(CT){sum(diag(CT))/sum(CT)})

    ## Save the model, calls, samplings and accuracy
    res=list(rf=rf,sampling=sampling,testset=testset,acc=acc)
    save(res,file=paste("./randomForests/rdata/",dataset,".RData",sep=""))
}

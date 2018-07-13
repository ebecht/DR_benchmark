#Evan Newell 2015, Ming 2016, Etienne Becht 2016, Evan 2017

if (!require(flowCore)) { 
  source("http://bioconductor.org/biocLite.R")
  biocLite("flowCore")
} 

if (!require(Rtsne)) {
  install.packages("Rtsne") # Install Rtsne package from CRAN
  library(Rtsne) # Load package
}

if (!require(Rphenograph)) {
  if(!require(devtools)){
    install.packages("devtools")
  }
  install_github("JinmiaoChenLab/Rphenograph")
  library(Rphenograph) # Load package
}



if(!require(devtools)){
install.packages("devtools")
}




FCStSNEone <- function (LoaderPATH ="fcs", #Folder with samples (even 1 sample ok)
                        useCSV = F,
                        CyTOF = F,
                        TransformOutput = T,
                        ceil = 20, #number of events to take per sample (unless sample has less)
                        FNnames="names2.csv", #Parameters to analyze in csv file
                        OutputSuffix = "out2",
                        DotSNE = T,
                        tSNEperpelxity = 30, #default = 30; increased perplexity => increased spread
                        DoOneSENSE = F,
                        Dophenograph = F,
                        kValue = 10, #default = 30; k is low => more clusters and inversely
                        DoFlowSOM = F,
                        MaxClusters = 30,
                        DoIsomap = F,
                        DoDiffMap = F,
                        ManDist = F,
                        DoPCA = F,
                        Do3DtSNE =F,
                        DoUMAP = F,
                        Do3DUMAP = F,
                        DoOneSUMAP = F)
{
 

  
  if(!useCSV){
    fs <- read.flowSet(path = LoaderPATH,transform=FALSE,ignore.text.offset=T)
    FcsFileNames <- rownames(keyword(fs, "FILENAME"))
    filenames <- FcsFileNames
    NumBC <- length(fs)
    FFdata <- NULL
    for (FFs in 1:NumBC){
      FFt <- exprs(fs[[FFs]])
      ## Downsample
      if (nrow(FFt)<=ceil)
        FFa <- FFt
      else
        FFa <- FFt[sample(nrow(FFt),ceil,replace=F),]
      #Fixup column names
      colnames(FFa) <- fs[[FFs]]@parameters$desc
      empties <- which(is.na(colnames(FFa)) | colnames(FFa)== " ")
      colnames(FFa)[empties] <- fs[[FFs]]@parameters$name[empties]
      fs[[FFs]]@parameters$desc <- colnames(FFa)
      #fs[[FFs]]@parameters$name <- colnames(FFa)
      #Add file label
      FFa <- cbind(FFa,rep(FFs,dim(FFa)[1]))
      colnames(FFa)[dim(FFa)[2]] <- "InFile"
      #Concatenate
      FFdata <- rbind(FFdata,FFa)
    }} else {  #load csv files instead of fcs
      
      csvfilenames <- list.files(path = LoaderPATH, pattern="*.csv")
      filenames <- csvfilenames
      csvdata <- lapply(paste0(LoaderPATH,"//",csvfilenames),function(x) read.csv(x, check.names = F))
      NumBC <- length(csvdata)
      FFdata<-NULL
      for (FFs in 1:NumBC){
        FFt <- csvdata[[FFs]]
        ## Downsample
        if (nrow(FFt)<=ceil)
          FFa <- FFt
        else
          FFa <- FFt[sample(nrow(FFt),ceil,replace=F),]
        
        FFa <- cbind(FFa,rep(FFs,dim(FFa)[1]))
        colnames(FFa)[dim(FFa)[2]] <- "InFile"
        #Concatenate
        FFdata <- rbind(FFdata,FFa)
      }}
  
  
  keeptable <- read.csv(FNnames)
  keeprowbool <- sapply(keeptable[,2], function(x) any(x=="y"))
  keeprows <- subset(keeptable, keeprowbool)
  data <- FFdata[,which (colnames(FFdata) %in% keeprows[,1])]
  
  if(CyTOF){
    lgcl <- logicleTransform(w=0.25, t=16409, m=4.5, a=0)} else
      lgcl <- logicleTransform(w=0.1, t=500000, m=4.5, a=0)
  ilgcl <- inverseLogicleTransform(trans = lgcl)
  
  data1 <- apply(data, 2, lgcl)
  FFdata1 <- apply(FFdata, 2, lgcl)
  
  
  ####  Run algorithms
  
  
  
  score <- NULL
  tSNEmat <- NULL
  if(DotSNE)
  {
    print(dim(data1))
    print("running tSNE")
    tSNEdata3 <- Rtsne(data1, dims=2, perplexity=tSNEperpelxity, check_duplicates=F,verbose = T) 
    tSNEmat <- tSNEdata3$Y
    colnames(tSNEmat) <- c("tSNE1","tSNE2")
    plot(tSNEmat[, 1], tSNEmat[, 2], pch=".", xlab="tSNE1", ylab="tSNE2", cex=0.1)
  } else tSNEmat <- NULL
  
  
  if(DoOneSENSE)
  {
    print("running one-sense")
    Xx1DtSNEmat <- NULL
    for (factor in 2:(dim(keeptable)[2])){
      OneDtSNEname <- colnames(keeptable)[factor]
      keeprowbool <- sapply(keeptable[,factor], function(x) any(x=="y"))
      keeprows <- subset(keeptable, keeprowbool)
      dataX <- FFdata1[,which (colnames(FFdata1) %in% keeprows[,1])]
      tSNEdata3 <- Rtsne(dataX, dims=1, check_duplicates=F,verbose = T,perplexity = tSNEperpelxity) 
      tSNEmat1 <-  matrix(ncol=1,data=tSNEdata3$Y,dimnames=list(NULL,OneDtSNEname))
      colnames(tSNEmat1) <- OneDtSNEname
      hist(tSNEmat1, 100, main= paste("Histogram of",colnames(keeptable[factor] )))
      Xx1DtSNEmat <- cbind(Xx1DtSNEmat,tSNEmat1)
    }
  }else Xx1DtSNEmat <- NULL
  
  usordata<-NULL
  if(DoIsomap){
    if (!require(vegan)) { 
      install.packages("vegan")    
      library(vegan)
    } 
    
    print("running isomap")
    dis <- vegdist(data1, method="euclidean")
    ord <- isomap(dis,ndim=3,k=3, fragmentedOK = TRUE)
    usordata <-  matrix(ord$points[,1:3], ncol = 3)
    colnames(usordata) <- c("Isomap1","Isomap2","Isomap3")
    
    plot(usordata[, 1], usordata[, 2], pch=".", xlab="isomap1", ylab="isomap2", cex=0.1)
  } else usordata <- NULL
  
  DiffMapMat <- NULL
  if(DoDiffMap)
  {
    
    if (!require(diffusionMap)) { 
      install.packages("diffusionMap")    
      library(diffusionMap)
    } 
    
    print("running diff map")
    if (ManDist){
      if (!require(biotools)) {
        install.packages("biotools") # Install biotools package from CRAN
        library(biotolls) # Load package
      }
      D <- D2.dist(data1, cov(data1))
    }else D = dist(data1) # use Euclidean distance
    dmap = diffuse(D, eps.val = epsilonCompute(D,  p = 0.01), neigen = 3, maxdim=3) # compute diffusion map & plot
    plot(dmap)
    DiffMapMat <- dmap$X[,1:3]
    plot(DiffMapMat[,1],DiffMapMat[,2],pch=".", xlab="DiffMap1", ylab="DiffMap2", cex=0.1)
    colnames(DiffMapMat) <- c("DiffMap1","DiffMap2","DiffMap3")
  } else DiffMapMat <- NULL
  
  
  TDtSNEmat <- NULL
  if(Do3DtSNE)
  {
    print("running 3d tSNE")
    TDtSNEdata3 <- Rtsne(data1, dims=3, perplexity=tSNEperpelxity, check_duplicates=F,verbose = T )
    TDtSNEmat <- TDtSNEdata3$Y
    colnames(TDtSNEmat) <- c("ThreeDtSNE1","ThreeDtSNE2", "ThreeDtSNE3")
    plot(TDtSNEmat[, 1], TDtSNEmat[, 2], pch=".", xlab="3DtSNE1", ylab="3DtSNE2", cex=0.1)
  } else TDtSNEmat <- NULL
  
  umapMat <- NULL
  if(DoUMAP)
  {
    print("running UMAP")
    write.csv(data1,"dataForUmap.csv",row.names=F)
    path2script="fileUmap.py"
    command = "python"
    n <- 15
    mdist <- 0.2
    metric <- "euclidean"
    ncomp <- 2
    string <- paste0("dataForUmap.csv","---","umapDataOut.csv","---",n,"---",ncomp,"---",mdist,"---",metric)
    pattern <- "---"
    args = c(string, pattern)
    allArgs = c(path2script,args)
   
    system2(command, args=allArgs, stdout=T)
    
    umapMat <- as.matrix(read.csv("umapDataOut.csv",header=F))
    file.remove("dataForUmap.csv")
    file.remove("umapDataOut.csv")
    
    
    #umapMat <- umap(data1,25,0.1,"euclidean")
    colnames(umapMat) <- c("UMAP1","UMAP2")
    plot(umapMat[, 1], umapMat[, 2], pch=".", xlab="UMAP1", ylab="UMAP2", cex=0.1)
  } else umapMat <- NULL
  
  TDumapMat <- NULL
  if(Do3DUMAP)
  {
    print("running 3D UMAP")
    write.csv(data1,"dataForUmap.csv",row.names=F)
    path2script="fileUmap.py"
    command = "python"
    n <- 15
    mdist <- 0.2
    metric <- "euclidean"
    ncomp <- 3
    string <- paste0("dataForUmap.csv","---","umapDataOut.csv","---",n,"---",ncomp,"---",mdist,"---",metric)
    pattern <- "---"
    args = c(string, pattern)
    allArgs = c(path2script,args)
    
    system2(command, args=allArgs, stdout=T)
    
    TDumapMat <- as.matrix(read.csv("umapDataOut.csv",header=F))
    file.remove("dataForUmap.csv")
    file.remove("umapDataOut.csv")
    
    
    #umapMat <- umap(data1,25,0.1,"euclidean")
    colnames(TDumapMat) <- c("TD_UMAP1","TD_UMAP2","TD_UMAP3")
    plot(TDumapMat[, 1], TDumapMat[, 2], pch=".", xlab="TD_UMAP1", ylab="TD_UMAP2", cex=0.1)
    
  } else TDumapMat <- NULL
  
  
  if(DoOneSUMAP)
  {
    print("running one-SUMAP")
    OneSUMAPmat <- NULL
    for (factor in 2:(dim(keeptable)[2])){
      OneDUAMPname <- paste0(colnames(keeptable)[factor],"_U")
      keeprowbool <- sapply(keeptable[,factor], function(x) any(x=="y"))
      keeprows <- subset(keeptable, keeprowbool)
      dataX <- FFdata1[,which (colnames(FFdata1) %in% keeprows[,1])]
      
      
     
      write.csv(dataX,"dataForUmap.csv",row.names=F)
      path2script="fileUmap.py"
      command = "python"
      n <- 15
      mdist <- 0.2
      metric <- "euclidean"
      ncomp <- 1
      string <- paste0("dataForUmap.csv","---","umapDataOut.csv","---",n,"---",ncomp,"---",mdist,"---",metric)
      pattern <- "---"
      args = c(string, pattern)
      allArgs = c(path2script,args)
      
      system2(command, args=allArgs, stdout=T)
      
      ODumapMat <- as.matrix(read.csv("umapDataOut.csv",header=F))
      file.remove("dataForUmap.csv")
      file.remove("umapDataOut.csv")
      
      ODumapMat1 <-  matrix(ncol=1,data=ODumapMat,dimnames=list(NULL,OneDUAMPname))
      colnames(ODumapMat1) <- OneDUAMPname
      hist(ODumapMat1, 100, main= paste("Histogram of",colnames(keeptable[factor] )))
      OneSUMAPmat <- cbind(OneSUMAPmat,ODumapMat1)
    }
  }else OneSUMAPmat <- NULL
  
  score<-NULL
  if(DoPCA){
    print("running pca")
    mdpca <- prcomp(data1)
    
    score <- data1 %*% mdpca$rotation
    eigs <- mdpca$sdev^2
    PCAsummary <- rbind(
      SD = sqrt(eigs),
      Proportion = eigs/sum(eigs),
      Cumulative = cumsum(eigs)/sum(eigs))
    write.csv(PCAsummary, "PCA summary.csv")
    write.csv(mdpca$rotation,"PCAloading.csv")
    
    
    score <- score[,1:5]
    plot(score[, 1], score[, 2], pch=".", xlab="PC1", ylab="PC2", cex=0.1)
  } else score <- NULL
  
   
  Rphenographmat = NULL
  if(Dophenograph)
  {
    #print(colnames(data1))
    #print(rownames(data1))
    #print(head(data1))
    print("running phenograph")
    Rphenograph_out <- Rphenograph(data1, k = kValue)
    #return(Rphenograph_out)
    modularity(Rphenograph_out[[2]])
    membership(Rphenograph_out[[2]])
    #Rphenograph_cluster <- as.matrix(membership(Rphenograph_out[[2]]))
    Rpmat <- as.matrix(membership(Rphenograph_out[[2]]))
    #--------------------------------------------------
    # ggplot(data1, aes(x=Sepal.Length, y=Sepal.Width, shape=Rphenograph_cluster)) + geom_point(size = 3)+theme_bw()
    #--------------------------------------------------
    Rphenographmat <- matrix(ncol=1,data=Rpmat,dimnames=list(NULL,"Phenograph"))
    #colnames(Rphenographmat) <- "phenograph"
    
  } else Rphenographmat <- NULL
  

  
  FlowSOMmat <- NULL
  if(DoFlowSOM)
  {
    if (!require(FlowSOM)) {
      source("https://bioconductor.org/biocLite.R")
      biocLite("FlowSOM")
      library(FlowSOM)
    }
    print("running FlowSOM")
    FlowSOMmat <- matrix(ncol=1,data=MetaClustering(data1,method="metaClustering_som",max=MaxClusters),dimnames = list(NULL,"FlowSOM"))
    
  }
 
  
   ### prepare export
  
  score2 <- cbind(score,tSNEmat,Xx1DtSNEmat,usordata, TDtSNEmat,DiffMapMat,umapMat,TDumapMat,OneSUMAPmat)
  
  ## Deal with cells that sometimes go missing with Rphenograph!
  if(!is.null(Rphenographmat))
    if(dim(Rphenographmat)[1] != dim(score2)[1]){
    
    rpnames <- as.numeric(row.names(Rpmat))
    newclustnum <- max(Rphenographmat)+1
    print(paste0((dim(score2)[1]-dim(Rphenographmat)[1]),
                 " events lost during running of Rphenograph - dunno why. Asigned as new cluster #",
                 newclustnum))
    losts <- setdiff(1:dim(score2)[1],rpnames)
    for(li in 1:length(losts))
      Rphenographmat <- append(Rphenographmat, newclustnum, after = (losts[li]-1) )
    Rphenographmat <- matrix(ncol=1,data=Rphenographmat,dimnames=list(NULL,"Phenograph"))
  }
  
  nontransformedData <- cbind(Rphenographmat,FlowSOMmat)
  
  ## 1 existing data - needs tranfrom
  ## 2 new data exported needs tranfrom
  ## 3 new data exported should be used as is
  
  if(!is.null(score2))  lscore2 <- dim(score2)[2] else lscore2<-0
  if(!is.null(nontransformedData))  lnontransformedData <- dim(nontransformedData)[2] else lnontransformedData<-0
    
  
  
  NewTransformBase <- c (rep(1, dim(FFdata)[2]-1))
  if(TransformOutput){
    NewTransformNew <- c(rep(2,lscore2),
                         rep(3,lnontransformedData))
  }else  NewTransformNew <- c(rep(3,lscore2),
                              rep(3,lnontransformedData))
  NewTransform <- c(NewTransformBase,NewTransformNew)
              
  #
  if(TransformOutput){
      Nscore <- apply(score2,2,function(x) ((x-min(x))/(max(x)-min(x)))*3.7 )
      ilgcl <- inverseLogicleTransform(trans = lgcl)
      NIscore <- apply(Nscore, 2, ilgcl)
      NIscore = cbind(NIscore,nontransformedData)
  } else if(!is.null(score2)){
      
      NIscore <- apply(score2,2,function(x) ((x-min(x))/(max(x)-min(x)))*10000 )
                                        #ilgcl <- inverseLogicleTransform(trans = lgcl)
                                        #NIscore <- apply(Nscore, 2, ilgcl)
      
      
      NIscore = cbind(NIscore,nontransformedData)
                                        #colnames(NIscore) <- c(colnames(score2),"phenograph")
  } else if(!is.null(nontransformedData)) {

    NIscore <- nontransformedData
  } else print("NEED TO SELECT AT LEAST ONE ALGORITHM")

  
  #Add * to duplicated parameter names

  preParams <- colnames(FFdata)
  newParams <- colnames(NIscore)
  
  if(!is.null(intersect(preParams, newParams)))
  {
    dupNames <- intersect(preParams, newParams)
    for( dN in dupNames){
      newDN <- paste0(dN,"*")
      newParams[newParams == dN] <- newDN
    }
  }
  colnames(NIscore) <- newParams
  
  
  ##### EXPORT
  
                                   #NIscore <- cbind(NIscore, NXx1)
    if(!useCSV){
    for (FFs in 1:NumBC){
      
      newFF <- fs[[1]]
      newBaseData <- FFdata[FFdata[,dim(FFdata)[2]]==FFs,-dim(FFdata)[2]]
      colnames(newBaseData)<- colnames(exprs(newFF))
      exprs(newFF) <- newBaseData
      subsetNIscore <- NIscore[FFdata[,dim(FFdata)[2]]==FFs,,drop=FALSE]
      newFF <- cbind2(newFF, subsetNIscore)
      if(!is.null(newFF@description$CREATOR)) {if(newFF@description$CREATOR == "ENewellScript") { 
        preNewTransform <- newFF@description$TF
        }}
      
      
      newFF@parameters$desc <- colnames(cbind(newBaseData, subsetNIscore))
      dir.create(paste0(LoaderPATH,"_",OutputSuffix))
      
      BaseFN <- sapply(strsplit(FcsFileNames[FFs], split ="\\."), "[", 1)
      FNresult <- paste0(LoaderPATH,"_",OutputSuffix,"/",BaseFN,"_",OutputSuffix,".fcs")
      newFF@description$'$FIL' <- paste0(BaseFN,"_",OutputSuffix,".fcs")
      if(!is.null(newFF@description$CREATOR)) {
        if( newFF@description$CREATOR == "ENewellScript") 
          newFF@description$TF <- paste0(preNewTransform,paste(unlist(NewTransformNew),collapse=''))
        else
          newFF@description$TF <- paste(unlist(NewTransform),collapse='')
        } else {
        newFF@description$TF <- paste(unlist(NewTransform),collapse='')
      }
      newFF@description$CREATOR <- "ENewellScript"
      newFF@description$FILENAME <- paste0(BaseFN,"_",OutputSuffix,".fcs")
      identifier(newFF) <- paste0(BaseFN,"_",OutputSuffix)
      
      newFF@parameters@data$range <- rep(10000,length(newFF@parameters@data$range))
      
      for(r in 1:length(newFF@parameters@data$name))
      {
       row.names(newFF@parameters@data)[r] <- paste0("$P",r) 
       
      }
      
      print(FNresult)
      write.FCS(newFF, FNresult)
    }}else
    {
      dir.create(paste0(LoaderPATH,"_",OutputSuffix,"_csv"))
      for (FFs in 1:NumBC){
        
        if (!require(Biobase)) { 
          install.packages("Biobase")
          library(Biobase)
        } 
        
        newBaseData <- FFdata[FFdata[,dim(FFdata)[2]]==FFs,-dim(FFdata)[2]]
        newBaseData <- cbind(newBaseData,NIscore[FFdata[,"InFile"]==FFs,])
        rowNames <- colnames(newBaseData)
        varLabels <- colnames(newBaseData)
        metaData <- data.frame(labelDescription = colnames(newBaseData))
        paramsDf <- data.frame(row.names = colnames(newBaseData), name = colnames(newBaseData), 
                               range = rep(4490, length(colnames(newBaseData))),
                               minRange = rep(-1.0, length(colnames(newBaseData))),
                               maxRange = rep(4490, length(colnames(newBaseData))))
        parameterAdf <- AnnotatedDataFrame(data=paramsDf)
        desc <- as.list(colnames(newBaseData))
        datamat <- data.matrix(newBaseData)
        newFF <- new("flowFrame", exprs=datamat, parameters=parameterAdf, description=desc)
        
        
        
        #colnames(newBaseData)<- colnames(exprs(newFF))
        #exprs(newFF) <- newBaseData
        #subsetNIscore <- NIscore[FFdata[,"InFile"]==FFs,]
        #newFF <- cbind2(newFF, subsetNIscore)
        newFF@parameters$desc <- colnames(newBaseData)
        #dir.create(paste0(LoaderPATH,"_",OutputSuffix))
        
        BaseFN <- sapply(strsplit(csvfilenames[FFs], split ="\\."), "[", 1)
        FNresult <- paste0(LoaderPATH,"_",OutputSuffix,"/",BaseFN,"_",OutputSuffix,".fcs")
        FNresultCsv <- paste0(LoaderPATH,"_",OutputSuffix,"_csv","/",BaseFN,"_",OutputSuffix,".csv")
        newFF@description$'$FIL' <- paste0(BaseFN,"_",OutputSuffix,".fcs")
        newFF@description$FILENAME <- paste0(BaseFN,"_",OutputSuffix,".fcs")
      
        identifier(newFF) <- paste0(BaseFN,"_",OutputSuffix)
        
        
        #This is not working: causes a crash!
        #print(FNresult)
        #write.FCS(newFF, FNresult)
        print(FNresultCsv)
        write.csv(exprs(newFF),FNresultCsv,row.names=F)
      }
    }
  
  
}






####
#####      MEANING PLOT FUNCTION
#####

meaningPlot <- function (LoaderPATH =sourceFcsForMeaningPlot,
                         useCSV = F, # T if using .csv files instead of fcs files - will make csv output als
                         CyTOF = F,
                         ceil = 100,
                         FNnames= "names.csv",
                         TransformOutput = T,
                         MeaningTopPercentile = .99,
                         MeaningBotPercentile = .01,
                         PC1 = "tSNE1",
                         PC2 = "tSNE2",
                         DoOneForEach = F,
                         prefix = OutputSuffix,
                         palette=c("black","blue","green","yellow","red"),
                         color.scale.type=c("relative","absolute")[1],
                         resolution=72,
                         cex=0.5,
                         pch=16)
  
{
  if (!require(gplots)) { # using heatmap.2 function
    install.packages("gplots", dependencies=TRUE, 
                     repos="http://cran.cnr.Berkeley.edu")
  }
  
  if (!require(gplots)) { # using heatmap.2 function
    install.packages("gplots", dependencies=TRUE, 
                     repos="http://cran.cnr.Berkeley.edu")
  }
  if (!require(flowCore)) { # using logicleTransform
    source("http://bioconductor.org/biocLite.R")
    biocLite("flowCore")
  }  
  if (!require(gridExtra)) {
    install.packages("gridExtra")
    library(gridExtra)
  }
  if (!require(grid)) {
    
    library(grid)
    
  }
  
  
  
  
  if(!useCSV){
    
    ##read data 
    fs <- read.flowSet(path = LoaderPATH,transform=FALSE,ignore.text.offset=T)
    FcsFileNames <- rownames(keyword(fs, "FILENAME"))
    filenames <- FcsFileNames
    NumBC <- length(fs)
    FFdata <- NULL
    for (FFs in 1:NumBC){
      FFt <- exprs(fs[[FFs]])
      ## Downsample
      if (nrow(FFt)<=ceil)
        FFa <- FFt
      else
        FFa <- FFt[sample(nrow(FFt),ceil,replace=F),]
      #Fixup column names
      colnames(FFa) <- fs[[FFs]]@parameters$desc
      empties <- which(is.na(colnames(FFa)) | colnames(FFa)== " ")
      colnames(FFa)[empties] <- fs[[FFs]]@parameters$name[empties]
      fs[[FFs]]@parameters$desc <- colnames(FFa)
      #fs[[FFs]]@parameters$name <- colnames(FFa)
      #Add file label
      FFa <- cbind(FFa,rep(FFs,dim(FFa)[1]))
      colnames(FFa)[dim(FFa)[2]] <- "InFile"
      #Concatenate
      FFdata <- rbind(FFdata,FFa)
    }} else {  #load csv files instead of fcs
      
      csvfilenames <- list.files(path = LoaderPATH, pattern="*.csv")
      filenames <- csvfilenames
      csvdata <- lapply(paste0(LoaderPATH,"//",csvfilenames),function(x) read.csv(x, check.names = F))
      NumBC <- length(csvdata)
      FFdata<-NULL
      for (FFs in 1:NumBC){
        FFt <- csvdata[[FFs]]
        ## Downsample
        if (nrow(FFt)<=ceil)
          FFa <- FFt
        else
          FFa <- FFt[sample(nrow(FFt),ceil,replace=F),]
        
        FFa <- cbind(FFa,rep(FFs,dim(FFa)[1]))
        colnames(FFa)[dim(FFa)[2]] <- "InFile"
        #Concatenate
        FFdata <- rbind(FFdata,FFa)
      }}
  
  
  transList <- strsplit(unlist(fs[[1]]@description$TF),"")[[1]]
  
  keeptable <- read.csv(FNnames)
  keeprowbool <- sapply(keeptable[,2], function(x) any(x=="y"))
  keeprows <- subset(keeptable, keeprowbool)
  
  #data1u and data1 are from the names file
  data1u <- FFdata[,which (colnames(FFdata) %in% keeprows[,1])]
  
  if(CyTOF){
    lgcl <- logicleTransform(w=0.25, t=16409, m=4.5, a=0)} else
      lgcl <- logicleTransform(w=0.1, t=500000, m=4.5, a=0)
  ilgcl <- inverseLogicleTransform(trans = lgcl)
  data1 <- apply(data1u, 2, lgcl)
  
  
  # Add new parameters:
  ## 1 existing data - needs tranfrom
  ## 2 new data exported needs tranfrom
  ## 3 new data exported should be used as is
  #those needing transform
  
  data2u <- FFdata[,transList == "2"]
  data2 <- apply(data2u, 2, lgcl)
  data3 <- FFdata[,transList == "3"]
  
  
  
  keeptable <- read.csv(FNnames,stringsAsFactors=F)
  
  sapply(c("png","raster"),function(package){
    if(!require(package,character.only=T)){
      install.packages(pkgs=package)
      library(package,character.only=T)
    }
  })
  
  
  
  
  
  
  
  data <- cbind(data1,data2,data3)
  data.range=range(data)
  
  #PCA has been run and appended to the FCS
  x <- data[,PC1]
  y <- data[,PC2]
  
  # code.channels=setNames(newFF@parameters$desc,newFF@parameters$name)
  # colnames.remap=intersect(colnames(data),names(code.channels))
  # colnames(data)[colnames(data)%in%colnames.remap]=code.channels[colnames(data)[colnames(data)%in%colnames.remap]]
  # 
  bottoms1 <- apply(data1,2,function(a) quantile(a, MeaningBotPercentile))
  tops1 <- apply(data1,2,function(a) quantile(a, MeaningTopPercentile))
  
  datat2 <- cbind(data2,data3)
  bottomst2 <- apply(datat2,2,min)
  topst2 <- apply(datat2,2,max)
  
  bottoms <- c(bottoms1, bottomst2)
  tops <- c(tops1, topst2)
  
  rasters=sapply(colnames(data),function(pname)
  {
    Exp <- data[,pname]
    PltDat <- data.frame(x,y,Exp)
    color.scale=unique(colorRampPalette(palette)(1000))
    
    if(color.scale.type=="relative"){
      points.colors=as.character(cut(Exp,breaks=seq(bottoms[pname],tops[pname],length.out=length(color.scale)+1),labels=color.scale))
    }
    if(color.scale.type=="absolute"){
      points.colors=as.character(cut(Exp,breaks=seq(data.range[1],data.range[2],length.out=length(color.scale)+1),labels=color.scale))
    }
    
    mainplot=paste(tmpDir(),"/mainplot.png",sep="")
    png(mainplot,res=resolution,height=480*resolution/72,width=480*resolution/72)
    par("bty"="l")
    plot(x,y,col=points.colors,xlab=PC1,ylab=PC2,main=pname,pch=pch, cex =cex)
    dev.off()
    
    if(color.scale.type=="relative"){
      colorscale=paste(tmpDir(),"/colorscale.png",sep="")
      png(colorscale,res=resolution,height=480/2*resolution/72,width=480*resolution/72)
      plot.new()
      par("mar"=c(2,1,2,1))
      xlims=par("usr")[1:2]
      ylims=par("usr")[3:4]
      
      n=length(color.scale)
      labels=signif((seq(bottoms[pname],tops[pname],length.out=length(color.scale)+1)),2)
      
      x_coords=seq(xlims[1],xlims[2],length.out=n+1)
      rect(border=NA,ybottom=ylims[1],ytop=ylims[2],xleft=x_coords[-length(x_coords)],xright=x_coords[-1],col=color.scale)
      labels.x_coords=seq(x_coords[1],x_coords[length(x_coords)],length.out=5)
      labels=labels[round(seq(1,length(labels),length.out=5))]
      text(xpd=T,y=ylims[1],pos=1,labels=labels,x=labels.x_coords)
      text(xpd=T,y=ylims[2],pos=3,labels=paste(pname,"intensity"),x=mean(xlims))
      dev.off()
    }
    if(color.scale.type=="absolute"){
      colorscale=paste(tmpDir(),"/colorscale.png",sep="")
      png(colorscale,res=resolution,height=480/2*resolution/72,width=480*resolution/72)
      plot.new()
      par("mar"=c(2,1,2,1))
      xlims=par("usr")[1:2]
      ylims=par("usr")[3:4]
      
      n=length(color.scale)
      labels=signif((seq(data.range[1],data.range[2],length.out=length(color.scale)+1)),2)
      
      x_coords=seq(xlims[1],xlims[2],length.out=n+1)
      rect(border=NA,ybottom=ylims[1],ytop=ylims[2],xleft=x_coords[-length(x_coords)],xright=x_coords[-1],col=color.scale)
      labels.x_coords=seq(x_coords[1],x_coords[length(x_coords)],length.out=5)
      labels=labels[round(seq(1,length(labels),length.out=5))]
      text(xpd=T,y=ylims[1],pos=1,labels=labels,x=labels.x_coords)
      text(xpd=T,y=ylims[2],pos=3,labels=paste("Intensity"),x=mean(xlims))
      dev.off()
    }
    return(list(raster.main=readPNG(mainplot,native=T),raster.scale=readPNG(colorscale,native=T)))
  },simplify=F)
  
  pltFN <- sprintf("Concatenated_%s.pdf",prefix)
  pdf(pltFN)
  if(color.scale.type=="absolute"){
    plot.new()
    grid.raster(rasters[[1]]$raster.scale)
  }
  sapply(rasters,function(x){
    par("mar"=c(0,0,0,0))
    if(color.scale.type=="relative"){
      grid.newpage()
      grid.raster(x$raster.main,y=0.6,height=0.8)
      grid.raster(x$raster.scale,y=0.1,height=0.2)
    }
    if(color.scale.type=="absolute"){
      par("mar"=c(0,0,0,0))
      grid.newpage()
      grid.raster(x$raster.main)
    }
  })
  dev.off()
  
  if(DoOneForEach)
  {
    
    for(FFs in 1:NumBC)
    {
      
      
      dataDbc <- data[FFdata[,"InFile"]==FFs,]
      x <- dataDbc[,PC1]
      y <- dataDbc[,PC2]
      
      # code.channels=setNames(newFF@parameters$desc,newFF@parameters$name)
      # colnames.remap=intersect(colnames(data),names(code.channels))
      # colnames(data)[colnames(data)%in%colnames.remap]=code.channels[colnames(data)[colnames(data)%in%colnames.remap]]
      # 
      
      rasters=sapply(colnames(dataDbc),function(pname)
      {
        Exp <- dataDbc[,pname]
        PltDat <- data.frame(x,y,Exp)
        color.scale=unique(colorRampPalette(palette)(1000))
        
        if(color.scale.type=="relative"){
          points.colors=as.character(cut(Exp,breaks=seq(bottoms[pname],tops[pname],length.out=length(color.scale)+1),labels=color.scale))
        }
        if(color.scale.type=="absolute"){
          points.colors=as.character(cut(Exp,breaks=seq(data.range[1],data.range[2],length.out=length(color.scale)+1),labels=color.scale))
        }
        
        mainplot=paste(tmpDir(),"/mainplot.png",sep="")
        png(mainplot,res=resolution,height=480*resolution/72,width=480*resolution/72)
        par("bty"="l")
        plot(x,y,col=points.colors,xlab=PC1,ylab=PC2,main=pname,pch=pch, cex =cex)
        dev.off()
        
        if(color.scale.type=="relative"){
          colorscale=paste(tmpDir(),"/colorscale.png",sep="")
          png(colorscale,res=resolution,height=480/2*resolution/72,width=480*resolution/72)
          plot.new()
          par("mar"=c(2,1,2,1))
          xlims=par("usr")[1:2]
          ylims=par("usr")[3:4]
          
          n=length(color.scale)
          labels=signif((seq(bottoms[pname],tops[pname],length.out=length(color.scale)+1)),2)
          
          x_coords=seq(xlims[1],xlims[2],length.out=n+1)
          rect(border=NA,ybottom=ylims[1],ytop=ylims[2],xleft=x_coords[-length(x_coords)],xright=x_coords[-1],col=color.scale)
          labels.x_coords=seq(x_coords[1],x_coords[length(x_coords)],length.out=5)
          labels=labels[round(seq(1,length(labels),length.out=5))]
          text(xpd=T,y=ylims[1],pos=1,labels=labels,x=labels.x_coords)
          text(xpd=T,y=ylims[2],pos=3,labels=paste(pname,"intensity"),x=mean(xlims))
          dev.off()
        }
        if(color.scale.type=="absolute"){
          colorscale=paste(tmpDir(),"/colorscale.png",sep="")
          png(colorscale,res=resolution,height=480/2*resolution/72,width=480*resolution/72)
          plot.new()
          par("mar"=c(2,1,2,1))
          xlims=par("usr")[1:2]
          ylims=par("usr")[3:4]
          
          n=length(color.scale)
          labels=signif((seq(data.range[1],data.range[2],length.out=length(color.scale)+1)),2)
          
          x_coords=seq(xlims[1],xlims[2],length.out=n+1)
          rect(border=NA,ybottom=ylims[1],ytop=ylims[2],xleft=x_coords[-length(x_coords)],xright=x_coords[-1],col=color.scale)
          labels.x_coords=seq(x_coords[1],x_coords[length(x_coords)],length.out=5)
          labels=labels[round(seq(1,length(labels),length.out=5))]
          text(xpd=T,y=ylims[1],pos=1,labels=labels,x=labels.x_coords)
          text(xpd=T,y=ylims[2],pos=3,labels=paste("Intensity"),x=mean(xlims))
          dev.off()
        }
        return(list(raster.main=readPNG(mainplot,native=T),raster.scale=readPNG(colorscale,native=T)))
      },simplify=F)
      
      BaseFN <- sapply(strsplit(filenames[FFs], split ="\\."), "[", 1)
      pltFN <- sprintf("%s_%s.pdf",BaseFN,prefix)
      pdf(pltFN)
      if(color.scale.type=="absolute"){
        plot.new()
        grid.raster(rasters[[1]]$raster.scale)
      }
      sapply(rasters,function(x){
        par("mar"=c(0,0,0,0))
        if(color.scale.type=="relative"){
          grid.newpage()
          grid.raster(x$raster.main,y=0.6,height=0.8)
          grid.raster(x$raster.scale,y=0.1,height=0.2)
        }
        if(color.scale.type=="absolute"){
          par("mar"=c(0,0,0,0))
          grid.newpage()
          grid.raster(x$raster.main)
        }
      })
      dev.off()
    }}
  
  }


fileHeatplot <- function (LoaderPATH ="fcs",
                         useCSV = F, # T if using .csv files instead of fcs files - will make csv output als
                         CyTOF = F,
                         ceil = 100,
                         TransformOutput = T,
                         FNnames = "names.csv",
                         OutputSuffix = "1",
                         palette=c("black","blue","lightblue","green","yellow","darkorange","darkred"),
                         DoClusters=F,
                         clustParam="Phenograph")
                  
  ##MAKE HEATMAP BY FILES
{  
  
  
  if (!require(gplots)) { # using heatmap.2 function
    install.packages("gplots", dependencies=TRUE, 
                     repos="http://cran.cnr.Berkeley.edu")
  }
  if (!require(flowCore)) { # using logicleTransform
    source("http://bioconductor.org/biocLite.R")
    biocLite("flowCore")
  }
  #To Start with flowcore:
  # LIBRARIES TO IMPORT
  if (!require(gplots)) { # using heatmap.2 function
    install.packages("gplots", dependencies=TRUE, 
                     repos="http://cran.cnr.Berkeley.edu")
  }
  if (!require(flowCore)) { # using logicleTransform
    source("http://bioconductor.org/biocLite.R")
    biocLite("flowCore")
  }  
  
  
  if (!require(ggplot2)) {
    install.packages("ggplot2")
    library(ggplot2)
    
  }
  if (!require(reshape)) {
    install.packages("reshape")
    library(reshape)
    
  }
  
  
  
  
  fs <- read.flowSet(path = LoaderPATH,transform=FALSE,ignore.text.offset=T)
  names <- fs[[1]]@parameters$name
  keeptable <- read.csv(FNnames)
  if(CyTOF){
    lgcl <- logicleTransform(w=0.25, t=16409, m=4.5, a=0)} else
      lgcl <- logicleTransform(w=0.1, t=500000, m=4.5, a=0)
  keeprowbool <- apply(keeptable, 1, function(x) any(x=="y"))
  keeprows <- subset(keeptable, keeprowbool)
  keepnames <- keeptable[keeprowbool,1]
  sampID <- NULL
  hpmat <- NULL
  cellnum <- NULL
  if(length(fs)>1) {
  for (i in 1:length(fs)){
    data <- exprs(fs[[i]])
    print(dim(data))
    fs[[i]]@parameters$desc[which(fs[[i]]@parameters$desc==" " | is.na(fs[[i]]@parameters$desc))] <- 
      fs[[i]]@parameters$name[which(fs[[i]]@parameters$desc==" " | is.na(fs[[i]]@parameters$desc))]
    colnames(data) <- fs[[i]]@parameters$desc
    tdata <- apply(data, 2, lgcl)
    colnames(tdata) <- fs[[i]]@parameters$desc
    tmdata <- tdata[,which (colnames(tdata) %in% keeprows[,1])]
    
    mtmdata <- apply(tmdata, 2, median)
    cellnum <- cbind(cellnum, dim(data)[1])
    sampID <- c(sampID, rep(i,dim(data)[1]))
    hpmat <- cbind(hpmat, mtmdata)
    
  }
  
  
  
  filnames <- strsplit(rownames(keyword(fs, c("$FIL"))), split = "\\.") 
  colnames(hpmat) <- c( sapply(filnames, "[", 1))
  #row.names(keeprows) <- keeptable[keeprowbool,1]
  #hpmat<-hpmat[keeprowbool,]
  
  
  pdf(file=paste0(LoaderPATH,"Medians",OutputSuffix,".pdf"), width=14, height =6)
  
  breaks = seq(0,2.5,by=0.05)
  
 
  my_palette <- colorRampPalette(palette)(n=length(breaks)-1)
  
  
  
  heatmap.2(t(hpmat), col=my_palette, 
            breaks = breaks,
            margins = c(10,20), 
            Colv = T,
            dendrogram = "both",
            cexCol = 1., cexRow =1., scale="none", key=TRUE,trace="none", 
            density.info=c("none"),
            keysize=1)    
  dev.off()
  
  write.csv(t(hpmat),paste0(LoaderPATH,"MedianValues",OutputSuffix,".csv"))
}
 if(DoClusters)
 {
   if(!useCSV){
     
     ##read data 
     #fs <- read.flowSet(path = LoaderPATH)
     FcsFileNames <- rownames(keyword(fs, "FILENAME"))
     filenames <- FcsFileNames
     NumBC <- length(fs)
     FFdata <- NULL
     for (FFs in 1:NumBC){
       FFt <- exprs(fs[[FFs]])
       ## Downsample
       if (nrow(FFt)<=ceil)
         FFa <- FFt
       else
         FFa <- FFt[sample(nrow(FFt),ceil,replace=F),]
       #Fixup column names
       colnames(FFa) <- fs[[FFs]]@parameters$desc
       empties <- which(is.na(colnames(FFa)) | colnames(FFa)== " ")
       colnames(FFa)[empties] <- fs[[FFs]]@parameters$name[empties]
       fs[[FFs]]@parameters$desc <- colnames(FFa)
       #fs[[FFs]]@parameters$name <- colnames(FFa)
       #Add file label
       FFa <- cbind(FFa,rep(FFs,dim(FFa)[1]))
       colnames(FFa)[dim(FFa)[2]] <- "InFile"
       #Concatenate
       FFdata <- rbind(FFdata,FFa)
     }} else {  #load csv files instead of fcs
       
       csvfilenames <- list.files(path = LoaderPATH, pattern="*.csv")
       filenames <- csvfilenames
       csvdata <- lapply(paste0(LoaderPATH,"//",csvfilenames),function(x) read.csv(x, check.names = F))
       NumBC <- length(csvdata)
       FFdata<-NULL
       for (FFs in 1:NumBC){
         FFt <- csvdata[[FFs]]
         ## Downsample
         if (nrow(FFt)<=ceil)
           FFa <- FFt
         else
           FFa <- FFt[sample(nrow(FFt),ceil,replace=F),]
         
         FFa <- cbind(FFa,rep(FFs,dim(FFa)[1]))
         colnames(FFa)[dim(FFa)[2]] <- "InFile"
         #Concatenate
         FFdata <- rbind(FFdata,FFa)
       }}
   
   
   transList <- strsplit(unlist(fs[[1]]@description$TF),"")[[1]]
   
   keeptable <- read.csv(FNnames)
   keeprowbool <- sapply(keeptable[,2], function(x) any(x=="y"))
   keeprows <- subset(keeptable, keeprowbool)
   
   #data1u and data1 are from the names file
   data1u <- FFdata[,which (colnames(FFdata) %in% keeprows[,1])]
   
   if(CyTOF){
     lgcl <- logicleTransform(w=0.25, t=16409, m=4.5, a=0)} else
       lgcl <- logicleTransform(w=0.1, t=500000, m=4.5, a=0)
   ilgcl <- inverseLogicleTransform(trans = lgcl)
   data1 <- apply(data1u, 2, lgcl)
   
   
   # Add new parameters:
   ## 1 existing data - needs tranfrom
   ## 2 new data exported needs tranfrom
   ## 3 new data exported should be used as is
   #those needing transform
   
   data2u <- FFdata[,transList == "2"]
   data2 <- apply(data2u, 2, lgcl)
   data3 <- FFdata[,transList == "3"]
   
   data<-cbind(data1,data2,data3)
   hpmat <- NULL
   for(cluster in 1:max(FFdata[,clustParam]))
   {
     
     mtmdata <- apply(data1[FFdata[,clustParam]==cluster,], 2, median)
     hpmat <- cbind(hpmat, mtmdata)
   }
   
   
   
   colnames(hpmat) <- 1:max(FFdata[,clustParam])
   #row.names(keeprows) <- keeptable[keeprowbool,1]
   #hpmat<-hpmat[row.names(keeprows),]
   
   
   pdf(file=paste0(LoaderPATH,"ClusterMedians",OutputSuffix,".pdf"), width=14, height =10)
   
   breaks = seq(0,2.5,by=0.05)
   
   
   my_palette <- colorRampPalette(palette)(n=length(breaks)-1)
   
   
   
   heatmap.2(t(hpmat), col=my_palette, 
             breaks = breaks,
             margins = c(10,20), 
             Colv = T,
             dendrogram = "both",
             cexCol = 1., cexRow =1., scale="none", key=TRUE,trace="none", 
             density.info=c("none"),
             keysize=1)    
   dev.off()
   
   write.csv(t(hpmat),paste0(LoaderPATH,"ClusterMedianValues",OutputSuffix,".csv"))
   
   
####  Tabulate frequencies of each cluster across each sample and make heatplot:   
   
   mtmdata<- NULL
   hpmat <- NULL
   hpmatCnames <- NULL
   for(sample in 1:length(fs)){
      cellsInSample <- length(which(sampID == sample))
      clustFreq <- NULL
       for(cluster in 1:max(FFdata[,clustParam]))
        {
     
          cellsInSampleInCluster <- length(which(sampID == sample & FFdata[,clustParam]==cluster)) 
          clustFreq <- c(clustFreq, (cellsInSampleInCluster/cellsInSample)*100)
       }
    
      hpmat <- cbind(hpmat, clustFreq)
   }
  
 colnames(hpmat) <- c( sapply(filnames, "[", 1))
   row.names(hpmat) <- 1:max(FFdata[,clustParam])
   #hpmat<-hpmat[row.names(keeprows),]
   
   
   pdf(file=paste0(LoaderPATH,"ClusterFrequencies",OutputSuffix,".pdf"), width=14, height =10)
   
   breaks = seq(0,max(hpmat),by=max(hpmat)/50)
   
   
   my_palette <- colorRampPalette(palette)(n=length(breaks)-1)
   
   
   
   heatmap.2(t(hpmat), col=my_palette, 
             breaks = breaks,
             margins = c(10,20), 
             Colv = T,
             dendrogram = "both",
             cexCol = 1., cexRow =1., scale="none", key=TRUE,trace="none", 
             density.info=c("none"),
             keysize=1)    
   dev.off()
   
   pdf(file=paste0(LoaderPATH,"ClusterLogFrequencies",OutputSuffix,".pdf"), width=14, height =10)
   
   breaks = seq(0,max(log10(hpmat+.001)),by=max(log10(hpmat+.001))/50)
   
   
   my_palette <- colorRampPalette(palette)(n=length(breaks)-1)
   
   
   
   heatmap.2(t(log10(hpmat+.001)), col=my_palette, 
             breaks = breaks,
             margins = c(10,20), 
             Colv = T,
             dendrogram = "both",
             cexCol = 1., cexRow =1., scale="none", key=TRUE,trace="none", 
             density.info=c("none"),
             keysize=1)    
   dev.off()
   
   write.csv(t(hpmat),paste0(LoaderPATH,"ClusterFreqValues",OutputSuffix,".csv"))
   
   
 }
}


ThreeDPlot <- function(LoaderPATH ="fcsfiles", CyTOF = T, FNames = "names.csv",
                       ceil = 300, OutputSuffix = "out",
                       parN1, parN2, parN3, colparN = "InFile", Ncolors = 4, ColorList =c("orange","red","green","blue") ){
  
  
  library(RColorBrewer)
  if (!require(flowCore)) { # using logicleTransform
    source("http://bioconductor.org/biocLite.R")
    biocLite("flowCore")
  }
  
  if (!require(scatterplot3d)) { # using logicleTransform
    install.packages("scatterplot3d")
    library(scatterplot3d)
  }
  
  if (!require(rgl)) { # using logicleTransform
    source("http://bioconductor.org/biocLite.R")
    biocLite("rgl")
  }
  
 
  

  
  
  
  fs <- read.flowSet(path = LoaderPATH,transform=FALSE,ignore.text.offset=T)
  FcsFileNames <- rownames(keyword(fs, "FILENAME"))
  NumBC <- length(fs)
  FFdata <- NULL
  for (FFs in 1:NumBC){
    FFt <- exprs(fs[[FFs]])
    ## Downsample
    if (nrow(FFt)<=ceil)
      FFa <- FFt
    else
      FFa <- FFt[sample(nrow(FFt),ceil,replace=F),]
    #Fixup column names
    colnames(FFa) <- fs[[FFs]]@parameters$desc
    empties <- which(is.na(colnames(FFa)) | colnames(FFa)== " ")
    colnames(FFa)[empties] <- fs[[FFs]]@parameters$name[empties]
    fs[[FFs]]@parameters$desc <- colnames(FFa)
    #fs[[FFs]]@parameters$name <- colnames(FFa)
    #Transform
    #lgcl <- logicleTransform(w=0.25, t=16409, m=4.5, a=0)
    #lgcl <- logicleTransform(w=.1, t=4000, m=4.5, a=0)
    #ilgcl <- inverseLogicleTransform(trans = lgcl)
    #FFaT <- apply(FFa, 2, lgcl)
    #Add file label
    FFaL <- cbind(FFa,rep(FFs,dim(FFa)[1]))
    colnames(FFaL)[dim(FFaL)[2]] <- "InFile"
    #Concatenate
    FFdata <- rbind(FFdata,FFaL)
    
    print(paste0("IF colored by files: File#",FFs," is ",FcsFileNames[FFs]," Color:",ColorList[FFs]))
  }
  
  transList <- strsplit(unlist(fs[[1]]@description$TF),"")[[1]]
  
  keeptable <- read.csv(FNnames)
  keeprowbool <- sapply(keeptable[,2], function(x) any(x=="y"))
  keeprows <- subset(keeptable, keeprowbool)
  #data1u and data1 are from the names file
  data1u <- FFdata[,which (colnames(FFdata) %in% keeprows[,1])]
  
  if(CyTOF){
    lgcl <- logicleTransform(w=0.25, t=16409, m=4.5, a=0)} else
      lgcl <- logicleTransform(w=0.1, t=500000, m=4.5, a=0)
  ilgcl <- inverseLogicleTransform(trans = lgcl)
  data1 <- apply(data1u, 2, lgcl)
  
  
  # Add new parameters:
  ## 1 existing data - needs tranfrom
  ## 2 new data exported needs tranfrom
  ## 3 new data exported should be used as is
  #those needing transform
  
  data2u <- FFdata[,transList == "2"]
  data2 <- apply(data2u, 2, lgcl)
  data3 <- FFdata[,transList == "3", drop=F]
  
  data <- cbind(data1, data2, data3, InFile = FFdata[,"InFile"])
  
 #backup plan to salvalge colparN data for the user 
  if (!colparN %in% colnames(data)){
    data <- cbind(data, FFdata[,colparN])
    colnames(data)[dim(data)[2]] <- colparN
  }
  
  if (Ncolors == 0){
    Ncolors <- max(data[,colparN])
    
  }
  print(paste("Number of Colors",Ncolors))  
 Ddata <- data[,c(parN1, parN2, parN3, colparN)]
  palette <- colorRampPalette(ColorList)(n=Ncolors)
  
  
  c <- data[,colparN]
  # for cluster numbers only
  #if(colparN == "cluster") c<-sapply(c,ilgcl)
  
  c <- cut(c, breaks=Ncolors)
  
  #scatterplot3d(Ddata[,-4], pch = ".", highlight.3d=TRUE, cex.symbols=1)
  
  plot3d(x=Ddata[,1], y=Ddata[,2], z=Ddata[,3], 
         xlab = " ", ylab = " ", zlab = " ",
         col = palette[as.numeric(c)], type = 'p',  size=1, box =F,
         axes = F)
  
  par3d(windowRect = c(0, 0, 1200, 1200)) # make the window large
  degrees <- seq(1,360, by = 2) # a sequence from 1 to 360
  
  dir.create(paste0(LoaderPATH,"_",OutputSuffix))
  FNresult <- paste0(LoaderPATH,"_",OutputSuffix,"/tSNE3D","_",OutputSuffix,".fcs")
  
  for(i in 1:length(degrees)){
    view3d(degrees[i], phi = 0) # pick the angle of view
    rgl.snapshot(paste(paste(FNresult, "-", 
                             formatC(i, digits = 3, flag = "0"), sep = ""), "png", sep = "."))
  }
  
  
  
  
}


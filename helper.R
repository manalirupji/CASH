contingencyfun <- function(memb)
{
  memb[,1] <- gsub("[[:punct:]]", "-", memb[,1])
  input <- cbind(apply(memb[1], 1, function(x) ifelse(grepl("-G1", x), "Group1", "Group2")), memb) #check input is from shiny?
  
  mytable <- table(input[,4], input[,1])
  mytable <- as.data.frame.matrix(mytable)
  df <- unmatrix(mytable,byrow=T)
  return(df)
}


pvaluefunc <- function(mytable)
{
  m <- t(matrix(mytable, 2, 2))
  #colnames(m) <- c("Normal", "Tumor")
  if(length(m[m<5]) != 0) {
    pvalue <- fisher.test(m)$p.value
  }else {
    pvalue <- chisq.test(m)$p.value
  }
  return(pvalue)
}


zClust <- function(x, scale, zlim)
{
  if(scale != "none")
  {
    if (scale=="row") z <- t(scale(t(x)))
    if (scale=="col") z <- scale(x)
    if (scale=="both") {
      z <- t(scale(t(x))) #row scaling
      z <- scale(z) # column scaling
    }
    z <- pmin(pmax(z,zlim[1]), zlim[2])
    return(list(data=z))
  }
  else {
    return(list(data))
  }
}


bootstrapfun <- function(obsdata, samplingdata, distmethod, clustmethod, scale, n, k, n.iter, zlim) #n is the sample size for the expected data
{
  #obsdata$Group <- ifelse(grepl("-01", obsdata$X), "Tumor", "Normal")
  #obsmatrix <- table(obsdata$x, obsdata$Group)
  #pobs <- pvaluefunc(obsmatrix)
  obsdata$Sample <- gsub("[[:punct:]]", "-", obsdata$Sample)
  
  # estimate no. of col groups for obs
  classfn.obs <- as.character(unlist(obsdata$Group)) 
  classfn.count.obs <- table(classfn.obs)
  classfn.names.obs <- names(table(classfn.obs))
  
  obsdata$Sample <- ifelse(grepl(classfn.names.obs[1], obsdata$Group) , paste(obsdata$Sample, ".G1", sep=""), paste(obsdata$Sample, ".G2", sep=""))
  
  obstable <- contingencyfun(obsdata)
  pobs <- pvaluefunc(obstable)
  if(pobs <= 0.05)
  {
    contab <- list()
    my.matrix <- matrix()
    if(n <= nrow(samplingdata))
    {
      for(i in 1:n.iter) #change to no. of interations required
      {
        #Remove non-data columns
        samplingdata2<- samplingdata[,c(-1, -2)]
        
        # estimate no. of col groups
        classfn <- as.character(unlist(samplingdata2[1,])) 
        classfn.count <- table(classfn)
        classfn.names <- names(table(classfn))
        
        colnames(samplingdata2) <- ifelse(grepl(classfn.names[1], samplingdata2[1,]) , paste(colnames(samplingdata2), ".G1", sep=""), paste(colnames(samplingdata2), ".G2", sep=""))
        
        #Remove non-data rows
        samplingdata3<- samplingdata2[-1,]
        
        # randomly select user selected no. of rows from a dataset with replacement
        bootdata <- samplingdata3[sample(1:nrow(samplingdata3), n, replace=TRUE),]
        
        # Check for missing data values
        # complete.cases(bootdata2[[1]])
        bootdata2 <- bootdata[complete.cases(bootdata),]
        bootdata3 <- data.matrix(bootdata2)
        
        # Call HEATPLOT2 EQUIVALENT HEATMAP2 CLUSTERING function
        mat <- zClust(bootdata3, scale, zlim )
        
        if(distmethod == "pearson correlation") {
          #   hm <- heatmap.2(as.matrix(mat[[1]]), Rowv=T, Colv=T, scale="none", hclust=function(x) hclust(x,method=clustmethod), distfun=function(x) as.dist((1-cor(t(x)))), margin = c(7,9), density.info=c("none"),trace=c("none"))
          d <- as.dist((1-cor(mat[[1]])))
          hc <- hclust(d, method=clustmethod)
        } else {
          #  hm <- heatmap.2(as.matrix(mat[[1]]), scale="none", Rowv=T, Colv=T, hclust=function(c) {hclust(c,method=clustmethod)}, distfun=function(c) {dist(c,method=distmethod)}, margin = c(7,9), density.info=c("none"),trace=c("none"))
          hc <- hclust(dist(t(mat$data), method= distmethod), method=clustmethod)
        }
        
        ##    Hierarchical clustering of samples
        #hc <- hclust(hm$colDendrogram)
        
        #hclust(dist(t(z$data),method= distmethod), method = clustmethod) # will need to change depending on the clustring used
        plot(hc)
        
        ### cut the tree into input number of clusters
        memb <- cutree(hc, k)[hc$order]
        # memb <- cutree(as.hclust(hm$colDendrogram), k)[as.hclust(hm$colDendrogram)$order]
        memb <- cbind(Row.Names = rownames(memb), memb, row.names = NULL)
        memb <- cbind.data.frame(rownames(memb),1:nrow(memb), memb) 
        memb
        
        contab[[i]] <- contingencyfun(memb)
        print(i)
      }
      
      # summarize each iteration row wise in a data frame
      my.matrix <- data.frame(matrix(unlist(contab), nrow=n.iter, byrow=T)) #no. of iterations
      my.matrix$p.value <- apply(my.matrix, 1, function(x) pvaluefunc(x))
      # my.matrix$Test <- length(which(my.matrix$p.value >= pobs))
      
      #calculate the pvalue to test significance of the CpG sites compared to  random sets of the same no. in separating T from N
      pvalue <- (length(which(my.matrix$p.value >= pobs))+1)/(n.iter+1)
      
      return(pvalue)
      
    }
    else
      stop("Sample size selected is larger than the dataset itself! Please select smaller sample size")
  }
  else
    stop("p-value from the obs data is not statistically significant, thus testing for random sites to assess their significance does not make sense")
}

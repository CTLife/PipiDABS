 


#dataFrame_temp2 <- data.frame(
#  mysampleID  = c(mySampleID_NC_g,  mySampleID_IVF_fresh_g),
#  mytreatment = c(myTreatment_NC_g, myTreatment_IVF_fresh_g),
#  mysex       = c(Sex_NC_g,  Sex_IVF_fresh_g),
#  mytech      = c(Tech_NC_g, Tech_IVF_fresh_g)    
#)
myMainFunction_speRegions_g  <- function(  myobj_temp2,   path_temp2, speRegions_temp2,   qvalue_temp2, differenceOfMethylation_temp2, mergeDistance_temp2,   binBases_temp2, dataFrame_temp2, numCores_temp2  ) {                                                   
  if( ! file.exists(path_temp2) ) { dir.create(path_temp2, recursive = TRUE) }
  sink( file=paste(path_temp2, "values_of_parameters.txt", sep="/") )
     print("path_temp2:")
     print(path_temp2)
     cat("\n\n\n")
     print("qvalue_temp2:")
     print(qvalue_temp2)
     cat("\n\n\n")
     print("differenceOfMethylation_temp2:")
     print(differenceOfMethylation_temp2)
     cat("\n\n\n")
     print("mergeDistance_temp2:")
     print(mergeDistance_temp2)
     cat("\n\n\n")
     print("speRegions_temp2:")
     print(speRegions_temp2)
     cat("\n\n\n")
     print("binBases_temp2:")
     print(binBases_temp2)
     cat("\n\n\n")
     print("dataFrame_temp2:")
     print(dataFrame_temp2)
     cat("\n\n\n")
     print("numCores_temp2:")
     print(numCores_temp2)
     cat("\n\n\n")
  sink()   

  meth_2two <- reorganize(myobj_temp2,  sample.ids = as.vector(dataFrame_temp2$mysampleID),  treatment  = as.vector(dataFrame_temp2$mytreatment) )
  
  path_temp2_sub1 = paste(path_temp2, "1_stats_information", sep="/")
  if( ! file.exists(path_temp2_sub1) ) { dir.create(path_temp2_sub1, recursive = TRUE) }
  
  write.table(dataFrame_temp2 , 
              file = paste(path_temp2_sub1,   "0_dataFrame.txt",  sep="/"), 
              append = FALSE, quote = FALSE, sep = "\t", eol = "\n", na = "NA", dec = ".", 
              row.names = FALSE,  col.names = TRUE, qmethod = c("escape", "double"),  fileEncoding = "")
  
  sink( file=paste(path_temp2_sub1,  "1_select-subSets.txt", sep="/")  )
  print( length(meth_2two) )
  print( getSampleID(meth_2two) )
  print( getTreatment(meth_2two) )
  sink()
  
  meth_2two = regionCounts(meth_2two,  speRegions_temp2,   cov.bases=binBases_temp2,  strand.aware=FALSE)  
  mat_2two  = percMethylation( meth_2two )
  
  sink( file=paste(path_temp2_sub1 , "2_dimensions-tiles-merged.txt", sep="/")  )
  print("#########dimensions:")
  print( dim(meth_2two)  )   
  print( dim(mat_2two)   )
  sink()
  
  
  write.table(meth_2two , 
              file = paste(path_temp2_sub1,   "3A_meth-tiles.txt",  sep="/"), 
              append = FALSE, quote = FALSE, sep = "\t", eol = "\n", na = "NA", dec = ".", 
              row.names = FALSE,  col.names = TRUE, qmethod = c("escape", "double"),  fileEncoding = "")
  write.table(mat_2two , 
              file = paste(path_temp2_sub1,   "3B_mat-tiles.txt",  sep="/"), 
              append = FALSE, quote = FALSE, sep = "\t", eol = "\n", na = "NA", dec = ".", 
              row.names = FALSE,  col.names = TRUE, qmethod = c("escape", "double"),  fileEncoding = "")
  write.table(getData(meth_2two)[,1:4] , 
              file = paste(path_temp2_sub1,   "3C_regions-tiles.txt",  sep="/"), 
              append = FALSE, quote = FALSE, sep = "\t", eol = "\n", na = "NA", dec = ".", 
              row.names = FALSE,  col.names = TRUE, qmethod = c("escape", "double"),  fileEncoding = "")
  
  
  
  meth_sub4_matrix_g <- getData(meth_2two)
  mySamples_firstRow = colnames( mat_2two )
  myCoverageMatrix = mat_2two 
  myMeLevelMatrix  = mat_2two 
  for(i  in  c(1: ncol(myCoverageMatrix)) )  {
    myCoverageMatrix[,i] = meth_sub4_matrix_g[,3*i+2]
    myMeLevelMatrix[,i]  = 100*meth_sub4_matrix_g[,3*i+3]/meth_sub4_matrix_g[,3*i+2] 
  }
  
  write.table(myCoverageMatrix , 
              file = paste(path_temp2_sub1,   "4A_totalCoverage_C+T.txt",  sep="/"), 
              append = FALSE, quote = FALSE, sep = "\t", eol = "\n", na = "NA", dec = ".", 
              row.names = FALSE,  col.names = TRUE, qmethod = c("escape", "double"),  fileEncoding = "")
  write.table(myMeLevelMatrix , 
              file = paste(path_temp2_sub1,   "4B_same-as-3B_mat-tiles.txt",  sep="/"), 
              append = FALSE, quote = FALSE, sep = "\t", eol = "\n", na = "NA", dec = ".", 
              row.names = FALSE,  col.names = TRUE, qmethod = c("escape", "double"),  fileEncoding = "")
  
  
  myCoverageMatrix_vector = as.vector( myCoverageMatrix ) 
  myMeLevelMatrix_vector  = as.vector( myMeLevelMatrix ) 
  
  
  pdf( file=paste(path_temp2_sub1, "5_Coverage_MeLevel_Cor_poolAllSamples.pdf", sep="/")  )
  par(mfrow=c(3,1))
  myTempBool3 =  (myCoverageMatrix_vector<=50) 
  myTempDataframe3 = data.frame(x3=myCoverageMatrix_vector[myTempBool3], y3=myMeLevelMatrix_vector[myTempBool3])
  boxplot(y3 ~ x3, data = myTempDataframe3  , xlab="Coverage", ylab="Methylation Level (%)", main="All Samples") 
  myTempFunction100 <- function() {
    myTempBool4 =  ( (myCoverageMatrix_vector>50)  & (myCoverageMatrix_vector<=100) )
    myTempDataframe4 = data.frame(x4=myCoverageMatrix_vector[myTempBool4], y4=myMeLevelMatrix_vector[myTempBool4] )
    boxplot(y4 ~ x4, data = myTempDataframe4  , xlab="Coverage", ylab="Methylation Level (%)", main="All Samples") 
    myTempBool5 =  ( (myCoverageMatrix_vector>100)  & (myCoverageMatrix_vector<=150) )
    myTempDataframe5 = data.frame(x5=myCoverageMatrix_vector[myTempBool5], y5=myMeLevelMatrix_vector[myTempBool5] )
    boxplot(y5 ~ x5, data = myTempDataframe5 , xlab="Coverage", ylab="Methylation Level (%)", main="All Samples" ) 
    myTempBool6 =  ( (myCoverageMatrix_vector>150)  & (myCoverageMatrix_vector<=200) )
    myTempDataframe6 = data.frame(x6=myCoverageMatrix_vector[myTempBool6], y6=myMeLevelMatrix_vector[myTempBool6] )
    boxplot(y6 ~ x6, data = myTempDataframe6 , xlab="Coverage", ylab="Methylation Level (%)", main="All Samples" ) 
    myTempBool6 =  ( (myCoverageMatrix_vector>200)  & (myCoverageMatrix_vector<=300) )
    myTempDataframe6 = data.frame(x6=myCoverageMatrix_vector[myTempBool6], y6=myMeLevelMatrix_vector[myTempBool6] )
    boxplot(y6 ~ x6, data = myTempDataframe6 , xlab="Coverage", ylab="Methylation Level (%)", main="All Samples" ) 
    myTempBool7 = (myCoverageMatrix_vector>300) 
    myTempDataframe7 = data.frame(x7=myCoverageMatrix_vector[myTempBool7], y7=myMeLevelMatrix_vector[myTempBool7] )
    boxplot(y7 ~ x7, data = myTempDataframe7 , xlab="Coverage", ylab="Methylation Level (%)", main="All Samples" ) 
  }
  tryCatch(
    myTempFunction100(),
    error = function(err){"Tiles_000111222"}
  )
  dev.off()
  
  
  
  sink( file=paste(path_temp2_sub1, "5A_Coverage.allSample.txt", sep="/")  )
  cat(   "min", "\t", "mean", "\t",  "median", "\t",  "max", "\t", "sample", "\n"  )    
  cat(   min(myCoverageMatrix_vector, na.rm = TRUE), "\t", mean(myCoverageMatrix_vector, na.rm = TRUE), "\t", 
         median(myCoverageMatrix_vector, na.rm = TRUE), "\t",  max(myCoverageMatrix_vector, na.rm = TRUE), "\t", "all", "\n"  )    
  sink()
  
  sink( file=paste(path_temp2_sub1, "5B_MeLevel.allSample.txt", sep="/")  )
  cat(   "min", "\t", "mean", "\t",  "median", "\t",  "max", "\t", "sample", "\n"  )    
  cat(   min(myMeLevelMatrix_vector, na.rm = TRUE), "\t", mean(myMeLevelMatrix_vector, na.rm = TRUE), "\t",  
         median(myMeLevelMatrix_vector, na.rm = TRUE), "\t",  max(myMeLevelMatrix_vector, na.rm = TRUE), "\t", "all", "\n"  )    
  sink()
  
  
  
  
  
  pdf( file=paste(path_temp2_sub1, "6A_Coverage.allSample.pdf", sep="/")  )
  print( hist(myCoverageMatrix_vector) )
  print( qplot(myCoverageMatrix_vector, binwidth=10)  )
  print( qplot(myCoverageMatrix_vector, binwidth=5) ) 
  print( qplot(myCoverageMatrix_vector, binwidth=1) )  
  print( qplot(myCoverageMatrix_vector, binwidth=10, xlim=c(0, 2000) )  )
  print( qplot(myCoverageMatrix_vector, binwidth=5,  xlim=c(0, 2000) ) ) 
  print( qplot(myCoverageMatrix_vector, binwidth=1,  xlim=c(0, 2000) ) )  
  print( qplot(myCoverageMatrix_vector, binwidth=5,  xlim=c(0, 1000) ) ) 
  print( qplot(myCoverageMatrix_vector, binwidth=5,  xlim=c(1000, 2000) ) )     
  dev.off() 
  
  pdf( file=paste(path_temp2_sub1, "6B_MeLevel.allSample.pdf", sep="/")  )
  print( hist(myMeLevelMatrix_vector) )
  print( qplot(myMeLevelMatrix_vector, binwidth=10)  )
  print( qplot(myMeLevelMatrix_vector, binwidth=5) ) 
  print( qplot(myMeLevelMatrix_vector, binwidth=1)  ) 
  dev.off() 
  
  pdf( file=paste(path_temp2_sub1, "7A_Coverage_eachSample.pdf", sep="/")  )
  ## par(mfrow=c(2,2))
  for(i  in  c(1: ncol(myCoverageMatrix)) )  {
    myTempVec1 = myCoverageMatrix[,i]
    myTitle1 = as.vector( dataFrame_temp2$mysampleID )[i]
    print( hist(myTempVec1, main=myTitle1) )
    print( qplot(myTempVec1, binwidth=10, main=myTitle1)  )
    print( qplot(myTempVec1, binwidth=5, main=myTitle1)  )
    print( qplot(myTempVec1, binwidth=1, main=myTitle1)  )
  }
  dev.off()
 
  pdf( file=paste(path_temp2_sub1, "7B_MeLevel_eachSample.pdf", sep="/")  )
  ## par(mfrow=c(2,2))
  for(i  in  c(1: ncol(myCoverageMatrix)) )  {
    myTempVec1 = myMeLevelMatrix[,i]
    myTitle1 = as.vector( dataFrame_temp2$mysampleID )[i]
    print( hist(myTempVec1, main=myTitle1) )
    print( qplot(myTempVec1, binwidth=10, main=myTitle1)  )
    print( qplot(myTempVec1, binwidth=5, main=myTitle1)  )
    print( qplot(myTempVec1, binwidth=1, main=myTitle1)  )
  }
  dev.off()
  
  
  myTempFunction101 <- function() {
    dev.off()
    dev.off()
    dev.off()
    dev.off()
    dev.off()
    dev.off()
    dev.off()
    dev.off()
    dev.off()
    dev.off()
    dev.off()
  }
  tryCatch(
    myTempFunction101(),
    error = function(err){"Tiles_000111333"}
  )
  

  myNumberSamples = length( as.vector(dataFrame_temp2$mysampleID) )   
  
  path_temp2_sub2 = paste(path_temp2, "2_HierarchicalClustering_byMethylKit", sep="/")
  if( ! file.exists(path_temp2_sub2) ) { dir.create(path_temp2_sub2, recursive = TRUE) }
  tryCatch(
      MyCluster_3_g(  mymeth2=meth_2two ,  path2=path_temp2_sub2,     file2="HierarchicalClustering_byMethylKit_",   width2 = myNumberSamples/3 + 2 ,   height2=5 ),
      error = function(err){"789000"}
  )
  
  ## MDS（multidimensional scaling）多维尺度分析
  ## Classical (Metric) Multidimensional Scaling
  path_temp2_sub3 = paste(path_temp2, "3_MultidimensionalScaling", sep="/")
  if( ! file.exists(path_temp2_sub3) ) { dir.create(path_temp2_sub3, recursive = TRUE) }
  MyMultidimensionalScaling_1_g(  meLevelMatrix2=mat_2two,   path2=path_temp2_sub3,   dataFrame_temp2=dataFrame_temp2  )
  
  path_temp2_sub4 = paste(path_temp2, "4_PCAinfor_byMethylKit", sep="/")
  if( ! file.exists(path_temp2_sub4) ) { dir.create(path_temp2_sub4, recursive = TRUE) }
  tryCatch(
      MyPCA_3A_g(  mymeth2=meth_2two ,  path2=path_temp2_sub4,     file2="PCAinfor_byMethylKit_",   width2=7,   height2=5,  dataFrame_temp2=dataFrame_temp2   ),
      error = function(err){"123000"}
  )
                                                                 
  path_temp2_sub5 = paste(path_temp2, "5_DMR", sep="/")
  if( ! file.exists(path_temp2_sub5) ) { dir.create(path_temp2_sub5, recursive = TRUE) }
  tryCatch(
        myDiff_DMC_DMR_g(  methobj2=meth_2two,   path2=path_temp2_sub5,    qvalue2=qvalue_temp2, differenceOfMethylation2=differenceOfMethylation_temp2,  mergeDistance2=mergeDistance_temp2,   numCores2=numCores_temp2  )  ,
        error = function(err){"5_DMR_000"}
  )
  


  meth_region_rmNA   =   meth_2two[ ! is.na( rowSums(mat_2two) ), ]
  
  path_temp2_sub7 = paste(path_temp2, "6_Correlation", sep="/")
  if( ! file.exists(path_temp2_sub7) ) { dir.create(path_temp2_sub7, recursive = TRUE) }
  
  sink( file=paste(path_temp2_sub7, "1A_pearsonCorrelation-regions_rmNA.txt", sep="/")  )
  cat( getCorrelation( meth_region_rmNA , method = "pearson",   plot=FALSE  ) )
  sink()
  
  sink( file=paste(path_temp2_sub7, "1B_spearmanCorrelation-regions_rmNA.txt", sep="/")  )
  getCorrelation(  meth_region_rmNA , method = "spearman",  plot=FALSE  )
  sink()
    

  myCorRaw_1 <- function() {
    sink( file=paste(path_temp2_sub7, "2A_pearsonCorrelation-regions_rmNA.txt", sep="/")  )
    cat( getCorrelation( meth_2two , method = "pearson",   plot=FALSE  ) )
    sink()
  
    sink( file=paste(path_temp2_sub7, "2B_spearmanCorrelation-regions_rmNA.txt", sep="/")  )
    getCorrelation(  meth_2two , method = "spearman",  plot=FALSE  )
    sink()
  }
  tryCatch(
        myCorRaw_1() ,
        error = function(err){"000_myCorRaw_1"}
  )

  

  path_temp2_sub7 = paste(path_temp2, "7_Correlation_Figures", sep="/") 
  if( ! file.exists(path_temp2_sub7) ) { dir.create(path_temp2_sub7, recursive = TRUE) }    
  myCorCov_matrix_1(matrix_temp10=mat_2two,   path_temp10=path_temp2_sub7, my_col1=colorRampPalette(  c( "blue", "skyblue", "black", "pink", "red") ) )
 
  path_temp2_sub8 = paste(path_temp2, "8_Correlation_Figures_anotherColor", sep="/") 
  if( ! file.exists(path_temp2_sub8) ) { dir.create(path_temp2_sub8, recursive = TRUE) }    
  myCorCov_matrix_1(matrix_temp10=mat_2two,   path_temp10=path_temp2_sub8, my_col1=colorRampPalette(  c( "cyan",  "black",    "red") ) )

  path_temp2_sub9 = paste(path_temp2, "9_ForEachClass", sep="/") 
  if( ! file.exists(path_temp2_sub9) ) { dir.create(path_temp2_sub9, recursive = TRUE) }    
  forEachType_f(path_temp=path_temp2_sub9, matrix_temp=mat_2two, dataFrame_temp=dataFrame_temp2)









  path_temp2_sub10 = paste(path_temp2, "10_Correct_NA", sep="/")
  if( ! file.exists(path_temp2_sub10) ) { dir.create(path_temp2_sub10, recursive = TRUE) }  


  ## estimate the NAs in each sample.
  mytech2 = dataFrame_temp2$mytech
  mytech2_level = levels(mytech2)
  matrix_region_g2 = mat_2two

  for(i in c(1:length(mytech2_level)) ) {
    col_index = which(mytech2==mytech2_level[i])
    for(j in c(1:nrow(mat_2two)) ) {
      myVector1 = mat_2two[j,col_index]
      means_non_NA_1 = mean(x=myVector1, na.rm = TRUE)
      myVector1[is.na(myVector1)] = means_non_NA_1
      matrix_region_g2[j,col_index] = myVector1
    }
  }
  dim(mat_2two) 
  dim(matrix_region_g2)
  head(mat_2two) 
  head(matrix_region_g2)
  write.table(mat_2two , 
              file = paste(path_temp2_sub10,   "regions_meLevel_withNA.txt",  sep="/"), 
              append = FALSE, quote = FALSE, sep = "\t", eol = "\n", na = "NA", dec = ".", 
              row.names = FALSE,  col.names = TRUE, qmethod = c("escape", "double"),  fileEncoding = "")
  write.table(matrix_region_g2 , 
              file = paste(path_temp2_sub10,   "regions_meLevel_Corrected.txt",  sep="/"), 
              append = FALSE, quote = FALSE, sep = "\t", eol = "\n", na = "NA", dec = ".", 
              row.names = FALSE,  col.names = TRUE, qmethod = c("escape", "double"),  fileEncoding = "")
  
  
  
  

  path_temp2_sub11 = paste(path_temp2, "11_HierarchicalClustering_Corrected", sep="/")
  if( ! file.exists(path_temp2_sub11) ) { dir.create(path_temp2_sub11, recursive = TRUE) }  
  tryCatch(
    myHierarchicalClustering_1_g(  mat_3three=matrix_region_g2,   path_temp1=path_temp2_sub11,  width1 = myNumberSamples/3 + 2, height1=5  ) ,
    error = function(err){"err_000"}
  )
  

  ## MDS（multidimensional scaling）多维尺度分析
  ## Classical (Metric) Multidimensional Scaling
  path_temp2_sub12 = paste(path_temp2, "12_MultidimensionalScaling_Corrected", sep="/")
  if( ! file.exists(path_temp2_sub12) ) { dir.create(path_temp2_sub12, recursive = TRUE) }
  MyMultidimensionalScaling_1_g(  meLevelMatrix2=matrix_region_g2,   path2=path_temp2_sub12,   dataFrame_temp2=dataFrame_temp2  )

 
  path_temp2_sub13 = paste(path_temp2, "13_Correlation_Corrected", sep="/") 
  if( ! file.exists(path_temp2_sub13) ) { dir.create(path_temp2_sub13, recursive = TRUE) }    
  myCorCov_matrix_1(matrix_temp10=matrix_region_g2,   path_temp10=path_temp2_sub13, my_col1=colorRampPalette(  c( "blue", "skyblue", "black", "pink", "red") )   )



  myTempFunction101 <- function() {
    dev.off()
    dev.off()
    dev.off()
    dev.off()
    dev.off()
    dev.off()
    dev.off()   
    dev.off()   
    dev.off()   
    dev.off()   
  }
  tryCatch(
    myTempFunction101(),
    error = function(err){"myMainFunction_speRegions_1_g_11133355555"}
  )



  myTempFunction102 <- function() {
    sink()
    sink()
    sink()
    sink()
    sink()
    sink()
    sink()   
    sink()   
    sink()   
    sink()   
  }
  tryCatch(
    myTempFunction102(),
    error = function(err){"myMainFunction_speRegions_1_g_111222333"}
  )

  
} 







 














myMainFunction_speRegions_All_g  <- function(   myobj_temp3,   path_temp3,   qvalue_temp3, differenceOfMethylation_temp3, mergeDistance_temp3,   binBases_temp3, dataFrame_temp3, numCores_temp3  )  {
  tryCatch(
  myMainFunction_speRegions_g(  myobj_temp2=myobj_temp3,   path_temp2=paste(path_temp3, "1A_mySpecificLoci_1A",  sep="/"), 
                                speRegions_temp2=mySpecificLoci_1A.obj_g$promoters,    qvalue_temp2=qvalue_temp3, 
                                differenceOfMethylation_temp2=differenceOfMethylation_temp3,   
                                mergeDistance_temp2=mergeDistance_temp3,   
                                binBases_temp2=binBases_temp3,   dataFrame_temp2=dataFrame_temp3,   
                                numCores_temp2=numCores_temp3 
                                
  ),
  error = function(err){"00000000000_1A_mySpecificLoci_1A"}
  )
  

  tryCatch(
  myMainFunction_speRegions_g(  myobj_temp2=myobj_temp3,   path_temp2=paste(path_temp3, "1B_mySpecificLoci_1B",  sep="/"), 
                                speRegions_temp2=mySpecificLoci_1B.obj_g$promoters,    qvalue_temp2=qvalue_temp3, 
                                differenceOfMethylation_temp2=differenceOfMethylation_temp3,   
                                mergeDistance_temp2=mergeDistance_temp3,   
                                binBases_temp2=binBases_temp3,   dataFrame_temp2=dataFrame_temp3,   
                                numCores_temp2=numCores_temp3 
                                
  ),
  error = function(err){"00000000000_1B_mySpecificLoci_1B"}
  )
  
 
   

  tryCatch(
  myMainFunction_speRegions_g(  myobj_temp2=myobj_temp3,   path_temp2=paste(path_temp3, "2_mySpecificLoci_2",  sep="/"), 
                                speRegions_temp2=mySpecificLoci_2.obj_g$targets,    qvalue_temp2=qvalue_temp3, 
                                differenceOfMethylation_temp2=differenceOfMethylation_temp3,   
                                mergeDistance_temp2=mergeDistance_temp3,   
                                binBases_temp2=binBases_temp3,   dataFrame_temp2=dataFrame_temp3,   
                                numCores_temp2=numCores_temp3 
                                
  ),
  error = function(err){"00000000000_2_mySpecificLoci_2"}
  )
  
 
 
 
   

  tryCatch(
  myMainFunction_speRegions_g(  myobj_temp2=myobj_temp3,   path_temp2=paste(path_temp3, "3_mySpecificLoci_3",  sep="/"), 
                                speRegions_temp2=mySpecificLoci_3.obj_g$targets,    qvalue_temp2=qvalue_temp3, 
                                differenceOfMethylation_temp2=differenceOfMethylation_temp3,   
                                mergeDistance_temp2=mergeDistance_temp3,   
                                binBases_temp2=binBases_temp3,   dataFrame_temp2=dataFrame_temp3,   
                                numCores_temp2=numCores_temp3 
                                
  ),
  error = function(err){"00000000000_3_mySpecificLoci_3"}
  )
  
 
 
 
   

  tryCatch(
  myMainFunction_speRegions_g(  myobj_temp2=myobj_temp3,   path_temp2=paste(path_temp3, "4_mySpecificLoci_4",  sep="/"), 
                                speRegions_temp2=mySpecificLoci_4.obj_g$targets,    qvalue_temp2=qvalue_temp3, 
                                differenceOfMethylation_temp2=differenceOfMethylation_temp3,   
                                mergeDistance_temp2=mergeDistance_temp3,   
                                binBases_temp2=binBases_temp3,   dataFrame_temp2=dataFrame_temp3,   
                                numCores_temp2=numCores_temp3 
                                
  ),
  error = function(err){"00000000000_4_mySpecificLoci_4"}
  )
  
 
 
 
   

  tryCatch(
  myMainFunction_speRegions_g(  myobj_temp2=myobj_temp3,   path_temp2=paste(path_temp3, "5_mySpecificLoci_5",  sep="/"), 
                                speRegions_temp2=mySpecificLoci_5.obj_g$targets,    qvalue_temp2=qvalue_temp3, 
                                differenceOfMethylation_temp2=differenceOfMethylation_temp3,   
                                mergeDistance_temp2=mergeDistance_temp3,   
                                binBases_temp2=binBases_temp3,   dataFrame_temp2=dataFrame_temp3,   
                                numCores_temp2=numCores_temp3 
                                
  ),
  error = function(err){"00000000000_5_mySpecificLoci_5"}
  )
  
 
 
 
   

  tryCatch(
  myMainFunction_speRegions_g(  myobj_temp2=myobj_temp3,   path_temp2=paste(path_temp3, "6_mySpecificLoci_6",  sep="/"), 
                                speRegions_temp2=mySpecificLoci_6.obj_g$targets,    qvalue_temp2=qvalue_temp3, 
                                differenceOfMethylation_temp2=differenceOfMethylation_temp3,   
                                mergeDistance_temp2=mergeDistance_temp3,   
                                binBases_temp2=binBases_temp3,   dataFrame_temp2=dataFrame_temp3,   
                                numCores_temp2=numCores_temp3 
                                
  ),
  error = function(err){"00000000000_6_mySpecificLoci_6"}
  )
  
 
 
 
   

  tryCatch(
  myMainFunction_speRegions_g(  myobj_temp2=myobj_temp3,   path_temp2=paste(path_temp3, "7_mySpecificLoci_7",  sep="/"), 
                                speRegions_temp2=mySpecificLoci_7.obj_g$targets,    qvalue_temp2=qvalue_temp3, 
                                differenceOfMethylation_temp2=differenceOfMethylation_temp3,   
                                mergeDistance_temp2=mergeDistance_temp3,   
                                binBases_temp2=binBases_temp3,   dataFrame_temp2=dataFrame_temp3,   
                                numCores_temp2=numCores_temp3 
                                
  ),
  error = function(err){"00000000000_7_mySpecificLoci_7"}
  )
  
 
 
   

  tryCatch(
  myMainFunction_speRegions_g(  myobj_temp2=myobj_temp3,   path_temp2=paste(path_temp3, "8_mySpecificLoci_8",  sep="/"), 
                                speRegions_temp2=mySpecificLoci_8.obj_g$targets,    qvalue_temp2=qvalue_temp3, 
                                differenceOfMethylation_temp2=differenceOfMethylation_temp3,   
                                mergeDistance_temp2=mergeDistance_temp3,   
                                binBases_temp2=binBases_temp3,   dataFrame_temp2=dataFrame_temp3,   
                                numCores_temp2=numCores_temp3 
                                
  ),
  error = function(err){"00000000000_8_mySpecificLoci_8"}
  )
  
 
 
 
   

  tryCatch(
  myMainFunction_speRegions_g(  myobj_temp2=myobj_temp3,   path_temp2=paste(path_temp3, "9_mySpecificLoci_9",  sep="/"), 
                                speRegions_temp2=mySpecificLoci_9.obj_g$targets,    qvalue_temp2=qvalue_temp3, 
                                differenceOfMethylation_temp2=differenceOfMethylation_temp3,   
                                mergeDistance_temp2=mergeDistance_temp3,   
                                binBases_temp2=binBases_temp3,   dataFrame_temp2=dataFrame_temp3,   
                                numCores_temp2=numCores_temp3 
                                
  ),
  error = function(err){"00000000000_9_mySpecificLoci_9"}
  )
  
 
 
 
   

  tryCatch(
  myMainFunction_speRegions_g(  myobj_temp2=myobj_temp3,   path_temp2=paste(path_temp3, "10_mySpecificLoci_10",  sep="/"), 
                                speRegions_temp2=mySpecificLoci_10.obj_g$targets,    qvalue_temp2=qvalue_temp3, 
                                differenceOfMethylation_temp2=differenceOfMethylation_temp3,   
                                mergeDistance_temp2=mergeDistance_temp3,   
                                binBases_temp2=binBases_temp3,   dataFrame_temp2=dataFrame_temp3,   
                                numCores_temp2=numCores_temp3 
                                
  ),
  error = function(err){"00000000000_10_mySpecificLoci_10"}
  )
  
 
 
 
   

  tryCatch(
  myMainFunction_speRegions_g(  myobj_temp2=myobj_temp3,   path_temp2=paste(path_temp3, "11_mySpecificLoci_11",  sep="/"), 
                                speRegions_temp2=mySpecificLoci_11.obj_g$targets,    qvalue_temp2=qvalue_temp3, 
                                differenceOfMethylation_temp2=differenceOfMethylation_temp3,   
                                mergeDistance_temp2=mergeDistance_temp3,   
                                binBases_temp2=binBases_temp3,   dataFrame_temp2=dataFrame_temp3,   
                                numCores_temp2=numCores_temp3 
                                
  ),
  error = function(err){"00000000000_11_mySpecificLoci_11"}
  )
  
 
 
 
   

  tryCatch(
  myMainFunction_speRegions_g(  myobj_temp2=myobj_temp3,   path_temp2=paste(path_temp3, "12_mySpecificLoci_12",  sep="/"), 
                                speRegions_temp2=mySpecificLoci_12.obj_g$targets,    qvalue_temp2=qvalue_temp3, 
                                differenceOfMethylation_temp2=differenceOfMethylation_temp3,   
                                mergeDistance_temp2=mergeDistance_temp3,   
                                binBases_temp2=binBases_temp3,   dataFrame_temp2=dataFrame_temp3,   
                                numCores_temp2=numCores_temp3 
                                
  ),
  error = function(err){"00000000000_12_mySpecificLoci_12"}
  )
  
 
 
 
   

  tryCatch(
  myMainFunction_speRegions_g(  myobj_temp2=myobj_temp3,   path_temp2=paste(path_temp3, "13_mySpecificLoci_13",  sep="/"), 
                                speRegions_temp2=mySpecificLoci_13.obj_g$targets,    qvalue_temp2=qvalue_temp3, 
                                differenceOfMethylation_temp2=differenceOfMethylation_temp3,   
                                mergeDistance_temp2=mergeDistance_temp3,   
                                binBases_temp2=binBases_temp3,   dataFrame_temp2=dataFrame_temp3,   
                                numCores_temp2=numCores_temp3 
                                
  ),
  error = function(err){"00000000000_13_mySpecificLoci_13"}
  )
  
 
 
 
   

  tryCatch(
  myMainFunction_speRegions_g(  myobj_temp2=myobj_temp3,   path_temp2=paste(path_temp3, "14_mySpecificLoci_14",  sep="/"), 
                                speRegions_temp2=mySpecificLoci_14.obj_g$targets,    qvalue_temp2=qvalue_temp3, 
                                differenceOfMethylation_temp2=differenceOfMethylation_temp3,   
                                mergeDistance_temp2=mergeDistance_temp3,   
                                binBases_temp2=binBases_temp3,   dataFrame_temp2=dataFrame_temp3,   
                                numCores_temp2=numCores_temp3 
                                
  ),
  error = function(err){"00000000000_14_mySpecificLoci_14"}
  )
  
 
 
 
   

  tryCatch(
  myMainFunction_speRegions_g(  myobj_temp2=myobj_temp3,   path_temp2=paste(path_temp3, "15_mySpecificLoci_15",  sep="/"), 
                                speRegions_temp2=mySpecificLoci_15.obj_g$targets,    qvalue_temp2=qvalue_temp3, 
                                differenceOfMethylation_temp2=differenceOfMethylation_temp3,   
                                mergeDistance_temp2=mergeDistance_temp3,   
                                binBases_temp2=binBases_temp3,   dataFrame_temp2=dataFrame_temp3,   
                                numCores_temp2=numCores_temp3 
                                
  ),
  error = function(err){"00000000000_15_mySpecificLoci_15"}
  )
  
 
 
 
   

  tryCatch(
  myMainFunction_speRegions_g(  myobj_temp2=myobj_temp3,   path_temp2=paste(path_temp3, "16_mySpecificLoci_16",  sep="/"), 
                                speRegions_temp2=mySpecificLoci_16.obj_g$targets,    qvalue_temp2=qvalue_temp3, 
                                differenceOfMethylation_temp2=differenceOfMethylation_temp3,   
                                mergeDistance_temp2=mergeDistance_temp3,   
                                binBases_temp2=binBases_temp3,   dataFrame_temp2=dataFrame_temp3,   
                                numCores_temp2=numCores_temp3 
                                
  ),
  error = function(err){"00000000000_16_mySpecificLoci_16"}
  )
  
 
 
 
   

  tryCatch(
  myMainFunction_speRegions_g(  myobj_temp2=myobj_temp3,   path_temp2=paste(path_temp3, "17_mySpecificLoci_17",  sep="/"), 
                                speRegions_temp2=mySpecificLoci_17.obj_g$targets,    qvalue_temp2=qvalue_temp3, 
                                differenceOfMethylation_temp2=differenceOfMethylation_temp3,   
                                mergeDistance_temp2=mergeDistance_temp3,   
                                binBases_temp2=binBases_temp3,   dataFrame_temp2=dataFrame_temp3,   
                                numCores_temp2=numCores_temp3 
                                
  ),
  error = function(err){"00000000000_17_mySpecificLoci_17"}
  )
  
 
 
 
   

  tryCatch(
  myMainFunction_speRegions_g(  myobj_temp2=myobj_temp3,   path_temp2=paste(path_temp3, "18_mySpecificLoci_18",  sep="/"), 
                                speRegions_temp2=mySpecificLoci_18.obj_g$targets,    qvalue_temp2=qvalue_temp3, 
                                differenceOfMethylation_temp2=differenceOfMethylation_temp3,   
                                mergeDistance_temp2=mergeDistance_temp3,   
                                binBases_temp2=binBases_temp3,   dataFrame_temp2=dataFrame_temp3,   
                                numCores_temp2=numCores_temp3 
                                
  ),
  error = function(err){"00000000000_18_mySpecificLoci_18"}
  )
  
 
 
 
   

  tryCatch(
  myMainFunction_speRegions_g(  myobj_temp2=myobj_temp3,   path_temp2=paste(path_temp3, "19_mySpecificLoci_19",  sep="/"), 
                                speRegions_temp2=mySpecificLoci_19.obj_g$targets,    qvalue_temp2=qvalue_temp3, 
                                differenceOfMethylation_temp2=differenceOfMethylation_temp3,   
                                mergeDistance_temp2=mergeDistance_temp3,   
                                binBases_temp2=binBases_temp3,   dataFrame_temp2=dataFrame_temp3,   
                                numCores_temp2=numCores_temp3 
                                
  ),
  error = function(err){"00000000000_19_mySpecificLoci_19"}
  )
  
 
 
 
   

  tryCatch(
  myMainFunction_speRegions_g(  myobj_temp2=myobj_temp3,   path_temp2=paste(path_temp3, "20_mySpecificLoci_20",  sep="/"), 
                                speRegions_temp2=mySpecificLoci_20.obj_g$targets,    qvalue_temp2=qvalue_temp3, 
                                differenceOfMethylation_temp2=differenceOfMethylation_temp3,   
                                mergeDistance_temp2=mergeDistance_temp3,   
                                binBases_temp2=binBases_temp3,   dataFrame_temp2=dataFrame_temp3,   
                                numCores_temp2=numCores_temp3 
                                
  ),
  error = function(err){"00000000000_20_mySpecificLoci_20"}
  )
  
 
 



                    
}























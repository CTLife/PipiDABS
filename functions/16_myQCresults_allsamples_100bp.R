
myQCresults_allsamples_100bp <- function(methylBase_temp1, path_temp1) {  
    myTempFunction <- function() {

  meth_2two = tileMethylCounts( methylBase_temp1,   win.size=100,   step.size=100,   cov.bases = 2  ) 
  mat_2two  = percMethylation( meth_2two )  # DNA methylation level
  if( ! file.exists(path_temp1) ) { dir.create(path_temp1, recursive = TRUE) }

  sink( file=paste(path_temp1,  "1_allDataSets.txt", sep="/")  )
  print( "length(meth_2two)" )
  print( length(meth_2two) )
  cat("\n\n\n")
  print( "getSampleID(meth_2two)" )
  print( getSampleID(meth_2two) )
  cat("\n\n\n")
  print( "getTreatment(meth_2two)" )
  print( getTreatment(meth_2two) )
  cat("\n\n\n")   
  sink()
  
  sink( file=paste(path_temp1 , "2_dimensions.txt", sep="/")  )
  print("#########dimensions:")
  print( dim(meth_2two)  )   
  print( dim(mat_2two)   )
  sink()
  
  write.table(meth_2two , 
              file = paste(path_temp1,   "3A_meth-tiles.txt",  sep="/"), 
              append = FALSE, quote = FALSE, sep = "\t", eol = "\n", na = "NA", dec = ".", 
              row.names = FALSE,  col.names = TRUE, qmethod = c("escape", "double"),  fileEncoding = "")
  write.table(mat_2two , 
              file = paste(path_temp1,   "3B_mat-tiles.txt",  sep="/"), 
              append = FALSE, quote = FALSE, sep = "\t", eol = "\n", na = "NA", dec = ".", 
              row.names = FALSE,  col.names = TRUE, qmethod = c("escape", "double"),  fileEncoding = "")
  write.table(getData(meth_2two)[,1:4] , 
              file = paste(path_temp1,   "3C_regions-tiles.txt",  sep="/"), 
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
              file = paste(path_temp1,   "4A_totalCoverage_C+T.txt",  sep="/"), 
              append = FALSE, quote = FALSE, sep = "\t", eol = "\n", na = "NA", dec = ".", 
              row.names = FALSE,  col.names = TRUE, qmethod = c("escape", "double"),  fileEncoding = "")
  write.table(myMeLevelMatrix , 
              file = paste(path_temp1,   "4B_same-as-3B_mat-tiles_bySelfCompute.txt",  sep="/"), 
              append = FALSE, quote = FALSE, sep = "\t", eol = "\n", na = "NA", dec = ".", 
              row.names = FALSE,  col.names = TRUE, qmethod = c("escape", "double"),  fileEncoding = "")
  
  
  myCoverageMatrix_vector = as.vector( myCoverageMatrix ) 
  myMeLevelMatrix_vector  = as.vector( myMeLevelMatrix ) 
  
  
  pdf( file=paste(path_temp1, "5_Coverage_MeLevel_Cor_poolAllSamples.pdf", sep="/")  )
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
    myTempBool7 = (myCoverageMatrix_vector>200) 
    myTempDataframe7 = data.frame(x7=myCoverageMatrix_vector[myTempBool7], y7=myMeLevelMatrix_vector[myTempBool7] )
    boxplot(y7 ~ x7, data = myTempDataframe7 , xlab="Coverage", ylab="Methylation Level (%)", main="All Samples" ) 
  }
  tryCatch(
    myTempFunction100(),
    error = function(err){"000111222"}
  )
  dev.off()
  
  
  
  sink( file=paste(path_temp1, "5A_Coverage.allSample.txt", sep="/")  )
  cat(   "min", "\t", "mean", "\t",  "median", "\t",  "max", "\t", "sample", "\n"  )    
  cat(   min(myCoverageMatrix_vector, na.rm = TRUE), "\t",     mean(myCoverageMatrix_vector, na.rm = TRUE), "\t", 
         median(myCoverageMatrix_vector, na.rm = TRUE), "\t",  max(myCoverageMatrix_vector, na.rm = TRUE), "\t", "all", "\n"  )    
  sink()
  
  sink( file=paste(path_temp1, "5B_MeLevel.allSample.txt", sep="/")  )
  cat(   "min", "\t", "mean", "\t",  "median", "\t",  "max", "\t", "sample", "\n"  )    
  cat(   min(myMeLevelMatrix_vector, na.rm = TRUE), "\t",     mean(myMeLevelMatrix_vector, na.rm = TRUE), "\t",  
         median(myMeLevelMatrix_vector, na.rm = TRUE), "\t",  max(myMeLevelMatrix_vector, na.rm = TRUE), "\t", "all", "\n"  )    
  sink()
  
  
  pdf( file=paste(path_temp1, "6A_Coverage.allSample.pdf", sep="/")  )
  print( hist(myCoverageMatrix_vector) )
  print( qplot(myCoverageMatrix_vector, binwidth=10)  )
  print( qplot(myCoverageMatrix_vector, binwidth=5) ) 
  print( qplot(myCoverageMatrix_vector, binwidth=1) )  
  print( qplot(myCoverageMatrix_vector, binwidth=10, xlim=c(0, 200) )  )
  print( qplot(myCoverageMatrix_vector, binwidth=5,  xlim=c(0, 200) ) ) 
  print( qplot(myCoverageMatrix_vector, binwidth=1,  xlim=c(0, 200) ) )  
  print( qplot(myCoverageMatrix_vector, binwidth=1,  xlim=c(0, 100) ) ) 
  print( qplot(myCoverageMatrix_vector, binwidth=1,  xlim=c(100, 200) ) ) 
  print( qplot(myCoverageMatrix_vector, binwidth=1,  xlim=c(200, 500) ) ) 
  dev.off() 
  
  pdf( file=paste(path_temp1, "6B_MeLevel.allSample.pdf", sep="/")  )
  print( hist(myMeLevelMatrix_vector) )
  print( qplot(myMeLevelMatrix_vector, binwidth=10)  )
  print( qplot(myMeLevelMatrix_vector, binwidth=5) ) 
  print( qplot(myMeLevelMatrix_vector, binwidth=1)  ) 
  dev.off() 
  



  dataFrame_temp2 <- data.frame(
      mysampleID  = c(SampleID_one_1_g,   SampleID_two_2_g,   SampleID_three_3_g,   SampleID_four_4_g,   SampleID_five_5_g,   SampleID_six_6_g,    SampleID_seven_7_g,    SampleID_eight_8_g,     SampleID_nine_9_g,    SampleID_ten_10_g),     
      mytreatment = c(Treatment_one_1_g,  Treatment_two_2_g,  Treatment_three_3_g,  Treatment_four_4_g,  Treatment_five_5_g,  Treatment_six_6_g,   Treatment_seven_7_g,   Treatment_eight_8_g,    Treatment_nine_9_g,   Treatment_ten_10_g  ),
      mysex       = c(Sex_one_1_g,        Sex_two_2_g,        Sex_three_3_g,         Sex_four_4_g,       Sex_five_5_g,        Sex_six_6_g,         Sex_seven_7_g,         Sex_eight_8_g,          Sex_nine_9_g,         Sex_ten_10_g  ),
      mytech      = c(Type_one_1_g,       Type_two_2_g,       Type_three_3_g,        Type_four_4_g,      Type_five_5_g,       Type_six_6_g,        Type_seven_7_g,        Type_eight_8_g,         Type_nine_9_g,        Type_ten_10_g  ), 
      myfiles     = c(Files_one_1_g,      Files_two_2_g,      Files_three_3_g,       Files_four_4_g,     Files_five_5_g,      Files_six_6_g,      Files_seven_7_g,        Files_eight_8_g,        Files_nine_9_g,       Files_ten_10_g  )
  )

  myNumberSamples = length( as.vector(dataFrame_temp2$mysampleID) )   
  
  path_temp2_sub2 = paste(path_temp1, "1_HierarchicalClustering_byMethylKit", sep="/")
  if( ! file.exists(path_temp2_sub2) ) { dir.create(path_temp2_sub2, recursive = TRUE) }
  tryCatch(
      MyCluster_3_g(  mymeth2=meth_2two ,  path2=path_temp2_sub2,     file2="HierarchicalClustering_byMethylKit_",   width2 = myNumberSamples/3 + 2 ,   height2=myNumberSamples/20 + 2 ),             
      error = function(err){"000"}
  )

  ## MDS（multidimensional scaling）多维尺度分析
  ## Classical (Metric) Multidimensional Scaling
  path_temp2_sub3 = paste(path_temp1, "2_MultidimensionalScaling", sep="/")
  if( ! file.exists(path_temp2_sub3) ) { dir.create(path_temp2_sub3, recursive = TRUE) }
  MyMultidimensionalScaling_1_g(  meLevelMatrix2=mat_2two,   path2=path_temp2_sub3,   dataFrame_temp2=dataFrame_temp2  )
  
  path_temp2_sub4 = paste(path_temp1, "3_PCAinfor_byMethylKit", sep="/")
  if( ! file.exists(path_temp2_sub4) ) { dir.create(path_temp2_sub4, recursive = TRUE) }
  tryCatch(
      MyPCA_3A_g(  mymeth2=meth_2two ,  path2=path_temp2_sub4,     file2="PCAinfor_byMethylKit_",   width2=7,   height2=5,  dataFrame_temp2=dataFrame_temp2   ),
      error = function(err){"MyPCA_3A_g_000"}
  )
 
  path_temp2_sub4 = paste(path_temp1, "4_Correlation", sep="/") 
  if( ! file.exists(path_temp2_sub4) ) { dir.create(path_temp2_sub4, recursive = TRUE) }    
  myCorCov_matrix_1(matrix_temp10=mat_2two,   path_temp10=path_temp2_sub4, my_col1=colorRampPalette(  c( "blue", "skyblue", "black", "pink", "red") ) )
 
  path_temp2_sub5 = paste(path_temp1, "5_Correlation_anotherColor", sep="/") 
  if( ! file.exists(path_temp2_sub5) ) { dir.create(path_temp2_sub5, recursive = TRUE) }    
  myCorCov_matrix_1(matrix_temp10=mat_2two,   path_temp10=path_temp2_sub5, my_col1=colorRampPalette(  c( "cyan",  "black",    "red") ) )
 
  path_temp2_sub6 = paste(path_temp1, "6_ForEachClass", sep="/") 
  if( ! file.exists(path_temp2_sub6) ) { dir.create(path_temp2_sub6, recursive = TRUE) }    
  forEachType_f(path_temp=path_temp2_sub6, matrix_temp=mat_2two, dataFrame_temp=dataFrame_temp2)


   
  }
  tryCatch(
    myTempFunction(),
    error = function(err){"mySelectDiffMe_9999999wwwwwwwwwwwwww999999"}
  ) 



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
    error = function(err){"myQCresults_allsamples_1bp_0001113338888888888888888999"}
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
    error = function(err){"myQCresults_allsamples_1bp_111222333888888888"}
  )


}  







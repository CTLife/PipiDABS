suppressPackageStartupMessages( library(Hmisc) )  





flattenCorrMatrix_1 <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}

reset_outliers <- function(x, na.rm = TRUE ) {
  qnt <- quantile(x, probs=c(0.05, 0.95) , type=1,  na.rm = na.rm )  
  y <- x
  y[x < qnt[1] ] <- qnt[1]
  y[x > qnt[2] ] <- qnt[2]    
  y
}

myScaleMatrix <- function( matrix_temp8, upper_temp8 = 1, lower_temp8 = -1 ) {
    rawMatrix_2 = reset_outliers(matrix_temp8)  
    rawMatrix_2 = lower_temp8 + (upper_temp8 - lower_temp8) * ( rawMatrix_2 - min(rawMatrix_2) )/( max(rawMatrix_2)- min(rawMatrix_2) )
    return(rawMatrix_2)
}





myCorCov_matrix_1 <- function(matrix_temp10, path_temp10, my_col1 ) {

  myTempFunction <- function() {

  if( ! file.exists(path_temp10) ) { dir.create(path_temp10, recursive = TRUE) }  
                           
  ## pearson covariance between column vectors.                                                                 
  path_temp10_sub1 = paste(path_temp10, "1_covariance_pearson", sep="/")
  if( ! file.exists(path_temp10_sub1) ) { dir.create(path_temp10_sub1, recursive = TRUE) }  
  covariance_raw         = cov(x=matrix_temp10,  use = "na.or.complete",  method =  "pearson" )
  covariance_scaled_temp = cov2cor( covariance_raw )
  covariance_scaled      = myScaleMatrix( matrix_temp8=covariance_scaled_temp )  
  write.table(x=covariance_raw, file = paste(path_temp10_sub1, "1_covariance_pearson_raw.txt", sep="/"), append = FALSE,
              quote = FALSE, sep = "\t",  eol = "\n",  na = "NA",  dec = ".",   row.names = TRUE,  col.names = TRUE )
  write.table(x=covariance_scaled_temp, file = paste(path_temp10_sub1, "2A_covariance_pearson_scaled_temp.txt", sep="/"), append = FALSE,
              quote = FALSE, sep = "\t",  eol = "\n",  na = "NA",  dec = ".",   row.names = TRUE,  col.names = TRUE )
  write.table(x=covariance_scaled, file = paste(path_temp10_sub1, "2B_covariance_pearson_scaled.txt", sep="/"), append = FALSE,
              quote = FALSE, sep = "\t",  eol = "\n",  na = "NA",  dec = ".",   row.names = TRUE,  col.names = TRUE )
  
  MyHeatmaps_1_f(matrix2=covariance_scaled,  path2=path_temp10_sub1,   fileName2="3_covariance_pearson_scaled.corrplot.pdf",          height2=30,   width2=30, is.corr2=TRUE,  my_col2=my_col1(10) )                             
  MyHeatmaps_2_f(matrix2=covariance_scaled,  path2=path_temp10_sub1,   fileName2="4_covariance_pearson_scaled.heatmap.pdf",           height2=30,   width2=30 )  
  MyHeatmaps_3_f(matrix2=covariance_scaled,  path2=path_temp10_sub1,   fileName2="5_covariance_pearson_scaled.ComplexHeatmap.pdf",    height2=30,   width2=30 )  
    
  ## spearman covariance between column vectors.                                                                 
  path_temp10_sub2 = paste(path_temp10, "2_covariance_spearman", sep="/")
  if( ! file.exists(path_temp10_sub2) ) { dir.create(path_temp10_sub2, recursive = TRUE) }  
  covariance_spearman_raw = cov(x=matrix_temp10,  use = "na.or.complete",  method = "spearman" )
  covariance_spearman_scaled_temp = cov2cor( covariance_spearman_raw )
  covariance_spearman_scaled = myScaleMatrix( matrix_temp8=covariance_spearman_scaled_temp )  
  write.table(x=covariance_spearman_raw, file = paste(path_temp10_sub2, "1_covariance_spearman_raw.txt", sep="/"), append = FALSE,
              quote = FALSE, sep = "\t",  eol = "\n",  na = "NA",  dec = ".",   row.names = TRUE,  col.names = TRUE )
  write.table(x=covariance_spearman_scaled_temp, file = paste(path_temp10_sub2, "2A_covariance_spearman_scaled_temp.txt", sep="/"), append = FALSE,
              quote = FALSE, sep = "\t",  eol = "\n",  na = "NA",  dec = ".",   row.names = TRUE,  col.names = TRUE )
  write.table(x=covariance_spearman_scaled, file = paste(path_temp10_sub2, "2B_covariance_spearman_scaled.txt", sep="/"), append = FALSE,
              quote = FALSE, sep = "\t",  eol = "\n",  na = "NA",  dec = ".",   row.names = TRUE,  col.names = TRUE )
   
  MyHeatmaps_1_f(matrix2=covariance_spearman_scaled,  path2=path_temp10_sub2,   fileName2="3_covariance_spearman_scaled.corrplot.pdf",          height2=30,   width2=30, is.corr2=TRUE,  my_col2=my_col1(10) )                             
  MyHeatmaps_2_f(matrix2=covariance_spearman_scaled,  path2=path_temp10_sub2,   fileName2="4_covariance_spearman_scaled.heatmap.pdf",           height2=30,   width2=30 )  
  MyHeatmaps_3_f(matrix2=covariance_spearman_scaled,  path2=path_temp10_sub2,   fileName2="5_covariance_spearman_scaled.ComplexHeatmap.pdf",    height2=30,   width2=30 )  
  

  ## pearson Correlation between column vectors.  
  path_temp10_sub3 = paste(path_temp10, "3_Correlation_pearson", sep="/")
  if( ! file.exists(path_temp10_sub3) ) { dir.create(path_temp10_sub3, recursive = TRUE) }  
  Correlation_pearson_temp   = cor(x=matrix_temp10,  use = "na.or.complete",  method = "pearson" )
  Correlation_pearson_scaled = myScaleMatrix( matrix_temp8=Correlation_pearson_temp )  
  write.table(x=Correlation_pearson_temp, file = paste(path_temp10_sub3, "1_Correlation_pearson_temp.txt", sep="/"), append = FALSE,
              quote = FALSE, sep = "\t",  eol = "\n",  na = "NA",  dec = ".",   row.names = TRUE,  col.names = TRUE )
  write.table(x=Correlation_pearson_scaled, file = paste(path_temp10_sub3, "2_Correlation_pearson_scaled.txt", sep="/"), append = FALSE,
              quote = FALSE, sep = "\t",  eol = "\n",  na = "NA",  dec = ".",   row.names = TRUE,  col.names = TRUE )

  MyHeatmaps_1_f(matrix2=Correlation_pearson_scaled,  path2=path_temp10_sub3,   fileName2="3_Correlation_pearson_scaled.corrplot.pdf",          height2=30,   width2=30, is.corr2=TRUE,  my_col2=my_col1(10) )                             
  MyHeatmaps_2_f(matrix2=Correlation_pearson_scaled,  path2=path_temp10_sub3,   fileName2="4_Correlation_pearson_scaled.heatmap.pdf",           height2=30,   width2=30 )  
  MyHeatmaps_3_f(matrix2=Correlation_pearson_scaled,  path2=path_temp10_sub3,   fileName2="5_Correlation_pearson_scaled.ComplexHeatmap.pdf",    height2=30,   width2=30 )  
  

  Correlation_pearson_rcorr <- Hmisc::rcorr(  as.matrix(matrix_temp10),  type="pearson"  )
  sink( paste(path_temp10_sub3, "11_Correlation_pearson_rcorr.txt", sep="/") ) 
      print(  Correlation_pearson_rcorr )
  sink()
  Correlation_pearson_rcorr_format = flattenCorrMatrix_1(Correlation_pearson_rcorr$r, Correlation_pearson_rcorr$P)
  write.table(x=Correlation_pearson_rcorr_format, file = paste(path_temp10_sub3, "12_Correlation_pearson_rcorr_format.txt", sep="/"), 
              append = FALSE,  quote = FALSE, sep = "\t",  eol = "\n",  na = "NA",  dec = ".",   row.names = FALSE,  col.names = TRUE )
  MyHeatmaps_1_f(matrix2=Correlation_pearson_rcorr$r,  path2=path_temp10_sub3,   fileName2="13_Correlation_pearson_scaled.corrplot.pdf",          height2=30,   width2=30, is.corr2=TRUE,  my_col2=my_col1(10) )                             
  MyHeatmaps_2_f(matrix2=Correlation_pearson_rcorr$r,  path2=path_temp10_sub3,   fileName2="14_Correlation_pearson_scaled.heatmap.pdf",           height2=30,   width2=30 )  
  MyHeatmaps_3_f(matrix2=Correlation_pearson_rcorr$r,  path2=path_temp10_sub3,   fileName2="15_Correlation_pearson_scaled.ComplexHeatmap.pdf",    height2=30,   width2=30 )  
 
 
  ## spearman Correlation between column vectors.  
  path_temp10_sub4 = paste(path_temp10, "4_Correlation_spearman", sep="/")
  if( ! file.exists(path_temp10_sub4) ) { dir.create(path_temp10_sub4, recursive = TRUE) }  
  Correlation_spearman_raw_temp = cor(x=matrix_temp10,  use = "na.or.complete",  method = "spearman" )
  Correlation_spearman_scaled   = myScaleMatrix( matrix_temp8=Correlation_spearman_raw_temp )  
  write.table(x=Correlation_spearman_raw_temp, file = paste(path_temp10_sub4, "1_Correlation_spearman_raw_temp.txt", sep="/"), append = FALSE,
              quote = FALSE, sep = "\t",  eol = "\n",  na = "NA",  dec = ".",   row.names = TRUE,  col.names = TRUE )
  write.table(x=Correlation_spearman_scaled, file = paste(path_temp10_sub4, "2_Correlation_spearman_scaled.txt", sep="/"), append = FALSE,
              quote = FALSE, sep = "\t",  eol = "\n",  na = "NA",  dec = ".",   row.names = TRUE,  col.names = TRUE )

  MyHeatmaps_1_f(matrix2=Correlation_spearman_scaled,  path2=path_temp10_sub4,   fileName2="3_Correlation_spearman_scaled.corrplot.pdf",          height2=30,   width2=30, is.corr2=TRUE,  my_col2=my_col1(10) )                             
  MyHeatmaps_2_f(matrix2=Correlation_spearman_scaled,  path2=path_temp10_sub4,   fileName2="4_Correlation_spearman_scaled.heatmap.pdf",           height2=30,   width2=30 )  
  MyHeatmaps_3_f(matrix2=Correlation_spearman_scaled,  path2=path_temp10_sub4,   fileName2="5_Correlation_spearman_scaled.ComplexHeatmap.pdf",    height2=30,   width2=30 )  
  

  Correlation_spearman_rcorr <- Hmisc::rcorr(  as.matrix(matrix_temp10),  type="spearman"  )
  sink( paste(path_temp10_sub4, "11_Correlation_spearman_rcorr.txt", sep="/") ) 
      print(  Correlation_spearman_rcorr )
  sink()
  Correlation_spearman_rcorr_format = flattenCorrMatrix_1(Correlation_spearman_rcorr$r, Correlation_spearman_rcorr$P)
  write.table(x=Correlation_spearman_rcorr_format, file = paste(path_temp10_sub4, "12_Correlation_spearman_rcorr_format.txt", sep="/"), 
              append = FALSE,  quote = FALSE, sep = "\t",  eol = "\n",  na = "NA",  dec = ".",   row.names = FALSE,  col.names = TRUE )
  MyHeatmaps_1_f(matrix2=Correlation_spearman_rcorr$r,  path2=path_temp10_sub4,   fileName2="13_Correlation_spearman_scaled.corrplot.pdf",          height2=30,   width2=30, is.corr2=TRUE,  my_col2=my_col1(10) )                             
  MyHeatmaps_2_f(matrix2=Correlation_spearman_rcorr$r,  path2=path_temp10_sub4,   fileName2="14_Correlation_spearman_scaled.heatmap.pdf",           height2=30,   width2=30 )  
  MyHeatmaps_3_f(matrix2=Correlation_spearman_rcorr$r,  path2=path_temp10_sub4,   fileName2="15_Correlation_spearman_scaled.ComplexHeatmap.pdf",    height2=30,   width2=30 )  
  
  }
  tryCatch(
    myTempFunction(),
    error = function(err){"myCorCov_matrix_1_9999985269"}
  ) 

}







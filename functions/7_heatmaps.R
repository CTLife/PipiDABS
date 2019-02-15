suppressPackageStartupMessages( library(corrplot) )
suppressPackageStartupMessages( library(ComplexHeatmap) )  





reset_outliers2 <- function(x, na.rm = TRUE ) {
  qnt <- quantile(x, probs=c(0.1, 0.9) , type=1,  na.rm = na.rm )  
  y <- x
  y[x < qnt[1] ] <- qnt[1]
  y[x > qnt[2] ] <- qnt[2]    
  y
}

myScaleMatrix2 <- function( matrix_temp8, upper_temp8 = 1, lower_temp8 = -1 ) {
    rawMatrix_2 = reset_outliers2(matrix_temp8)  
    rawMatrix_2 = lower_temp8 + (upper_temp8 - lower_temp8) * ( rawMatrix_2 - min(rawMatrix_2) )/( max(rawMatrix_2)- min(rawMatrix_2) )
    return(rawMatrix_2)
}

MyHeatmaps_1_f <- function(matrix2,  path2,   fileName2,   height2=30,   width2=30, is.corr2=TRUE,  my_col2 ) {                                                         
  matrix2 = myScaleMatrix2( matrix2 )  
  pdf( file = paste(path2, fileName2, sep="/"),  width=width2, height=height2  )
      print( corrplot::corrplot(matrix2, method = "circle", type = "full",  title = "", is.corr = is.corr2,  order = "hclust",  hclust.method = "ward.D2",  tl.col = "black", tl.srt = 45, col = my_col2 )  )   
      print( corrplot::corrplot(matrix2, method = "circle", type = "upper", title = "", is.corr = is.corr2,  order = "hclust",  hclust.method = "ward.D2",  tl.col = "black", tl.srt = 45, col = my_col2 )  )                                       
      print( corrplot::corrplot(matrix2, method = "number", type = "upper", title = "", is.corr = is.corr2,  order = "hclust",  hclust.method = "ward.D2",  tl.col = "black", tl.srt = 45, col = my_col2 )  )   
      print( corrplot::corrplot(matrix2, method = "pie",    type = "upper", title = "", is.corr = is.corr2,  order = "hclust",  hclust.method = "ward.D2",  tl.col = "black", tl.srt = 45, col = my_col2 )  )   
      print( corrplot::corrplot(matrix2, method = "color",  type = "upper", title = "", is.corr = is.corr2,  order = "hclust",  hclust.method = "ward.D2",  tl.col = "black", tl.srt = 45, col = my_col2 )  )   
      print( corrplot::corrplot(matrix2, method = "color",  type = "full",  title = "", is.corr = is.corr2,  order = "hclust",  hclust.method = "ward.D2",  tl.col = "black", tl.srt = 45, col = my_col2 )  )   
  dev.off()
}  





MyHeatmaps_1A_f <- function(matrix2,  path2,   fileName2,   height2=30,   width2=30, is.corr2=TRUE,  my_col2 ) {  
  myTempFunction <- function() {                                                           
  matrix2 = myScaleMatrix2( matrix2 )  
  pdf( file = paste(path2, fileName2, sep="/"),  width=width2, height=height2  )
      print( corrplot::corrplot(matrix2, method = "circle", type = "full",  title = "", is.corr = is.corr2,  order = "original",  tl.col = "black", tl.srt = 45, col = my_col2 )  )   
      print( corrplot::corrplot(matrix2, method = "circle", type = "upper", title = "", is.corr = is.corr2,  order = "original",  tl.col = "black", tl.srt = 45, col = my_col2 )  )                                       
      print( corrplot::corrplot(matrix2, method = "number", type = "upper", title = "", is.corr = is.corr2,  order = "original",  tl.col = "black", tl.srt = 45, col = my_col2 )  )   
      print( corrplot::corrplot(matrix2, method = "pie",    type = "upper", title = "", is.corr = is.corr2,  order = "original",  tl.col = "black", tl.srt = 45, col = my_col2 )  )   
      print( corrplot::corrplot(matrix2, method = "color",  type = "upper", title = "", is.corr = is.corr2,  order = "original",  tl.col = "black", tl.srt = 45, col = my_col2 )  )   
      print( corrplot::corrplot(matrix2, method = "color",  type = "full",  title = "", is.corr = is.corr2,  order = "original",  tl.col = "black", tl.srt = 45, col = my_col2 )  )   
  dev.off()   
  }
  tryCatch(
    myTempFunction(),
    error = function(err){"MyHeatmaps_1A_f_9999999999999"}
  )  
}  





MyHeatmaps_2_f <- function(matrix2,  path2,   fileName2,   height2=30,   width2=30 ) { 
  matrix2 = myScaleMatrix2( matrix2 )  
  pdf( file = paste(path2, fileName2, sep="/"),  width=width2, height=height2  )
      print( heatmap(x = matrix2,  col = colorRampPalette(c("blue", "white", "red"))(20),       symm = FALSE,  scale =  "none"     )  )
      print( heatmap(x = matrix2,  col = colorRampPalette(c("green4", "white", "purple"))(20),  symm = FALSE,  scale =  "none"     )  )
      print( heatmap(x = matrix2,  col = colorRampPalette(c("cyan", "white", "red"))(20),       symm = FALSE,  scale =  "none"     )  )
      print( heatmap(x = matrix2,  col = colorRampPalette(c("cyan", "white", "purple"))(20),    symm = FALSE,  scale =  "none"     )  )
      print( heatmap(x = matrix2,  col = colorRampPalette(c("blue", "white", "purple"))(20),    symm = FALSE,  scale =  "none"     )  )
  dev.off()
}  





MyHeatmaps_3_f <- function(matrix2,  path2,   fileName2,   height2=30,   width2=30 ) { 
  matrix2 = myScaleMatrix2( matrix2 )  
  pdf( file = paste(path2, fileName2, sep="/"),  width=width2, height=height2  )
      print( ComplexHeatmap::Heatmap(matrix2,  col = colorRampPalette(c("blue", "white", "red"))(20)        , heatmap_legend_param = list(legend_height = unit(10, "cm")) )  )          
      print( ComplexHeatmap::Heatmap(matrix2,  col = colorRampPalette(c("blue", "white", "red"))(20)        , heatmap_legend_param = list(legend_height = unit(10, "cm")) )  )
      print( ComplexHeatmap::Heatmap(matrix2,  col = colorRampPalette(c("green4", "white", "purple"))(20)   , heatmap_legend_param = list(legend_height = unit(10, "cm")) )  )
      print( ComplexHeatmap::Heatmap(matrix2,  col = colorRampPalette(c("green4", "white", "purple"))(20)   , heatmap_legend_param = list(legend_height = unit(10, "cm")) )  )
      print( ComplexHeatmap::Heatmap(matrix2,  col = colorRampPalette(c("cyan", "white", "red"))(20)        , heatmap_legend_param = list(legend_height = unit(10, "cm")) )  )
      print( ComplexHeatmap::Heatmap(matrix2,  col = colorRampPalette(c("cyan", "white", "red"))(20)        , heatmap_legend_param = list(legend_height = unit(10, "cm")) )  )
      print( ComplexHeatmap::Heatmap(matrix2,  col = colorRampPalette(c("cyan", "white", "purple"))(20)     , heatmap_legend_param = list(legend_height = unit(10, "cm")) )  )
      print( ComplexHeatmap::Heatmap(matrix2,  col = colorRampPalette(c("cyan", "white", "purple"))(20)     , heatmap_legend_param = list(legend_height = unit(10, "cm")) )  )
      print( ComplexHeatmap::Heatmap(matrix2,  col = colorRampPalette(c("blue", "white", "purple"))(20)     , heatmap_legend_param = list(legend_height = unit(10, "cm")) )  )
      print( ComplexHeatmap::Heatmap(matrix2,  col = colorRampPalette(c("blue", "white", "purple"))(20)     , heatmap_legend_param = list(legend_height = unit(10, "cm")) )  )
  dev.off()
}  






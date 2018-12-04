




descriptiveStatistics_f <- function( vector_temp ) {
  myTempFunction <- function() {

      vector_temp = vector_temp[!is.na(vector_temp)]
      quartiles_temp  = quantile(x=vector_temp, probs=c(0.00, 0.25, 0.50, 0.75, 1.00) , type=1,  na.rm = TRUE )  
      quartile0_temp  = quartiles_temp[1]
      quartile25_temp = quartiles_temp[2]
      quartile50_temp = quartiles_temp[3]
      quartile75_temp = quartiles_temp[4]
      quartile100_temp= quartiles_temp[5]
      InterquartileRange_temp = quartile75_temp - quartile25_temp  
      range_temp   = quartile100_temp - quartile0_temp
      trimean_temp = quartile25_temp/4 + quartile50_temp/2 + quartile75_temp/4 

      variance_temp          = var(vector_temp)
      standardDeviation_temp = sd(vector_temp) 

      Harmonic_mean_temp   = psych::harmonic.mean(vector_temp,  na.rm=TRUE, zero=FALSE) ## return the harmonic mean of the non-zero elements.                
      Geometric_mean_temp  = psych::geometric.mean(vector_temp[vector_temp>0])   
      Arithmetic_mean_temp = mean(vector_temp)
      Quadratic_mean_temp  = power_mean(values=vector_temp, order = 2)
      Trim_Arithmetic_mean_temp  = mean(vector_temp, trim = 0.01)

      coefficientOfVariation_temp = standardDeviation_temp/Arithmetic_mean_temp  
      
      return( list(quartile0  = quartile0_temp,  quartile25  = quartile25_temp,  quartile50 = quartile50_temp, 
                   quartile75 = quartile75_temp, quartile100 = quartile100_temp, InterquartileRange = InterquartileRange_temp, 
                   range      = range_temp,      trimean     = trimean_temp,     variance = variance_temp, 
                   standardDeviation = standardDeviation_temp, Harmonic_mean = Harmonic_mean_temp,  
                   Geometric_mean =Geometric_mean_temp,        Arithmetic_mean =Arithmetic_mean_temp, 
                   Quadratic_mean =Quadratic_mean_temp,        Trim_Arithmetic_mean =Trim_Arithmetic_mean_temp,
                   coefficientOfVariation = coefficientOfVariation_temp     
              ) )

 
  }
  tryCatch(
    myTempFunction(),
    error = function(err){"descriptiveStatistics_f_9999999999999"}
  ) 

}




 








forEachType_f <- function(path_temp, matrix_temp, dataFrame_temp) {
    if( ! file.exists(path_temp) ) { dir.create(path_temp, recursive = TRUE) }
    mytech2 = dataFrame_temp$mytech
    mytech2_level = levels(mytech2)

    sink( paste(path_temp, "values_of_parameters.txt", sep="/") )
        print( "path_temp:" )
        print( path_temp )
        cat("\n\n\n")
        print( "colnames(matrix_temp):" )
        print( colnames(matrix_temp) )
        cat("\n\n\n")
        print( "dataFrame_temp:" )    
        print( dataFrame_temp )  
        cat("\n\n\n")  
        print( "dataFrame_temp$mytech:" )    
        print( mytech2 )  
        cat("\n\n\n")  
        print( "mytech2_level:" )    
        print( mytech2_level )  
        cat("\n\n\n")
    sink() 
         
    quartile0_matrix2   = matrix( nrow = nrow(matrix_temp), ncol = length(mytech2_level) ) 
    quartile25_matrix2  = matrix( nrow = nrow(matrix_temp), ncol = length(mytech2_level) ) 
    quartile50_matrix2  = matrix( nrow = nrow(matrix_temp), ncol = length(mytech2_level) ) 
    quartile75_matrix2  = matrix( nrow = nrow(matrix_temp), ncol = length(mytech2_level) ) 
    quartile100_matrix2 = matrix( nrow = nrow(matrix_temp), ncol = length(mytech2_level) ) 
    InterquartileRange_matrix2 = matrix( nrow = nrow(matrix_temp), ncol = length(mytech2_level) ) 
    range_matrix2    = matrix( nrow = nrow(matrix_temp), ncol = length(mytech2_level) ) 
    trimean_matrix2  = matrix( nrow = nrow(matrix_temp), ncol = length(mytech2_level) ) 
    variance_matrix2 = matrix( nrow = nrow(matrix_temp), ncol = length(mytech2_level) ) 
    standardDeviation_matrix2 = matrix( nrow = nrow(matrix_temp), ncol = length(mytech2_level) ) 
    Harmonic_mean_matrix2     = matrix( nrow = nrow(matrix_temp), ncol = length(mytech2_level) ) 
    Geometric_mean_matrix2    = matrix( nrow = nrow(matrix_temp), ncol = length(mytech2_level) ) 
    Arithmetic_mean_matrix2   = matrix( nrow = nrow(matrix_temp), ncol = length(mytech2_level) ) 
    Quadratic_mean_matrix2    = matrix( nrow = nrow(matrix_temp), ncol = length(mytech2_level) ) 
    Trim_Arithmetic_mean_matrix2   = matrix( nrow = nrow(matrix_temp), ncol = length(mytech2_level) ) 
    coefficientOfVariation_matrix2 = matrix( nrow = nrow(matrix_temp), ncol = length(mytech2_level) ) 
         
    colnames( quartile0_matrix2 )   = mytech2_level 
    colnames( quartile25_matrix2 )  = mytech2_level 
    colnames( quartile50_matrix2 )  = mytech2_level 
    colnames( quartile75_matrix2 )  = mytech2_level 
    colnames( quartile100_matrix2 ) = mytech2_level
    colnames( InterquartileRange_matrix2 ) = mytech2_level 
    colnames( range_matrix2 )    = mytech2_level 
    colnames( trimean_matrix2 )  = mytech2_level 
    colnames( variance_matrix2 ) = mytech2_level
    colnames( standardDeviation_matrix2 ) = mytech2_level 
    colnames( Harmonic_mean_matrix2 )     = mytech2_level 
    colnames( Geometric_mean_matrix2 )    = mytech2_level
    colnames( Arithmetic_mean_matrix2 )   = mytech2_level 
    colnames( Quadratic_mean_matrix2 )    = mytech2_level
    colnames( Trim_Arithmetic_mean_matrix2 )   = mytech2_level 
    colnames( coefficientOfVariation_matrix2 ) = mytech2_level 


    ################# descriptive statistics for raw matrix
    path_temp_sub1 = paste(path_temp, "1_descriptiveStatistics", sep="/")
    if( ! file.exists(path_temp_sub1) ) { dir.create(path_temp_sub1, recursive = TRUE) }

    for(i in c(1:length(mytech2_level)) ) {
        col_index = which(mytech2==mytech2_level[i]) 
        for(j in c(1:nrow(matrix_temp))) {
            my16stat_temp       = descriptiveStatistics_f( vector_temp=matrix_temp[j, col_index])
            quartile0_matrix2[j,i]   = my16stat_temp$quartile0
            quartile25_matrix2[j,i]  = my16stat_temp$quartile25 
            quartile50_matrix2[j,i]  = my16stat_temp$quartile50
            quartile75_matrix2[j,i]  = my16stat_temp$quartile75 
            quartile100_matrix2[j,i] = my16stat_temp$quartile100
            InterquartileRange_matrix2[j,i] = my16stat_temp$InterquartileRange 
            range_matrix2[j,i]    = my16stat_temp$range  
            trimean_matrix2[j,i]  = my16stat_temp$trimean  
            variance_matrix2[j,i] = my16stat_temp$variance 
            standardDeviation_matrix2[j,i] = my16stat_temp$standardDeviation  
            Harmonic_mean_matrix2[j,i]     = my16stat_temp$Harmonic_mean 
            Geometric_mean_matrix2[j,i]    = my16stat_temp$Geometric_mean 
            Arithmetic_mean_matrix2[j,i]   = my16stat_temp$Arithmetic_mean 
            Quadratic_mean_matrix2[j,i]    = my16stat_temp$Quadratic_mean 
            Trim_Arithmetic_mean_matrix2[j,i]   = my16stat_temp$Trim_Arithmetic_mean 
            coefficientOfVariation_matrix2[j,i] = my16stat_temp$coefficientOfVariation 
        }
    }


   

   sink( paste(path_temp_sub1, "dimensions_16stats.txt", sep="/") )
       cat("matrix_temp:",         dim(matrix_temp), "\n")
       cat("quartile0_matrix2:",   dim(quartile0_matrix2), "\n")
       cat("quartile25_matrix2:",  dim(quartile25_matrix2), "\n")
       cat("quartile50_matrix2:",  dim(quartile50_matrix2), "\n")
       cat("quartile75_matrix2:",  dim(quartile75_matrix2), "\n")
       cat("quartile100_matrix2:", dim(quartile100_matrix2), "\n")
       cat("InterquartileRange_matrix2:", dim(InterquartileRange_matrix2), "\n")
       cat("range_matrix2:",    dim(range_matrix2), "\n")
       cat("trimean_matrix2:",  dim(trimean_matrix2), "\n")
       cat("variance_matrix2:", dim(variance_matrix2), "\n")
       cat("standardDeviation_matrix2:", dim(standardDeviation_matrix2), "\n")
       cat("Harmonic_mean_matrix2:",     dim(Harmonic_mean_matrix2), "\n")
       cat("Geometric_mean_matrix2:",    dim(Geometric_mean_matrix2), "\n")
       cat("Arithmetic_mean_matrix2:",   dim(Arithmetic_mean_matrix2), "\n")
       cat("Quadratic_mean_matrix2:",    dim(Quadratic_mean_matrix2), "\n")
       cat("Trim_Arithmetic_mean_matrix2:",   dim(Trim_Arithmetic_mean_matrix2), "\n")
       cat("coefficientOfVariation_matrix2:", dim(coefficientOfVariation_matrix2), "\n")
    sink()  








    write.table(quartile50_matrix2 , 
              file = paste(path_temp_sub1,   "3A_quartile50_matrix.txt",  sep="/"), 
              append = FALSE, quote = FALSE, sep = "\t", eol = "\n", na = "NA", dec = ".", 
              row.names = FALSE,  col.names = TRUE, qmethod = c("escape", "double"),  fileEncoding = "")  
    MyBoxViolinPlot_1_f(vector2 = as.vector(quartile50_matrix2) ,   
                    sampleType2 = rep(x= mytech2_level ,  each=nrow(matrix_temp)  )   ,  
                    colours2    = MyTech_color_g(  mytech2_level  ),   
                    path2       = path_temp_sub1,   fileName2 = "3B_quartile50_matrix",  
                    title2="quartile50",  xLab2="Samples",  yLab2="quartile50 of methylation level (%)",    
                    height2=4,   width2= length(mytech2_level)/2+2,   Ymin2=0, Ymax2=100)
    write.table( cbind(as.vector(quartile50_matrix2), rep(x= mytech2_level ,  each=nrow(matrix_temp)  ) ) , 
              file = paste(path_temp_sub1,   "3C_quartile50_matrix.txt",  sep="/"), 
              append = FALSE, quote = FALSE, sep = "\t", eol = "\n", na = "NA", dec = ".", 
              row.names = FALSE,  col.names = TRUE, qmethod = c("escape", "double"),  fileEncoding = "") 




    write.table(InterquartileRange_matrix2 , 
              file = paste(path_temp_sub1,   "6A_InterquartileRange_matrix.txt",  sep="/"), 
              append = FALSE, quote = FALSE, sep = "\t", eol = "\n", na = "NA", dec = ".", 
              row.names = FALSE,  col.names = TRUE, qmethod = c("escape", "double"),  fileEncoding = "")  
    MyBoxViolinPlot_1_f(vector2 = as.vector(InterquartileRange_matrix2) ,   
                    sampleType2 = rep(x= mytech2_level ,  each=nrow(matrix_temp)  )   ,  
                    colours2    = MyTech_color_g(  mytech2_level  ),   
                    path2       = path_temp_sub1,   fileName2 = "6B_InterquartileRange_matrix",  
                    title2="InterquartileRange",  xLab2="Samples",  yLab2="InterquartileRange of methylation level (%)",    
                    height2=4,   width2= length(mytech2_level)/2+2,   Ymin2=0, Ymax2=20)
    MyBoxViolinPlot_1_f(vector2 = as.vector(InterquartileRange_matrix2) ,   
                    sampleType2 = rep(x= mytech2_level ,  each=nrow(matrix_temp)  )   ,  
                    colours2    = MyTech_color_g(  mytech2_level  ),   
                    path2       = path_temp_sub1,   fileName2 = "6B_InterquartileRange_matrix_max10",  
                    title2="InterquartileRange",  xLab2="Samples",  yLab2="InterquartileRange of methylation level (%)",    
                    height2=4,   width2= length(mytech2_level)/2+2,   Ymin2=0, Ymax2=10)
    write.table( cbind(as.vector(InterquartileRange_matrix2), rep(x= mytech2_level ,  each=nrow(matrix_temp)  ) ) , 
              file = paste(path_temp_sub1,   "6C_InterquartileRange_matrix.txt",  sep="/"), 
              append = FALSE, quote = FALSE, sep = "\t", eol = "\n", na = "NA", dec = ".", 
              row.names = FALSE,  col.names = TRUE, qmethod = c("escape", "double"),  fileEncoding = "") 





    write.table(range_matrix2 , 
              file = paste(path_temp_sub1,   "7A_range_matrix.txt",  sep="/"), 
              append = FALSE, quote = FALSE, sep = "\t", eol = "\n", na = "NA", dec = ".", 
              row.names = FALSE,  col.names = TRUE, qmethod = c("escape", "double"),  fileEncoding = "")  
    MyBoxViolinPlot_1_f(vector2 = as.vector(range_matrix2) ,   
                    sampleType2 = rep(x= mytech2_level ,  each=nrow(matrix_temp)  )   ,  
                    colours2    = MyTech_color_g(  mytech2_level  ),   
                    path2       = path_temp_sub1,   fileName2 = "7B_range_matrix",  
                    title2="range",  xLab2="Samples",  yLab2="range of methylation level (%)",    
                    height2=4,   width2= length(mytech2_level)/2+2,   Ymin2=0, Ymax2=50)
    MyBoxViolinPlot_1_f(vector2 = as.vector(range_matrix2) ,   
                    sampleType2 = rep(x= mytech2_level ,  each=nrow(matrix_temp)  )   ,  
                    colours2    = MyTech_color_g(  mytech2_level  ),   
                    path2       = path_temp_sub1,   fileName2 = "7B_range_matrix_max25",  
                    title2="range",  xLab2="Samples",  yLab2="range of methylation level (%)",    
                    height2=4,   width2= length(mytech2_level)/2+2,   Ymin2=0, Ymax2=25)
    write.table( cbind(as.vector(range_matrix2), rep(x= mytech2_level ,  each=nrow(matrix_temp)  ) ) , 
              file = paste(path_temp_sub1,   "7C_range_matrix.txt",  sep="/"), 
              append = FALSE, quote = FALSE, sep = "\t", eol = "\n", na = "NA", dec = ".", 
              row.names = FALSE,  col.names = TRUE, qmethod = c("escape", "double"),  fileEncoding = "") 



    write.table(trimean_matrix2 , 
              file = paste(path_temp_sub1,   "8A_trimean_matrix.txt",  sep="/"), 
              append = FALSE, quote = FALSE, sep = "\t", eol = "\n", na = "NA", dec = ".", 
              row.names = FALSE,  col.names = TRUE, qmethod = c("escape", "double"),  fileEncoding = "")  
    MyBoxViolinPlot_1_f(vector2 = as.vector(trimean_matrix2) ,   
                    sampleType2 = rep(x= mytech2_level ,  each=nrow(matrix_temp)  )   ,  
                    colours2    = MyTech_color_g(  mytech2_level  ),   
                    path2       = path_temp_sub1,   fileName2 = "8B_trimean_matrix",  
                    title2="trimean",  xLab2="Samples",  yLab2="trimean of methylation level (%)",    
                    height2=4,   width2= length(mytech2_level)/2+2,   Ymin2=0, Ymax2=100)
    write.table( cbind(as.vector(trimean_matrix2), rep(x= mytech2_level ,  each=nrow(matrix_temp)  ) ) , 
              file = paste(path_temp_sub1,   "8C_trimean_matrix.txt",  sep="/"), 
              append = FALSE, quote = FALSE, sep = "\t", eol = "\n", na = "NA", dec = ".", 
              row.names = FALSE,  col.names = TRUE, qmethod = c("escape", "double"),  fileEncoding = "") 




    write.table(variance_matrix2 , 
              file = paste(path_temp_sub1,   "9A_variance_matrix.txt",  sep="/"), 
              append = FALSE, quote = FALSE, sep = "\t", eol = "\n", na = "NA", dec = ".", 
              row.names = FALSE,  col.names = TRUE, qmethod = c("escape", "double"),  fileEncoding = "")  
    MyBoxViolinPlot_1_f(vector2 = as.vector(variance_matrix2) ,   
                    sampleType2 = rep(x= mytech2_level ,  each=nrow(matrix_temp)  )   ,  
                    colours2    = MyTech_color_g(  mytech2_level  ),   
                    path2       = path_temp_sub1,   fileName2 = "9B_variance_matrix",  
                    title2="variance",  xLab2="Samples",  yLab2="variance of methylation level (%)",    
                    height2=4,   width2= length(mytech2_level)/2+2,   Ymin2=0, Ymax2=50)
    MyBoxViolinPlot_1_f(vector2 = as.vector(variance_matrix2) ,   
                    sampleType2 = rep(x= mytech2_level ,  each=nrow(matrix_temp)  )   ,  
                    colours2    = MyTech_color_g(  mytech2_level  ),   
                    path2       = path_temp_sub1,   fileName2 = "9B_variance_matrix_max25",  
                    title2="variance",  xLab2="Samples",  yLab2="variance of methylation level (%)",    
                    height2=4,   width2= length(mytech2_level)/2+2,   Ymin2=0, Ymax2=25)
    write.table( cbind(as.vector(variance_matrix2), rep(x= mytech2_level ,  each=nrow(matrix_temp)  ) ) , 
              file = paste(path_temp_sub1,   "9C_variance_matrix.txt",  sep="/"), 
              append = FALSE, quote = FALSE, sep = "\t", eol = "\n", na = "NA", dec = ".", 
              row.names = FALSE,  col.names = TRUE, qmethod = c("escape", "double"),  fileEncoding = "") 



    write.table(standardDeviation_matrix2 , 
              file = paste(path_temp_sub1,   "10A_standardDeviation_matrix.txt",  sep="/"), 
              append = FALSE, quote = FALSE, sep = "\t", eol = "\n", na = "NA", dec = ".", 
              row.names = FALSE,  col.names = TRUE, qmethod = c("escape", "double"),  fileEncoding = "")  
    MyBoxViolinPlot_1_f(vector2 = as.vector(standardDeviation_matrix2) ,   
                    sampleType2 = rep(x= mytech2_level ,  each=nrow(matrix_temp)  )   ,  
                    colours2    = MyTech_color_g(  mytech2_level  ),   
                    path2       = path_temp_sub1,   fileName2 = "10B_standardDeviation_matrix",  
                    title2="standardDeviation",  xLab2="Samples",  yLab2="standardDeviation of methylation level (%)",    
                    height2=4,   width2= length(mytech2_level)/2+2,   Ymin2=0, Ymax2=20)
    MyBoxViolinPlot_1_f(vector2 = as.vector(standardDeviation_matrix2) ,   
                    sampleType2 = rep(x= mytech2_level ,  each=nrow(matrix_temp)  )   ,  
                    colours2    = MyTech_color_g(  mytech2_level  ),   
                    path2       = path_temp_sub1,   fileName2 = "10B_standardDeviation_matrix_max8",  
                    title2="standardDeviation",  xLab2="Samples",  yLab2="standardDeviation of methylation level (%)",    
                    height2=4,   width2= length(mytech2_level)/2+2,   Ymin2=0, Ymax2=8)
    write.table( cbind(as.vector(standardDeviation_matrix2), rep(x= mytech2_level ,  each=nrow(matrix_temp)  ) ) , 
              file = paste(path_temp_sub1,   "10C_standardDeviation_matrix.txt",  sep="/"), 
              append = FALSE, quote = FALSE, sep = "\t", eol = "\n", na = "NA", dec = ".", 
              row.names = FALSE,  col.names = TRUE, qmethod = c("escape", "double"),  fileEncoding = "") 




    write.table(Arithmetic_mean_matrix2 , 
              file = paste(path_temp_sub1,   "13A_Arithmetic_mean_matrix.txt",  sep="/"), 
              append = FALSE, quote = FALSE, sep = "\t", eol = "\n", na = "NA", dec = ".", 
              row.names = FALSE,  col.names = TRUE, qmethod = c("escape", "double"),  fileEncoding = "")  
    MyBoxViolinPlot_1_f(vector2 = as.vector(Arithmetic_mean_matrix2) ,   
                    sampleType2 = rep(x= mytech2_level ,  each=nrow(matrix_temp)  )   ,  
                    colours2    = MyTech_color_g(  mytech2_level  ),   
                    path2       = path_temp_sub1,   fileName2 = "13B_Arithmetic_mean_matrix",  
                    title2="Arithmetic_mean",  xLab2="Samples",  yLab2="Arithmetic_mean of methylation level (%)",    
                    height2=4,   width2= length(mytech2_level)/2+2,   Ymin2=0, Ymax2=100)
    write.table( cbind(as.vector(Arithmetic_mean_matrix2), rep(x= mytech2_level ,  each=nrow(matrix_temp)  ) ) , 
              file = paste(path_temp_sub1,   "13C_Arithmetic_mean_matrix.txt",  sep="/"), 
              append = FALSE, quote = FALSE, sep = "\t", eol = "\n", na = "NA", dec = ".", 
              row.names = FALSE,  col.names = TRUE, qmethod = c("escape", "double"),  fileEncoding = "") 







    write.table(coefficientOfVariation_matrix2 , 
              file = paste(path_temp_sub1,   "16A_coefficientOfVariation_matrix.txt",  sep="/"), 
              append = FALSE, quote = FALSE, sep = "\t", eol = "\n", na = "NA", dec = ".", 
              row.names = FALSE,  col.names = TRUE, qmethod = c("escape", "double"),  fileEncoding = "")  
    MyBoxViolinPlot_1_f(vector2 = as.vector(coefficientOfVariation_matrix2) ,   
                    sampleType2 = rep(x= mytech2_level ,  each=nrow(matrix_temp)  )   ,  
                    colours2    = MyTech_color_g(  mytech2_level  ),   
                    path2       = path_temp_sub1,   fileName2 = "16B_coefficientOfVariation_matrix",  
                    title2="coefficientOfVariation",  xLab2="Samples",  yLab2="coefficientOfVariation of methylation level (%)",    
                    height2=4,   width2= length(mytech2_level)/2+2,   Ymin2=0, Ymax2=1.5 )
    MyBoxViolinPlot_1_f(vector2 = as.vector(coefficientOfVariation_matrix2) ,   
                    sampleType2 = rep(x= mytech2_level ,  each=nrow(matrix_temp)  )   ,  
                    colours2    = MyTech_color_g(  mytech2_level  ),   
                    path2       = path_temp_sub1,   fileName2 = "16B_coefficientOfVariation_matrix_max0.8",  
                    title2="coefficientOfVariation",  xLab2="Samples",  yLab2="coefficientOfVariation of methylation level (%)",    
                    height2=4,   width2= length(mytech2_level)/2+2,   Ymin2=0, Ymax2=0.8 )
    write.table( cbind(as.vector(coefficientOfVariation_matrix2), rep(x= mytech2_level ,  each=nrow(matrix_temp)  ) ) , 
              file = paste(path_temp_sub1,   "16C_coefficientOfVariation_matrix.txt",  sep="/"), 
              append = FALSE, quote = FALSE, sep = "\t", eol = "\n", na = "NA", dec = ".", 
              row.names = FALSE,  col.names = TRUE, qmethod = c("escape", "double"),  fileEncoding = "") 





    ################# descriptive statistics for raw matrix
    path_temp_sub2 = paste(path_temp, "2_HypothesisTest", sep="/")
    if( ! file.exists(path_temp_sub2) ) { dir.create(path_temp_sub2, recursive = TRUE) }

    #MyHypothesisTest_matrix_1_f( matrix_p=quartile0_matrix2,   path_p=paste(path_temp_sub2, "1_quartile0",   sep="/") )
    #MyHypothesisTest_matrix_1_f( matrix_p=quartile25_matrix2,  path_p=paste(path_temp_sub2, "2_quartile25",  sep="/") )
    MyHypothesisTest_matrix_1_f( matrix_p=quartile50_matrix2,  path_p=paste(path_temp_sub2, "3_quartile50",  sep="/") )
    #MyHypothesisTest_matrix_1_f( matrix_p=quartile75_matrix2,  path_p=paste(path_temp_sub2, "4_quartile75",  sep="/") )
    #MyHypothesisTest_matrix_1_f( matrix_p=quartile100_matrix2, path_p=paste(path_temp_sub2, "5_quartile100", sep="/") )
    MyHypothesisTest_matrix_1_f( matrix_p=InterquartileRange_matrix2, path_p=paste(path_temp_sub2, "6_InterquartileRange", sep="/") )
    MyHypothesisTest_matrix_1_f( matrix_p=range_matrix2,    path_p=paste(path_temp_sub2, "7_range", sep="/") )
    MyHypothesisTest_matrix_1_f( matrix_p=trimean_matrix2,  path_p=paste(path_temp_sub2, "8_trimean", sep="/") )
    MyHypothesisTest_matrix_1_f( matrix_p=variance_matrix2, path_p=paste(path_temp_sub2, "9_variance", sep="/") )
    MyHypothesisTest_matrix_1_f( matrix_p=standardDeviation_matrix2, path_p=paste(path_temp_sub2, "10_standardDeviation", sep="/") )
    #MyHypothesisTest_matrix_1_f( matrix_p=Harmonic_mean_matrix2,     path_p=paste(path_temp_sub2, "11_Harmonic_mean", sep="/") )
    #MyHypothesisTest_matrix_1_f( matrix_p=Geometric_mean_matrix2,    path_p=paste(path_temp_sub2, "12_Geometric_mean", sep="/") )
    MyHypothesisTest_matrix_1_f( matrix_p=Arithmetic_mean_matrix2,   path_p=paste(path_temp_sub2, "13_Arithmetic_mean", sep="/") )
    #MyHypothesisTest_matrix_1_f( matrix_p=Quadratic_mean_matrix2,    path_p=paste(path_temp_sub2, "14_Quadratic_mean", sep="/") )
    #MyHypothesisTest_matrix_1_f( matrix_p=Trim_Arithmetic_mean_matrix2,   path_p=paste(path_temp_sub2, "15_Trim_Arithmetic_mean", sep="/") )
    MyHypothesisTest_matrix_1_f( matrix_p=coefficientOfVariation_matrix2, path_p=paste(path_temp_sub2, "16_coefficientOfVariation_mean", sep="/") )
  





    ################# DNA methylation level for each group
    path_temp_sub3 = paste(path_temp, "3_MethylationLevel", sep="/")
    if( ! file.exists(path_temp_sub3) ) { dir.create(path_temp_sub3, recursive = TRUE) }
    
    vector_local       = as.vector(matrix_temp)
    vector_type_local  = rep(x= dataFrame_temp$mytech ,  each=nrow(matrix_temp)  )
    vector_sex_local   = rep(x= dataFrame_temp$mysex ,   each=nrow(matrix_temp)  )
  
    MyBoxViolinPlot_2_f(vector2   = vector_local ,   
                      sampleType2 = vector_sex_local ,  
                      sampleType3 = vector_type_local  ,  
                      colours2    = MyTech_color_g(  mytech2_level  ),   
                      path2       = path_temp_sub3,   
                      fileName2   = "1_all_Samples",  
                      title2="All Samples ",  xLab2="Samples",  yLab2="Methylation Level (%)",    
                      height2=4,   width2=length( mytech2_level )/1.5 + length( levels(vector_sex_local) ) + 1,   
                      Ymin2=0, Ymax2=100)
  
    MyBoxViolinPlot_1_f(vector2   = vector_local ,   
                      sampleType2 = vector_type_local ,  
                      colours2    = MyTech_color_g(  mytech2_level  ),   
                      path2       = path_temp_sub3,   
                      fileName2   = "2_only_by_Type",  
                      title2      = "All Samples ",  xLab2="Samples",  yLab2="Methylation Level (%)",    
                      height2=4,   width2=length( mytech2_level )/1.3 + 1,   Ymin2=0, Ymax2=100)
  
    MyBoxViolinPlot_1_f(vector2   = vector_local ,   
                      sampleType2 = vector_sex_local ,  
                      colours2    = MyTech_color_g(  mytech2_level  ),   
                      path2       = path_temp_sub3,   
                      fileName2   = "3_only_by_Sex",  
                      title2="All Samples ",  xLab2="Gender",  yLab2="Methylation Level (%)",    
                      height2=4,   width2=length( levels(vector_sex_local) ) + 1,   Ymin2=0, Ymax2=100)

    vector_type1_each = rep(x= colnames(matrix_temp) ,  each=nrow(matrix_temp)  )
    MyBoxViolinPlot_3_f(vector2   = vector_local ,   
                      sampleType2 = vector_sex_local ,  
                      sampleType3 = vector_type1_each  ,  
                      sampleType4 = vector_type_local  ,  
                      path2       = path_temp_sub3,   
                      fileName2   = "4_show-Each-Sample",  
                      title2="All Samples ",  xLab2="Samples",  yLab2="Methylation Level (%)",    
                      height2=4,   width2=length( colnames(matrix_temp) )/5 + length( levels(vector_sex_local) ) + 5 ,   
                      Ymin2=0, Ymax2=100)
  





    ################# more plots for the DNA methylation level of each group
    path_temp_sub4 = paste(path_temp, "4_MeLevel_MoreFigures", sep="/")
    if( ! file.exists(path_temp_sub4) ) { dir.create(path_temp_sub4, recursive = TRUE) }


     vector_local_5categories = vector_local
     vector_local_5categories[ is.na(vector_local)   ] = "1Lowest"
     vector_local_5categories[ (vector_local>=0 ) & (vector_local<10) ]  = "1Lowest"
     vector_local_5categories[ (vector_local>=10) & (vector_local<40) ]  = "2Low"
     vector_local_5categories[ (vector_local>=40) & (vector_local<60) ]  = "3Medium"
     vector_local_5categories[ (vector_local>=60) & (vector_local<90) ]  = "4High"
     vector_local_5categories[ (vector_local>=90) & (vector_local<=100)] = "5Highest"

    MyHistogram_more1_f(vector2=vector_type_local,   sampleType2=vector_local_5categories ,  
                      path2=path_temp_sub4,   fileName2="1_by_Type_Histogram",    
                      title2="By type",  xLab2="Type",   height2=4,  width2=length( mytech2_level )/2+2 )

    MyHistogram_more1_f(vector2=vector_sex_local,   sampleType2=vector_local_5categories ,  
                      path2=path_temp_sub4,   fileName2="2_by_Sex_Histogram",    
                      title2="By Gender",  xLab2="Gender",   height2=4,  width2=length( levels(vector_sex_local) )/2+2 )

    MyHistogram_1_f(vector2   = vector_local ,   
                    sampleType2 = vector_type_local ,  
                    colours2    = MyTech_color_g(  mytech2_level  ),   
                    path2       = path_temp_sub4,   
                    fileName2   = "3_by_Type_probabilityDensity",  
                    title2      = "By type",  xLab2="Methylation Level (%)",     
                    height2=4,   width2=8,   xMin2=0,  xMax2=100,   yMin2=0,  yMax2=0.06)
  








  if(0 == 1) {
    ################# Correct the NA values
    path_temp_sub4 = paste(path_temp, "4_Correct-Methylation-Level", sep="/")
    if( ! file.exists(path_temp_sub4) ) { dir.create(path_temp_sub4, recursive = TRUE) }

    sink( paste(path_temp_sub4, "Correct_processes.txt", sep="/") )

  matrix_temp2 = matrix_temp
  for(i in c(1:length(mytech2_level)) ) {
    col_index = which(mytech2==mytech2_level[i])
    for(j in c(1:nrow(matrix_temp)) ) {
      myVector1 = matrix_temp[j,col_index]
      means_non_NA_1 = mean(x=myVector1, na.rm = TRUE)
      myVector1[is.na(myVector1)] = means_non_NA_1
      matrix_temp2[j,col_index] = myVector1
    }
  }
  dim(matrix_temp) 
  dim(matrix_temp2)
  head(matrix_temp) 
  head(matrix_temp2)
  write.table(matrix_temp , 
              file = paste(path_temp_sub4,   "methylation-Level_raw_withNA.txt",  sep="/"), 
              append = FALSE, quote = FALSE, sep = "\t", eol = "\n", na = "NA", dec = ".", 
              row.names = FALSE,  col.names = TRUE, qmethod = c("escape", "double"),  fileEncoding = "")
  write.table(matrix_temp2 , 
              file = paste(path_temp_sub4,   "methylation-Level_Corrected.txt",  sep="/"), 
              append = FALSE, quote = FALSE, sep = "\t", eol = "\n", na = "NA", dec = ".", 
              row.names = FALSE,  col.names = TRUE, qmethod = c("escape", "double"),  fileEncoding = "")
  
    sink()  
  
  



  if(length( mytech2_level ) == 2) {
      
        ## scatter Diagram for 2 samples and colour each category.
        MyScatterDiagram_1_f(vector2X=as.vector(matrix_temp[,1]),  vector2Y=as.vector(matrix_temp[,2]),  
              path2=path_temp_sub4,   fileName2="6_ScatterDiagram",   
              xLab2=mytech2_level[1],   yLab2==mytech2_level[2],  title2="Scatter Diagram",  
              height2=4,  width2=4,  yMin2=0, yMax2=100,  xMin2=0, xMax2=100,  
              alpha2=0.5, diffThres2=5, colours2=c("red", "blue", "purple")) 
                    
        ## scatter Diagram for 2 samples and colour each category.
        MyScatterDiagram_1_f(vector2X=as.vector(matrix_temp[,1]),  vector2Y=as.vector(matrix_temp[,2]),  
              path2=path_temp_sub4,   fileName2="6_ScatterDiagram",   
              xLab2=mytech2_level[1],   yLab2==mytech2_level[2],  title2="Scatter Diagram",  
              height2=4,  width2=4,  yMin2=0, yMax2=100,  xMin2=0, xMax2=100,  
              alpha2=0.5, diffThres2=5, colours2=c("red", "blue", "purple"))                     


  }
 






## 2D density plot
#MyScatterDiagram_2_f(vector2X,  vector2Y,  path2,   fileName2,   
#xLab2,   yLab2,  title2,  height2=4,  width2=4,  yMin2=0, yMax2=2,  xMin2=0, xMax2=2 ) 


#MyDistributionLine_1_f(x2,  path2,  fileName2,  title2,  xLab2,  
#yLab2,   Ymin2=0,   Ymax2=10,   height2=4,  width2=7, yintercept2=c(-10, 0, 10)   ) 











    ################# descriptive statistics for Corrected matrix
    path_temp_sub5 = paste(path_temp, "5_descriptiveStatistics_Corrected", sep="/")
    if( ! file.exists(path_temp_sub5) ) { dir.create(path_temp_sub5, recursive = TRUE) }

    for(i in c(1:length(mytech2_level)) ) {
        col_index = which(mytech2==mytech2_level[i]) 
        for(j in c(1:nrow(matrix_temp2))) {
            my16stat_temp       = descriptiveStatistics_f( vector_temp=matrix_temp2[j, col_index])
            quartile0_matrix2[j,i]   = my16stat_temp$quartile0
            quartile25_matrix2[j,i]  = my16stat_temp$quartile25 
            quartile50_matrix2[j,i]  = my16stat_temp$quartile50
            quartile75_matrix2[j,i]  = my16stat_temp$quartile75 
            quartile100_matrix2[j,i] = my16stat_temp$quartile100
            InterquartileRange_matrix2[j,i] = my16stat_temp$InterquartileRange 
            range_matrix2[j,i]    = my16stat_temp$range  
            trimean_matrix2[j,i]  = my16stat_temp$trimean  
            variance_matrix2[j,i] = my16stat_temp$variance 
            standardDeviation_matrix2[j,i] = my16stat_temp$standardDeviation  
            Harmonic_mean_matrix2[j,i]     = my16stat_temp$Harmonic_mean 
            Geometric_mean_matrix2[j,i]    = my16stat_temp$Geometric_mean 
            Arithmetic_mean_matrix2[j,i]   = my16stat_temp$Arithmetic_mean 
            Quadratic_mean_matrix2[j,i]    = my16stat_temp$Quadratic_mean 
            Trim_Arithmetic_mean_matrix2[j,i]   = my16stat_temp$Trim_Arithmetic_mean 
            coefficientOfVariation_matrix2[j,i] = my16stat_temp$coefficientOfVariation 
        }
    }


   

   sink( paste(path_temp_sub5, "dimensions_16stats.txt", sep="/") )
       cat("matrix_temp2:",         dim(matrix_temp2), "\n")
       cat("quartile0_matrix2:",   dim(quartile0_matrix2), "\n")
       cat("quartile25_matrix2:",  dim(quartile25_matrix2), "\n")
       cat("quartile50_matrix2:",  dim(quartile50_matrix2), "\n")
       cat("quartile75_matrix2:",  dim(quartile75_matrix2), "\n")
       cat("quartile100_matrix2:", dim(quartile100_matrix2), "\n")
       cat("InterquartileRange_matrix2:", dim(InterquartileRange_matrix2), "\n")
       cat("range_matrix2:",    dim(range_matrix2), "\n")
       cat("trimean_matrix2:",  dim(trimean_matrix2), "\n")
       cat("variance_matrix2:", dim(variance_matrix2), "\n")
       cat("standardDeviation_matrix2:", dim(standardDeviation_matrix2), "\n")
       cat("Harmonic_mean_matrix2:",     dim(Harmonic_mean_matrix2), "\n")
       cat("Geometric_mean_matrix2:",    dim(Geometric_mean_matrix2), "\n")
       cat("Arithmetic_mean_matrix2:",   dim(Arithmetic_mean_matrix2), "\n")
       cat("Quadratic_mean_matrix2:",    dim(Quadratic_mean_matrix2), "\n")
       cat("Trim_Arithmetic_mean_matrix2:",   dim(Trim_Arithmetic_mean_matrix2), "\n")
       cat("coefficientOfVariation_matrix2:", dim(coefficientOfVariation_matrix2), "\n")
    sink()  


    write.table(quartile0_matrix2 , 
              file = paste(path_temp_sub5,   "1A_quartile0_matrix.txt",  sep="/"), 
              append = FALSE, quote = FALSE, sep = "\t", eol = "\n", na = "NA", dec = ".", 
              row.names = FALSE,  col.names = TRUE, qmethod = c("escape", "double"),  fileEncoding = "")  
    MyBoxViolinPlot_1_f(vector2 = as.vector(quartile0_matrix2) ,   
                    sampleType2 = rep(x= mytech2_level ,  each=nrow(matrix_temp2)  )   ,  
                    colours2    = MyTech_color_g(  mytech2_level  ),   
                    path2       = path_temp_sub5,   fileName2 = "1B_quartile0_matrix",  
                    title2="quartile0",  xLab2="Samples",  yLab2="quartile0 of methylation level (%)",    
                    height2=4,   width2= length(mytech2_level)/2+2,   Ymin2=0, Ymax2=100)
    write.table( cbind(as.vector(quartile0_matrix2), rep(x= mytech2_level ,  each=nrow(matrix_temp2)  ) ) , 
              file = paste(path_temp_sub5,   "1C_quartile0_matrix.txt",  sep="/"), 
              append = FALSE, quote = FALSE, sep = "\t", eol = "\n", na = "NA", dec = ".", 
              row.names = FALSE,  col.names = TRUE, qmethod = c("escape", "double"),  fileEncoding = "") 


    write.table(quartile25_matrix2 , 
              file = paste(path_temp_sub5,   "2A_quartile25_matrix.txt",  sep="/"), 
              append = FALSE, quote = FALSE, sep = "\t", eol = "\n", na = "NA", dec = ".", 
              row.names = FALSE,  col.names = TRUE, qmethod = c("escape", "double"),  fileEncoding = "")  
    MyBoxViolinPlot_1_f(vector2 = as.vector(quartile25_matrix2) ,   
                    sampleType2 = rep(x= mytech2_level ,  each=nrow(matrix_temp2)  )   ,  
                    colours2    = MyTech_color_g(  mytech2_level  ),   
                    path2       = path_temp_sub5,   fileName2 = "2B_quartile25_matrix",  
                    title2="quartile25",  xLab2="Samples",  yLab2="quartile25 of methylation level (%)",    
                    height2=4,   width2= length(mytech2_level)/2+2,   Ymin2=0, Ymax2=100)
    write.table( cbind(as.vector(quartile25_matrix2), rep(x= mytech2_level ,  each=nrow(matrix_temp2)  ) ) , 
              file = paste(path_temp_sub5,   "2C_quartile25_matrix.txt",  sep="/"), 
              append = FALSE, quote = FALSE, sep = "\t", eol = "\n", na = "NA", dec = ".", 
              row.names = FALSE,  col.names = TRUE, qmethod = c("escape", "double"),  fileEncoding = "") 




    write.table(quartile50_matrix2 , 
              file = paste(path_temp_sub5,   "3A_quartile50_matrix.txt",  sep="/"), 
              append = FALSE, quote = FALSE, sep = "\t", eol = "\n", na = "NA", dec = ".", 
              row.names = FALSE,  col.names = TRUE, qmethod = c("escape", "double"),  fileEncoding = "")  
    MyBoxViolinPlot_1_f(vector2 = as.vector(quartile50_matrix2) ,   
                    sampleType2 = rep(x= mytech2_level ,  each=nrow(matrix_temp2)  )   ,  
                    colours2    = MyTech_color_g(  mytech2_level  ),   
                    path2       = path_temp_sub5,   fileName2 = "3B_quartile50_matrix",  
                    title2="quartile50",  xLab2="Samples",  yLab2="quartile50 of methylation level (%)",    
                    height2=4,   width2= length(mytech2_level)/2+2,   Ymin2=0, Ymax2=100)
    write.table( cbind(as.vector(quartile50_matrix2), rep(x= mytech2_level ,  each=nrow(matrix_temp2)  ) ) , 
              file = paste(path_temp_sub5,   "3C_quartile50_matrix.txt",  sep="/"), 
              append = FALSE, quote = FALSE, sep = "\t", eol = "\n", na = "NA", dec = ".", 
              row.names = FALSE,  col.names = TRUE, qmethod = c("escape", "double"),  fileEncoding = "") 



    write.table(quartile75_matrix2 , 
              file = paste(path_temp_sub5,   "4A_quartile75_matrix.txt",  sep="/"), 
              append = FALSE, quote = FALSE, sep = "\t", eol = "\n", na = "NA", dec = ".", 
              row.names = FALSE,  col.names = TRUE, qmethod = c("escape", "double"),  fileEncoding = "")  
    MyBoxViolinPlot_1_f(vector2 = as.vector(quartile75_matrix2) ,   
                    sampleType2 = rep(x= mytech2_level ,  each=nrow(matrix_temp2)  )   ,  
                    colours2    = MyTech_color_g(  mytech2_level  ),   
                    path2       = path_temp_sub5,   fileName2 = "4B_quartile75_matrix",  
                    title2="quartile75",  xLab2="Samples",  yLab2="quartile75 of methylation level (%)",    
                    height2=4,   width2= length(mytech2_level)/2+2,   Ymin2=0, Ymax2=100)
    write.table( cbind(as.vector(quartile75_matrix2), rep(x= mytech2_level ,  each=nrow(matrix_temp2)  ) ) , 
              file = paste(path_temp_sub5,   "4C_quartile75_matrix.txt",  sep="/"), 
              append = FALSE, quote = FALSE, sep = "\t", eol = "\n", na = "NA", dec = ".", 
              row.names = FALSE,  col.names = TRUE, qmethod = c("escape", "double"),  fileEncoding = "") 



    write.table(quartile100_matrix2 , 
              file = paste(path_temp_sub5,   "5A_quartile100_matrix.txt",  sep="/"), 
              append = FALSE, quote = FALSE, sep = "\t", eol = "\n", na = "NA", dec = ".", 
              row.names = FALSE,  col.names = TRUE, qmethod = c("escape", "double"),  fileEncoding = "")  
    MyBoxViolinPlot_1_f(vector2 = as.vector(quartile100_matrix2) ,   
                    sampleType2 = rep(x= mytech2_level ,  each=nrow(matrix_temp2)  )   ,  
                    colours2    = MyTech_color_g(  mytech2_level  ),   
                    path2       = path_temp_sub5,   fileName2 = "5B_quartile100_matrix",  
                    title2="quartile100",  xLab2="Samples",  yLab2="quartile100 of methylation level (%)",    
                    height2=4,   width2= length(mytech2_level)/2+2,   Ymin2=0, Ymax2=100)
    write.table( cbind(as.vector(quartile100_matrix2), rep(x= mytech2_level ,  each=nrow(matrix_temp2)  ) ) , 
              file = paste(path_temp_sub5,   "5C_quartile100_matrix.txt",  sep="/"), 
              append = FALSE, quote = FALSE, sep = "\t", eol = "\n", na = "NA", dec = ".", 
              row.names = FALSE,  col.names = TRUE, qmethod = c("escape", "double"),  fileEncoding = "") 


    write.table(InterquartileRange_matrix2 , 
              file = paste(path_temp_sub5,   "6A_InterquartileRange_matrix.txt",  sep="/"), 
              append = FALSE, quote = FALSE, sep = "\t", eol = "\n", na = "NA", dec = ".", 
              row.names = FALSE,  col.names = TRUE, qmethod = c("escape", "double"),  fileEncoding = "")  
    MyBoxViolinPlot_1_f(vector2 = as.vector(InterquartileRange_matrix2) ,   
                    sampleType2 = rep(x= mytech2_level ,  each=nrow(matrix_temp2)  )   ,  
                    colours2    = MyTech_color_g(  mytech2_level  ),   
                    path2       = path_temp_sub5,   fileName2 = "6B_InterquartileRange_matrix",  
                    title2="InterquartileRange",  xLab2="Samples",  yLab2="InterquartileRange of methylation level (%)",    
                    height2=4,   width2= length(mytech2_level)/2+2,   Ymin2=0, Ymax2=20)
    MyBoxViolinPlot_1_f(vector2 = as.vector(InterquartileRange_matrix2) ,   
                    sampleType2 = rep(x= mytech2_level ,  each=nrow(matrix_temp2)  )   ,  
                    colours2    = MyTech_color_g(  mytech2_level  ),   
                    path2       = path_temp_sub5,   fileName2 = "6B_InterquartileRange_matrix_max10",  
                    title2="InterquartileRange",  xLab2="Samples",  yLab2="InterquartileRange of methylation level (%)",    
                    height2=4,   width2= length(mytech2_level)/2+2,   Ymin2=0, Ymax2=10)
    write.table( cbind(as.vector(InterquartileRange_matrix2), rep(x= mytech2_level ,  each=nrow(matrix_temp2)  ) ) , 
              file = paste(path_temp_sub5,   "6C_InterquartileRange_matrix.txt",  sep="/"), 
              append = FALSE, quote = FALSE, sep = "\t", eol = "\n", na = "NA", dec = ".", 
              row.names = FALSE,  col.names = TRUE, qmethod = c("escape", "double"),  fileEncoding = "") 





    write.table(range_matrix2 , 
              file = paste(path_temp_sub5,   "7A_range_matrix.txt",  sep="/"), 
              append = FALSE, quote = FALSE, sep = "\t", eol = "\n", na = "NA", dec = ".", 
              row.names = FALSE,  col.names = TRUE, qmethod = c("escape", "double"),  fileEncoding = "")  
    MyBoxViolinPlot_1_f(vector2 = as.vector(range_matrix2) ,   
                    sampleType2 = rep(x= mytech2_level ,  each=nrow(matrix_temp2)  )   ,  
                    colours2    = MyTech_color_g(  mytech2_level  ),   
                    path2       = path_temp_sub5,   fileName2 = "7B_range_matrix",  
                    title2="range",  xLab2="Samples",  yLab2="range of methylation level (%)",    
                    height2=4,   width2= length(mytech2_level)/2+2,   Ymin2=0, Ymax2=50)
    MyBoxViolinPlot_1_f(vector2 = as.vector(range_matrix2) ,   
                    sampleType2 = rep(x= mytech2_level ,  each=nrow(matrix_temp2)  )   ,  
                    colours2    = MyTech_color_g(  mytech2_level  ),   
                    path2       = path_temp_sub5,   fileName2 = "7B_range_matrix_max25",  
                    title2="range",  xLab2="Samples",  yLab2="range of methylation level (%)",    
                    height2=4,   width2= length(mytech2_level)/2+2,   Ymin2=0, Ymax2=25)
    write.table( cbind(as.vector(range_matrix2), rep(x= mytech2_level ,  each=nrow(matrix_temp2)  ) ) , 
              file = paste(path_temp_sub5,   "7C_range_matrix.txt",  sep="/"), 
              append = FALSE, quote = FALSE, sep = "\t", eol = "\n", na = "NA", dec = ".", 
              row.names = FALSE,  col.names = TRUE, qmethod = c("escape", "double"),  fileEncoding = "") 



    write.table(trimean_matrix2 , 
              file = paste(path_temp_sub5,   "8A_trimean_matrix.txt",  sep="/"), 
              append = FALSE, quote = FALSE, sep = "\t", eol = "\n", na = "NA", dec = ".", 
              row.names = FALSE,  col.names = TRUE, qmethod = c("escape", "double"),  fileEncoding = "")  
    MyBoxViolinPlot_1_f(vector2 = as.vector(trimean_matrix2) ,   
                    sampleType2 = rep(x= mytech2_level ,  each=nrow(matrix_temp2)  )   ,  
                    colours2    = MyTech_color_g(  mytech2_level  ),   
                    path2       = path_temp_sub5,   fileName2 = "8B_trimean_matrix",  
                    title2="trimean",  xLab2="Samples",  yLab2="trimean of methylation level (%)",    
                    height2=4,   width2= length(mytech2_level)/2+2,   Ymin2=0, Ymax2=100)
    write.table( cbind(as.vector(trimean_matrix2), rep(x= mytech2_level ,  each=nrow(matrix_temp2)  ) ) , 
              file = paste(path_temp_sub5,   "8C_trimean_matrix.txt",  sep="/"), 
              append = FALSE, quote = FALSE, sep = "\t", eol = "\n", na = "NA", dec = ".", 
              row.names = FALSE,  col.names = TRUE, qmethod = c("escape", "double"),  fileEncoding = "") 




    write.table(variance_matrix2 , 
              file = paste(path_temp_sub5,   "9A_variance_matrix.txt",  sep="/"), 
              append = FALSE, quote = FALSE, sep = "\t", eol = "\n", na = "NA", dec = ".", 
              row.names = FALSE,  col.names = TRUE, qmethod = c("escape", "double"),  fileEncoding = "")  
    MyBoxViolinPlot_1_f(vector2 = as.vector(variance_matrix2) ,   
                    sampleType2 = rep(x= mytech2_level ,  each=nrow(matrix_temp2)  )   ,  
                    colours2    = MyTech_color_g(  mytech2_level  ),   
                    path2       = path_temp_sub5,   fileName2 = "9B_variance_matrix",  
                    title2="variance",  xLab2="Samples",  yLab2="variance of methylation level (%)",    
                    height2=4,   width2= length(mytech2_level)/2+2,   Ymin2=0, Ymax2=50)
    MyBoxViolinPlot_1_f(vector2 = as.vector(variance_matrix2) ,   
                    sampleType2 = rep(x= mytech2_level ,  each=nrow(matrix_temp2)  )   ,  
                    colours2    = MyTech_color_g(  mytech2_level  ),   
                    path2       = path_temp_sub5,   fileName2 = "9B_variance_matrix_max25",  
                    title2="variance",  xLab2="Samples",  yLab2="variance of methylation level (%)",    
                    height2=4,   width2= length(mytech2_level)/2+2,   Ymin2=0, Ymax2=25)    
    write.table( cbind(as.vector(variance_matrix2), rep(x= mytech2_level ,  each=nrow(matrix_temp2)  ) ) , 
              file = paste(path_temp_sub5,   "9C_variance_matrix.txt",  sep="/"), 
              append = FALSE, quote = FALSE, sep = "\t", eol = "\n", na = "NA", dec = ".", 
              row.names = FALSE,  col.names = TRUE, qmethod = c("escape", "double"),  fileEncoding = "") 



    write.table(standardDeviation_matrix2 , 
              file = paste(path_temp_sub5,   "10A_standardDeviation_matrix.txt",  sep="/"), 
              append = FALSE, quote = FALSE, sep = "\t", eol = "\n", na = "NA", dec = ".", 
              row.names = FALSE,  col.names = TRUE, qmethod = c("escape", "double"),  fileEncoding = "")  
    MyBoxViolinPlot_1_f(vector2 = as.vector(standardDeviation_matrix2) ,   
                    sampleType2 = rep(x= mytech2_level ,  each=nrow(matrix_temp2)  )   ,  
                    colours2    = MyTech_color_g(  mytech2_level  ),   
                    path2       = path_temp_sub5,   fileName2 = "10B_standardDeviation_matrix",  
                    title2="standardDeviation",  xLab2="Samples",  yLab2="standardDeviation of methylation level (%)",    
                    height2=4,   width2= length(mytech2_level)/2+2,   Ymin2=0, Ymax2=20)
    MyBoxViolinPlot_1_f(vector2 = as.vector(standardDeviation_matrix2) ,   
                    sampleType2 = rep(x= mytech2_level ,  each=nrow(matrix_temp2)  )   ,  
                    colours2    = MyTech_color_g(  mytech2_level  ),   
                    path2       = path_temp_sub5,   fileName2 = "10B_standardDeviation_matrix_max8",  
                    title2="standardDeviation",  xLab2="Samples",  yLab2="standardDeviation of methylation level (%)",    
                    height2=4,   width2= length(mytech2_level)/2+2,   Ymin2=0, Ymax2=8)
    write.table( cbind(as.vector(standardDeviation_matrix2), rep(x= mytech2_level ,  each=nrow(matrix_temp2)  ) ) , 
              file = paste(path_temp_sub5,   "10C_standardDeviation_matrix.txt",  sep="/"), 
              append = FALSE, quote = FALSE, sep = "\t", eol = "\n", na = "NA", dec = ".", 
              row.names = FALSE,  col.names = TRUE, qmethod = c("escape", "double"),  fileEncoding = "") 



    write.table(Harmonic_mean_matrix2 , 
              file = paste(path_temp_sub5,   "11A_Harmonic_mean_matrix.txt",  sep="/"), 
              append = FALSE, quote = FALSE, sep = "\t", eol = "\n", na = "NA", dec = ".", 
              row.names = FALSE,  col.names = TRUE, qmethod = c("escape", "double"),  fileEncoding = "")  
    MyBoxViolinPlot_1_f(vector2 = as.vector(Harmonic_mean_matrix2) ,   
                    sampleType2 = rep(x= mytech2_level ,  each=nrow(matrix_temp2)  )   ,  
                    colours2    = MyTech_color_g(  mytech2_level  ),   
                    path2       = path_temp_sub5,   fileName2 = "11B_Harmonic_mean_matrix",  
                    title2="Harmonic_mean",  xLab2="Samples",  yLab2="Harmonic_mean of methylation level (%)",    
                    height2=4,   width2= length(mytech2_level)/2+2,   Ymin2=0, Ymax2=100)
    write.table( cbind(as.vector(Harmonic_mean_matrix2), rep(x= mytech2_level ,  each=nrow(matrix_temp2)  ) ) , 
              file = paste(path_temp_sub5,   "11C_Harmonic_mean_matrix.txt",  sep="/"), 
              append = FALSE, quote = FALSE, sep = "\t", eol = "\n", na = "NA", dec = ".", 
              row.names = FALSE,  col.names = TRUE, qmethod = c("escape", "double"),  fileEncoding = "") 



    write.table(Geometric_mean_matrix2 , 
              file = paste(path_temp_sub5,   "12A_Geometric_mean_matrix.txt",  sep="/"), 
              append = FALSE, quote = FALSE, sep = "\t", eol = "\n", na = "NA", dec = ".", 
              row.names = FALSE,  col.names = TRUE, qmethod = c("escape", "double"),  fileEncoding = "")  
    MyBoxViolinPlot_1_f(vector2 = as.vector(Geometric_mean_matrix2) ,   
                    sampleType2 = rep(x= mytech2_level ,  each=nrow(matrix_temp2)  )   ,  
                    colours2    = MyTech_color_g(  mytech2_level  ),   
                    path2       = path_temp_sub5,   fileName2 = "12B_Geometric_mean_matrix",  
                    title2="Geometric_mean",  xLab2="Samples",  yLab2="Geometric_mean of methylation level (%)",    
                    height2=4,   width2= length(mytech2_level)/2+2,   Ymin2=0, Ymax2=100)
    write.table( cbind(as.vector(Geometric_mean_matrix2), rep(x= mytech2_level ,  each=nrow(matrix_temp2)  ) ) , 
              file = paste(path_temp_sub5,   "12C_Geometric_mean_matrix.txt",  sep="/"), 
              append = FALSE, quote = FALSE, sep = "\t", eol = "\n", na = "NA", dec = ".", 
              row.names = FALSE,  col.names = TRUE, qmethod = c("escape", "double"),  fileEncoding = "") 



    write.table(Arithmetic_mean_matrix2 , 
              file = paste(path_temp_sub5,   "13A_Arithmetic_mean_matrix.txt",  sep="/"), 
              append = FALSE, quote = FALSE, sep = "\t", eol = "\n", na = "NA", dec = ".", 
              row.names = FALSE,  col.names = TRUE, qmethod = c("escape", "double"),  fileEncoding = "")  
    MyBoxViolinPlot_1_f(vector2 = as.vector(Arithmetic_mean_matrix2) ,   
                    sampleType2 = rep(x= mytech2_level ,  each=nrow(matrix_temp2)  )   ,  
                    colours2    = MyTech_color_g(  mytech2_level  ),   
                    path2       = path_temp_sub5,   fileName2 = "13B_Arithmetic_mean_matrix",  
                    title2="Arithmetic_mean",  xLab2="Samples",  yLab2="Arithmetic_mean of methylation level (%)",    
                    height2=4,   width2= length(mytech2_level)/2+2,   Ymin2=0, Ymax2=100)
    write.table( cbind(as.vector(Arithmetic_mean_matrix2), rep(x= mytech2_level ,  each=nrow(matrix_temp2)  ) ) , 
              file = paste(path_temp_sub5,   "13C_Arithmetic_mean_matrix.txt",  sep="/"), 
              append = FALSE, quote = FALSE, sep = "\t", eol = "\n", na = "NA", dec = ".", 
              row.names = FALSE,  col.names = TRUE, qmethod = c("escape", "double"),  fileEncoding = "") 



    write.table(Quadratic_mean_matrix2 , 
              file = paste(path_temp_sub5,   "14A_Quadratic_mean_matrix.txt",  sep="/"), 
              append = FALSE, quote = FALSE, sep = "\t", eol = "\n", na = "NA", dec = ".", 
              row.names = FALSE,  col.names = TRUE, qmethod = c("escape", "double"),  fileEncoding = "")  
    MyBoxViolinPlot_1_f(vector2 = as.vector(Quadratic_mean_matrix2) ,   
                    sampleType2 = rep(x= mytech2_level ,  each=nrow(matrix_temp2)  )   ,  
                    colours2    = MyTech_color_g(  mytech2_level  ),   
                    path2       = path_temp_sub5,   fileName2 = "14B_Quadratic_mean_matrix",  
                    title2="Quadratic_mean",  xLab2="Samples",  yLab2="Quadratic_mean of methylation level (%)",    
                    height2=4,   width2= length(mytech2_level)/2+2,   Ymin2=0, Ymax2=100)
    write.table( cbind(as.vector(Quadratic_mean_matrix2), rep(x= mytech2_level ,  each=nrow(matrix_temp2)  ) ) , 
              file = paste(path_temp_sub5,   "14C_Quadratic_mean_matrix.txt",  sep="/"), 
              append = FALSE, quote = FALSE, sep = "\t", eol = "\n", na = "NA", dec = ".", 
              row.names = FALSE,  col.names = TRUE, qmethod = c("escape", "double"),  fileEncoding = "") 



    write.table(Trim_Arithmetic_mean_matrix2 , 
              file = paste(path_temp_sub5,   "15A_Trim_Arithmetic_mean_matrix.txt",  sep="/"), 
              append = FALSE, quote = FALSE, sep = "\t", eol = "\n", na = "NA", dec = ".", 
              row.names = FALSE,  col.names = TRUE, qmethod = c("escape", "double"),  fileEncoding = "")  
    MyBoxViolinPlot_1_f(vector2 = as.vector(Trim_Arithmetic_mean_matrix2) ,   
                    sampleType2 = rep(x= mytech2_level ,  each=nrow(matrix_temp2)  )   ,  
                    colours2    = MyTech_color_g(  mytech2_level  ),   
                    path2       = path_temp_sub5,   fileName2 = "15B_Trim_Arithmetic_mean_matrix",  
                    title2="Trim_Arithmetic_mean",  xLab2="Samples",  yLab2="Trim_Arithmetic_mean of methylation level (%)",    
                    height2=4,   width2= length(mytech2_level)/2+2,   Ymin2=0, Ymax2=100)
    write.table( cbind(as.vector(Trim_Arithmetic_mean_matrix2), rep(x= mytech2_level ,  each=nrow(matrix_temp2)  ) ) , 
              file = paste(path_temp_sub5,   "15C_Trim_Arithmetic_mean_matrix.txt",  sep="/"), 
              append = FALSE, quote = FALSE, sep = "\t", eol = "\n", na = "NA", dec = ".", 
              row.names = FALSE,  col.names = TRUE, qmethod = c("escape", "double"),  fileEncoding = "") 




    write.table(coefficientOfVariation_matrix2 , 
              file = paste(path_temp_sub5,   "16A_coefficientOfVariation_matrix.txt",  sep="/"), 
              append = FALSE, quote = FALSE, sep = "\t", eol = "\n", na = "NA", dec = ".", 
              row.names = FALSE,  col.names = TRUE, qmethod = c("escape", "double"),  fileEncoding = "")  
    MyBoxViolinPlot_1_f(vector2 = as.vector(coefficientOfVariation_matrix2) ,   
                    sampleType2 = rep(x= mytech2_level ,  each=nrow(matrix_temp2)  )   ,  
                    colours2    = MyTech_color_g(  mytech2_level  ),   
                    path2       = path_temp_sub5,   fileName2 = "16B_coefficientOfVariation_matrix",  
                    title2="coefficientOfVariation",  xLab2="Samples",  yLab2="coefficientOfVariation of methylation level (%)",    
                    height2=4,   width2= length(mytech2_level)/2+2,   Ymin2=0, Ymax2=1.6 )
    MyBoxViolinPlot_1_f(vector2 = as.vector(coefficientOfVariation_matrix2) ,   
                    sampleType2 = rep(x= mytech2_level ,  each=nrow(matrix_temp2)  )   ,  
                    colours2    = MyTech_color_g(  mytech2_level  ),   
                    path2       = path_temp_sub5,   fileName2 = "16B_coefficientOfVariation_matrix_max0.8",  
                    title2="coefficientOfVariation",  xLab2="Samples",  yLab2="coefficientOfVariation of methylation level (%)",    
                    height2=4,   width2= length(mytech2_level)/2+2,   Ymin2=0, Ymax2=0.8 )
    write.table( cbind(as.vector(coefficientOfVariation_matrix2), rep(x= mytech2_level ,  each=nrow(matrix_temp2)  ) ) , 
              file = paste(path_temp_sub5,   "16C_coefficientOfVariation_matrix.txt",  sep="/"), 
              append = FALSE, quote = FALSE, sep = "\t", eol = "\n", na = "NA", dec = ".", 
              row.names = FALSE,  col.names = TRUE, qmethod = c("escape", "double"),  fileEncoding = "") 





    #################  for Corrected matrix
    path_temp_sub6 = paste(path_temp, "6_HypothesisTest_Corrected", sep="/")
    if( ! file.exists(path_temp_sub6) ) { dir.create(path_temp_sub6, recursive = TRUE) }

    MyHypothesisTest_matrix_1_f( matrix_p=quartile0_matrix2,   path_p=paste(path_temp_sub6, "1_quartile0",   sep="/") )
    MyHypothesisTest_matrix_1_f( matrix_p=quartile25_matrix2,  path_p=paste(path_temp_sub6, "2_quartile25",  sep="/") )
    MyHypothesisTest_matrix_1_f( matrix_p=quartile50_matrix2,  path_p=paste(path_temp_sub6, "3_quartile50",  sep="/") )
    MyHypothesisTest_matrix_1_f( matrix_p=quartile75_matrix2,  path_p=paste(path_temp_sub6, "4_quartile75",  sep="/") )
    MyHypothesisTest_matrix_1_f( matrix_p=quartile100_matrix2, path_p=paste(path_temp_sub6, "5_quartile100", sep="/") )
    MyHypothesisTest_matrix_1_f( matrix_p=InterquartileRange_matrix2, path_p=paste(path_temp_sub6, "6_InterquartileRange", sep="/") )
    MyHypothesisTest_matrix_1_f( matrix_p=range_matrix2,    path_p=paste(path_temp_sub6, "7_range", sep="/") )
    MyHypothesisTest_matrix_1_f( matrix_p=trimean_matrix2,  path_p=paste(path_temp_sub6, "8_trimean", sep="/") )
    MyHypothesisTest_matrix_1_f( matrix_p=variance_matrix2, path_p=paste(path_temp_sub6, "9_variance", sep="/") )
    MyHypothesisTest_matrix_1_f( matrix_p=standardDeviation_matrix2, path_p=paste(path_temp_sub6, "10_standardDeviation", sep="/") )
    MyHypothesisTest_matrix_1_f( matrix_p=Harmonic_mean_matrix2,     path_p=paste(path_temp_sub6, "11_Harmonic_mean", sep="/") )
    MyHypothesisTest_matrix_1_f( matrix_p=Geometric_mean_matrix2,    path_p=paste(path_temp_sub6, "12_Geometric_mean", sep="/") )
    MyHypothesisTest_matrix_1_f( matrix_p=Arithmetic_mean_matrix2,   path_p=paste(path_temp_sub6, "13_Arithmetic_mean", sep="/") )
    MyHypothesisTest_matrix_1_f( matrix_p=Quadratic_mean_matrix2,    path_p=paste(path_temp_sub6, "14_Quadratic_mean", sep="/") )
    MyHypothesisTest_matrix_1_f( matrix_p=Trim_Arithmetic_mean_matrix2,   path_p=paste(path_temp_sub6, "15_Trim_Arithmetic_mean", sep="/") )
    MyHypothesisTest_matrix_1_f( matrix_p=coefficientOfVariation_matrix2, path_p=paste(path_temp_sub6, "16_coefficientOfVariation_mean", sep="/") )
  





    ################# DNA methylation level for each group
    path_temp_sub7 = paste(path_temp, "7_MethylationLevel_Corrected", sep="/")
    if( ! file.exists(path_temp_sub7) ) { dir.create(path_temp_sub7, recursive = TRUE) }
    
    vector_local       = as.vector(matrix_temp2)
    vector_type_local  = rep(x= dataFrame_temp$mytech ,  each=nrow(matrix_temp2)  )
    vector_sex_local   = rep(x= dataFrame_temp$mysex ,   each=nrow(matrix_temp2)  )
  
    MyBoxViolinPlot_2_f(vector2   = vector_local ,   
                      sampleType2 = vector_sex_local ,  
                      sampleType3 = vector_type_local  ,  
                      colours2    = MyTech_color_g(  mytech2_level  ),   
                      path2       = path_temp_sub7,   
                      fileName2   = "1_all_Samples",  
                      title2="All Samples ",  xLab2="Samples",  yLab2="Methylation Level (%)",    
                      height2=4,   width2=length( mytech2_level ) + length( levels(vector_sex_local) ) + 1,   
                      Ymin2=0, Ymax2=100)
  
    MyBoxViolinPlot_1_f(vector2   = vector_local ,   
                      sampleType2 = vector_type_local ,  
                      colours2    = MyTech_color_g(  mytech2_level  ),   
                      path2       = path_temp_sub7,   
                      fileName2   = "2_only_by_Type",  
                      title2      = "All Samples ",  xLab2="Samples",  yLab2="Methylation Level (%)",    
                      height2=4,   width2=length( mytech2_level ) + 1,   Ymin2=0, Ymax2=100)
  
    MyBoxViolinPlot_1_f(vector2   = vector_local ,   
                      sampleType2 = vector_sex_local , 
                      colours2    = MyTech_color_g(  mytech2_level  ),    
                      path2       = path_temp_sub7,   
                      fileName2   = "3_only_by_Sex",  
                      title2="All Samples ",  xLab2="Gender",  yLab2="Methylation Level (%)",    
                      height2=4,   width2=length( levels(vector_sex_local) ) + 1,   Ymin2=0, Ymax2=100)

    vector_type1_each = rep(x= colnames(matrix_temp2) ,  each=nrow(matrix_temp2)  )
    MyBoxViolinPlot_3_f(vector2   = vector_local ,   
                      sampleType2 = vector_sex_local ,  
                      sampleType3 = vector_type1_each  ,  
                      sampleType4 = vector_type_local  ,  
                      path2       = path_temp_sub7,   
                      fileName2   = "4_show-Each-Sample",  
                      title2="All Samples ",  xLab2="Samples",  yLab2="Methylation Level (%)",    
                      height2=4,   width2=length( colnames(matrix_temp2) )/5 + length( levels(vector_sex_local) ) + 5 ,   
                      Ymin2=0, Ymax2=100)
  


  path_temp2_sub3 = paste(path_temp, "8_MultidimensionalScaling_Corrected", sep="/")
  if( ! file.exists(path_temp2_sub3) ) { dir.create(path_temp2_sub3, recursive = TRUE) }
  MyMultidimensionalScaling_1_g(  meLevelMatrix2=matrix_temp2,   path2=path_temp2_sub3,   dataFrame_temp2=dataFrame_temp  )

  path_temp2_sub4 = paste(path_temp, "9_Correlation_Corrected", sep="/") 
  if( ! file.exists(path_temp2_sub4) ) { dir.create(path_temp2_sub4, recursive = TRUE) }    
  myCorCov_matrix_1(matrix_temp10=matrix_temp2,   path_temp10=path_temp2_sub4, my_col1=colorRampPalette(  c( "blue", "skyblue", "black", "pink", "red") ) )

  path_temp2_sub5 = paste(path_temp, "10_HierarchicalClustering_Corrected", sep="/") 
  if( ! file.exists(path_temp2_sub5) ) { dir.create(path_temp2_sub5, recursive = TRUE) }    
  myHierarchicalClustering_1_g(  mat_3three=matrix_temp2,   path_temp1=path_temp2_sub5,  width1=length( as.vector(dataFrame_temp$mysampleID) )/3 + 2,  height1=length( as.vector(dataFrame_temp$mysampleID) )/20 + 2   )           

 }


}


 





















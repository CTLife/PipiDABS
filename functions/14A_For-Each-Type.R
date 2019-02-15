suppressPackageStartupMessages( library(psych) )  
suppressPackageStartupMessages( library(rdiversity) )  





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

      variance_temp          = var(vector_temp,  na.rm=TRUE)
      standardDeviation_temp = sd(vector_temp,   na.rm=TRUE) 

      Harmonic_mean_temp   =mean(vector_temp,  na.rm=TRUE) #psych::harmonic.mean(vector_temp,  na.rm=TRUE, zero=FALSE) ## return the harmonic mean of the non-zero elements.                
      Geometric_mean_temp  =mean(vector_temp,  na.rm=TRUE) #psych::geometric.mean(vector_temp[vector_temp>0])   
      Arithmetic_mean_temp =mean(vector_temp,  na.rm=TRUE)
      Quadratic_mean_temp  =mean(vector_temp,  na.rm=TRUE) #rdiversity::power_mean(values=vector_temp, order = 2)
      Trim_Arithmetic_mean_temp  = mean(vector_temp, trim = 0.01,  na.rm=TRUE)

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
    error = function(err){"descriptiveStatistics_f_369852147"}
  ) 

}
 




forEachType_f <- function(path_temp, matrix_temp, dataFrame_temp) {
  myTempFunction <- function() {

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
    path_temp_sub1 = paste(path_temp, "1_descriptive-Statistics", sep="/")
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
    path_temp_sub2 = paste(path_temp, "2_Hypothesis-Test", sep="/")
    if( ! file.exists(path_temp_sub2) ) { dir.create(path_temp_sub2, recursive = TRUE) }

    #MyHypothesisTest_matrix_1_f( matrix_p=quartile0_matrix2,   path_p=paste(path_temp_sub2, "1_quartile0",   sep="/") )
    #MyHypothesisTest_matrix_1_f( matrix_p=quartile25_matrix2,  path_p=paste(path_temp_sub2, "2_quartile25",  sep="/") )
    MyHypothesisTest_matrix_1_f( matrix_p=quartile50_matrix2,  path_p=paste(path_temp_sub2, "3_quartile50",  sep="/") )
    #MyHypothesisTest_matrix_1_f( matrix_p=quartile75_matrix2,  path_p=paste(path_temp_sub2, "4_quartile75",  sep="/") )
    #MyHypothesisTest_matrix_1_f( matrix_p=quartile100_matrix2, path_p=paste(path_temp_sub2, "5_quartile100", sep="/") )
    MyHypothesisTest_matrix_1_f( matrix_p=InterquartileRange_matrix2, path_p=paste(path_temp_sub2, "6_InterquartileRange", sep="/") )
    MyHypothesisTest_matrix_1_f( matrix_p=range_matrix2,    path_p=paste(path_temp_sub2, "7_range", sep="/") )
    MyHypothesisTest_matrix_1_f( matrix_p=trimean_matrix2,  path_p=paste(path_temp_sub2, "8_trimean", sep="/") )
    #MyHypothesisTest_matrix_1_f( matrix_p=variance_matrix2, path_p=paste(path_temp_sub2, "9_variance", sep="/") )
    #MyHypothesisTest_matrix_1_f( matrix_p=standardDeviation_matrix2, path_p=paste(path_temp_sub2, "10_standardDeviation", sep="/") )
    #MyHypothesisTest_matrix_1_f( matrix_p=Harmonic_mean_matrix2,     path_p=paste(path_temp_sub2, "11_Harmonic_mean", sep="/") )
    #MyHypothesisTest_matrix_1_f( matrix_p=Geometric_mean_matrix2,    path_p=paste(path_temp_sub2, "12_Geometric_mean", sep="/") )
    MyHypothesisTest_matrix_1_f( matrix_p=Arithmetic_mean_matrix2,   path_p=paste(path_temp_sub2, "13_Arithmetic_mean", sep="/") )
    #MyHypothesisTest_matrix_1_f( matrix_p=Quadratic_mean_matrix2,    path_p=paste(path_temp_sub2, "14_Quadratic_mean", sep="/") )
    #MyHypothesisTest_matrix_1_f( matrix_p=Trim_Arithmetic_mean_matrix2,   path_p=paste(path_temp_sub2, "15_Trim_Arithmetic_mean", sep="/") )
    #MyHypothesisTest_matrix_1_f( matrix_p=coefficientOfVariation_matrix2, path_p=paste(path_temp_sub2, "16_coefficientOfVariation_mean", sep="/") )
  



    ################# DNA methylation level for each group
    path_temp_sub3 = paste(path_temp, "3_Methylation-Level", sep="/")
    if( ! file.exists(path_temp_sub3) ) { dir.create(path_temp_sub3, recursive = TRUE) }
    
    MyHypothesisTest_matrix_1_f( matrix_p=matrix_temp,   path_p=paste(path_temp_sub3, "Hypothesis-Test", sep="/") )
    write.table(matrix_temp , 
              file = paste(path_temp_sub3,   "MeLevel.matrix.txt",  sep="/"), 
              append = FALSE, quote = FALSE, sep = "\t", eol = "\n", na = "NA", dec = ".", 
              row.names = FALSE,  col.names = TRUE, qmethod = c("escape", "double"),  fileEncoding = "")  

    vector_local       = as.vector(matrix_temp)
    vector_type_local  = rep(x= dataFrame_temp$mytech ,  each=nrow(matrix_temp)  )
    vector_sex_local   = rep(x= dataFrame_temp$mysex ,   each=nrow(matrix_temp)  )
  
    MyBoxViolinPlot_2_f(vector2   = vector_local ,   
                      sampleType2 = vector_sex_local ,  
                      sampleType3 = vector_type_local  ,  
                      colours2    = MyTech_color_g(  mytech2_level  ),   
                      path2       = path_temp_sub3,   
                      fileName2   = "1_all-Samples",  
                      title2="All Samples ",  xLab2="Samples",  yLab2="Methylation Level (%)",    
                      height2=4,   width2=length( mytech2_level )/1.5 + length( levels(vector_sex_local) ) + 1,   
                      Ymin2=0, Ymax2=100)
  
    MyBoxViolinPlot_1_f(vector2   = vector_local ,   
                      sampleType2 = vector_type_local ,  
                      colours2    = MyTech_color_g(  mytech2_level  ),   
                      path2       = path_temp_sub3,   
                      fileName2   = "2_only-by-Type",  
                      title2      = "All Samples ",  xLab2="Samples",  yLab2="Methylation Level (%)",    
                      height2=4,   width2=length( mytech2_level )/1.3 + 1,   Ymin2=0, Ymax2=100)
  
    MyBoxViolinPlot_1_f(vector2   = vector_local ,   
                      sampleType2 = vector_sex_local ,  
                      colours2    = MyTech_color_g(  mytech2_level  ),   
                      path2       = path_temp_sub3,   
                      fileName2   = "3_only-by-Sex",  
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
    path_temp_sub4 = paste(path_temp, "4_Methylation-Level-More", sep="/")
    if( ! file.exists(path_temp_sub4) ) { dir.create(path_temp_sub4, recursive = TRUE) }


     vector_local_5categories = vector_local
     vector_local_5categories[ is.na(vector_local)   ] = "1Lowest"
     vector_local_5categories[ (vector_local>=0 ) & (vector_local<10) ]  = "1Lowest"
     vector_local_5categories[ (vector_local>=10) & (vector_local<40) ]  = "2Low"
     vector_local_5categories[ (vector_local>=40) & (vector_local<60) ]  = "3Medium"
     vector_local_5categories[ (vector_local>=60) & (vector_local<90) ]  = "4High"
     vector_local_5categories[ (vector_local>=90) & (vector_local<=100)] = "5Highest"

    MyHistogram_more1_f(vector2=vector_type_local,   sampleType2=vector_local_5categories ,  
                      path2=path_temp_sub4,   fileName2="1_by-Type-Histogram",    
                      title2="By type",  xLab2="Type",   height2=4,  width2=length( mytech2_level )/2+2 )

    MyHistogram_more1_f(vector2=vector_sex_local,   sampleType2=vector_local_5categories ,  
                      path2=path_temp_sub4,   fileName2="2_by-Sex-Histogram",    
                      title2="By Gender",  xLab2="Gender",   height2=4,  width2=length( levels(vector_sex_local) )/2+2 )

    MyHistogram_1_f(vector2   = vector_local ,   
                    sampleType2 = vector_type_local ,  
                    colours2    = MyTech_color_g(  mytech2_level  ),   
                    path2       = path_temp_sub4,   
                    fileName2   = "3_by-Type-Probability-Density",  
                    title2      = "By type",  xLab2="Methylation Level (%)",     
                    height2=4,   width2=8,   xMin2=0,  xMax2=100,   yMin2=0,  yMax2=0.06)

  }
  tryCatch(
    myTempFunction(),
    error = function(err){"descriptiveStatistics_f_000258951"}
  ) 

}


 





















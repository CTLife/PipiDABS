## T test and Wilcoxon test  (unpaired).
MyHypothesisTest_1_f <- function(vector1, vector2, file1) {
  myTempFunction <- function() {

  sink(file=file1)
  print("######################## Apply continuity correction in the normal approximation for the p-value. ###############################################")
  
  print("##################################################################################################################################for comparing boxplot")
  wilcoxTest_2 <- wilcox.test(x=vector1, y=vector2, alternative="two.sided",  mu=0,   paired=FALSE,  exact=FALSE, correct=TRUE,  conf.int=TRUE,  conf.level=0.95)
  print( wilcoxTest_2  )
  cat( "\n\nExact p-value:", wilcoxTest_2$p.value, "\n\n\n\n\n" ) 
  
  print("##################################################################################################################################")
  wilcoxTest_4 <- wilcox.test(x=vector1, y=vector2, alternative="less",       mu=0,   paired=FALSE,  exact=FALSE, correct=TRUE,  conf.int=TRUE,  conf.level=0.95)
  print( wilcoxTest_4  )
  cat( "\n\nExact p-value:", wilcoxTest_4$p.value, "\n\n\n\n\n" )
  print("##################################################################################################################################")
  
  print("##################################################################################################################################")
  wilcoxTest_6 <- wilcox.test(x=vector1, y=vector2, alternative="greater",    mu=0,   paired=FALSE,  exact=FALSE, correct=TRUE,  conf.int=TRUE,  conf.level=0.95)
  print( wilcoxTest_6  )
  cat( "\n\nExact p-value:", wilcoxTest_6$p.value, "\n\n\n\n\n\n" )
  
  
  print("######################## Don't apply continuity correction in the normal approximation for the p-value. ###############################################")
  print("##################################################################################################################################")
  
  print("##################################################################################################################################")
  wilcoxTestB_2 <- wilcox.test(x=vector1, y=vector2, alternative="two.sided",  mu=0,   paired=FALSE,  exact=FALSE, correct=FALSE,  conf.int=TRUE,  conf.level=0.95)
  print( wilcoxTestB_2  )
  cat( "\n\nExact p-value:", wilcoxTestB_2$p.value, "\n\n\n\n\n" ) 
  print("##################################################################################################################################")
  
  print("##################################################################################################################################")
  wilcoxTestB_4 <- wilcox.test(x=vector1, y=vector2, alternative="less",       mu=0,   paired=FALSE,  exact=FALSE, correct=FALSE,  conf.int=TRUE,  conf.level=0.95)
  print( wilcoxTestB_4  )
  cat( "\n\nExact p-value:", wilcoxTestB_4$p.value, "\n\n\n\n\n" )
  print("##################################################################################################################################")
  
  print("##################################################################################################################################")
  wilcoxTestB_6 <- wilcox.test(x=vector1, y=vector2, alternative="greater",    mu=0,   paired=FALSE,  exact=FALSE, correct=FALSE,  conf.int=TRUE,  conf.level=0.95)
  print( wilcoxTestB_6  )
  cat( "\n\nExact p-value:", wilcoxTestB_6$p.value, "\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n" )
  
  
  
  print("######################## T-test, var.equal=FALSE. ###############################################")
  print("##################################################################################################################################")
  
  print("##################################################################################################################################")
  tTest_2 <- t.test(x=vector1, y=vector2, alternative="two.sided",  mu=0,   paired=FALSE,   var.equal=FALSE,  conf.level=0.95)
  print( tTest_2  )
  cat( "\n\nExact p-value:", tTest_2$p.value, "\n\n\n\n\n" )
  print("##################################################################################################################################")
  
  print("##################################################################################################################################")
  tTest_4 <- t.test(x=vector1, y=vector2, alternative="less",       mu=0,   paired=FALSE,   var.equal=FALSE,  conf.level=0.95)
  print( tTest_4  )
  cat( "\n\nExact p-value:", tTest_4$p.value, "\n\n\n\n\n" )
  print("##################################################################################################################################")
  
  print("##################################################################################################################################")
  tTest_6 <- t.test(x=vector1, y=vector2, alternative="greater",    mu=0,   paired=FALSE,   var.equal=FALSE,  conf.level=0.95)
  print( tTest_6  )
  cat( "\n\nExact p-value:", tTest_6$p.value, "\n\n\n\n\n\n" )
  
  
  print("######################## T-test, var.equal=TRUE. ##############################################################################################")
  
  print("##################################################################################################################################")
  tTestB_2 <- t.test(x=vector1, y=vector2, alternative="two.sided",  mu=0,   paired=FALSE,   var.equal=TRUE,  conf.level=0.95)
  print( tTestB_2  )
  cat( "\n\nExact p-value:", tTestB_2$p.value, "\n\n\n\n\n" )
  print("##################################################################################################################################")
  
  print("##################################################################################################################################")
  tTestB_4 <- t.test(x=vector1, y=vector2, alternative="less",       mu=0,   paired=FALSE,   var.equal=TRUE,  conf.level=0.95)
  print( tTestB_4 )
  cat( "\n\nExact p-value:", tTestB_4$p.value, "\n\n\n\n\n" )
  
  print("##################################################################################################################################")
  tTestB_6 <- t.test(x=vector1, y=vector2, alternative="greater",    mu=0,   paired=FALSE,   var.equal=TRUE,  conf.level=0.95)
  print( tTestB_6  )
  cat( "\n\nExact p-value:", tTestB_6$p.value, "\n\n\n\n\n" )
  
  sink()
 
  }
  tryCatch(
    myTempFunction(),
    error = function(err){"descriptiveStatistics_f_9999999999999"}
  ) 

}





## T test and Wilcoxon test  (paired)
MyHypothesisTest_2_f <- function(vector1, vector2, file1) {
  myTempFunction <- function() {

  sink(file=file1)
  
  print("######################## Apply continuity correction in the normal approximation for the p-value. ###############################################")
  print("##################################################################################################################################")
  wilcoxTest_1 <- wilcox.test(x=vector1, y=vector2, alternative="two.sided",  mu=0,   paired=TRUE,   exact=TRUE, correct=TRUE,  conf.int=TRUE,  conf.level=0.95)
  print( wilcoxTest_1  )
  cat( "\n\nExact p-value:", wilcoxTest_1$p.value, "\n\n\n\n\n" )
  
  print("##################################################################################################################################")
  wilcoxTest_3 <- wilcox.test(x=vector1, y=vector2, alternative="less",       mu=0,   paired=TRUE,   exact=TRUE, correct=TRUE,  conf.int=TRUE,  conf.level=0.95)
  print( wilcoxTest_3  )
  cat( "\n\nExact p-value:", wilcoxTest_3$p.value, "\n\n\n\n\n" ) 
  
  print("##################################################################################################################################")
  wilcoxTest_5 <- wilcox.test(x=vector1, y=vector2, alternative="greater",    mu=0,   paired=TRUE,   exact=TRUE, correct=TRUE,  conf.int=TRUE,  conf.level=0.95)
  print( wilcoxTest_5  )
  cat( "\n\nExact p-value:", wilcoxTest_5$p.value, "\n\n\n\n\n" )
  
  
  print("######################## Don't apply continuity correction in the normal approximation for the p-value. ###############################################")
  print("##################################################################################################################################")
  wilcoxTestB_1 <- wilcox.test(x=vector1, y=vector2, alternative="two.sided",  mu=0,   paired=TRUE,   exact=TRUE, correct=FALSE,  conf.int=TRUE,  conf.level=0.95)
  print( wilcoxTestB_1  )
  cat( "\n\nExact p-value:", wilcoxTestB_1$p.value, "\n\n\n\n\n" )
  
  print("##################################################################################################################################")
  wilcoxTestB_3 <- wilcox.test(x=vector1, y=vector2, alternative="less",       mu=0,   paired=TRUE,   exact=TRUE, correct=FALSE,  conf.int=TRUE,  conf.level=0.95)
  print( wilcoxTestB_3 )
  cat( "\n\nExact p-value:", wilcoxTestB_3$p.value, "\n\n\n\n\n" ) 
  
  print("##################################################################################################################################")
  wilcoxTestB_5 <- wilcox.test(x=vector1, y=vector2, alternative="greater",    mu=0,   paired=TRUE,   exact=TRUE, correct=FALSE,  conf.int=TRUE,  conf.level=0.95)
  print( wilcoxTestB_5  )
  cat( "\n\nExact p-value:", wilcoxTestB_5$p.value, "\n\n\n\n\n" )
  
  
  
  
  print("######################## T-test, var.equal=FALSE. ###############################################")
  print("##################################################################################################################################")
  tTest_1 <- t.test(x=vector1, y=vector2, alternative="two.sided",  mu=0,   paired=TRUE,    var.equal=FALSE,  conf.level=0.95)
  print( tTest_1  )
  cat( "\n\nExact p-value:", tTest_1$p.value, "\n\n\n\n\n" )
  
  print("##################################################################################################################################")
  tTest_3 <- t.test(x=vector1, y=vector2, alternative="less",       mu=0,   paired=TRUE,    var.equal=FALSE,  conf.level=0.95)
  print( tTest_3  )
  cat( "\n\nExact p-value:", tTest_3$p.value, "\n\n\n\n\n" )
  
  print("##################################################################################################################################")
  tTest_5 <- t.test(x=vector1, y=vector2, alternative="greater",    mu=0,   paired=TRUE,    var.equal=FALSE,  conf.level=0.95)
  print( tTest_5  )
  cat( "\n\nExact p-value:", tTest_5$p.value, "\n\n\n\n\n" )
  
  
  print("######################## T-test, var.equal=TRUE. ##############################################################################################")
  print("##################################################################################################################################")
  tTestB_1 <- t.test(x=vector1, y=vector2, alternative="two.sided",  mu=0,   paired=TRUE,    var.equal=TRUE,  conf.level=0.95)
  print( tTestB_1  )
  cat( "\n\nExact p-value:", tTestB_1$p.value, "\n\n\n\n\n" )
  
  print("##################################################################################################################################")
  tTestB_3 <- t.test(x=vector1, y=vector2, alternative="less",       mu=0,   paired=TRUE,    var.equal=TRUE,  conf.level=0.95)
  print( tTestB_3  )
  cat( "\n\nExact p-value:", tTestB_3$p.value, "\n\n\n\n\n" )
  
  print("##################################################################################################################################")
  tTestB_5 <- t.test(x=vector1, y=vector2, alternative="greater",    mu=0,   paired=TRUE,    var.equal=TRUE,  conf.level=0.95)
  print( tTestB_5  )
  cat( "\n\nExact p-value:", tTestB_5$p.value, "\n\n\n\n\n" )
  
  sink() 
 
  }
  tryCatch(
    myTempFunction(),
    error = function(err){"descriptiveStatistics_f_9999999999999"}
  ) 

}





## Kolmogorov-Smirnov tests
MyHypothesisTest_3_f <- function(vector1, vector2, file1) {
  myTempFunction <- function() {

  sink(file=file1)
  
  print("######################## two sides ###############################################")
  print("##################################################################################################################################")
  ksTest_1 <- ks.test(x=vector1, y=vector2, alternative="two.sided",    exact=TRUE )
  print( ksTest_1  )
  cat( "\n\nExact p-value:", ksTest_1$p.value, "\n\n\n\n\n" )
  
  print("######################## less ###############################################")
  print("##################################################################################################################################")
  ksTest_2 <- ks.test(x=vector1, y=vector2, alternative="less",    exact=TRUE )
  print( ksTest_2  )
  cat( "\n\nExact p-value:", ksTest_2$p.value, "\n\n\n\n\n" )
  
  print("######################## greater ###############################################")
  print("##################################################################################################################################")
  ksTest_3 <- ks.test(x=vector1, y=vector2, alternative="greater",    exact=TRUE )
  print( ksTest_3  )
  cat( "\n\nExact p-value:", ksTest_3$p.value, "\n\n\n\n\n" )
  
  sink() 
 
  }
  tryCatch(
    myTempFunction(),
    error = function(err){"descriptiveStatistics_f_9999999999999"}
  ) 

}





## T test and Wilcoxon test for large datasets.  
MyHypothesisTest_4_f <- function(vector1, vector2, file1) {
  myTempFunction <- function() {

  sink(file=file1)
  
  print("######################## Apply continuity correction in the normal approximation for the p-value. ###############################################")
  print("##################################################################################################################################")
  wilcoxTest_1 <- wilcox.test(x=vector1, y=vector2 )
  print( wilcoxTest_1  )
  cat( "\n\nExact p-value:", wilcoxTest_1$p.value, "\n\n\n\n\n" )
  

  print("######################## T-test, var.equal=FALSE. ###############################################")
  print("##################################################################################################################################")
  tTest_1 <- t.test(x=vector1, y=vector2 )
  print( tTest_1  )
  cat( "\n\nExact p-value:", tTest_1$p.value, "\n\n\n\n\n" )
  

  sink() 
 
  }
  tryCatch(
    myTempFunction(),
    error = function(err){"descriptiveStatistics_f_9999999999999"}
  ) 

}





## compare any two columns of matrix
MyHypothesisTest_matrix_1_f <- function( matrix_p, path_p ) {
  myTempFunction <- function() {

    if( ! file.exists(path_p) ) { dir.create(path_p, recursive = TRUE) }    
    matrix_p = as.matrix(matrix_p)  
    sink(file= paste(path_p, "column_names_of_matrix.txt", sep="/") )
        print( "dim(matrix_p):" )
        print( dim(matrix_p) )   
        cat("\n\n\n")
        print( "colnames(matrix_p):" )
        print( colnames(matrix_p) )
        cat("\n\n\n")
    sink()
    for( i in c(1:ncol(matrix_p)) ) {
        for( j in c(i:ncol(matrix_p)) ) {
            myPath_temp = paste(path_p, "/", i, "_", j, sep="" )
            if( ! file.exists(myPath_temp) ) { dir.create(myPath_temp, recursive = TRUE) }  
            MyHypothesisTest_4_f( vector1=matrix_p[,i], vector2=matrix_p[,j], file1=paste(myPath_temp, "T-test_and_Wilcoxon-test_for_large_datasets.txt", sep="/") )   
            MyHypothesisTest_3_f( vector1=matrix_p[,i], vector2=matrix_p[,j], file1=paste(myPath_temp, "Kolmogorov-Smirnov_tests.txt", sep="/") )   
            #MyHypothesisTest_2_f( vector1=matrix_p[,i], vector2=matrix_p[,j], file1=paste(myPath_temp, "MoreDetails_Paired_T-test_and_Wilcoxon-test.txt", sep="/") )   
            #MyHypothesisTest_1_f( vector1=matrix_p[,i], vector2=matrix_p[,j], file1=paste(myPath_temp, "MoreDetails_unpaired_T-test_and_Wilcoxon-test.txt", sep="/") )   
        }
    }   
 
  }
  tryCatch(
    myTempFunction(),
    error = function(err){"MyHypothesisTest_matrix_1_f_9999999999999"}
  ) 
}






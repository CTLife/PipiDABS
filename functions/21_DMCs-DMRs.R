 


myDiff_DMC_DMR_g <- function(  methobj2,   path2, qvalue2,  differenceOfMethylation2, mergeDistance2, numCores2   ) {
  myTempFunction <- function() {

  sink(paste(path2,  "0_methobj2-for-diffMe.txt",  sep="/"))
  print(methobj2)
  sink() 
  
  sink( file=paste(path2, "values_of_parameters.txt", sep="/") )  
     print("path2:")
     print(path2)
     cat("\n\n\n")
     print("qvalue2:")
     print(qvalue2)
     cat("\n\n\n")
     print("differenceOfMethylation2:")
     print(differenceOfMethylation2)
     cat("\n\n\n")
     print("mergeDistance2:")
     print(mergeDistance2)
     cat("\n\n\n")
     print("numCores2:")
     print(numCores2)
     cat("\n\n\n")
  sink()   


  ## Depending on the sample size per each set it will either use Fisher’s exact or logistic 
  ##  regression to calculate P-values.
  ##  If you have replicates, the function will automatically use logistic regression.
  cat("\n\n\n\n\n")
  print("################## start calculateDiffMeth:")
  myDiff_2two_sub2 = calculateDiffMeth(methobj2,  mc.cores=numCores2 )   
  print("################## End calculateDiffMeth") 
  cat("\n\n\n\n\n") 

  sink( file=paste(path2, "myDiff_2two_sub2.txt", sep="/") )     
  dim(myDiff_2two_sub2)
  names(myDiff_2two_sub2)
  head(myDiff_2two_sub2)
  sink()


  myOutDir_sub2_g3 = paste(path2, "/Segmentation",  sep="") 
  if( ! file.exists(myOutDir_sub2_g3) ) { dir.create(myOutDir_sub2_g3, recursive = TRUE) }

  myTempFunction_Segmentation <- function() {
      pdf( file=paste(path2, "Segmentation.pdf", sep="/")  )
      res_temp_1 = methSeg( myDiff_2two_sub2 , diagnostic.plot=TRUE, maxInt=100,  minSeg=10,  G=1:5)
      methSeg2bed(res_temp_1,   filename= paste(myOutDir_sub2_g3, "Segmentation.bed", sep="/") )
      dev.off()
  }
  tryCatch(
    myTempFunction_Segmentation(),
    error = function(err){"myTempFunction_Segmentation_987654321"}
  )


  myQvalue_2two_sub2 = myDiff_2two_sub2$qvalue
  myMethDi_2two_sub2 = myDiff_2two_sub2$meth.diff
  
  pdf(paste(path2,  "1A_qvalue_distribution.pdf",  sep="/"))
  print( hist( myQvalue_2two_sub2,  nclass=100, xlim=c(0, 1),    freq=FALSE) )
  print( hist( myQvalue_2two_sub2,  nclass=20,  xlim=c(0, 0.2),  freq=FALSE) )
  print( hist( myQvalue_2two_sub2,  nclass=20,  xlim=c(0, 0.1),  freq=FALSE) )
  print( hist( myQvalue_2two_sub2,  nclass=20,  xlim=c(0, 0.05), freq=FALSE) )
  print( hist( myQvalue_2two_sub2,  nclass=20,  xlim=c(0, 0.01), freq=FALSE) )   
  print( hist( myQvalue_2two_sub2,  nclass=100, xlim=c(0, 1),    freq=TRUE ) )
  print( hist( myQvalue_2two_sub2,  nclass=20,  xlim=c(0, 0.2),  freq=TRUE ) )
  print( hist( myQvalue_2two_sub2,  nclass=20,  xlim=c(0, 0.1),  freq=TRUE ) )
  print( hist( myQvalue_2two_sub2,  nclass=20,  xlim=c(0, 0.05), freq=TRUE ) )
  print( hist( myQvalue_2two_sub2,  nclass=20,  xlim=c(0, 0.01), freq=TRUE ) )   
  dev.off() 

  myQvalue_2two_sub2_log10 <- -log10(myQvalue_2two_sub2)
  pdf(paste(path2,  "1B_qvalue_distribution_log10.pdf",  sep="/"))
  print( hist( myQvalue_2two_sub2_log10,  nclass=100, xlim=c(0, 100), freq=FALSE) )
  print( hist( myQvalue_2two_sub2_log10,  nclass=20,  xlim=c(0, 10),  freq=FALSE) )
  print( hist( myQvalue_2two_sub2_log10,  nclass=20,  xlim=c(0, 2),   freq=FALSE) )
  print( hist( myQvalue_2two_sub2_log10,  nclass=100, xlim=c(0, 100), freq=TRUE ) )
  print( hist( myQvalue_2two_sub2_log10,  nclass=20,  xlim=c(0, 10),  freq=TRUE ) )
  print( hist( myQvalue_2two_sub2_log10,  nclass=20,  xlim=c(0, 2),   freq=TRUE ) )
  dev.off() 
  
  pdf(paste(path2,  "1C_methDiff_distribution.pdf",  sep="/"))
  print( hist(myMethDi_2two_sub2, nclass=100, xlim=c(0, 100),  freq=FALSE) )
  print( hist(myMethDi_2two_sub2, nclass=50,  xlim=c(0, 50),   freq=FALSE) )
  print( hist(myMethDi_2two_sub2, nclass=30,  xlim=c(0, 30),   freq=FALSE) )
  print( hist(myMethDi_2two_sub2, nclass=20,  xlim=c(0, 20),   freq=FALSE) )
  print( hist(myMethDi_2two_sub2, nclass=10,  xlim=c(0, 10),   freq=FALSE) )
  print( hist(myMethDi_2two_sub2, nclass=100, xlim=c(0, 100),  freq=TRUE ) )
  print( hist(myMethDi_2two_sub2, nclass=50,  xlim=c(0, 50),   freq=TRUE ) )
  print( hist(myMethDi_2two_sub2, nclass=30,  xlim=c(0, 30),   freq=TRUE ) )
  print( hist(myMethDi_2two_sub2, nclass=20,  xlim=c(0, 20),   freq=TRUE ) )
  print( hist(myMethDi_2two_sub2, nclass=10,  xlim=c(0, 10),   freq=TRUE ) )
  dev.off() 
             
  qvalue_cutoff = qvalue2
  methDiff_cutoff = differenceOfMethylation2*100
  
  myColor1_2two_sub2 <- rep( "no",   times= length(myQvalue_2two_sub2) )
  myColor1_2two_sub2[ (abs(myMethDi_2two_sub2)>methDiff_cutoff) & (myQvalue_2two_sub2<qvalue_cutoff) ]  <- "yes"
  number_yes = length( myColor1_2two_sub2[myColor1_2two_sub2=="yes"] )
  
  if( number_yes < 30 ) {
    qvalue_cutoff = 0.01
    myColor1_2two_sub2[ (abs(myMethDi_2two_sub2)>methDiff_cutoff) & (myQvalue_2two_sub2<qvalue_cutoff) ]  <- "yes"
    number_yes = length( myColor1_2two_sub2[myColor1_2two_sub2=="yes"] )
  }
  if( number_yes < 30 ) {
    qvalue_cutoff = 0.05
    myColor1_2two_sub2[ (abs(myMethDi_2two_sub2)>methDiff_cutoff) & (myQvalue_2two_sub2<qvalue_cutoff) ]  <- "yes"
    number_yes = length( myColor1_2two_sub2[myColor1_2two_sub2=="yes"] )
  }
  if( number_yes < 30 ) {
    qvalue_cutoff = 0.1
    methDiff_cutoff = 5
    myColor1_2two_sub2[ (abs(myMethDi_2two_sub2)>methDiff_cutoff) & (myQvalue_2two_sub2<qvalue_cutoff) ]  <- "yes"
    number_yes = length( myColor1_2two_sub2[myColor1_2two_sub2=="yes"] )
  }
  if( number_yes < 30 ) {
    qvalue_cutoff = 0.5
    methDiff_cutoff = 1
    myColor1_2two_sub2[ (abs(myMethDi_2two_sub2)>methDiff_cutoff) & (myQvalue_2two_sub2<qvalue_cutoff) ]  <- "yes"
    number_yes = length( myColor1_2two_sub2[myColor1_2two_sub2=="yes"] )
  }
  
  print("##############################")
  print("##############################")
  print("The final parameters:")
  print(number_yes)
  print(qvalue_cutoff)
  print(methDiff_cutoff)
  print("##############################")
  print("##############################")

  
  DataFrame2_2two_sub2 <- data.frame(myx1 = myMethDi_2two_sub2,   
                                     myy1 =  -log10(myQvalue_2two_sub2),  
                                     mycolor1 = myColor1_2two_sub2 )
  
  ggplot(DataFrame2_2two_sub2, aes(x=myx1, y=myy1, color= mycolor1 ) ) + 
    geom_point( shape = 20, alpha = 0.5 ) + scale_colour_manual(values=c("no"="black", "yes"="red")) +
    xlab( "Difference (%)" ) + ylab( "-log10(q-value)" ) + MyTheme_1_g(textSize1=14)   +  ylim(0, 100)  
  ggsave( filename = paste(path2,  "1D_methDiff_qvalue-1.png",  sep="/"),  height=4, width=6, dpi = 1200 )
  
  ggplot(DataFrame2_2two_sub2, aes(x=myx1, y=myy1, color= mycolor1 ) ) + 
    geom_point( shape = 20, alpha = 0.5 ) + scale_colour_manual(values=c("no"="black", "yes"="red")) +
    xlab( "Difference (%)" ) + ylab( "-log10(q-value)" ) + MyTheme_1_g(textSize1=14)  + xlim(-40, 40)    +  ylim(0, 100)   
  ggsave( filename = paste(path2,  "1D_methDiff_qvalue-2.png",  sep="/"),  height=4, width=6, dpi = 1200 )
  
  ggplot(DataFrame2_2two_sub2, aes(x=myx1, y=myy1, color= mycolor1 ) ) + 
    geom_point( shape = 20, alpha = 0.5 ) + scale_colour_manual(values=c("no"="black", "yes"="red")) +
    xlab( "Difference (%)" ) + ylab( "-log10(q-value)" ) + MyTheme_1_g(textSize1=14)  + xlim(-20, 20)   +  ylim(0, 100)  
  ggsave( filename = paste(path2,  "1D_methDiff_qvalue-3.png",  sep="/"),  height=4, width=6, dpi = 1200 )
  
  
  ggplot(DataFrame2_2two_sub2, aes(x=myx1, y=myy1, color= mycolor1 ) ) + 
    geom_point( shape = 20, alpha = 0.5 ) + scale_colour_manual(values=c("no"="black", "yes"="red")) +
    xlab( "Difference (%)" ) + ylab( "-log10(q-value)" ) + MyTheme_1_g(textSize1=14)  + ylim(0, 50)  
  ggsave( filename = paste(path2,  "1E_methDiff_qvalue-1.png",  sep="/"),  height=4, width=6, dpi = 1200 )
  
  ggplot(DataFrame2_2two_sub2, aes(x=myx1, y=myy1, color= mycolor1 ) ) + 
    geom_point( shape = 20, alpha = 0.5 ) + scale_colour_manual(values=c("no"="black", "yes"="red")) +
    xlab( "Difference (%)" ) + ylab( "-log10(q-value)" ) + MyTheme_1_g(textSize1=14)  +  ylim(0, 20)  
  ggsave( filename = paste(path2,  "1E_methDiff_qvalue-2.png",  sep="/"),  height=4, width=6, dpi = 1200 )
  
  ggplot(DataFrame2_2two_sub2, aes(x=myx1, y=myy1, color= mycolor1 ) ) + 
    geom_point( shape = 20, alpha = 0.5 ) + scale_colour_manual(values=c("no"="black", "yes"="red")) +
    xlab( "Difference (%)" ) + ylab( "-log10(q-value)" ) + MyTheme_1_g(textSize1=14)  +  ylim(0, 10)  
  ggsave( filename = paste(path2,  "1E_methDiff_qvalue-3.png",  sep="/"),  height=4, width=6, dpi = 1200 )

  
  
  myDiff25p.hypo_2two_sub2  = getMethylDiff(myDiff_2two_sub2, difference=methDiff_cutoff, qvalue=qvalue_cutoff, type="hypo" )  ## less enrich in ART
  myDiff25p.hyper_2two_sub2 = getMethylDiff(myDiff_2two_sub2, difference=methDiff_cutoff, qvalue=qvalue_cutoff, type="hyper")  ## more enrich in ART
  myDiff25p_2two_sub2       = getMethylDiff(myDiff_2two_sub2, difference=methDiff_cutoff, qvalue=qvalue_cutoff)
  myDiffTemp_2two_sub2      = getMethylDiff(myDiff_2two_sub2, difference=0,  qvalue=0.05)
  
  write.table(myDiff_2two_sub2 , file = paste(path2,"2A_diffMe-allsites.txt",  sep="/"), 
              append = FALSE, quote = FALSE, sep = "\t", eol = "\n", na = "NA", dec = ".", 
              row.names = FALSE,  col.names = TRUE, qmethod = c("escape", "double"),  fileEncoding = "")
  
  write.table(myDiff25p.hypo_2two_sub2 , file = paste(path2,"2B_diffMe-hypo.txt",  sep="/"), 
              append = FALSE, quote = FALSE, sep = "\t", eol = "\n", na = "NA", dec = ".", 
              row.names = FALSE,  col.names = TRUE, qmethod = c("escape", "double"),  fileEncoding = "")
  
  write.table(myDiff25p.hyper_2two_sub2 , file = paste(path2,"2C_diffMe-hyper.txt",  sep="/"), 
              append = FALSE, quote = FALSE, sep = "\t", eol = "\n", na = "NA", dec = ".", 
              row.names = FALSE,  col.names = TRUE, qmethod = c("escape", "double"),  fileEncoding = "")
  
  write.table(myDiff25p_2two_sub2 , file = paste(path2,"2D_AlldiffMesites.txt",  sep="/"), 
              append = FALSE, quote = FALSE, sep = "\t", eol = "\n", na = "NA", dec = ".", 
              row.names = FALSE,  col.names = TRUE, qmethod = c("escape", "double"),  fileEncoding = "")
  
  write.table(myDiffTemp_2two_sub2 , file = paste(path2,"2E_AlldiffMesites_q0.05_diff0.txt",  sep="/"), 
              append = FALSE, quote = FALSE, sep = "\t", eol = "\n", na = "NA", dec = ".", 
              row.names = FALSE,  col.names = TRUE, qmethod = c("escape", "double"),  fileEncoding = "")
  
  
  sink( file=paste(path2, "3A-distribution-of-hypoORhyper-methylated-bases-regions-per-chromosome.txt", sep="/")   )
  print( diffMethPerChr(myDiff_2two_sub2,  plot=FALSE,  qvalue.cutoff=qvalue_cutoff, meth.cutoff=methDiff_cutoff) )
  sink()
  
  pdf( file=paste(path2, "3B-distribution-of-hypoORhyper-methylated-bases-regions-per-chromosome.pdf", sep="/"), width=8, height=8    )
  print( diffMethPerChr(myDiff_2two_sub2,  plot=TRUE,  qvalue.cutoff=qvalue_cutoff, meth.cutoff=methDiff_cutoff) )
  dev.off()
  

  sink(file=paste(path2, "myDMRs_annotation_g_Hypo.runLog.txt", sep="/")) 
  tryCatch(
        myDMRs_annotation_g(  myDiffDMR_5 = myDiff25p.hypo_2two_sub2,    path2_5 = paste(path2, "Annotation_Hypo",  sep="/") )  ,
        error = function(err){"myDMRs_annotation_g_1_000"}
  )
  sink()

  sink(file=paste(path2, "myDMRs_annotation_g_Hyper.runLog.txt", sep="/")) 
  tryCatch(
        myDMRs_annotation_g(  myDiffDMR_5 = myDiff25p.hyper_2two_sub2,   path2_5 = paste(path2, "Annotation_Hyper", sep="/") ), 
        error = function(err){"myDMRs_annotation_g_2_000"}
  )
  sink()

  sink(file=paste(path2, "myDMRs_annotation_g_All.runLog.txt", sep="/")) 
  tryCatch(
        myDMRs_annotation_g(  myDiffDMR_5 = myDiff25p_2two_sub2,         path2_5 = paste(path2, "Annotation_All",   sep="/") ),
        error = function(err){"myDMRs_annotation_g_3_000"}
  )
  sink() 



  mySelectDiffMe( path_temp3=path2, file_temp3="2A_diffMe-allsites.txt" ) 
 
  }
  tryCatch(
    myTempFunction(),
    error = function(err){"myDiff_DMC_DMR_g_9999999999999"}
  ) 
  tryCatch(
    myTempFunction101(),
    error = function(err){"myTempFunction101_1bp_000111333"}
  )
  
}






















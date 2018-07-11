


My_for_MultidimensionalScaling_1_g <- function(  path3,   dataFrame_temp3  ) {
  FigureTemp1_2two  <- ggplot( data = dataFrame_temp3, aes(x = myX, y = myY, shape=as.factor(mySex), color=as.factor(myTech) )) + 
    geom_point(size=3, alpha=1  ) + xlab("Dimension 1") +   ylab("Dimension 2") +   
    scale_colour_manual(values=MyTech_color_g( as.vector(dataFrame_temp3$myTech) )  ) +  scale_shape_manual(values=MySex_Shape_g( as.vector(dataFrame_temp3$mySex) )) + 
    MyTheme_1_g( textSize=14,  hjust1=NULL, vjust1=NULL,  angle1=NULL) +
    guides( colour = guide_legend(override.aes = list(size=5)) ,  shape = guide_legend(override.aes = list(size=5)))  
  MySaveGgplot2_1_g(ggplot2Figure1=FigureTemp1_2two ,  path1=path3, fileName1="A_MDS",  height1=3.5,  width1=6)
  
  FigureTemp2_2two  <- ggplot( data = dataFrame_temp3, aes(x = myX, y = myY, shape=as.factor(mySex), color=as.factor(myTech) )) + 
    geom_point(size=5, alpha=0.5  )+ xlab("Dimension 1") +   ylab("Dimension 2") +   
    scale_colour_manual(values=MyTech_color_g( as.vector(dataFrame_temp3$myTech) )  ) +  scale_shape_manual(values=MySex_Shape_g( as.vector(dataFrame_temp3$mySex) )) +  
    MyTheme_1_g( textSize=14,  hjust1=NULL, vjust1=NULL,  angle1=NULL) +
    guides( colour = guide_legend(override.aes = list(size=5)) ,  shape = guide_legend(override.aes = list(size=5))) 
  MySaveGgplot2_1_g(ggplot2Figure1=FigureTemp2_2two ,  path1=path3, fileName1="B_MDS-alpha",   height1=3.5,  width1=6)
  
  FigureTemp3_2two  <- ggplot( data = dataFrame_temp3, aes(x = myX, y = myY, shape=as.factor(mySex), color=as.factor(myTech) )) + 
    geom_point(size=2, alpha=1  ) + xlab("Dimension 1") +   ylab("Dimension 2") +   
    scale_colour_manual(values=MyTech_color_g( as.vector(dataFrame_temp3$myTech) )  ) +  scale_shape_manual(values=MySex_Shape_g( as.vector(dataFrame_temp3$mySex) )) + 
    MyTheme_1_g( textSize=14,  hjust1=NULL, vjust1=NULL,  angle1=NULL) +
    guides( colour = guide_legend(override.aes = list(size=5)) ,  shape = guide_legend(override.aes = list(size=5))) 
  MySaveGgplot2_1_g(ggplot2Figure1=FigureTemp3_2two ,  path1=path3, fileName1="C_MDS-smallDot",   height1=3.5,  width1=6)
  
  FigureTemp3_2two  <- ggplot( data = dataFrame_temp3, aes(x = myX, y = myY, shape=as.factor(mySex), color=as.factor(myTech) )) + 
    geom_point(size=2, alpha=0.5  ) + xlab("Dimension 1") +   ylab("Dimension 2") +   
    scale_colour_manual(values=MyTech_color_g( as.vector(dataFrame_temp3$myTech) )  ) +  scale_shape_manual(values=MySex_Shape_g( as.vector(dataFrame_temp3$mySex) )) + 
    MyTheme_1_g( textSize=14,  hjust1=NULL, vjust1=NULL,  angle1=NULL) +
    guides( colour = guide_legend(override.aes = list(size=5)) ,  shape = guide_legend(override.aes = list(size=5))) 
  MySaveGgplot2_1_g(ggplot2Figure1=FigureTemp3_2two ,  path1=path3, fileName1="D_MDS-smallDot-alpha",   height1=3.5,  width1=6)
  
  FigureTemp4 <- ggplot( data = dataFrame_temp3, aes(x = myX, y = myY, shape=as.factor(mySex), color=as.factor(myTech) )) + 
    geom_point(size=6, alpha=0.5  ) + xlab("Dimension 1") +   ylab("Dimension 2") +     
    scale_colour_manual(values=MyTech_color_g( as.vector(dataFrame_temp3$myTech) )  ) +  scale_shape_manual(values=MySex_Shape_g( as.vector(dataFrame_temp3$mySex) )) +  
    MyTheme_1_g( textSize=14,  hjust1=NULL, vjust1=NULL,  angle1=NULL) +
    guides( colour = guide_legend(override.aes = list(size=5)) ,  shape = guide_legend(override.aes = list(size=5))) 
  MySaveGgplot2_1_g(ggplot2Figure1=FigureTemp4_2two ,  path1=path3, fileName1="E_MDS-big",   height1=3.5,  width1=6)
  
  FigureTemp5_2two  <- ggplot( data = dataFrame_temp3, aes(x = myX, y = myY, shape=as.factor(mySex), color=as.factor(myTech), label=myLabel )) + 
    geom_point(size=2, alpha=0.7  ) + xlab("Dimension 1") +   ylab("Dimension 2") +   
    scale_colour_manual(values=MyTech_color_g( as.vector(dataFrame_temp3$myTech) )  ) +  scale_shape_manual(values=MySex_Shape_g( as.vector(dataFrame_temp3$mySex) )) + 
    MyTheme_1_g( textSize=14,  hjust1=NULL, vjust1=NULL,  angle1=NULL) + geom_text_repel(aes(label = myLabel)) +
    guides( colour = guide_legend(override.aes = list(size=5)) ,  shape = guide_legend(override.aes = list(size=5))) 
  MySaveGgplot2_1_g(ggplot2Figure1=FigureTemp5_2two ,  path1=path3, fileName1="F_MDS-Labels",   height1=3.5,  width1=6)
  
  FigureTemp6_2two  <- ggplot( data = dataFrame_temp3, aes(x = myX, y = myY, shape=as.factor(mySex), color=as.factor(myTech), label=myLabel )) + 
    geom_point(size=5, alpha=0.5  ) + xlab("Dimension 1") +   ylab("Dimension 2") +   
    scale_colour_manual(values=MyTech_color_g( as.vector(dataFrame_temp3$myTech) )  ) +  scale_shape_manual(values=MySex_Shape_g( as.vector(dataFrame_temp3$mySex) )) + 
    MyTheme_1_g( textSize=14,  hjust1=NULL, vjust1=NULL,  angle1=NULL) + geom_text_repel(aes(label = myLabel  )) +
    guides( colour = guide_legend(override.aes = list(size=5)) ,  shape = guide_legend(override.aes = list(size=5))) 
  MySaveGgplot2_1_g(ggplot2Figure1=FigureTemp6_2two ,  path1=path3, fileName1="G_MDS-Labels",   height1=3.5,  width1=6)
  
}





MyMultidimensionalScaling_1_g <- function(  meLevelMatrix2,   path2,   dataFrame_temp2 ) {
  if( ! file.exists(path2) ) { dir.create(path2, recursive = TRUE) }
  path2_temp1 = paste(path2, "/Classical-Metric-Multidimensional-Scaling", sep="")
  if( ! file.exists(path2_temp1) ) { dir.create(path2_temp1, recursive = TRUE) }
  res.dist_temp1 <- get_dist( t(meLevelMatrix2) ,   method = "euclidean")
  mds_temp1 = cmdscale(res.dist_temp1,   k = 2)
  dataframeA_temp1  <- data.frame( myX=mds_temp1[,1],  myY=mds_temp1[,2],  myLabel=rownames(mds_temp1),
                                   mySex= as.vector(dataFrame_temp2$mysex),  myTech=as.vector(dataFrame_temp2$mytech)  ) 
  My_for_MultidimensionalScaling_1_g(  path3=path2_temp1,   dataFrame_temp3=dataframeA_temp1  )
  
  path2_temp2 = paste(path2, "/Nonmetric-Multidimensional-Scaling", sep="")
  if( ! file.exists(path2_temp2) ) { dir.create(path2_temp2, recursive = TRUE) }
  res.dist_temp2 <- res.dist_temp1
  mds_temp2 = isoMDS(res.dist_temp2,   k = 2)
  dataframeA_temp2  <- data.frame( myX=mds_temp2$points[,1],  myY=mds_temp2$points[,2],  myLabel=rownames(mds_temp2$points),
                                   mySex= as.vector(dataFrame_temp2$mysex),  myTech=as.vector(dataFrame_temp2$mytech)  ) 
  My_for_MultidimensionalScaling_1_g(  path3=path2_temp2,   dataFrame_temp3=dataframeA_temp2  )
  
  cat("\n\n\n\n\nMDS:")
  cat(mds_temp1)
  cat("\n\n")
  print(mds_temp2)
  cat("\n\n\n\n\n")
  
  sink( file=paste(path2, "MDS_info.txt", sep="/") )
  cat("\n\n\n\n\nMDS:\n\n")
  cat(mds_temp1)
  cat("\n\n\n\n\n\n\n")
  print(mds_temp2)
  cat("\n\n\n\n\n")
  sink() 


  ClassicalMetric_top20 = cmdscale(res.dist_temp1,   k = 20) 
  Nonmetric_top20       = isoMDS(res.dist_temp1,   k = 20) 

  isoMDS(res.dist_temp2,   k = 20)
  sink( file=paste(path2, "MDS_info_top20.txt", sep="/") )
  cat( "Classical-Metric-Multidimensional-Scaling:\n" )
  cat(  ClassicalMetric_top20 )
  cat("\n\n\n\n\n\n\n")
  cat( "Nonmetric-Multidimensional-Scaling:\n" )
  print(  Nonmetric_top20  )
  cat("\n\n\n\n\n")
  sink() 


  write.table(ClassicalMetric_top20 , 
              file = paste(path2,   "ClassicalMetric_top20.txt",  sep="/"), 
              append = FALSE, quote = FALSE, sep = "\t", eol = "\n", na = "NA", dec = ".", 
              row.names = TRUE,  col.names = TRUE, qmethod = c("escape", "double"),  fileEncoding = "")
  write.table(   Nonmetric_top20$points  , 
              file = paste(path2,   "Nonmetric_top20.txt",  sep="/"), 
              append = FALSE, quote = FALSE, sep = "\t", eol = "\n", na = "NA", dec = ".", 
              row.names = TRUE,  col.names = TRUE, qmethod = c("escape", "double"),  fileEncoding = "")

  my_col1=colorRampPalette(  c( "blue", "skyblue", "black", "pink", "red") )   

  MyHeatmaps_1A_f(as.matrix( ClassicalMetric_top20 ),  path2=path2,   fileName2="ClassicalMetric_top20.corrplot.pdf",          height2=30,   width2=30, is.corr2=FALSE,  my_col2=my_col1(10) )                             
  MyHeatmaps_2_f(as.matrix( ClassicalMetric_top20 ),   path2=path2,   fileName2="ClassicalMetric_top20.heatmap.pdf",           height2=30,   width2=30 )  
  MyHeatmaps_3_f(as.matrix( ClassicalMetric_top20 ),   path2=path2,   fileName2="ClassicalMetric_top20.ComplexHeatmap.pdf",    height2=30,   width2=33 )  
 
  MyHeatmaps_1A_f(as.matrix( Nonmetric_top20$points ),  path2=path2,   fileName2="Nonmetric_top20.corrplot.pdf",          height2=30,   width2=30, is.corr2=FALSE,  my_col2=my_col1(10) )                             
  MyHeatmaps_2_f(as.matrix( Nonmetric_top20$points ),   path2=path2,   fileName2="Nonmetric_top20.heatmap.pdf",           height2=30,   width2=30 )  
  MyHeatmaps_3_f(as.matrix( Nonmetric_top20$points ),   path2=path2,   fileName2="Nonmetric_top20.ComplexHeatmap.pdf",    height2=30,   width2=33 )  
 


}





















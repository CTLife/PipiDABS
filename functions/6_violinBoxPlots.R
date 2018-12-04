


## 1 feature as type
MyBoxViolinPlot_1_f <- function(vector2,   sampleType2,  colours2,   path2,   fileName2,  title2,  xLab2,  yLab2,    height2=4,   width2=4,   Ymin2=0, Ymax2=3) { 
    myTempFunction <- function() {

  vector2[vector2>Ymax2] <- Ymax2
  vector2[vector2<Ymin2] <- Ymin2
  DataFrame_Local  <- data.frame(   sampleType=sampleType2,   yAxis=vector2    ) 
  
  FigureTemp1 <- ggplot( DataFrame_Local, aes(x=sampleType, y=yAxis ) ) +  
    geom_boxplot( outlier.shape=NA, outlier.size=0, notch=FALSE,  notchwidth = 0.1,  alpha=1  ) +   
    stat_summary( position=position_dodge(width=0.9), fun.y=mean,  color="yellow4",  geom="point", shape=19, size=1.5, show.legend = FALSE) + 
    xlab(xLab2 ) + ylab( yLab2 ) + ggtitle( title2 )  + MyTheme_1_g(textSize1=12, hjust1=1, vjust1=1,  angle1=30 ) + ylim(Ymin2, Ymax2 )
  MySaveGgplot2_1_g(ggplot2Figure1=FigureTemp1,  path1=path2, fileName1=paste(fileName2, "_boxPlot_NoFacet",   sep="",  collapse=NULL),  height1=height2, width1=width2)
  
  FigureTemp1a <- ggplot( DataFrame_Local, aes(x=sampleType, y=yAxis, fill=sampleType ) ) +  
    geom_boxplot( outlier.shape=NA, outlier.size=0, notch=FALSE,  notchwidth = 0.1,  alpha=1  ) +   
    stat_summary( position=position_dodge(width=0.9), fun.y=mean,  color="yellow4",  geom="point", shape=19, size=1.5, show.legend = FALSE) + 
    xlab(xLab2 ) + ylab( yLab2 ) + ggtitle( title2 )  + MyTheme_1_g(textSize1=12, hjust1=1, vjust1=1,  angle1=30 ) + ylim(Ymin2, Ymax2 )
  MySaveGgplot2_1_g(ggplot2Figure1=FigureTemp1a,  path1=path2, fileName1=paste(fileName2, "_boxPlot_NoFacet2",   sep="",  collapse=NULL),  height1=height2, width1=width2+2)
  
  FigureTemp1b <- ggplot( DataFrame_Local, aes(x=sampleType, y=yAxis, fill=sampleType ) ) +  
    geom_boxplot( outlier.shape=NA, outlier.size=0, notch=FALSE,  notchwidth = 0.1,  alpha=1  ) +   
    stat_summary( position=position_dodge(width=0.9), fun.y=mean,  color="yellow4",  geom="point", shape=19, size=1.5, show.legend = FALSE) + 
    scale_fill_manual( values = colours2 ) + 
    xlab(xLab2 ) + ylab( yLab2 ) + ggtitle( title2 )  + MyTheme_1_g(textSize1=12, hjust1=1, vjust1=1,  angle1=30 ) + ylim(Ymin2, Ymax2 )
  MySaveGgplot2_1_g(ggplot2Figure1=FigureTemp1b,  path1=path2, fileName1=paste(fileName2, "_boxPlot_NoFacet3",   sep="",  collapse=NULL),  height1=height2, width1=width2+2)
  
  


  FigureTemp1 <- ggplot( DataFrame_Local, aes(x=sampleType, y=yAxis ) ) +  
    geom_boxplot( outlier.size=0.1, notch=FALSE,  notchwidth = 0.1,  alpha=1  ) +   
    stat_summary( position=position_dodge(width=0.9), fun.y=mean,  color="yellow4",  geom="point", shape=19, size=1.5, show.legend = FALSE) + 
    xlab(xLab2 ) + ylab( yLab2 ) + ggtitle( title2 )  + MyTheme_1_g(textSize1=12, hjust1=1, vjust1=1,  angle1=30 ) + ylim(Ymin2, Ymax2 )
  MySaveGgplot2_1_g(ggplot2Figure1=FigureTemp1,  path1=path2, fileName1=paste(fileName2, "_boxPlot_NoFacet_withOutlier",   sep="",  collapse=NULL),  height1=height2, width1=width2)                             
 
  FigureTemp1 <- ggplot( DataFrame_Local, aes(x=sampleType, y=yAxis, fill=sampleType  ) ) +  
    geom_boxplot( outlier.size=0.1, notch=FALSE,  notchwidth = 0.1,  alpha=1  ) +   
    stat_summary( position=position_dodge(width=0.9), fun.y=mean,  color="yellow4",  geom="point", shape=19, size=1.5, show.legend = FALSE) + 
    xlab(xLab2 ) + ylab( yLab2 ) + ggtitle( title2 )  + MyTheme_1_g(textSize1=12, hjust1=1, vjust1=1,  angle1=30 ) + ylim(Ymin2, Ymax2 )
  MySaveGgplot2_1_g(ggplot2Figure1=FigureTemp1,  path1=path2, fileName1=paste(fileName2, "_boxPlot_NoFacet_withOutlier2",   sep="",  collapse=NULL),  height1=height2, width1=width2+2)

  FigureTemp1 <- ggplot( DataFrame_Local, aes(x=sampleType, y=yAxis, fill=sampleType  ) ) +  
    geom_boxplot( outlier.size=0.1, notch=FALSE,  notchwidth = 0.1,  alpha=1  ) +   
    stat_summary( position=position_dodge(width=0.9), fun.y=mean,  color="yellow4",  geom="point", shape=19, size=1.5, show.legend = FALSE) + 
    scale_fill_manual( values = colours2 ) + 
    xlab(xLab2 ) + ylab( yLab2 ) + ggtitle( title2 )  + MyTheme_1_g(textSize1=12, hjust1=1, vjust1=1,  angle1=30 ) + ylim(Ymin2, Ymax2 )
  MySaveGgplot2_1_g(ggplot2Figure1=FigureTemp1,  path1=path2, fileName1=paste(fileName2, "_boxPlot_NoFacet_withOutlier3",   sep="",  collapse=NULL),  height1=height2, width1=width2+2)
 
  
  
  
  
  
  FigureTemp4a <- ggplot( DataFrame_Local, aes(x=sampleType, y=yAxis ) ) +  
    geom_violin(  colour = "red", fill="red"  ) + 
    geom_boxplot( outlier.shape=NA, outlier.size=0, size=0.6,    width=0.3,     alpha=0.00001, position=position_dodge(width=0.9)    ) +   
    stat_summary( position=position_dodge(width=0.9), fun.y=mean,  color="yellow4",  geom="point", shape=19, size=1.5, show.legend = FALSE) + 
    xlab(xLab2 ) + ylab( yLab2 ) + ggtitle( title2 )  + MyTheme_1_g(textSize1=12, hjust1=1, vjust1=1,  angle1=30 ) + ylim(Ymin2, Ymax2 )
  MySaveGgplot2_1_g(ggplot2Figure1=FigureTemp4a,  path1=path2, fileName1=paste(fileName2, "_violinBoxPlot_NoFacet",   sep="",  collapse=NULL),  height1=height2, width1=width2)
   
  FigureTemp4a <- ggplot( DataFrame_Local, aes(x=sampleType, y=yAxis, fill=sampleType ) ) +  
    geom_violin(  colour = NA  ) + 
    geom_boxplot( outlier.shape=NA, outlier.size=0, size=0.6,    width=0.3,     alpha=0.00001, position=position_dodge(width=0.9)    ) +   
    stat_summary( position=position_dodge(width=0.9), fun.y=mean,  color="yellow4",  geom="point", shape=19, size=1.5, show.legend = FALSE) + 
    xlab(xLab2 ) + ylab( yLab2 ) + ggtitle( title2 )  + MyTheme_1_g(textSize1=12, hjust1=1, vjust1=1,  angle1=30 ) + ylim(Ymin2, Ymax2 )
  MySaveGgplot2_1_g(ggplot2Figure1=FigureTemp4a,  path1=path2, fileName1=paste(fileName2, "_violinBoxPlot_NoFacet2",   sep="",  collapse=NULL),  height1=height2, width1=width2+2)
  
  FigureTemp4a <- ggplot( DataFrame_Local, aes(x=sampleType, y=yAxis, fill=sampleType ) ) +  
    geom_violin(  colour = NA  ) + 
    geom_boxplot( outlier.shape=NA, outlier.size=0, size=0.6,    width=0.3,     alpha=0.00001, position=position_dodge(width=0.9)    ) +   
    stat_summary( position=position_dodge(width=0.9), fun.y=mean,  color="yellow4",  geom="point", shape=19, size=1.5, show.legend = FALSE) + 
    scale_fill_manual( values = colours2 ) +
    xlab(xLab2 ) + ylab( yLab2 ) + ggtitle( title2 )  + MyTheme_1_g(textSize1=12, hjust1=1, vjust1=1,  angle1=30 ) + ylim(Ymin2, Ymax2 )
  MySaveGgplot2_1_g(ggplot2Figure1=FigureTemp4a,  path1=path2, fileName1=paste(fileName2, "_violinBoxPlot_NoFacet3",   sep="",  collapse=NULL),  height1=height2, width1=width2+2)
  
 
  }
  tryCatch(
    myTempFunction(),
    error = function(err){"violinPlots_f_8888888"}
  ) 
 
}  
















## 2 features as type
MyBoxViolinPlot_2_f <- function(vector2,   sampleType2,  sampleType3,  colours2,   path2,   fileName2,  title2,  xLab2,  yLab2,    height2=4,   width2=4,   Ymin2=0, Ymax2=3) { 
  myTempFunction <- function() {

  vector2[vector2>Ymax2] <- Ymax2
  vector2[vector2<Ymin2] <- Ymin2
  DataFrame_Local  <- data.frame(   sampleType=sampleType2,   yAxis=vector2,  sampleTypeB=sampleType3   ) 
  
  FigureTemp1 <- ggplot( DataFrame_Local, aes(x=sampleType, y=yAxis, fill=sampleTypeB) ) +  
    geom_boxplot( outlier.shape=NA, outlier.size=0, notch=FALSE,  notchwidth = 0.1,  alpha=1  ) +   
    stat_summary( position=position_dodge(width=0.75), fun.y=mean,  color="yellow4",  geom="point", shape=19, size=1.5, show.legend = FALSE) + 
    scale_fill_manual( values = colours2 ) +
    xlab(xLab2 ) + ylab( yLab2 ) + ggtitle( title2 )  + MyTheme_1_g(textSize1=12, hjust1=1, vjust1=1,  angle1=30 ) + ylim(Ymin2, Ymax2 )
  MySaveGgplot2_1_g(ggplot2Figure1=FigureTemp1,  path1=path2, fileName1=paste(fileName2, "_boxPlot_NoFacet",   sep="",  collapse=NULL),  height1=height2, width1=width2+2)
  
  

  
  FigureTemp2a <- ggplot( DataFrame_Local, aes(x=sampleTypeB, y=yAxis, fill=sampleTypeB) ) +  
    facet_grid(.~sampleType)+
    geom_boxplot( outlier.shape=NA, outlier.size=0, notch=FALSE,  notchwidth = 0.1,  alpha=1  ) +   
    stat_summary( position=position_dodge(width=0.9), fun.y=mean,  color="yellow4",  geom="point", shape=19, size=1.5, show.legend = FALSE) + 
    scale_fill_manual( values = colours2 ) +
    xlab(xLab2 ) + ylab( yLab2 ) + ggtitle( title2 )  + MyTheme_1_g(textSize1=12, hjust1=1, vjust1=1,  angle1=30 ) + ylim(Ymin2, Ymax2 )
  MySaveGgplot2_1_g(ggplot2Figure1=FigureTemp2a,  path1=path2, fileName1=paste(fileName2, "_boxPlot",   sep="",  collapse=NULL),  height1=height2, width1=width2)
  

  
  
  FigureTemp3a <- ggplot( DataFrame_Local, aes(x=sampleType, y=yAxis , fill=sampleTypeB) ) +  
    geom_violin(  colour = NA  ) + 
    stat_summary( position=position_dodge(width=0.9), fun.y=mean,  color="yellow4",  geom="point", shape=19, size=1.5, show.legend = FALSE) + 
    scale_fill_manual( values = colours2 ) +
    xlab(xLab2 ) + ylab( yLab2 ) + ggtitle( title2 )  + MyTheme_1_g(textSize1=12, hjust1=1, vjust1=1,  angle1=30 ) + ylim(Ymin2, Ymax2 )
  MySaveGgplot2_1_g(ggplot2Figure1=FigureTemp3a,  path1=path2, fileName1=paste(fileName2, "_violinPlot_NoFacet",   sep="",  collapse=NULL),  height1=height2, width1=width2+2)
  
  FigureTemp3b <- ggplot( DataFrame_Local, aes(x=sampleTypeB, y=yAxis , fill=sampleTypeB) ) +  
    geom_violin(  colour = NA  ) + 
    stat_summary( position=position_dodge(width=0.9), fun.y=mean,  color="yellow4",  geom="point", shape=19, size=1.5, show.legend = FALSE) + 
    scale_fill_manual( values = colours2 ) +
    facet_grid(.~sampleType)+
    xlab(xLab2 ) + ylab( yLab2 ) + ggtitle( title2 )  + MyTheme_1_g(textSize1=12, hjust1=1, vjust1=1,  angle1=30 ) + ylim(Ymin2, Ymax2 )
  MySaveGgplot2_1_g(ggplot2Figure1=FigureTemp3b,  path1=path2, fileName1=paste(fileName2, "_violinPlot",   sep="",  collapse=NULL),  height1=height2, width1=width2)
  
  
  
  
  FigureTemp4a <- ggplot( DataFrame_Local, aes(x=sampleType, y=yAxis , fill=sampleTypeB) ) +  
    geom_violin(  colour = NA  ) + 
    geom_boxplot( outlier.shape=NA, outlier.size=0, size=0.6,    width=0.3,     alpha=0.00001, position=position_dodge(width=0.9)    ) +   
    stat_summary( position=position_dodge(width=0.9), fun.y=mean,  color="yellow4",  geom="point", shape=19, size=1.5, show.legend = FALSE) + 
    scale_fill_manual( values = colours2 ) +
    xlab(xLab2 ) + ylab( yLab2 ) + ggtitle( title2 )  + MyTheme_1_g(textSize1=12, hjust1=1, vjust1=1,  angle1=30 ) + ylim(Ymin2, Ymax2 )
  MySaveGgplot2_1_g(ggplot2Figure1=FigureTemp4a,  path1=path2, fileName1=paste(fileName2, "_violinBoxPlot_NoFacet",   sep="",  collapse=NULL),  height1=height2, width1=width2+2)
  
  FigureTemp4b <- ggplot( DataFrame_Local, aes(x=sampleTypeB, y=yAxis , fill=sampleTypeB) ) +  
    geom_violin(  colour = NA  ) + 
    geom_boxplot( outlier.shape=NA, outlier.size=0, size=0.6,    width=0.3,     alpha=0.001   ) +   
    stat_summary( position=position_dodge(width=0.9), fun.y=mean,  color="yellow4",  geom="point", shape=19, size=1.5, show.legend = FALSE) + 
    scale_fill_manual( values = colours2 ) +
    facet_grid(.~sampleType)+
    xlab(xLab2 ) + ylab( yLab2 ) + ggtitle( title2 )  + MyTheme_1_g(textSize1=12, hjust1=1, vjust1=1,  angle1=30 ) + ylim(Ymin2, Ymax2 )
  MySaveGgplot2_1_g(ggplot2Figure1=FigureTemp4b,  path1=path2, fileName1=paste(fileName2, "_violinBoxPlot",   sep="",  collapse=NULL),  height1=height2, width1=width2)
  
  
  
  FigureTemp5 <- ggplot( DataFrame_Local, aes(x=sampleTypeB, y=yAxis , fill=sampleTypeB) ) +  
    geom_violin(colour = NA ,  adjust = 2 ) + 
    geom_boxplot( outlier.shape=NA, outlier.size=0, size=0.6,    width=0.3,     alpha=0.001   ) +   
    stat_summary( position=position_dodge(width=0.9), fun.y=mean,  color="yellow4",  geom="point", shape=19, size=1.5, show.legend = FALSE) + 
    scale_fill_manual( values = colours2 ) +
    facet_grid(.~sampleType)+
    xlab(xLab2 ) + ylab( yLab2 ) + ggtitle( title2 )  + MyTheme_1_g(textSize1=12, hjust1=1, vjust1=1,  angle1=30 ) + ylim(Ymin2, Ymax2 )
  MySaveGgplot2_1_g(ggplot2Figure1=FigureTemp5,  path1=path2, fileName1=paste(fileName2, "_violinBoxPlot-2Ajust",   sep="",  collapse=NULL),  height1=height2, width1=width2)
  
  FigureTemp6 <- ggplot( DataFrame_Local, aes(x=sampleTypeB, y=yAxis , fill=sampleTypeB) ) +  
    geom_boxplot( outlier.shape=NA, outlier.size=0, size=0.6,    width=0.3,     alpha=0.001   ) +   
    geom_violin(colour = NA, alpha=0.5  ) + 
    stat_summary( position=position_dodge(width=0.9), fun.y=mean,  color="yellow4",  geom="point", shape=19, size=1.5, show.legend = FALSE) + 
    scale_fill_manual( values = colours2 ) +
    facet_grid(.~sampleType)+
    xlab(xLab2 ) + ylab( yLab2 ) + ggtitle( title2 )  + MyTheme_1_g(textSize1=12, hjust1=1, vjust1=1,  angle1=30 ) + ylim(Ymin2, Ymax2 )
  MySaveGgplot2_1_g(ggplot2Figure1=FigureTemp6,  path1=path2, fileName1=paste(fileName2, "_violinBoxPlot-another",   sep="",  collapse=NULL),  height1=height2, width1=width2)
   
  }
  tryCatch(
    myTempFunction(),
    error = function(err){"violinPlots_f_8888888"}
  ) 
 
}  










## 3 features as type
MyBoxViolinPlot_3_f <- function(vector2,   sampleType2,  sampleType3, sampleType4,  path2,   fileName2,  title2,  xLab2,  yLab2,    height2=4,   width2=4,   Ymin2=0, Ymax2=3) { 
  myTempFunction <- function() {

  vector2[vector2>Ymax2] <- Ymax2
  vector2[vector2<Ymin2] <- Ymin2
  DataFrame_Local  <- data.frame(   sampleType=sampleType2,   yAxis=vector2,  sampleTypeB=sampleType3 , sampleTypeC=sampleType4   ) 
  
  FigureTemp1 <- ggplot( DataFrame_Local, aes(x=sampleType, y=yAxis, fill=sampleTypeB) ) +  
    geom_boxplot( outlier.shape=NA, outlier.size=0, notch=FALSE,  notchwidth = 0.1,  alpha=1  ) +   
    stat_summary( position=position_dodge(width=0.75), fun.y=mean,  color="yellow4",  geom="point", shape=19, size=1.5, show.legend = FALSE) + 
    xlab(xLab2 ) + ylab( yLab2 ) + ggtitle( title2 )  + MyTheme_1_g(textSize1=14, hjust1=1, vjust1=1,  angle1=30 ) + ylim(Ymin2, Ymax2 )
  MySaveGgplot2_1_g(ggplot2Figure1=FigureTemp1,  path1=path2, fileName1=paste(fileName2, "_boxPlot_NoFacet",   sep="",  collapse=NULL),  height1=height2, width1=width2+2)
  

  
  FigureTemp2a <- ggplot( DataFrame_Local, aes(x=sampleTypeC, y=yAxis, fill=sampleTypeB) ) +  
    facet_grid(.~sampleType)+
    geom_boxplot( outlier.shape=NA, outlier.size=0, notch=FALSE,  notchwidth = 0.1,  alpha=1  ) +   
    stat_summary( position=position_dodge(width=0.75), fun.y=mean,  color="yellow4",  geom="point", shape=19, size=1.5, show.legend = FALSE) + 
    xlab(xLab2 ) + ylab( yLab2 ) + ggtitle( title2 )  + MyTheme_1_g(textSize1=14, hjust1=1, vjust1=1,  angle1=30 ) + ylim(Ymin2, Ymax2 )
  MySaveGgplot2_1_g(ggplot2Figure1=FigureTemp2a,  path1=path2, fileName1=paste(fileName2, "_boxPlot",   sep="",  collapse=NULL),  height1=height2, width1=width2)
  
  
  FigureTemp3a <- ggplot( DataFrame_Local, aes(x=sampleType, y=yAxis , fill=sampleTypeB) ) +  
    geom_violin(  colour = NA  ) + 
    stat_summary( position=position_dodge(width=0.9), fun.y=mean,  color="yellow4",  geom="point", shape=19, size=1.5, show.legend = FALSE) + 
    xlab(xLab2 ) + ylab( yLab2 ) + ggtitle( title2 )  + MyTheme_1_g(textSize1=14, hjust1=1, vjust1=1,  angle1=30 ) + ylim(Ymin2, Ymax2 )
  MySaveGgplot2_1_g(ggplot2Figure1=FigureTemp3a,  path1=path2, fileName1=paste(fileName2, "_violinPlot_NoFacet",   sep="",  collapse=NULL),  height1=height2, width1=width2+2)
  
  FigureTemp3b <- ggplot( DataFrame_Local, aes(x=sampleTypeC, y=yAxis , fill=sampleTypeB) ) +  
    geom_violin(  colour = NA  ) + 
    stat_summary( position=position_dodge(width=0.9), fun.y=mean,  color="yellow4",  geom="point", shape=19, size=1.5, show.legend = FALSE) + 
    facet_grid(.~sampleType)+
    xlab(xLab2 ) + ylab( yLab2 ) + ggtitle( title2 )  + MyTheme_1_g(textSize1=14, hjust1=1, vjust1=1,  angle1=30 ) + ylim(Ymin2, Ymax2 )
  MySaveGgplot2_1_g(ggplot2Figure1=FigureTemp3b,  path1=path2, fileName1=paste(fileName2, "_violinPlot",   sep="",  collapse=NULL),  height1=height2, width1=width2)
  
  
  
  
  FigureTemp4a <- ggplot( DataFrame_Local, aes(x=sampleType, y=yAxis , fill=sampleTypeB) ) +  
    geom_violin(  colour = NA  ) + 
    geom_boxplot( outlier.shape=NA, outlier.size=0, size=0.6,    width=0.3,     alpha=0.001, position=position_dodge(width=0.9)   ) +   
    stat_summary( position=position_dodge(width=0.9), fun.y=mean,  color="yellow4",  geom="point", shape=19, size=1.5, show.legend = FALSE) + 
    xlab(xLab2 ) + ylab( yLab2 ) + ggtitle( title2 )  + MyTheme_1_g(textSize1=14, hjust1=1, vjust1=1,  angle1=30 ) + ylim(Ymin2, Ymax2 )
  MySaveGgplot2_1_g(ggplot2Figure1=FigureTemp4a,  path1=path2, fileName1=paste(fileName2, "_violinBoxPlot_NoFacet",   sep="",  collapse=NULL),  height1=height2, width1=width2+2)
  
  FigureTemp4b <- ggplot( DataFrame_Local, aes(x=sampleTypeC, y=yAxis , fill=sampleTypeB) ) +  
    geom_violin(  colour = NA  ) + 
    geom_boxplot( outlier.shape=NA, outlier.size=0, size=0.6,    width=0.3,     alpha=0.001, position=position_dodge(width=0.9)     ) +   
    stat_summary( position=position_dodge(width=0.9), fun.y=mean,  color="yellow4",  geom="point", shape=19, size=1.5, show.legend = FALSE) + 
    facet_grid(.~sampleType)+
    xlab(xLab2 ) + ylab( yLab2 ) + ggtitle( title2 )  + MyTheme_1_g(textSize1=14, hjust1=1, vjust1=1,  angle1=30 ) + ylim(Ymin2, Ymax2 )
  MySaveGgplot2_1_g(ggplot2Figure1=FigureTemp4b,  path1=path2, fileName1=paste(fileName2, "_violinBoxPlot",   sep="",  collapse=NULL),  height1=height2, width1=width2)
  
  
  
  FigureTemp5 <- ggplot( DataFrame_Local, aes(x=sampleTypeC, y=yAxis , fill=sampleTypeB) ) +  
    geom_violin(colour = NA ,  adjust = 2 ) + 
    geom_boxplot( outlier.shape=NA, outlier.size=0, size=0.6,    width=0.3,     alpha=0.001, position=position_dodge(width=0.9)   ) +   
    stat_summary( position=position_dodge(width=0.9), fun.y=mean,  color="yellow4",  geom="point", shape=19, size=1.5, show.legend = FALSE) + 
    facet_grid(.~sampleType)+
    xlab(xLab2 ) + ylab( yLab2 ) + ggtitle( title2 )  + MyTheme_1_g(textSize1=14, hjust1=1, vjust1=1,  angle1=30 ) + ylim(Ymin2, Ymax2 )
  MySaveGgplot2_1_g(ggplot2Figure1=FigureTemp5,  path1=path2, fileName1=paste(fileName2, "_violinBoxPlot-2Ajust",   sep="",  collapse=NULL),  height1=height2, width1=width2)
  
  FigureTemp6 <- ggplot( DataFrame_Local, aes(x=sampleTypeC, y=yAxis , fill=sampleTypeB) ) +  
    geom_boxplot( outlier.shape=NA, outlier.size=0, size=0.6,    width=0.3,     alpha=0.001, position=position_dodge(width=0.9)   ) +   
    geom_violin(colour = NA, alpha=0.5  ) + 
    stat_summary( position=position_dodge(width=0.9), fun.y=mean,  color="yellow4",  geom="point", shape=19, size=1.5, show.legend = FALSE) + 
    facet_grid(.~sampleType)+
    xlab(xLab2 ) + ylab( yLab2 ) + ggtitle( title2 )  + MyTheme_1_g(textSize1=14, hjust1=1, vjust1=1,  angle1=30 ) + ylim(Ymin2, Ymax2 )
  MySaveGgplot2_1_g(ggplot2Figure1=FigureTemp6,  path1=path2, fileName1=paste(fileName2, "_violinBoxPlot-another",   sep="",  collapse=NULL),  height1=height2, width1=width2)
 
  }
  tryCatch(
    myTempFunction(),
    error = function(err){"violinPlots_f_8888888"}
  ) 
  
}  









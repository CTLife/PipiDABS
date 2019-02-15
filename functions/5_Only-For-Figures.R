suppressPackageStartupMessages( library(ggplot2)  ) 
suppressPackageStartupMessages( library(reshape2)  ) 





MyTheme_1_g <- function(textSize1=14, hjust1=NULL, vjust1=NULL,  angle1=NULL) {    # "hjust=1, vjust=1, angle=30" for some boxplots.
  ggplot2::theme(  
    line  = element_line(colour="black",  size=1.0,   linetype=1,      lineend=NULL),                                                                                        
    rect  = element_rect(colour="black",  size=1.0,   linetype=1,      fill="transparent" ),                                                                                 
    text  = element_text(family="serif",  face="plain",  colour="black",  size=textSize1, hjust=0.5, vjust=0.5,   angle=0, lineheight=1.0,  margin = NULL, debug = NULL),     
    title = element_text(family="serif",  face="plain",  colour="black",  size=textSize1, hjust=0.5, vjust=0.5,   angle=0, lineheight=1.0,  margin = NULL, debug = NULL),    
    ## aspect.ratio = 1,       
    axis.title    = element_text(family="serif", face="plain", colour="black", size=textSize1,    hjust=0.5,    vjust=0.5,    angle=0,       lineheight=1.0,  margin = NULL, debug = NULL),        
    axis.title.x  = element_text(family="serif", face="plain", colour="black", size=textSize1,    hjust=0.5,    vjust=0.5,    angle=0,       lineheight=1.0,  margin = NULL, debug = NULL),        
    axis.title.y  = element_text(family="serif", face="plain", colour="black", size=textSize1,    hjust=0.5,    vjust=0.5,    angle=90,      lineheight=1.0,  margin = NULL, debug = NULL),        
    axis.text     = element_text(family="serif", face="plain", colour="black", size=textSize1,    hjust=0.5,    vjust=0.5,    angle=0,       lineheight=1.0,  margin = NULL, debug = NULL),                                                          
    axis.text.x   = element_text(family="serif", face="plain", colour="black", size=textSize1,    hjust=hjust1, vjust=vjust1, angle=angle1,  lineheight=1.0,  margin = NULL, debug = NULL),        
    axis.text.y   = element_text(family="serif", face="plain", colour="black", size=textSize1,    hjust=0.5,    vjust=0.5,    angle=0,       lineheight=1.0,  margin = NULL, debug = NULL),      
    axis.ticks        = element_line(colour="black", size=0.5, linetype=1, lineend=NULL),          ## tick marks along axes (element_line; inherits from line). 
    axis.ticks.x      = element_line(colour="black", size=0.5, linetype=1, lineend=NULL),          ## x axis tick marks (element_line; inherits from axis.ticks)
    axis.ticks.y      = element_line(colour="black", size=0.5, linetype=1, lineend=NULL),          ## y axis tick marks (element_line; inherits from axis.ticks)
    axis.ticks.length = grid::unit(2.0,   "mm",   data=NULL),                                      ## length of tick marks (unit), ‘"mm"’ Millimetres.  10 mm = 1 cm. 
    axis.line         = element_line(colour="transparent", size=0.3, linetype=1, lineend=NULL),    ## lines along axes (element_line; inherits from line). 
    axis.line.x       = element_line(colour="transparent", size=0.3, linetype=1, lineend=NULL),    ## line along x axis (element_line; inherits from axis.line)
    axis.line.y       = element_line(colour="transparent", size=0.3, linetype=1, lineend=NULL),	   ## line along y axis (element_line; inherits from axis.line)    
    legend.background    = element_rect(colour="transparent", size=1, linetype=1, fill="transparent" ), 	## background of legend (element_rect; inherits from rect)
    legend.spacing       = grid::unit(1, "mm", data=NULL), 	                                                ## extra space added around legend (unit). 
    legend.key           = element_rect(colour="transparent", size=2, linetype=1, fill="transparent" ), 	## background underneath legend keys. 
    legend.key.size      = grid::unit(6,   "mm", data=NULL) , 	                                                ## size of legend keys   (unit; inherits from legend.key.size)
    legend.key.height    = grid::unit(6.5, "mm", data=NULL) , 	                                                ## key background height (unit; inherits from legend.key.size)
    legend.key.width     = grid::unit(8,   "mm", data=NULL) ,                                                   ## key background width  (unit; inherits from legend.key.size)
    legend.text          = element_text(family="serif", face=NULL, colour="black", size=textSize1, hjust=NULL, vjust=NULL, angle=NULL, lineheight=NULL), 	##legend item labels. 
    legend.text.align    = 0, 	                    ## alignment of legend labels (number from 0 (left) to 1 (right))
    legend.title         = element_blank(),   	    ## title of legend (element_text; inherits from title)
    legend.title.align   = 0, 	                    ## alignment of legend title (number from 0 (left) to 1 (right))
    legend.position      = "right", 	            ## the position of legends. ("left", "right", "bottom", "top", or two-element numeric vector)
    legend.direction     = "vertical",        	    ## layout of items in legends  ("horizontal" or "vertical")    
    legend.justification = "center",      	    ## anchor point for positioning legend inside plot ("center" or two-element numeric vector)   
    legend.box           = NULL, 	            ## arrangement of multiple legends ("horizontal" or "vertical")  
    legend.box.just      = NULL, 	            ## justification of each legend within the overall bounding box, when there are multiple legends ("top", "bottom", "left", or "right")       
    panel.background   = element_rect(colour="transparent", size=0.0, linetype=1, fill="transparent" ),     ## background of plotting area, drawn underneath plot (element_rect; inherits from rect)
    panel.border       = element_rect(colour="black", size=0.5, linetype=1, fill=NA ), 	                    ## border around plotting area, drawn on top of plot so that it covers tick marks and grid lines. This should be used with fill=NA (element_rect; inherits from rect)                                     
    panel.spacing      = grid::unit(1, "mm", data=NULL) , 	                                            ## margin around facet panels (unit)   
    panel.spacing.x    = grid::unit(1, "mm", data=NULL) ,
    panel.spacing.y    = grid::unit(1, "mm", data=NULL) ,
    panel.grid         = element_blank(), 	                                                            ## grid lines (element_line; inherits from line)   
    panel.grid.major   = element_line(colour="transparent", size=NULL, linetype=NULL, lineend=NULL) , 	    ## major grid lines (element_line; inherits from panel.grid)  
    panel.grid.minor   = element_line(colour="transparent", size=NULL, linetype=NULL, lineend=NULL) ,       ## minor grid lines (element_line; inherits from panel.grid)   
    panel.grid.major.x = element_line(colour="transparent", size=NULL, linetype=NULL, lineend=NULL) , 	    ## vertical major grid lines (element_line; inherits from panel.grid.major)
    panel.grid.major.y = element_line(colour="transparent", size=NULL, linetype=NULL, lineend=NULL) ,       ## horizontal major grid lines (element_line; inherits from panel.grid.major)
    panel.grid.minor.x = element_line(colour="transparent", size=NULL, linetype=NULL, lineend=NULL) ,       ## vertical minor grid lines (element_line; inherits from panel.grid.minor)
    panel.grid.minor.y = element_line(colour="transparent", size=NULL, linetype=NULL, lineend=NULL) ,       ## horizontal minor grid lines (element_line; inherits from panel.grid.minor)    
    plot.background  = element_rect(colour="transparent", size=NULL, linetype=NULL, fill="transparent" ),                                            ## background of the entire plot (element_rect; inherits from rect)   
    plot.title       = element_text(family="serif", face=NULL, colour="black", size=textSize1, hjust=0.5, vjust=0.5,   angle=NULL, lineheight=NULL),     ## plot title (text appearance) (element_text; inherits from title)   
    plot.margin      = grid::unit(c(5, 5, 5, 5), "mm", data=NULL), 	                                                                                ## margin around entire plot (unit with the sizes of the top, right, bottom, and left margins)    
    strip.background = element_rect(colour=NULL,    size=NULL, linetype=NULL, fill=NULL ), 	                                                      ## background of facet labels (element_rect; inherits from rect)   
    strip.text       = element_text(family="serif", face=NULL, colour=NULL, size=NULL, hjust=NULL, vjust=NULL, angle=NULL, lineheight=NULL), 	      ## facet labels (element_text; inherits from text)
    strip.text.x     = element_text(family="serif", face=NULL, colour=NULL, size=NULL, hjust=NULL, vjust=NULL, angle=NULL, lineheight=NULL), 	      ## facet labels along horizontal direction (element_text; inherits from strip.text)
    strip.text.y     = element_text(family="serif", face=NULL, colour=NULL, size=NULL, hjust=NULL, vjust=NULL, angle=NULL, lineheight=NULL)   	      ## facet labels along vertical direction (element_text; inherits from strip.text) 
  ) 
} 





MySaveGgplot2_1_g <- function(ggplot2Figure1,  path1, fileName1,  height1, width1) {
    SVG1 <- paste(path1,  "/",  "SVG",  sep = "",  collapse = NULL)
    PDF1 <- paste(path1,  "/",  "PDF",  sep = "",  collapse = NULL)
    EPS1 <- paste(path1,  "/",  "EPS",  sep = "",  collapse = NULL)
    if( ! file.exists(SVG1) ) { dir.create(SVG1) }
    if( ! file.exists(PDF1) ) { dir.create(PDF1) }
    if( ! file.exists(EPS1) ) { dir.create(EPS1) }
    ggplot2::ggsave(filename=paste(SVG1, "/", fileName1, ".svg", sep=""),  plot = last_plot(), device = "svg",   path = NULL, scale = 1, width = width1, height = height1, units = "in", dpi = 3000, limitsize = FALSE)
    ggplot2::ggsave(filename=paste(PDF1, "/", fileName1, ".pdf", sep=""),  plot = last_plot(), device = "pdf",   path = NULL, scale = 1, width = width1, height = height1, units = "in", dpi = 3000, limitsize = FALSE)
    ggplot2::ggsave(filename=paste(EPS1, "/", fileName1, ".eps", sep=""),  plot = last_plot(), device =cairo_ps, path = NULL, scale = 1, width = width1, height = height1, units = "in", dpi = 3000, limitsize = FALSE)
}





## df contains two columns, the first column (cond_col=1) is sample type, the second column (val_col=2) is value. (must be).
whisk_1_g <- function(df, cond_col=1, val_col=2) {  
  require(reshape2)
  condname <- names(df)[cond_col]  ## save the name of the first column.
  names(df)[cond_col] <- "cond" 
  names(df)[val_col]  <- "value"
  b   <- boxplot(value~cond, data=df, plot=FALSE)   
  df2 <- cbind(as.data.frame(b$stats), c("min","lq","m","uq","max"))
  names(df2) <- c(levels(df$cond), "pos")
  df2 <- reshape2::melt(df2, id="pos", variable.name="cond")
  df2 <- reshape2::dcast(df2, cond~pos)   
  names(df2)[1] <- condname 
  print(df2)
  df2
}



 

## compare  probability  density
MyHistogram_1_f <- function(vector2, sampleType2,  colours2,  path2,   fileName2,  title2,  xLab2, height2=4,  width2=4,  xMin2=0,  xMax2=1.5,   yMin2=0,  yMax2=10) {
  vector2[vector2>xMax2] <- xMax2
  vector2[vector2<xMin2] <- xMin2
  dataframeB  <- data.frame( xAxis = vector2,  sampleType=sampleType2 )
  
  FigureTemp1 <- { ggplot(data=dataframeB, aes(x=xAxis, fill=sampleType, colour=sampleType) )   +  xlab(xLab2) + ylab("Probability density") +  ggtitle(title2)  +  
      geom_density(mapping = NULL, data = NULL, stat = "density", position = "identity", na.rm = FALSE, alpha=0.3  ) +
      scale_colour_manual( values=colours2   ) + scale_fill_manual( values = colours2) +  ylim(yMin2, yMax2) + 
      scale_x_continuous(limits=c(xMin2, xMax2)  ) +  
      #geom_hline(yintercept=-0.01, lty=1, col="white", size=0.6) +
      MyTheme_1_g( hjust1=NULL, vjust1=NULL,  angle1=NULL,   textSize=14 )    +  guides( colour = guide_legend(override.aes = list(size=0, shape=1)) ) }
  MySaveGgplot2_1_g(ggplot2Figure1=FigureTemp1,  path1=path2, fileName1=paste(fileName2, "-density-limitY",      sep="",  collapse=NULL),  height1=height2,  width1=width2)
   
  FigureTemp2 <- { ggplot(data=dataframeB, aes(x=xAxis, fill=sampleType, colour=sampleType) )   +  xlab(xLab2) + ylab("Probability density") +  ggtitle(title2)  +  
      geom_density(mapping = NULL, data = NULL, stat = "density", position = "identity", na.rm = FALSE, alpha=0.3  ) +
      scale_colour_manual( values=colours2   ) + scale_fill_manual( values = colours2) +  
      scale_x_continuous(limits=c(xMin2, xMax2)  ) +  
      #geom_hline(yintercept=-0.01, lty=1, col="white", size=0.6) +
      MyTheme_1_g( hjust1=NULL, vjust1=NULL,  angle1=NULL,   textSize=14 )    +  guides( colour = guide_legend(override.aes = list(size=0, shape=1)) ) }
  MySaveGgplot2_1_g(ggplot2Figure1=FigureTemp2,  path1=path2, fileName1=paste(fileName2, "-density",             sep="",  collapse=NULL),  height1=height2,  width1=width2)
  
  FigureTemp4 <- { ggplot(data=dataframeB, aes(x=xAxis, fill=sampleType, colour=sampleType) )   +  xlab(xLab2) + ylab("Probability density") +  ggtitle(title2)  +  
      geom_density(mapping = NULL, data = NULL, stat = "density", position = "identity", na.rm = FALSE, alpha=0.0  ) +
      scale_colour_manual( values=colours2   ) + scale_fill_manual( values = colours2) +  
      scale_x_continuous(limits=c(xMin2, xMax2)  ) +  
      #geom_hline(yintercept=-0.01, lty=1, col="white", size=0.6) +
      MyTheme_1_g( hjust1=NULL, vjust1=NULL,  angle1=NULL,   textSize=14 )   +  guides( colour = guide_legend(override.aes = list(size=1.5, shape=1)) ) }
  MySaveGgplot2_1_g(ggplot2Figure1=FigureTemp4,  path1=path2, fileName1=paste(fileName2, "-density2",            sep="",  collapse=NULL),  height1=height2,  width1=width2)
    
  FigureTemp5 <- { ggplot(data=dataframeB, aes(x=xAxis, fill=sampleType, colour=sampleType) )   +  xlab(xLab2) + ylab("Probability density") +  ggtitle(title2)  +  
      geom_line(stat="density", alpha=1.0 ) +
      scale_colour_manual( values=colours2   ) + scale_fill_manual( values = colours2) +  ylim(yMin2, yMax2) + 
      scale_x_continuous(limits=c(xMin2, xMax2)  ) +  
      #geom_hline(yintercept=-0.01, lty=1, col="white", size=0.6) +
      MyTheme_1_g( hjust1=NULL, vjust1=NULL,  angle1=NULL,   textSize=14 )    +  guides( colour = guide_legend(override.aes = list(size=1.5, shape=1)) ) }
  MySaveGgplot2_1_g(ggplot2Figure1=FigureTemp5,  path1=path2, fileName1=paste(fileName2, "-density3-limitY",     sep="",  collapse=NULL),  height1=height2,  width1=width2)
    
  FigureTemp6 <- { ggplot(data=dataframeB, aes(x=xAxis, fill=sampleType, colour=sampleType) )   +  xlab(xLab2) + ylab("Probability density") +  ggtitle(title2)  +  
      geom_line(stat="density", alpha=1.0 ) +
      scale_colour_manual( values=colours2   ) + scale_fill_manual( values = colours2) +  
      scale_x_continuous(limits=c(xMin2, xMax2)  ) +  
      #geom_hline(yintercept=-0.01, lty=1, col="white", size=0.6) +
      MyTheme_1_g( hjust1=NULL, vjust1=NULL,  angle1=NULL,   textSize=14 )   +  guides( colour = guide_legend(override.aes = list(size=1.5, shape=1)) ) }
  MySaveGgplot2_1_g(ggplot2Figure1=FigureTemp6,  path1=path2, fileName1=paste(fileName2, "-density3",            sep="",  collapse=NULL),  height1=height2,  width1=width2)
}





## scatter Diagram for 2 samples and colour each category.
MyScatterDiagram_1_f <- function(vector2X,  vector2Y,  path2,   fileName2,   xLab2,   yLab2,  title2,  height2=4,  width2=4,  yMin2=0, yMax2=2,  xMin2=0, xMax2=2,  alpha2=0.5, diffThres2=1.5, colours2=c("red", "blue", "purple")) {                     
  vector2Y[vector2Y>yMax2] <- yMax2
  vector2Y[vector2Y<yMin2] <- yMin2
  vector2X[vector2X>xMax2] <- xMax2
  vector2X[vector2X<xMin2] <- xMin2
  myRatio  <- (vector2X - vector2Y) 
  myRatio1 <- myRatio
  myRatio2 <- myRatio
  myRatio3 <- myRatio
  myRatio4 <- myRatio
  myRatio5 <- myRatio
  
  myRatio1[myRatio1 >= diffThres2] <- 200 
  myRatio1[myRatio1 <= -diffThres2] <- -200 
  myRatio1[(myRatio1 < diffThres2)&(myRatio1 > -diffThres2)] <- 0
  print(myRatio1)
  
  dataframeA <- data.frame( xAxis = vector2X,  yAxis = vector2Y , SampleType = factor(myRatio1) ) 
  FigureTemp1 <- ggplot( data = dataframeA, aes(x = xAxis, y = yAxis,  color=factor(SampleType) )) + 
    geom_point(size=0.1, alpha=alpha2, shape=20 ) +  geom_abline(slope=1, intercept=0, lty=2, col="black", size=0.3) +
    ylim(yMin2, yMax2) +  xlim(xMin2, xMax2)+  xlab(xLab2) +   ylab(yLab2) +   ggtitle(title2) + scale_colour_manual( values=colours2 ) +
    guides( colour = guide_legend(override.aes = list(size=10)) )  + MyTheme_1_g( textSize=14,  hjust1=NULL, vjust1=NULL,  angle1=NULL)
  MySaveGgplot2_1_g(ggplot2Figure1=FigureTemp1,  path1=path2, fileName1=paste(fileName2, "_1_diffThres2",   sep="",  collapse=NULL),  height1=height2,  width1=width2)
  
  myRatio2[myRatio2 >= 50] <- 200
  myRatio2[myRatio2 <= -50] <- -200
  myRatio2[(myRatio2 < 50)&(myRatio2 > -50)] <- 0
  dataframeB <- data.frame( xAxis = vector2X,  yAxis = vector2Y , SampleType = factor(myRatio2) ) 
  FigureTemp1 <- ggplot( data = dataframeB, aes(x = xAxis, y = yAxis,  color=factor(SampleType) )) + 
    geom_point(size=0.1, alpha=alpha2, shape=20 ) +  geom_abline(slope=1, intercept=0, lty=2, col="black", size=0.3) +
    ylim(yMin2, yMax2) +  xlim(xMin2, xMax2)+  xlab(xLab2) +   ylab(yLab2) +   ggtitle(title2) + scale_colour_manual( values=colours2 ) +
    guides( colour = guide_legend(override.aes = list(size=10)) )  + MyTheme_1_g( textSize=14,  hjust1=NULL, vjust1=NULL,  angle1=NULL)
  MySaveGgplot2_1_g(ggplot2Figure1=FigureTemp1,  path1=path2, fileName1=paste(fileName2, "_2_diff50",   sep="",  collapse=NULL),  height1=height2,  width1=width2)
  
  myRatio3[myRatio3 >= 25] <- 200
  myRatio3[myRatio3 <= -25] <- -200
  myRatio3[(myRatio3 < 25)&(myRatio3 > -25)] <- 0
  dataframeC <- data.frame( xAxis = vector2X,  yAxis = vector2Y , SampleType = factor(myRatio3) ) 
  FigureTemp1 <- ggplot( data = dataframeC, aes(x = xAxis, y = yAxis,  color=factor(SampleType) )) + 
    geom_point(size=0.1, alpha=alpha2, shape=20 ) +  geom_abline(slope=1, intercept=0, lty=2, col="black", size=0.3) +
    ylim(yMin2, yMax2) +  xlim(xMin2, xMax2)+  xlab(xLab2) +   ylab(yLab2) +   ggtitle(title2) + scale_colour_manual( values=colours2 ) +
    guides( colour = guide_legend(override.aes = list(size=10)) )  + MyTheme_1_g( textSize=14,  hjust1=NULL, vjust1=NULL,  angle1=NULL)
  MySaveGgplot2_1_g(ggplot2Figure1=FigureTemp1,  path1=path2, fileName1=paste(fileName2, "_3_diff25",   sep="",  collapse=NULL),  height1=height2,  width1=width2)
   
  myRatio4[myRatio4 >= 20] <- 200
  myRatio4[myRatio4 <= -20] <- -200
  myRatio4[(myRatio4 < 20)&(myRatio4 > -20)] <- 0
  dataframeC <- data.frame( xAxis = vector2X,  yAxis = vector2Y , SampleType = factor(myRatio4) ) 
  FigureTemp1 <- ggplot( data = dataframeC, aes(x = xAxis, y = yAxis,  color=factor(SampleType) )) + 
    geom_point(size=0.1, alpha=alpha2, shape=20 ) +  geom_abline(slope=1, intercept=0, lty=2, col="black", size=0.3) +
    ylim(yMin2, yMax2) +  xlim(xMin2, xMax2)+  xlab(xLab2) +   ylab(yLab2) +   ggtitle(title2) + scale_colour_manual( values=colours2 ) +
    guides( colour = guide_legend(override.aes = list(size=10)) )  + MyTheme_1_g( textSize=14,  hjust1=NULL, vjust1=NULL,  angle1=NULL)
  MySaveGgplot2_1_g(ggplot2Figure1=FigureTemp1,  path1=path2, fileName1=paste(fileName2, "_4_diff20",   sep="",  collapse=NULL),  height1=height2,  width1=width2)
  
  myRatio5[myRatio5 >= 5] <- 200
  myRatio5[myRatio5 <= -5] <- -200
  myRatio5[(myRatio5 < 5)&(myRatio5 > -5)] <- 0
  dataframeC <- data.frame( xAxis = vector2X,  yAxis = vector2Y , SampleType = factor(myRatio5) ) 
  FigureTemp1 <- ggplot( data = dataframeC, aes(x = xAxis, y = yAxis,  color=factor(SampleType) )) + 
    geom_point(size=0.1, alpha=alpha2, shape=20 ) +  geom_abline(slope=1, intercept=0, lty=2, col="black", size=0.3) +
    ylim(yMin2, yMax2) +  xlim(xMin2, xMax2)+  xlab(xLab2) +   ylab(yLab2) +   ggtitle(title2) + scale_colour_manual( values=colours2 ) +
    guides( colour = guide_legend(override.aes = list(size=10)) )  + MyTheme_1_g( textSize=14,  hjust1=NULL, vjust1=NULL,  angle1=NULL)
  MySaveGgplot2_1_g(ggplot2Figure1=FigureTemp1,  path1=path2, fileName1=paste(fileName2, "_5_diff5",   sep="",  collapse=NULL),  height1=height2,  width1=width2)
}





## 2D density plot
MyScatterDiagram_2_f <- function(vector2X,  vector2Y,  path2,   fileName2,   xLab2,   yLab2,  title2,  height2=4,  width2=4,  yMin2=0, yMax2=2,  xMin2=0, xMax2=2 ) {
  vector2Y[vector2Y>yMax2] <- yMax2
  vector2Y[vector2Y<yMin2] <- yMin2
  vector2X[vector2X>xMax2] <- xMax2
  vector2X[vector2X<xMin2] <- xMin2  
  dataframeA <- data.frame( xAxis = vector2X,  yAxis = vector2Y  )
  
  FigureTemp1 <- ggplot( data = dataframeA   ) +  
    stat_density_2d(  aes(x=xAxis, y=yAxis, fill = ..density.. ) ,  geom = "raster" , contour=FALSE ) + 
    geom_abline(slope=1, intercept=10, lty=2, col="black", size=0.3) + geom_abline(slope=1, intercept= -10, lty=2, col="black", size=0.3) +
    geom_abline(slope=1, intercept=0, lty=2, col="black", size=0.3) +  scale_fill_continuous( low=c("white", "blue") ,  high=c("yellow", "red") , na.value = "white" ) +
    ylim(yMin2, yMax2) +  xlim(xMin2, xMax2)+  xlab(xLab2) +   ylab(yLab2) +   ggtitle(title2) + 
    guides( colour = guide_legend(override.aes = list(size=10)) )  + MyTheme_1_g( textSize=14,  hjust1=NULL, vjust1=NULL,  angle1=NULL)
  MySaveGgplot2_1_g(ggplot2Figure1=FigureTemp1,  path1=path2, fileName1=paste(fileName2, "_1_4colours",   sep="",  collapse=NULL),  height1=height2,  width1=width2)
  
  FigureTemp2 <- ggplot( data = dataframeA   ) +  
    stat_density_2d(  aes(x=xAxis, y=yAxis, fill = ..density.. ) ,  geom = "raster" , contour=FALSE ) + 
    geom_abline(slope=1, intercept=10, lty=2, col="black", size=0.3) + geom_abline(slope=1, intercept= -10, lty=2, col="black", size=0.3) +
    geom_abline(slope=1, intercept=0, lty=2, col="black", size=0.3) +  scale_fill_continuous( low=c("white", "blue") ,  high="red" , na.value = "white" ) +
    ylim(yMin2, yMax2) +  xlim(xMin2, xMax2)+  xlab(xLab2) +   ylab(yLab2) +   ggtitle(title2) + 
    guides( colour = guide_legend(override.aes = list(size=10)) )  + MyTheme_1_g( textSize=14,  hjust1=NULL, vjust1=NULL,  angle1=NULL)
  MySaveGgplot2_1_g(ggplot2Figure1=FigureTemp2,  path1=path2, fileName1=paste(fileName2, "_2_3colours",   sep="",  collapse=NULL),  height1=height2,  width1=width2)
  
  FigureTemp3 <- ggplot( data = dataframeA   ) +  
    stat_density_2d(  aes(x=xAxis, y=yAxis, fill = ..density.. ) ,  geom = "raster" , contour=FALSE  ) + 
    geom_abline(slope=1, intercept=10, lty=2, col="black", size=0.3) + geom_abline(slope=1, intercept= -10, lty=2, col="black", size=0.3) +
    geom_abline(slope=1, intercept=0, lty=2, col="black", size=0.3) +  scale_fill_continuous( low= "white"  ,  high="red" , na.value = "white" ) +
    ylim(yMin2, yMax2) +  xlim(xMin2, xMax2)+  xlab(xLab2) +   ylab(yLab2) +   ggtitle(title2) + 
    guides( colour = guide_legend(override.aes = list(size=10)) )  + MyTheme_1_g( textSize=14,  hjust1=NULL, vjust1=NULL,  angle1=NULL)
  MySaveGgplot2_1_g(ggplot2Figure1=FigureTemp3,  path1=path2, fileName1=paste(fileName2, "_3_2colours",   sep="",  collapse=NULL),  height1=height2,  width1=width2)
  
  FigureTemp4 <- ggplot( data = dataframeA   ) +  
    stat_density_2d(  aes(x=xAxis, y=yAxis, fill = ..density.. ) ,  geom = "raster" , contour=FALSE  ) + 
    geom_abline(slope=1, intercept=10, lty=2, col="black", size=0.3) + geom_abline(slope=1, intercept= -10, lty=2, col="black", size=0.3) +
    geom_abline(slope=1, intercept=0, lty=2, col="black", size=0.3) +  scale_fill_continuous( low= c("white", "red")  ,  high="red4" , na.value = "white" ) +
    ylim(yMin2, yMax2) +  xlim(xMin2, xMax2)+  xlab(xLab2) +   ylab(yLab2) +   ggtitle(title2) + 
    guides( colour = guide_legend(override.aes = list(size=10)) )  + MyTheme_1_g( textSize=14,  hjust1=NULL, vjust1=NULL,  angle1=NULL)
  MySaveGgplot2_1_g(ggplot2Figure1=FigureTemp4,  path1=path2, fileName1=paste(fileName2, "_4_other3colours",   sep="",  collapse=NULL),  height1=height2,  width1=width2)
  
  FigureTemp5 <- ggplot( data = dataframeA   ) +  
    stat_density_2d(  aes(x=xAxis, y=yAxis, fill = ..density.. ) ,  geom = "raster" , contour=FALSE  ) + 
    geom_abline(slope=1, intercept=10, lty=2, col="black", size=0.3) + geom_abline(slope=1, intercept= -10, lty=2, col="black", size=0.3) +
    geom_abline(slope=1, intercept=0, lty=2, col="black", size=0.3) +  scale_fill_continuous( low= c("white", "blue")  ,  high=c("red", "red4") , na.value = "white" ) +
    ylim(yMin2, yMax2) +  xlim(xMin2, xMax2)+  xlab(xLab2) +   ylab(yLab2) +   ggtitle(title2) + 
    guides( colour = guide_legend(override.aes = list(size=10)) )  + MyTheme_1_g( textSize=14,  hjust1=NULL, vjust1=NULL,  angle1=NULL)
  MySaveGgplot2_1_g(ggplot2Figure1=FigureTemp5,  path1=path2, fileName1=paste(fileName2, "_5_other4colours",   sep="",  collapse=NULL),  height1=height2,  width1=width2)  
} 





MyDistributionLine_1_f <- function(x2,  path2,  fileName2,  title2,  xLab2,  yLab2,   Ymin2=0,   Ymax2=10,   height2=4,  width2=7, yintercept2=c(-10, 0, 10)   ) {  
  myTempFrame_1 <- data.frame( xAxis = c(1:length(x2)),      yAxis = sort(x2) )
  
  FigureTemp1 <- ggplot(myTempFrame_1,   aes(x=xAxis, y=yAxis )  )  +  xlab(xLab2) +  ylab(yLab2) +  ggtitle(title2) + 
    geom_line(size=0.5, colour="red") + geom_hline(yintercept =yintercept2[1], colour="blue") + 
    geom_hline(yintercept = yintercept2[2], colour="blue") + geom_hline(yintercept = yintercept2[3], colour="blue") + 
    MyTheme_1_g(textSize1=14,  hjust1=NULL, vjust1=NULL,  angle1=NULL )    
  MySaveGgplot2_1_g(ggplot2Figure1=FigureTemp1,  path1=path2, fileName1=paste(fileName2, "-1",  sep="",  collapse=NULL),  height1=height2, width1=width2)
  
  FigureTemp1 <- ggplot(myTempFrame_1,   aes(x=xAxis, y=yAxis )  )  +  xlab(xLab2) +  ylab(yLab2) +  ggtitle(title2) + ylim(Ymin2, Ymax2) + 
    geom_line(size=0.5, colour="red") + geom_hline(yintercept =yintercept2[1], colour="blue") + 
    geom_hline(yintercept = yintercept2[2], colour="blue") + geom_hline(yintercept = yintercept2[3], colour="blue") + 
    MyTheme_1_g(textSize1=14,  hjust1=NULL, vjust1=NULL,  angle1=NULL )    
  MySaveGgplot2_1_g(ggplot2Figure1=FigureTemp1,  path1=path2, fileName1=paste(fileName2, "-2-limitY",  sep="",  collapse=NULL),  height1=height2, width1=width2)        
}  





## x and y are discrete, x is factor, y is numeric.
MyHeatmap_1_f <- function(matrix2,  path2,  fileName2,  title2,  xLab2,  yLab2,   Ymin2=0,   Ymax2=10,   height2=3, width2=4, midpoint2=0.5,  limits2=c(0, 1)  ) {  
  heatmap_1 <- melt(matrix2)
  
  FigureTemp1 <- ggplot(data=heatmap_1, aes(y=as.numeric(Var1), x=as.factor(Var2), fill=value) )  +  xlab(xLab2) +  ylab(yLab2) +  ggtitle(title2) + 
    geom_tile() + scale_fill_gradient2( low = "blue", mid = "white", high = "red", midpoint = midpoint2,  limits=limits2 )  + 
    scale_x_discrete(expand = c(0, 0))  + scale_y_discrete(expand = c(0, 0)) +  #scale_y_reverse() +  coord_flip() + 
    MyTheme_1_g(textSize1=14,  hjust1=NULL, vjust1=NULL,  angle1=NULL )  
  MySaveGgplot2_1_g(ggplot2Figure1=FigureTemp1,  path1=path2, fileName1=paste(fileName2, "-1",  sep="",  collapse=NULL),  height1=height2, width1=width2)
  
  FigureTemp2 <- ggplot(data=heatmap_1, aes(y=Var1, x=Var2, fill=value) )  +  xlab(xLab2) +  ylab(yLab2) +  ggtitle(title2) + 
    geom_tile() + scale_fill_gradient2( low = "yellow", mid = "yellowgreen", high = "green", midpoint = midpoint2,  limits=limits2 )  + 
    #scale_x_discrete(expand = c(0, 0))  + scale_y_discrete(expand = c(0, 0)) +  #scale_y_reverse() +  coord_flip() + 
    MyTheme_1_g(textSize1=14,  hjust1=NULL, vjust1=NULL,  angle1=NULL )  
  MySaveGgplot2_1_g(ggplot2Figure1=FigureTemp2,  path1=path2, fileName1=paste(fileName2, "-2",  sep="",  collapse=NULL),  height1=height2, width1=width2)
}  





## histogram for many categories in each column
MyHistogram_more1_f <- function(vector2, sampleType2,  path2,   fileName2,  title2,  xLab2, height2=4,  width2=4 ) {
  dataframeB  <- data.frame( xAxis = vector2,  sampleType=sampleType2 ) 
  
  FigureTemp1 <- ggplot(data=dataframeB, aes(x=xAxis) )  + geom_bar(aes(fill = sampleType), position = "fill") +  
    xlab(xLab2) + ylab("Relative frequency") +  ggtitle(title2)  +   
    MyTheme_1_g( hjust1=1, vjust1=1,  angle1=30,   textSize=14 )   
  MySaveGgplot2_1_g(ggplot2Figure1=FigureTemp1,  path1=path2, fileName1=paste(fileName2, "_relative",    sep="",  collapse=NULL),  height1=height2,  width1=width2)
  
  FigureTemp2 <- ggplot(data=dataframeB, aes(x=xAxis, fill=sampleType) )  + geom_bar(aes(fill = sampleType) ) +  
    xlab(xLab2) + ylab("Absolute frequency") +  ggtitle(title2)  +   
    MyTheme_1_g( hjust1=1, vjust1=1,  angle1=30,   textSize=14 )  
  MySaveGgplot2_1_g(ggplot2Figure1=FigureTemp2,  path1=path2, fileName1=paste(fileName2, "_absolute",    sep="",  collapse=NULL),  height1=height2,  width1=width2)
}






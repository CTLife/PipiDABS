MyCluster_1_g <- function( mymeth1 ,  path1, file1, width1, height1, sdThres1 ) { 
  if( nrow(mymeth1)*(1-sdThres1) > 10 ) {   
  pdf( file=paste(path1, file1, sep="/") , width=width1, height=height1  )
  methylKit::clusterSamples(mymeth1, dist="correlation", method="ward",         sd.filter=TRUE,   sd.threshold=sdThres1,        filterByQuantile=TRUE,   plot=TRUE )
  methylKit::clusterSamples(mymeth1, dist="euclidean",   method="ward",         sd.filter=TRUE,   sd.threshold=sdThres1,        filterByQuantile=TRUE,   plot=TRUE )
  methylKit::clusterSamples(mymeth1, dist="correlation", method="complete",     sd.filter=TRUE,   sd.threshold=sdThres1,        filterByQuantile=TRUE,   plot=TRUE )
  methylKit::clusterSamples(mymeth1, dist="euclidean",   method="complete",     sd.filter=TRUE,   sd.threshold=sdThres1,        filterByQuantile=TRUE,   plot=TRUE )
  dev.off()
  }
}





MyCluster_2_g <- function( mymeth1 ,  path1, file1, width1, height1, sdThres1 ) {  
  myTempValue = tryCatch(
    methylKit::clusterSamples(mymeth1, dist="correlation", method="ward",     sd.filter=TRUE,   sd.threshold=sdThres1,        filterByQuantile=FALSE, plot=FALSE  ), 
    error = function(err){"000"}
  )
  if( length( as.character(myTempValue) ) > 1) {
  pdf( file=paste(path1, file1, sep="/") , width=width1, height=height1  )
  methylKit::clusterSamples(mymeth1, dist="correlation", method="ward",         sd.filter=TRUE,   sd.threshold=sdThres1,        filterByQuantile=FALSE,   plot=TRUE )
  methylKit::clusterSamples(mymeth1, dist="euclidean",   method="ward",         sd.filter=TRUE,   sd.threshold=sdThres1,        filterByQuantile=FALSE,   plot=TRUE )
  methylKit::clusterSamples(mymeth1, dist="correlation", method="complete",     sd.filter=TRUE,   sd.threshold=sdThres1,        filterByQuantile=FALSE,   plot=TRUE )
  methylKit::clusterSamples(mymeth1, dist="euclidean",   method="complete",     sd.filter=TRUE,   sd.threshold=sdThres1,        filterByQuantile=FALSE,   plot=TRUE )
  dev.off()
  }
}





MyCluster_3_g <- function(  mymeth2 ,  path2,   file2, width2, height2 ) {
  myTempFunction <- function() {
  MyCluster_1_g(  mymeth1 = mymeth2 ,   path1 = path2, file1 = paste(file2, ".ByQuantile.1A.kept100percent.pdf", sep=""),   width1 = width2,   height1 = height2,   sdThres1 = 0   )                     
  MyCluster_1_g(  mymeth1 = mymeth2 ,   path1 = path2, file1 = paste(file2, ".ByQuantile.1B.kept90percent.pdf",  sep=""),   width1 = width2,   height1 = height2,   sdThres1 = 0.1 )                    
  MyCluster_1_g(  mymeth1 = mymeth2 ,   path1 = path2, file1 = paste(file2, ".ByQuantile.1C.kept80percent.pdf",  sep=""),   width1 = width2,   height1 = height2,   sdThres1 = 0.2 )                    
  MyCluster_1_g(  mymeth1 = mymeth2 ,   path1 = path2, file1 = paste(file2, ".ByQuantile.1D.kept70percent.pdf",  sep=""),   width1 = width2,   height1 = height2,   sdThres1 = 0.3 )                    
  MyCluster_1_g(  mymeth1 = mymeth2 ,   path1 = path2, file1 = paste(file2, ".ByQuantile.1E.kept60percent.pdf",  sep=""),   width1 = width2,   height1 = height2,   sdThres1 = 0.4 )                    
  MyCluster_1_g(  mymeth1 = mymeth2 ,   path1 = path2, file1 = paste(file2, ".ByQuantile.1F.kept50percent.pdf",  sep=""),   width1 = width2,   height1 = height2,   sdThres1 = 0.5 )                    
  MyCluster_1_g(  mymeth1 = mymeth2 ,   path1 = path2, file1 = paste(file2, ".ByQuantile.1G.kept40percent.pdf",  sep=""),   width1 = width2,   height1 = height2,   sdThres1 = 0.6 )                    
  MyCluster_1_g(  mymeth1 = mymeth2 ,   path1 = path2, file1 = paste(file2, ".ByQuantile.1H.kept30percent.pdf",  sep=""),   width1 = width2,   height1 = height2,   sdThres1 = 0.7 )                    
  MyCluster_1_g(  mymeth1 = mymeth2 ,   path1 = path2, file1 = paste(file2, ".ByQuantile.1I.kept20percent.pdf",  sep=""),   width1 = width2,   height1 = height2,   sdThres1 = 0.8 )                    
  MyCluster_1_g(  mymeth1 = mymeth2 ,   path1 = path2, file1 = paste(file2, ".ByQuantile.1J.kept10percent.pdf",  sep=""),   width1 = width2,   height1 = height2,   sdThres1 = 0.9 )                    
  MyCluster_1_g(  mymeth1 = mymeth2 ,   path1 = path2, file1 = paste(file2, ".ByQuantile.1K.kept5percent.pdf",   sep=""),   width1 = width2,   height1 = height2,   sdThres1 = 0.95)                    
  MyCluster_1_g(  mymeth1 = mymeth2 ,   path1 = path2, file1 = paste(file2, ".ByQuantile.1L.kept4percent.pdf",   sep=""),   width1 = width2,   height1 = height2,   sdThres1 = 0.96)                    
  MyCluster_1_g(  mymeth1 = mymeth2 ,   path1 = path2, file1 = paste(file2, ".ByQuantile.1M.kept3percent.pdf",   sep=""),   width1 = width2,   height1 = height2,   sdThres1 = 0.97)                    
  MyCluster_1_g(  mymeth1 = mymeth2 ,   path1 = path2, file1 = paste(file2, ".ByQuantile.1N.kept2percent.pdf",   sep=""),   width1 = width2,   height1 = height2,   sdThres1 = 0.98)                    
  MyCluster_1_g(  mymeth1 = mymeth2 ,   path1 = path2, file1 = paste(file2, ".ByQuantile.1O.kept1percent.pdf",   sep=""),   width1 = width2,   height1 = height2,   sdThres1 = 0.99)                    
  
  MyCluster_2_g(  mymeth1 = mymeth2 ,   path1 = path2, file1 = paste(file2, ".NoQuantile.2A.sd0.pdf",     sep=""),   width1 = width2,   height1 = height2,   sdThres1 = 0    )                     
  MyCluster_2_g(  mymeth1 = mymeth2 ,   path1 = path2, file1 = paste(file2, ".NoQuantile.2B.sd0.01.pdf",  sep=""),   width1 = width2,   height1 = height2,   sdThres1 = 0.01 )    
  MyCluster_2_g(  mymeth1 = mymeth2 ,   path1 = path2, file1 = paste(file2, ".NoQuantile.2C.sd0.03.pdf",  sep=""),   width1 = width2,   height1 = height2,   sdThres1 = 0.03 )                                    
  MyCluster_2_g(  mymeth1 = mymeth2 ,   path1 = path2, file1 = paste(file2, ".NoQuantile.2D.sd0.04.pdf",  sep=""),   width1 = width2,   height1 = height2,   sdThres1 = 0.04 )  
  MyCluster_2_g(  mymeth1 = mymeth2 ,   path1 = path2, file1 = paste(file2, ".NoQuantile.2E.sd0.05.pdf",  sep=""),   width1 = width2,   height1 = height2,   sdThres1 = 0.05 )                    
  MyCluster_2_g(  mymeth1 = mymeth2 ,   path1 = path2, file1 = paste(file2, ".NoQuantile.2F.sd0.07.pdf",  sep=""),   width1 = width2,   height1 = height2,   sdThres1 = 0.07 )                                      
  MyCluster_2_g(  mymeth1 = mymeth2 ,   path1 = path2, file1 = paste(file2, ".NoQuantile.2G.sd0.10.pdf",  sep=""),   width1 = width2,   height1 = height2,   sdThres1 = 0.10 )                    
  MyCluster_2_g(  mymeth1 = mymeth2 ,   path1 = path2, file1 = paste(file2, ".NoQuantile.2H.sd0.15.pdf",  sep=""),   width1 = width2,   height1 = height2,   sdThres1 = 0.15 )                    
  MyCluster_2_g(  mymeth1 = mymeth2 ,   path1 = path2, file1 = paste(file2, ".NoQuantile.2I.sd0.20.pdf",  sep=""),   width1 = width2,   height1 = height2,   sdThres1 = 0.20 )                    
  }
  tryCatch(
    myTempFunction(),
    error = function(err){"MyCluster_3_g_000123"}
  )
}






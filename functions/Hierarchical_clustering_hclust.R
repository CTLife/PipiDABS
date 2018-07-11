




myHierarchicalClustering_1_g  <- function(  mat_3three,   path_temp1,  width1, height1   )  {
  class_3three <- colnames(mat_3three)
  class_3three <- gsub(pattern="_\\d+$", replacement="", x=class_3three, ignore.case = FALSE, perl = TRUE )
  MyTech_color_g( class_3three )
  
  res.dist1_3three <- get_dist( t(mat_3three) ,   method = "euclidean")
  res.dist2_3three <- get_dist( t(mat_3three) ,   method = "maximum"  )
  res.dist3_3three <- get_dist( t(mat_3three) ,   method = "manhattan")
  res.dist4_3three <- get_dist( t(mat_3three) ,   method = "canberra" )
  res.dist5_3three <- get_dist( t(mat_3three) ,   method = "binary"   )
  res.dist6_3three <- get_dist( t(mat_3three) ,   method = "minkowski")
  res.dist7_3three <- get_dist( t(mat_3three) ,   method = "pearson"  )
  res.dist8_3three <- get_dist( t(mat_3three) ,   method = "spearman" )
  
  pdf( file = paste(path_temp1, "1A_visualizing-distance-matrix.pdf",  sep="/"), width=12, height=10 )
  print( fviz_dist(res.dist1_3three,  gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07")) )
  print( fviz_dist(res.dist2_3three,  gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07")) ) 
  print( fviz_dist(res.dist3_3three,  gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07")) ) 
  print( fviz_dist(res.dist4_3three,  gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07")) ) 
  print( fviz_dist(res.dist5_3three,  gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07")) ) 
  print( fviz_dist(res.dist6_3three,  gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07")) ) 
  print( fviz_dist(res.dist7_3three,  gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07")) ) 
  print( fviz_dist(res.dist8_3three,  gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07")) ) 
  dev.off() 
  
  sink( file = paste(path_temp1, "1B_all-distance-matrix.txt",  sep="/") )
  print("################### euclidean: ")
  print(res.dist1_3three ) 
  print("################### maximum: ")
  print(res.dist2_3three ) 
  print("################### manhattan: ")
  print(res.dist3_3three ) 
  print("################### canberra: ")
  print(res.dist4_3three ) 
  print("################### binary: ")
  print(res.dist5_3three ) 
  print("################### minkowski: ")
  print(res.dist6_3three ) 
  print("################### pearson: ")
  print(res.dist7_3three ) 
  print("################### spearman: ")
  print(res.dist8_3three ) 
  sink() 
  
  
  # Compute hierarchical clustering by "ward.D"
  res.hc1_3three <- hclust(res.dist1_3three, method = "ward.D"  )   
  res.hc2_3three <- hclust(res.dist2_3three, method = "ward.D"  )   
  res.hc3_3three <- hclust(res.dist3_3three, method = "ward.D"  )  
  res.hc4_3three <- hclust(res.dist4_3three, method = "ward.D"  )   
  res.hc5_3three <- hclust(res.dist5_3three, method = "ward.D"  )  
  res.hc6_3three <- hclust(res.dist6_3three, method = "ward.D"  )  
  res.hc7_3three <- hclust(res.dist7_3three, method = "ward.D"  )  
  res.hc8_3three <- hclust(res.dist8_3three, method = "ward.D"  ) 
  
  pdf( file = paste(path_temp1, "2A_hierarchical-ward.D-rectangle.pdf",  sep="/") , width=width1, height=height1 )
  print(  fviz_dend(res.hc1_3three,   type="rectangle", main="ward.D, euclidean"  )  )
  print(  fviz_dend(res.hc2_3three,   type="rectangle", main="ward.D, maximum"    )  )
  print(  fviz_dend(res.hc3_3three,   type="rectangle", main="ward.D, manhattan"  )  )
  print(  fviz_dend(res.hc4_3three,   type="rectangle", main="ward.D, canberra"   )  )
  print(  fviz_dend(res.hc5_3three,   type="rectangle", main="ward.D, binary"     )  )
  print(  fviz_dend(res.hc6_3three,   type="rectangle", main="ward.D, minkowski"  )  )
  print(  fviz_dend(res.hc7_3three,   type="rectangle", main="ward.D, pearson"    )  )
  print(  fviz_dend(res.hc8_3three,   type="rectangle", main="ward.D, spearman"   )  )
  dev.off() 
  
  pdf( file = paste(path_temp1, "2B_hierarchical-ward.D-phylogenic.pdf",  sep="/")  )
  print(  fviz_dend(res.hc1_3three,   type="phylogenic", main="ward.D, euclidean"  , repel = TRUE)  )
  print(  fviz_dend(res.hc2_3three,   type="phylogenic", main="ward.D, maximum"    , repel = TRUE)  )
  print(  fviz_dend(res.hc3_3three,   type="phylogenic", main="ward.D, manhattan"  , repel = TRUE)  )
  print(  fviz_dend(res.hc4_3three,   type="phylogenic", main="ward.D, canberra"   , repel = TRUE)  )
  print(  fviz_dend(res.hc5_3three,   type="phylogenic", main="ward.D, binary"     , repel = TRUE)  )
  print(  fviz_dend(res.hc6_3three,   type="phylogenic", main="ward.D, minkowski"  , repel = TRUE)  )
  print(  fviz_dend(res.hc7_3three,   type="phylogenic", main="ward.D, pearson"    , repel = TRUE)  )
  print(  fviz_dend(res.hc8_3three,   type="phylogenic", main="ward.D, spearman"   , repel = TRUE)  )          
  dev.off() 
  
  res.hca.color1_3three <- as.dendrogram( res.hc1_3three )   
  res.hca.color2_3three <- as.dendrogram( res.hc2_3three )   
  res.hca.color3_3three <- as.dendrogram( res.hc3_3three )  
  res.hca.color4_3three <- as.dendrogram( res.hc4_3three )   
  res.hca.color5_3three <- as.dendrogram( res.hc5_3three )  
  res.hca.color6_3three <- as.dendrogram( res.hc6_3three )  
  res.hca.color7_3three <- as.dendrogram( res.hc7_3three )  
  res.hca.color8_3three <- as.dendrogram( res.hc8_3three ) 
  
  colors_to_use <- as.numeric( MyTech_number_g( class_3three ) ) 
  length(colors_to_use)
  
  colors_to_use1 <- colors_to_use[order.dendrogram(res.hca.color1_3three)]
  colors_to_use2 <- colors_to_use[order.dendrogram(res.hca.color2_3three)]
  colors_to_use3 <- colors_to_use[order.dendrogram(res.hca.color3_3three)]
  colors_to_use4 <- colors_to_use[order.dendrogram(res.hca.color4_3three)]
  colors_to_use5 <- colors_to_use[order.dendrogram(res.hca.color5_3three)]
  colors_to_use6 <- colors_to_use[order.dendrogram(res.hca.color6_3three)]
  colors_to_use7 <- colors_to_use[order.dendrogram(res.hca.color7_3three)]
  colors_to_use8 <- colors_to_use[order.dendrogram(res.hca.color8_3three)]
  
  dendextend::labels_colors(res.hca.color1_3three) <-   colors_to_use1 
  dendextend::labels_colors(res.hca.color2_3three) <-   colors_to_use2 
  dendextend::labels_colors(res.hca.color3_3three) <-   colors_to_use3 
  dendextend::labels_colors(res.hca.color4_3three) <-   colors_to_use4 
  dendextend::labels_colors(res.hca.color5_3three) <-   colors_to_use5 
  dendextend::labels_colors(res.hca.color6_3three) <-   colors_to_use6
  dendextend::labels_colors(res.hca.color7_3three) <-   colors_to_use7 
  dendextend::labels_colors(res.hca.color8_3three) <-   colors_to_use8 
  
  pdf( file = paste(path_temp1, "2C_hierarchical-ward.D-plot.pdf",  sep="/") , width=width1, height=height1 )
  print(  plot(res.hca.color1_3three,   main="ward.D, euclidean" )  )
  print(  plot(res.hca.color2_3three,   main="ward.D, maximum"   )  )
  print(  plot(res.hca.color3_3three,   main="ward.D, manhattan" )  )
  print(  plot(res.hca.color4_3three,   main="ward.D, canberra"  )  )
  print(  plot(res.hca.color5_3three,   main="ward.D, binary"    )  )
  print(  plot(res.hca.color6_3three,   main="ward.D, minkowski" )  )
  print(  plot(res.hca.color7_3three,   main="ward.D, pearson"   )  )
  print(  plot(res.hca.color8_3three,   main="ward.D, spearman"  )  )          
  dev.off() 
  
  
  
  # Compute hierarchical clustering by "ward.D2"
  res.hc1_3three1 <- hclust(res.dist1_3three, method = "ward.D2"  )   
  res.hc2_3three1 <- hclust(res.dist2_3three, method = "ward.D2"  )   
  res.hc3_3three1 <- hclust(res.dist3_3three, method = "ward.D2"  )  
  res.hc4_3three1 <- hclust(res.dist4_3three, method = "ward.D2"  )   
  res.hc5_3three1 <- hclust(res.dist5_3three, method = "ward.D2"  )  
  res.hc6_3three1 <- hclust(res.dist6_3three, method = "ward.D2"  )  
  res.hc7_3three1 <- hclust(res.dist7_3three, method = "ward.D2"  )  
  res.hc8_3three1 <- hclust(res.dist8_3three, method = "ward.D2"  ) 
  
  pdf( file = paste(path_temp1, "3A_hierarchical-ward.D2-rectangle.pdf",  sep="/") , width=width1, height=height1 )
  print(  fviz_dend(res.hc1_3three1,   type="rectangle", main="ward.D2, euclidean"  )  )
  print(  fviz_dend(res.hc2_3three1,   type="rectangle", main="ward.D2, maximum"    )  )
  print(  fviz_dend(res.hc3_3three1,   type="rectangle", main="ward.D2, manhattan"  )  )
  print(  fviz_dend(res.hc4_3three1,   type="rectangle", main="ward.D2, canberra"   )  )
  print(  fviz_dend(res.hc5_3three1,   type="rectangle", main="ward.D2, binary"     )  )
  print(  fviz_dend(res.hc6_3three1,   type="rectangle", main="ward.D2, minkowski"  )  )
  print(  fviz_dend(res.hc7_3three1,   type="rectangle", main="ward.D2, pearson"    )  )
  print(  fviz_dend(res.hc8_3three1,   type="rectangle", main="ward.D2, spearman"   )  )
  dev.off() 
  
  pdf( file = paste(path_temp1, "3B_hierarchical-ward.D2-phylogenic.pdf",  sep="/")  )
  print(  fviz_dend(res.hc1_3three1,   type="phylogenic", main="ward.D2, euclidean"  , repel = TRUE)  )
  print(  fviz_dend(res.hc2_3three1,   type="phylogenic", main="ward.D2, maximum"    , repel = TRUE)  )
  print(  fviz_dend(res.hc3_3three1,   type="phylogenic", main="ward.D2, manhattan"  , repel = TRUE)  )
  print(  fviz_dend(res.hc4_3three1,   type="phylogenic", main="ward.D2, canberra"   , repel = TRUE)  )
  print(  fviz_dend(res.hc5_3three1,   type="phylogenic", main="ward.D2, binary"     , repel = TRUE)  )
  print(  fviz_dend(res.hc6_3three1,   type="phylogenic", main="ward.D2, minkowski"  , repel = TRUE)  )
  print(  fviz_dend(res.hc7_3three1,   type="phylogenic", main="ward.D2, pearson"    , repel = TRUE)  )
  print(  fviz_dend(res.hc8_3three1,   type="phylogenic", main="ward.D2, spearman"   , repel = TRUE)  )          
  dev.off() 
  
  
  res.hca.color1_3three1 <- as.dendrogram( res.hc1_3three1 )   
  res.hca.color2_3three1 <- as.dendrogram( res.hc2_3three1 )   
  res.hca.color3_3three1 <- as.dendrogram( res.hc3_3three1 )  
  res.hca.color4_3three1 <- as.dendrogram( res.hc4_3three1 )   
  res.hca.color5_3three1 <- as.dendrogram( res.hc5_3three1 )  
  res.hca.color6_3three1 <- as.dendrogram( res.hc6_3three1 )  
  res.hca.color7_3three1 <- as.dendrogram( res.hc7_3three1 )  
  res.hca.color8_3three1 <- as.dendrogram( res.hc8_3three1 ) 
  
  colors_to_use <- as.numeric( MyTech_number_g( class_3three ) ) 
  length(colors_to_use)
  
  colors_to_use1 <- colors_to_use[order.dendrogram(res.hca.color1_3three1)]
  colors_to_use2 <- colors_to_use[order.dendrogram(res.hca.color2_3three1)]
  colors_to_use3 <- colors_to_use[order.dendrogram(res.hca.color3_3three1)]
  colors_to_use4 <- colors_to_use[order.dendrogram(res.hca.color4_3three1)]
  colors_to_use5 <- colors_to_use[order.dendrogram(res.hca.color5_3three1)]
  colors_to_use6 <- colors_to_use[order.dendrogram(res.hca.color6_3three1)]
  colors_to_use7 <- colors_to_use[order.dendrogram(res.hca.color7_3three1)]
  colors_to_use8 <- colors_to_use[order.dendrogram(res.hca.color8_3three1)]
  
  dendextend::labels_colors(res.hca.color1_3three1) <-   colors_to_use1 
  dendextend::labels_colors(res.hca.color2_3three1) <-   colors_to_use2 
  dendextend::labels_colors(res.hca.color3_3three1) <-   colors_to_use3 
  dendextend::labels_colors(res.hca.color4_3three1) <-   colors_to_use4 
  dendextend::labels_colors(res.hca.color5_3three1) <-   colors_to_use5 
  dendextend::labels_colors(res.hca.color6_3three1) <-   colors_to_use6
  dendextend::labels_colors(res.hca.color7_3three1) <-   colors_to_use7 
  dendextend::labels_colors(res.hca.color8_3three1) <-   colors_to_use8 
  
  pdf( file = paste(path_temp1, "3C_hierarchical-ward.D2-plot.pdf",  sep="/") , width=width1, height=height1 )
  print(  plot(res.hca.color1_3three1,   main="ward.D2, euclidean" )  )
  print(  plot(res.hca.color2_3three1,   main="ward.D2, maximum"   )  )
  print(  plot(res.hca.color3_3three1,   main="ward.D2, manhattan" )  )
  print(  plot(res.hca.color4_3three1,   main="ward.D2, canberra"  )  )
  print(  plot(res.hca.color5_3three1,   main="ward.D2, binary"    )  )
  print(  plot(res.hca.color6_3three1,   main="ward.D2, minkowski" )  )
  print(  plot(res.hca.color7_3three1,   main="ward.D2, pearson"   )  )
  print(  plot(res.hca.color8_3three1,   main="ward.D2, spearman"  )  )          
  dev.off() 
  
  
  
  
  # Compute hierarchical clustering by "single"
  res.hc1_3three <- hclust(res.dist1_3three, method = "single"  )   
  res.hc2_3three <- hclust(res.dist2_3three, method = "single"  )   
  res.hc3_3three <- hclust(res.dist3_3three, method = "single"  )  
  res.hc4_3three <- hclust(res.dist4_3three, method = "single"  )   
  res.hc5_3three <- hclust(res.dist5_3three, method = "single"  )  
  res.hc6_3three <- hclust(res.dist6_3three, method = "single"  )  
  res.hc7_3three <- hclust(res.dist7_3three, method = "single"  )  
  res.hc8_3three <- hclust(res.dist8_3three, method = "single"  ) 
  
  pdf( file = paste(path_temp1, "4A_hierarchical-single-rectangle.pdf",  sep="/") , width=width1, height=height1 )
  print(  fviz_dend(res.hc1_3three,   type="rectangle", main="single, euclidean"  )  )
  print(  fviz_dend(res.hc2_3three,   type="rectangle", main="single, maximum"    )  )
  print(  fviz_dend(res.hc3_3three,   type="rectangle", main="single, manhattan"  )  )
  print(  fviz_dend(res.hc4_3three,   type="rectangle", main="single, canberra"   )  )
  print(  fviz_dend(res.hc5_3three,   type="rectangle", main="single, binary"     )  )
  print(  fviz_dend(res.hc6_3three,   type="rectangle", main="single, minkowski"  )  )
  print(  fviz_dend(res.hc7_3three,   type="rectangle", main="single, pearson"    )  )
  print(  fviz_dend(res.hc8_3three,   type="rectangle", main="single, spearman"   )  )
  dev.off() 
  
  pdf( file = paste(path_temp1, "4B_hierarchical-single-phylogenic.pdf",  sep="/") )
  print(  fviz_dend(res.hc1_3three,   type="phylogenic", main="single, euclidean"  , repel = TRUE)  )
  print(  fviz_dend(res.hc2_3three,   type="phylogenic", main="single, maximum"    , repel = TRUE)  )
  print(  fviz_dend(res.hc3_3three,   type="phylogenic", main="single, manhattan"  , repel = TRUE)  )
  print(  fviz_dend(res.hc4_3three,   type="phylogenic", main="single, canberra"   , repel = TRUE)  )
  print(  fviz_dend(res.hc5_3three,   type="phylogenic", main="single, binary"     , repel = TRUE)  )
  print(  fviz_dend(res.hc6_3three,   type="phylogenic", main="single, minkowski"  , repel = TRUE)  )
  print(  fviz_dend(res.hc7_3three,   type="phylogenic", main="single, pearson"    , repel = TRUE)  )
  print(  fviz_dend(res.hc8_3three,   type="phylogenic", main="single, spearman"   , repel = TRUE)  )         
  dev.off() 
  
  
  
  
  res.hca.color1_3three <- as.dendrogram( res.hc1_3three )   
  res.hca.color2_3three <- as.dendrogram( res.hc2_3three )   
  res.hca.color3_3three <- as.dendrogram( res.hc3_3three )  
  res.hca.color4_3three <- as.dendrogram( res.hc4_3three )   
  res.hca.color5_3three <- as.dendrogram( res.hc5_3three )  
  res.hca.color6_3three <- as.dendrogram( res.hc6_3three )  
  res.hca.color7_3three <- as.dendrogram( res.hc7_3three )  
  res.hca.color8_3three <- as.dendrogram( res.hc8_3three ) 
  
  colors_to_use <- as.numeric( MyTech_number_g( class_3three ) ) 
  length(colors_to_use)
  
  colors_to_use1 <- colors_to_use[order.dendrogram(res.hca.color1_3three)]
  colors_to_use2 <- colors_to_use[order.dendrogram(res.hca.color2_3three)]
  colors_to_use3 <- colors_to_use[order.dendrogram(res.hca.color3_3three)]
  colors_to_use4 <- colors_to_use[order.dendrogram(res.hca.color4_3three)]
  colors_to_use5 <- colors_to_use[order.dendrogram(res.hca.color5_3three)]
  colors_to_use6 <- colors_to_use[order.dendrogram(res.hca.color6_3three)]
  colors_to_use7 <- colors_to_use[order.dendrogram(res.hca.color7_3three)]
  colors_to_use8 <- colors_to_use[order.dendrogram(res.hca.color8_3three)]
  
  dendextend::labels_colors(res.hca.color1_3three) <-   colors_to_use1 
  dendextend::labels_colors(res.hca.color2_3three) <-   colors_to_use2 
  dendextend::labels_colors(res.hca.color3_3three) <-   colors_to_use3 
  dendextend::labels_colors(res.hca.color4_3three) <-   colors_to_use4 
  dendextend::labels_colors(res.hca.color5_3three) <-   colors_to_use5 
  dendextend::labels_colors(res.hca.color6_3three) <-   colors_to_use6
  dendextend::labels_colors(res.hca.color7_3three) <-   colors_to_use7 
  dendextend::labels_colors(res.hca.color8_3three) <-   colors_to_use8 
  
  pdf( file = paste(path_temp1, "4C_hierarchical-single-plot.pdf",  sep="/") , width=width1, height=height1 )
  print(  plot(res.hca.color1_3three,   main="single, euclidean" )  )
  print(  plot(res.hca.color2_3three,   main="single, maximum"   )  )
  print(  plot(res.hca.color3_3three,   main="single, manhattan" )  )
  print(  plot(res.hca.color4_3three,   main="single, canberra"  )  )
  print(  plot(res.hca.color5_3three,   main="single, binary"    )  )
  print(  plot(res.hca.color6_3three,   main="single, minkowski" )  )
  print(  plot(res.hca.color7_3three,   main="single, pearson"   )  )
  print(  plot(res.hca.color8_3three,   main="single, spearman"  )  )          
  dev.off() 
  
  
  
  
  # Compute hierarchical clustering by "complete"
  res.hc1_3three <- hclust(res.dist1_3three, method = "complete"  )   
  res.hc2_3three <- hclust(res.dist2_3three, method = "complete"  )   
  res.hc3_3three <- hclust(res.dist3_3three, method = "complete"  )  
  res.hc4_3three <- hclust(res.dist4_3three, method = "complete"  )   
  res.hc5_3three <- hclust(res.dist5_3three, method = "complete"  )  
  res.hc6_3three <- hclust(res.dist6_3three, method = "complete"  )  
  res.hc7_3three <- hclust(res.dist7_3three, method = "complete"  )  
  res.hc8_3three <- hclust(res.dist8_3three, method = "complete"  ) 
  
  pdf( file = paste(path_temp1, "5A_hierarchical-complete-rectangle.pdf",  sep="/") , width=width1, height=height1 )
  print(  fviz_dend(res.hc1_3three,   type="rectangle", main="complete, euclidean"  )  )
  print(  fviz_dend(res.hc2_3three,   type="rectangle", main="complete, maximum"    )  )
  print(  fviz_dend(res.hc3_3three,   type="rectangle", main="complete, manhattan"  )  )
  print(  fviz_dend(res.hc4_3three,   type="rectangle", main="complete, canberra"   )  )
  print(  fviz_dend(res.hc5_3three,   type="rectangle", main="complete, binary"     )  )
  print(  fviz_dend(res.hc6_3three,   type="rectangle", main="complete, minkowski"  )  )
  print(  fviz_dend(res.hc7_3three,   type="rectangle", main="complete, pearson"    )  )
  print(  fviz_dend(res.hc8_3three,   type="rectangle", main="complete, spearman"   )  )
  dev.off() 
  
  pdf( file = paste(path_temp1, "5B_hierarchical-complete-phylogenic.pdf",  sep="/") )
  print(  fviz_dend(res.hc1_3three,   type="phylogenic", main="complete, euclidean"  , repel = TRUE)  )
  print(  fviz_dend(res.hc2_3three,   type="phylogenic", main="complete, maximum"    , repel = TRUE)  )
  print(  fviz_dend(res.hc3_3three,   type="phylogenic", main="complete, manhattan"  , repel = TRUE)  )
  print(  fviz_dend(res.hc4_3three,   type="phylogenic", main="complete, canberra"   , repel = TRUE)  )
  print(  fviz_dend(res.hc5_3three,   type="phylogenic", main="complete, binary"     , repel = TRUE)  )
  print(  fviz_dend(res.hc6_3three,   type="phylogenic", main="complete, minkowski"  , repel = TRUE)  )
  print(  fviz_dend(res.hc7_3three,   type="phylogenic", main="complete, pearson"    , repel = TRUE)  )
  print(  fviz_dend(res.hc8_3three,   type="phylogenic", main="complete, spearman"   , repel = TRUE)  )          
  dev.off() 
  
  res.hca.color1_3three <- as.dendrogram( res.hc1_3three )   
  res.hca.color2_3three <- as.dendrogram( res.hc2_3three )   
  res.hca.color3_3three <- as.dendrogram( res.hc3_3three )  
  res.hca.color4_3three <- as.dendrogram( res.hc4_3three )   
  res.hca.color5_3three <- as.dendrogram( res.hc5_3three )  
  res.hca.color6_3three <- as.dendrogram( res.hc6_3three )  
  res.hca.color7_3three <- as.dendrogram( res.hc7_3three )  
  res.hca.color8_3three <- as.dendrogram( res.hc8_3three ) 
  
  colors_to_use <- as.numeric( MyTech_number_g( class_3three ) ) 
  length(colors_to_use)
  
  colors_to_use1 <- colors_to_use[order.dendrogram(res.hca.color1_3three)]
  colors_to_use2 <- colors_to_use[order.dendrogram(res.hca.color2_3three)]
  colors_to_use3 <- colors_to_use[order.dendrogram(res.hca.color3_3three)]
  colors_to_use4 <- colors_to_use[order.dendrogram(res.hca.color4_3three)]
  colors_to_use5 <- colors_to_use[order.dendrogram(res.hca.color5_3three)]
  colors_to_use6 <- colors_to_use[order.dendrogram(res.hca.color6_3three)]
  colors_to_use7 <- colors_to_use[order.dendrogram(res.hca.color7_3three)]
  colors_to_use8 <- colors_to_use[order.dendrogram(res.hca.color8_3three)]
  
  dendextend::labels_colors(res.hca.color1_3three) <-   colors_to_use1 
  dendextend::labels_colors(res.hca.color2_3three) <-   colors_to_use2 
  dendextend::labels_colors(res.hca.color3_3three) <-   colors_to_use3 
  dendextend::labels_colors(res.hca.color4_3three) <-   colors_to_use4 
  dendextend::labels_colors(res.hca.color5_3three) <-   colors_to_use5 
  dendextend::labels_colors(res.hca.color6_3three) <-   colors_to_use6
  dendextend::labels_colors(res.hca.color7_3three) <-   colors_to_use7 
  dendextend::labels_colors(res.hca.color8_3three) <-   colors_to_use8 
  
  pdf( file = paste(path_temp1, "5C_hierarchical-complete-plot.pdf",  sep="/") , width=width1, height=height1 )
  print(  plot(res.hca.color1_3three,   main="complete, euclidean" )  )
  print(  plot(res.hca.color2_3three,   main="complete, maximum"   )  )
  print(  plot(res.hca.color3_3three,   main="complete, manhattan" )  )
  print(  plot(res.hca.color4_3three,   main="complete, canberra"  )  )
  print(  plot(res.hca.color5_3three,   main="complete, binary"    )  )
  print(  plot(res.hca.color6_3three,   main="complete, minkowski" )  )
  print(  plot(res.hca.color7_3three,   main="complete, pearson"   )  )
  print(  plot(res.hca.color8_3three,   main="complete, spearman"  )  )          
  dev.off() 
  
  
  
  # Compute hierarchical clustering by "average"
  res.hc1_3three <- hclust(res.dist1_3three, method = "average"  )   
  res.hc2_3three <- hclust(res.dist2_3three, method = "average"  )   
  res.hc3_3three <- hclust(res.dist3_3three, method = "average"  )  
  res.hc4_3three <- hclust(res.dist4_3three, method = "average"  )   
  res.hc5_3three <- hclust(res.dist5_3three, method = "average"  )  
  res.hc6_3three <- hclust(res.dist6_3three, method = "average"  )  
  res.hc7_3three <- hclust(res.dist7_3three, method = "average"  )  
  res.hc8_3three <- hclust(res.dist8_3three, method = "average"  ) 
  
  pdf( file = paste(path_temp1, "6A_hierarchical-average-rectangle.pdf",  sep="/") , width=width1, height=height1 )
  print(  fviz_dend(res.hc1_3three,   type="rectangle", main="average, euclidean"  )  )
  print(  fviz_dend(res.hc2_3three,   type="rectangle", main="average, maximum"    )  )
  print(  fviz_dend(res.hc3_3three,   type="rectangle", main="average, manhattan"  )  )
  print(  fviz_dend(res.hc4_3three,   type="rectangle", main="average, canberra"   )  )
  print(  fviz_dend(res.hc5_3three,   type="rectangle", main="average, binary"     )  )
  print(  fviz_dend(res.hc6_3three,   type="rectangle", main="average, minkowski"  )  )
  print(  fviz_dend(res.hc7_3three,   type="rectangle", main="average, pearson"    )  )
  print(  fviz_dend(res.hc8_3three,   type="rectangle", main="average, spearman"   )  )
  dev.off() 
  
  pdf( file = paste(path_temp1, "6B_hierarchical-average-phylogenic.pdf",  sep="/") )
  print(  fviz_dend(res.hc1_3three,   type="phylogenic", main="average, euclidean"  , repel = TRUE)  )
  print(  fviz_dend(res.hc2_3three,   type="phylogenic", main="average, maximum"    , repel = TRUE)  )
  print(  fviz_dend(res.hc3_3three,   type="phylogenic", main="average, manhattan"  , repel = TRUE)  )
  print(  fviz_dend(res.hc4_3three,   type="phylogenic", main="average, canberra"   , repel = TRUE)  )
  print(  fviz_dend(res.hc5_3three,   type="phylogenic", main="average, binary"     , repel = TRUE)  )
  print(  fviz_dend(res.hc6_3three,   type="phylogenic", main="average, minkowski"  , repel = TRUE)  )
  print(  fviz_dend(res.hc7_3three,   type="phylogenic", main="average, pearson"    , repel = TRUE)  )
  print(  fviz_dend(res.hc8_3three,   type="phylogenic", main="average, spearman"   , repel = TRUE)  )          
  dev.off() 
  
  
  res.hca.color1_3three <- as.dendrogram( res.hc1_3three )   
  res.hca.color2_3three <- as.dendrogram( res.hc2_3three )   
  res.hca.color3_3three <- as.dendrogram( res.hc3_3three )  
  res.hca.color4_3three <- as.dendrogram( res.hc4_3three )   
  res.hca.color5_3three <- as.dendrogram( res.hc5_3three )  
  res.hca.color6_3three <- as.dendrogram( res.hc6_3three )  
  res.hca.color7_3three <- as.dendrogram( res.hc7_3three )  
  res.hca.color8_3three <- as.dendrogram( res.hc8_3three ) 
  
  colors_to_use <- as.numeric( MyTech_number_g( class_3three ) ) 
  length(colors_to_use)
  
  colors_to_use1 <- colors_to_use[order.dendrogram(res.hca.color1_3three)]
  colors_to_use2 <- colors_to_use[order.dendrogram(res.hca.color2_3three)]
  colors_to_use3 <- colors_to_use[order.dendrogram(res.hca.color3_3three)]
  colors_to_use4 <- colors_to_use[order.dendrogram(res.hca.color4_3three)]
  colors_to_use5 <- colors_to_use[order.dendrogram(res.hca.color5_3three)]
  colors_to_use6 <- colors_to_use[order.dendrogram(res.hca.color6_3three)]
  colors_to_use7 <- colors_to_use[order.dendrogram(res.hca.color7_3three)]
  colors_to_use8 <- colors_to_use[order.dendrogram(res.hca.color8_3three)]
  
  dendextend::labels_colors(res.hca.color1_3three) <-   colors_to_use1 
  dendextend::labels_colors(res.hca.color2_3three) <-   colors_to_use2 
  dendextend::labels_colors(res.hca.color3_3three) <-   colors_to_use3 
  dendextend::labels_colors(res.hca.color4_3three) <-   colors_to_use4 
  dendextend::labels_colors(res.hca.color5_3three) <-   colors_to_use5 
  dendextend::labels_colors(res.hca.color6_3three) <-   colors_to_use6
  dendextend::labels_colors(res.hca.color7_3three) <-   colors_to_use7 
  dendextend::labels_colors(res.hca.color8_3three) <-   colors_to_use8 
  
  pdf( file = paste(path_temp1, "6C_hierarchical-average-plot.pdf",  sep="/") , width=width1, height=height1 )
  print(  plot(res.hca.color1_3three,   main="average, euclidean" )  )
  print(  plot(res.hca.color2_3three,   main="average, maximum"   )  )
  print(  plot(res.hca.color3_3three,   main="average, manhattan" )  )
  print(  plot(res.hca.color4_3three,   main="average, canberra"  )  )
  print(  plot(res.hca.color5_3three,   main="average, binary"    )  )
  print(  plot(res.hca.color6_3three,   main="average, minkowski" )  )
  print(  plot(res.hca.color7_3three,   main="average, pearson"   )  )
  print(  plot(res.hca.color8_3three,   main="average, spearman"  )  )          
  dev.off() 
  
  
  
  # Compute hierarchical clustering by "mcquitty"
  res.hc1_3three <- hclust(res.dist1_3three, method = "mcquitty"  )   
  res.hc2_3three <- hclust(res.dist2_3three, method = "mcquitty"  )   
  res.hc3_3three <- hclust(res.dist3_3three, method = "mcquitty"  )  
  res.hc4_3three <- hclust(res.dist4_3three, method = "mcquitty"  )   
  res.hc5_3three <- hclust(res.dist5_3three, method = "mcquitty"  )  
  res.hc6_3three <- hclust(res.dist6_3three, method = "mcquitty"  )  
  res.hc7_3three <- hclust(res.dist7_3three, method = "mcquitty"  )  
  res.hc8_3three <- hclust(res.dist8_3three, method = "mcquitty"  ) 
  
  pdf( file = paste(path_temp1, "7A_hierarchical-mcquitty-rectangle.pdf",  sep="/") , width=width1, height=height1 )
  print(  fviz_dend(res.hc1_3three,   type="rectangle", main="mcquitty, euclidean"  )  )
  print(  fviz_dend(res.hc2_3three,   type="rectangle", main="mcquitty, maximum"    )  )
  print(  fviz_dend(res.hc3_3three,   type="rectangle", main="mcquitty, manhattan"  )  )
  print(  fviz_dend(res.hc4_3three,   type="rectangle", main="mcquitty, canberra"   )  )
  print(  fviz_dend(res.hc5_3three,   type="rectangle", main="mcquitty, binary"     )  )
  print(  fviz_dend(res.hc6_3three,   type="rectangle", main="mcquitty, minkowski"  )  )
  print(  fviz_dend(res.hc7_3three,   type="rectangle", main="mcquitty, pearson"    )  )
  print(  fviz_dend(res.hc8_3three,   type="rectangle", main="mcquitty, spearman"   )  )
  dev.off() 
  
  pdf( file = paste(path_temp1, "7B_hierarchical-mcquitty-phylogenic.pdf",  sep="/") )
  print(  fviz_dend(res.hc1_3three,   type="phylogenic", main="mcquitty, euclidean"  , repel = TRUE)  )
  print(  fviz_dend(res.hc2_3three,   type="phylogenic", main="mcquitty, maximum"    , repel = TRUE)  )
  print(  fviz_dend(res.hc3_3three,   type="phylogenic", main="mcquitty, manhattan"  , repel = TRUE)  )
  print(  fviz_dend(res.hc4_3three,   type="phylogenic", main="mcquitty, canberra"   , repel = TRUE)  )
  print(  fviz_dend(res.hc5_3three,   type="phylogenic", main="mcquitty, binary"     , repel = TRUE)  )
  print(  fviz_dend(res.hc6_3three,   type="phylogenic", main="mcquitty, minkowski"  , repel = TRUE)  )
  print(  fviz_dend(res.hc7_3three,   type="phylogenic", main="mcquitty, pearson"    , repel = TRUE)  )
  print(  fviz_dend(res.hc8_3three,   type="phylogenic", main="mcquitty, spearman"   , repel = TRUE)  )          
  dev.off() 
  
  
  res.hca.color1_3three <- as.dendrogram( res.hc1_3three )   
  res.hca.color2_3three <- as.dendrogram( res.hc2_3three )   
  res.hca.color3_3three <- as.dendrogram( res.hc3_3three )  
  res.hca.color4_3three <- as.dendrogram( res.hc4_3three )   
  res.hca.color5_3three <- as.dendrogram( res.hc5_3three )  
  res.hca.color6_3three <- as.dendrogram( res.hc6_3three )  
  res.hca.color7_3three <- as.dendrogram( res.hc7_3three )  
  res.hca.color8_3three <- as.dendrogram( res.hc8_3three ) 
  
  colors_to_use <- as.numeric( MyTech_number_g( class_3three ) ) 
  length(colors_to_use)
  
  colors_to_use1 <- colors_to_use[order.dendrogram(res.hca.color1_3three)]
  colors_to_use2 <- colors_to_use[order.dendrogram(res.hca.color2_3three)]
  colors_to_use3 <- colors_to_use[order.dendrogram(res.hca.color3_3three)]
  colors_to_use4 <- colors_to_use[order.dendrogram(res.hca.color4_3three)]
  colors_to_use5 <- colors_to_use[order.dendrogram(res.hca.color5_3three)]
  colors_to_use6 <- colors_to_use[order.dendrogram(res.hca.color6_3three)]
  colors_to_use7 <- colors_to_use[order.dendrogram(res.hca.color7_3three)]
  colors_to_use8 <- colors_to_use[order.dendrogram(res.hca.color8_3three)]
  
  dendextend::labels_colors(res.hca.color1_3three) <-   colors_to_use1 
  dendextend::labels_colors(res.hca.color2_3three) <-   colors_to_use2 
  dendextend::labels_colors(res.hca.color3_3three) <-   colors_to_use3 
  dendextend::labels_colors(res.hca.color4_3three) <-   colors_to_use4 
  dendextend::labels_colors(res.hca.color5_3three) <-   colors_to_use5 
  dendextend::labels_colors(res.hca.color6_3three) <-   colors_to_use6
  dendextend::labels_colors(res.hca.color7_3three) <-   colors_to_use7 
  dendextend::labels_colors(res.hca.color8_3three) <-   colors_to_use8 
  
  pdf( file = paste(path_temp1, "7C_hierarchical-mcquitty-plot.pdf",  sep="/") , width=width1, height=height1 )
  print(  plot(res.hca.color1_3three,   main="mcquitty, euclidean" )  )
  print(  plot(res.hca.color2_3three,   main="mcquitty, maximum"   )  )
  print(  plot(res.hca.color3_3three,   main="mcquitty, manhattan" )  )
  print(  plot(res.hca.color4_3three,   main="mcquitty, canberra"  )  )
  print(  plot(res.hca.color5_3three,   main="mcquitty, binary"    )  )
  print(  plot(res.hca.color6_3three,   main="mcquitty, minkowski" )  )
  print(  plot(res.hca.color7_3three,   main="mcquitty, pearson"   )  )
  print(  plot(res.hca.color8_3three,   main="mcquitty, spearman"  )  )          
  dev.off() 
  
  
  
  # Compute hierarchical clustering by "median"
  res.hc1_3three <- hclust(res.dist1_3three, method = "median"  )   
  res.hc2_3three <- hclust(res.dist2_3three, method = "median"  )   
  res.hc3_3three <- hclust(res.dist3_3three, method = "median"  )  
  res.hc4_3three <- hclust(res.dist4_3three, method = "median"  )   
  res.hc5_3three <- hclust(res.dist5_3three, method = "median"  )  
  res.hc6_3three <- hclust(res.dist6_3three, method = "median"  )  
  res.hc7_3three <- hclust(res.dist7_3three, method = "median"  )  
  res.hc8_3three <- hclust(res.dist8_3three, method = "median"  ) 
  
  pdf( file = paste(path_temp1, "8A_hierarchical-median-rectangle.pdf",  sep="/") , width=width1, height=height1 )
  print(  fviz_dend(res.hc1_3three,   type="rectangle", main="median, euclidean"  )  )
  print(  fviz_dend(res.hc2_3three,   type="rectangle", main="median, maximum"    )  )
  print(  fviz_dend(res.hc3_3three,   type="rectangle", main="median, manhattan"  )  )
  print(  fviz_dend(res.hc4_3three,   type="rectangle", main="median, canberra"   )  )
  print(  fviz_dend(res.hc5_3three,   type="rectangle", main="median, binary"     )  )
  print(  fviz_dend(res.hc6_3three,   type="rectangle", main="median, minkowski"  )  )
  print(  fviz_dend(res.hc7_3three,   type="rectangle", main="median, pearson"    )  )
  print(  fviz_dend(res.hc8_3three,   type="rectangle", main="median, spearman"   )  )
  dev.off() 
  
  
  res.hca.color1_3three <- as.dendrogram( res.hc1_3three )   
  res.hca.color2_3three <- as.dendrogram( res.hc2_3three )   
  res.hca.color3_3three <- as.dendrogram( res.hc3_3three )  
  res.hca.color4_3three <- as.dendrogram( res.hc4_3three )   
  res.hca.color5_3three <- as.dendrogram( res.hc5_3three )  
  res.hca.color6_3three <- as.dendrogram( res.hc6_3three )  
  res.hca.color7_3three <- as.dendrogram( res.hc7_3three )  
  res.hca.color8_3three <- as.dendrogram( res.hc8_3three ) 
  
  colors_to_use <- as.numeric( MyTech_number_g( class_3three ) ) 
  length(colors_to_use)
  
  colors_to_use1 <- colors_to_use[order.dendrogram(res.hca.color1_3three)]
  colors_to_use2 <- colors_to_use[order.dendrogram(res.hca.color2_3three)]
  colors_to_use3 <- colors_to_use[order.dendrogram(res.hca.color3_3three)]
  colors_to_use4 <- colors_to_use[order.dendrogram(res.hca.color4_3three)]
  colors_to_use5 <- colors_to_use[order.dendrogram(res.hca.color5_3three)]
  colors_to_use6 <- colors_to_use[order.dendrogram(res.hca.color6_3three)]
  colors_to_use7 <- colors_to_use[order.dendrogram(res.hca.color7_3three)]
  colors_to_use8 <- colors_to_use[order.dendrogram(res.hca.color8_3three)]
  
  dendextend::labels_colors(res.hca.color1_3three) <-   colors_to_use1 
  dendextend::labels_colors(res.hca.color2_3three) <-   colors_to_use2 
  dendextend::labels_colors(res.hca.color3_3three) <-   colors_to_use3 
  dendextend::labels_colors(res.hca.color4_3three) <-   colors_to_use4 
  dendextend::labels_colors(res.hca.color5_3three) <-   colors_to_use5 
  dendextend::labels_colors(res.hca.color6_3three) <-   colors_to_use6
  dendextend::labels_colors(res.hca.color7_3three) <-   colors_to_use7 
  dendextend::labels_colors(res.hca.color8_3three) <-   colors_to_use8 
  
  pdf( file = paste(path_temp1, "8C_hierarchical-median-plot.pdf",  sep="/") , width=width1, height=height1 )
  print(  plot(res.hca.color1_3three,   main="median, euclidean" )  )
  print(  plot(res.hca.color2_3three,   main="median, maximum"   )  )
  print(  plot(res.hca.color3_3three,   main="median, manhattan" )  )
  print(  plot(res.hca.color4_3three,   main="median, canberra"  )  )
  print(  plot(res.hca.color5_3three,   main="median, binary"    )  )
  print(  plot(res.hca.color6_3three,   main="median, minkowski" )  )
  print(  plot(res.hca.color7_3three,   main="median, pearson"   )  )
  print(  plot(res.hca.color8_3three,   main="median, spearman"  )  )          
  dev.off() 
  
  
  
  
  
  # Compute hierarchical clustering by "centroid"
  res.hc1_3three <- hclust(res.dist1_3three, method = "centroid"  )   
  res.hc2_3three <- hclust(res.dist2_3three, method = "centroid"  )   
  res.hc3_3three <- hclust(res.dist3_3three, method = "centroid"  )  
  res.hc4_3three <- hclust(res.dist4_3three, method = "centroid"  )   
  res.hc5_3three <- hclust(res.dist5_3three, method = "centroid"  )  
  res.hc6_3three <- hclust(res.dist6_3three, method = "centroid"  )  
  res.hc7_3three <- hclust(res.dist7_3three, method = "centroid"  )  
  res.hc8_3three <- hclust(res.dist8_3three, method = "centroid"  ) 
  
  pdf( file = paste(path_temp1, "9A_hierarchical-centroid-rectangle.pdf",  sep="/"), width=width1, height=height1  )
  print(  fviz_dend(res.hc1_3three,   type="rectangle", main="centroid, euclidean"  )  )
  print(  fviz_dend(res.hc2_3three,   type="rectangle", main="centroid, maximum"    )  )
  print(  fviz_dend(res.hc3_3three,   type="rectangle", main="centroid, manhattan"  )  )
  print(  fviz_dend(res.hc4_3three,   type="rectangle", main="centroid, canberra"   )  )
  print(  fviz_dend(res.hc5_3three,   type="rectangle", main="centroid, binary"     )  )
  print(  fviz_dend(res.hc6_3three,   type="rectangle", main="centroid, minkowski"  )  )
  print(  fviz_dend(res.hc7_3three,   type="rectangle", main="centroid, pearson"    )  )
  print(  fviz_dend(res.hc8_3three,   type="rectangle", main="centroid, spearman"   )  )
  dev.off() 
  
  
  
  res.hca.color1_3three <- as.dendrogram( res.hc1_3three )   
  res.hca.color2_3three <- as.dendrogram( res.hc2_3three )   
  res.hca.color3_3three <- as.dendrogram( res.hc3_3three )  
  res.hca.color4_3three <- as.dendrogram( res.hc4_3three )   
  res.hca.color5_3three <- as.dendrogram( res.hc5_3three )  
  res.hca.color6_3three <- as.dendrogram( res.hc6_3three )  
  res.hca.color7_3three <- as.dendrogram( res.hc7_3three )  
  res.hca.color8_3three <- as.dendrogram( res.hc8_3three ) 
  
  colors_to_use <- as.numeric( MyTech_number_g( class_3three ) ) 
  length(colors_to_use)
  
  colors_to_use1 <- colors_to_use[order.dendrogram(res.hca.color1_3three)]
  colors_to_use2 <- colors_to_use[order.dendrogram(res.hca.color2_3three)]
  colors_to_use3 <- colors_to_use[order.dendrogram(res.hca.color3_3three)]
  colors_to_use4 <- colors_to_use[order.dendrogram(res.hca.color4_3three)]
  colors_to_use5 <- colors_to_use[order.dendrogram(res.hca.color5_3three)]
  colors_to_use6 <- colors_to_use[order.dendrogram(res.hca.color6_3three)]
  colors_to_use7 <- colors_to_use[order.dendrogram(res.hca.color7_3three)]
  colors_to_use8 <- colors_to_use[order.dendrogram(res.hca.color8_3three)]
  
  dendextend::labels_colors(res.hca.color1_3three) <-   colors_to_use1 
  dendextend::labels_colors(res.hca.color2_3three) <-   colors_to_use2 
  dendextend::labels_colors(res.hca.color3_3three) <-   colors_to_use3 
  dendextend::labels_colors(res.hca.color4_3three) <-   colors_to_use4 
  dendextend::labels_colors(res.hca.color5_3three) <-   colors_to_use5 
  dendextend::labels_colors(res.hca.color6_3three) <-   colors_to_use6
  dendextend::labels_colors(res.hca.color7_3three) <-   colors_to_use7 
  dendextend::labels_colors(res.hca.color8_3three) <-   colors_to_use8 
  
  pdf( file = paste(path_temp1, "9C_hierarchical-centroid-plot.pdf",  sep="/") , width=width1, height=height1 )
  print(  plot(res.hca.color1_3three,   main="centroid, euclidean" )  )
  print(  plot(res.hca.color2_3three,   main="centroid, maximum"   )  )
  print(  plot(res.hca.color3_3three,   main="centroid, manhattan" )  )
  print(  plot(res.hca.color4_3three,   main="centroid, canberra"  )  )
  print(  plot(res.hca.color5_3three,   main="centroid, binary"    )  )
  print(  plot(res.hca.color6_3three,   main="centroid, minkowski" )  )
  print(  plot(res.hca.color7_3three,   main="centroid, pearson"   )  )
  print(  plot(res.hca.color8_3three,   main="centroid, spearman"  )  )          
  dev.off() 
  
}







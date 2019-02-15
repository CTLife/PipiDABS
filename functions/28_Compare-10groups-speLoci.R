


myCompare_10groups_speLoci_g <- function(methylBase_temp_1,  myPath_temp_1,  qvalue_temp_1,  differenceOfMethylation_temp_1,  mergeDistance_temp1=100,  minBases_temp_1,  numCores_temp_1=8 ) {                                             

if( (! is.null(Label_one_1_g)) && (! is.null(Label_two_2_g))   ) {
    dataFrame_temp1_A <- data.frame(
      mysampleID  = c(SampleID_one_1_g,   SampleID_two_2_g),
      mytreatment = c(Treatment_one_1_g,  Treatment_two_2_g),
      mysex       = c(Sex_one_1_g,        Sex_two_2_g),
      mytech      = c(Type_one_1_g,       Type_two_2_g), 
      myfiles     = c(Files_one_1_g,      Files_two_2_g)
    )
     myMainFunction_speRegions_All_g(myobj_temp3 = methylBase_temp_1,     
                       path_temp3  = paste(myPath_temp_1, "/1_",  Label_one_1_g, "_vs_", Label_two_2_g,  sep="")  ,   
                       qvalue_temp3=qvalue_temp_1,  differenceOfMethylation_temp3=differenceOfMethylation_temp_1,  
                       mergeDistance_temp3=mergeDistance_temp1,      
                       binBases_temp3=minBases_temp_1, dataFrame_temp3=dataFrame_temp1_A, numCores_temp3=numCores_temp_1 
                      ) 
}

if( (! is.null(Label_one_1_g)) && (! is.null(Label_three_3_g))   ) {
    dataFrame_temp1_A <- data.frame(
      mysampleID  = c(SampleID_one_1_g,   SampleID_three_3_g),
      mytreatment = c(Treatment_one_1_g,  Treatment_three_3_g),
      mysex       = c(Sex_one_1_g,        Sex_three_3_g),
      mytech      = c(Type_one_1_g,       Type_three_3_g), 
      myfiles     = c(Files_one_1_g,      Files_three_3_g)
    )
     myMainFunction_speRegions_All_g(myobj_temp3 = methylBase_temp_1,     
                       path_temp3  = paste(myPath_temp_1, "/2_",  Label_one_1_g, "_vs_", Label_three_3_g,  sep="")  ,   
                       qvalue_temp3=qvalue_temp_1,  differenceOfMethylation_temp3=differenceOfMethylation_temp_1,  
                       mergeDistance_temp3=mergeDistance_temp1,           
                        binBases_temp3=minBases_temp_1, dataFrame_temp3=dataFrame_temp1_A, numCores_temp3=numCores_temp_1 
                      ) 
}

if( (! is.null(Label_one_1_g)) && (! is.null(Label_four_4_g))   ) {
    dataFrame_temp1_A <- data.frame(
      mysampleID  = c(SampleID_one_1_g,   SampleID_four_4_g),
      mytreatment = c(Treatment_one_1_g,  Treatment_four_4_g),
      mysex       = c(Sex_one_1_g,        Sex_four_4_g),
      mytech      = c(Type_one_1_g,       Type_four_4_g), 
      myfiles     = c(Files_one_1_g,      Files_four_4_g)
    )
     myMainFunction_speRegions_All_g(myobj_temp3 = methylBase_temp_1,     
                       path_temp3  = paste(myPath_temp_1, "/3_",  Label_one_1_g, "_vs_", Label_four_4_g,  sep="")  ,   
                       qvalue_temp3=qvalue_temp_1,  differenceOfMethylation_temp3=differenceOfMethylation_temp_1,  
                       mergeDistance_temp3=mergeDistance_temp1,           
                        binBases_temp3=minBases_temp_1, dataFrame_temp3=dataFrame_temp1_A, numCores_temp3=numCores_temp_1 
                      ) 
}

if( (! is.null(Label_one_1_g)) && (! is.null(Label_five_5_g))   ) {
    dataFrame_temp1_A <- data.frame(
      mysampleID  = c(SampleID_one_1_g,   SampleID_five_5_g),
      mytreatment = c(Treatment_one_1_g,  Treatment_five_5_g),
      mysex       = c(Sex_one_1_g,        Sex_five_5_g),
      mytech      = c(Type_one_1_g,       Type_five_5_g), 
      myfiles     = c(Files_one_1_g,      Files_five_5_g)
    )
     myMainFunction_speRegions_All_g(myobj_temp3 = methylBase_temp_1,     
                       path_temp3  = paste(myPath_temp_1, "/4_",  Label_one_1_g, "_vs_", Label_five_5_g,  sep="")  ,   
                       qvalue_temp3=qvalue_temp_1,  differenceOfMethylation_temp3=differenceOfMethylation_temp_1, 
                       mergeDistance_temp3=mergeDistance_temp1,            
                        binBases_temp3=minBases_temp_1, dataFrame_temp3=dataFrame_temp1_A, numCores_temp3=numCores_temp_1 
                      ) 
}

if( (! is.null(Label_one_1_g)) && (! is.null(Label_six_6_g))   ) {
    dataFrame_temp1_A <- data.frame(
      mysampleID  = c(SampleID_one_1_g,   SampleID_six_6_g),
      mytreatment = c(Treatment_one_1_g,  Treatment_six_6_g),
      mysex       = c(Sex_one_1_g,        Sex_six_6_g),
      mytech      = c(Type_one_1_g,       Type_six_6_g), 
      myfiles     = c(Files_one_1_g,      Files_six_6_g)
    )
     myMainFunction_speRegions_All_g(myobj_temp3 = methylBase_temp_1,     
                       path_temp3  = paste(myPath_temp_1, "/5_",  Label_one_1_g, "_vs_", Label_six_6_g,  sep="")  ,   
                       qvalue_temp3=qvalue_temp_1,  differenceOfMethylation_temp3=differenceOfMethylation_temp_1,   
                       mergeDistance_temp3=mergeDistance_temp1,          
                        binBases_temp3=minBases_temp_1, dataFrame_temp3=dataFrame_temp1_A, numCores_temp3=numCores_temp_1 
                      ) 
}

if( (! is.null(Label_one_1_g)) && (! is.null(Label_seven_7_g))   ) {
    dataFrame_temp1_A <- data.frame(
      mysampleID  = c(SampleID_one_1_g,   SampleID_seven_7_g),
      mytreatment = c(Treatment_one_1_g,  Treatment_seven_7_g),
      mysex       = c(Sex_one_1_g,        Sex_seven_7_g),
      mytech      = c(Type_one_1_g,       Type_seven_7_g), 
      myfiles     = c(Files_one_1_g,      Files_seven_7_g)
    )
     myMainFunction_speRegions_All_g(myobj_temp3 = methylBase_temp_1,     
                       path_temp3  = paste(myPath_temp_1, "/6_",  Label_one_1_g, "_vs_", Label_seven_7_g,  sep="")  ,   
                       qvalue_temp3=qvalue_temp_1,  differenceOfMethylation_temp3=differenceOfMethylation_temp_1,  
                       mergeDistance_temp3=mergeDistance_temp1,           
                        binBases_temp3=minBases_temp_1, dataFrame_temp3=dataFrame_temp1_A, numCores_temp3=numCores_temp_1 
                      ) 
}

if( (! is.null(Label_one_1_g)) && (! is.null(Label_eight_8_g))   ) {
    dataFrame_temp1_A <- data.frame(
      mysampleID  = c(SampleID_one_1_g,   SampleID_eight_8_g),
      mytreatment = c(Treatment_one_1_g,  Treatment_eight_8_g),
      mysex       = c(Sex_one_1_g,        Sex_eight_8_g),
      mytech      = c(Type_one_1_g,       Type_eight_8_g), 
      myfiles     = c(Files_one_1_g,      Files_eight_8_g)
    )
     myMainFunction_speRegions_All_g(myobj_temp3 = methylBase_temp_1,     
                       path_temp3  = paste(myPath_temp_1, "/7_",  Label_one_1_g, "_vs_", Label_eight_8_g,  sep="")  ,   
                       qvalue_temp3=qvalue_temp_1,  differenceOfMethylation_temp3=differenceOfMethylation_temp_1,    
                       mergeDistance_temp3=mergeDistance_temp1,         
                        binBases_temp3=minBases_temp_1, dataFrame_temp3=dataFrame_temp1_A, numCores_temp3=numCores_temp_1 
                      ) 
}

if( (! is.null(Label_one_1_g)) && (! is.null(Label_nine_9_g))   ) {
    dataFrame_temp1_A <- data.frame(
      mysampleID  = c(SampleID_one_1_g,   SampleID_nine_9_g),
      mytreatment = c(Treatment_one_1_g,  Treatment_nine_9_g),
      mysex       = c(Sex_one_1_g,        Sex_nine_9_g),
      mytech      = c(Type_one_1_g,       Type_nine_9_g), 
      myfiles     = c(Files_one_1_g,      Files_nine_9_g)
    )
     myMainFunction_speRegions_All_g(myobj_temp3 = methylBase_temp_1,     
                       path_temp3  = paste(myPath_temp_1, "/8_",  Label_one_1_g, "_vs_", Label_nine_9_g,  sep="")  ,   
                       qvalue_temp3=qvalue_temp_1,  differenceOfMethylation_temp3=differenceOfMethylation_temp_1,   
                       mergeDistance_temp3=mergeDistance_temp1,          
                        binBases_temp3=minBases_temp_1, dataFrame_temp3=dataFrame_temp1_A, numCores_temp3=numCores_temp_1 
                      ) 
}


if( (! is.null(Label_one_1_g)) && (! is.null(Label_ten_10_g))   ) {
    dataFrame_temp1_A <- data.frame(
      mysampleID  = c(SampleID_one_1_g,   SampleID_ten_10_g),
      mytreatment = c(Treatment_one_1_g,  Treatment_ten_10_g),
      mysex       = c(Sex_one_1_g,        Sex_ten_10_g),
      mytech      = c(Type_one_1_g,       Type_ten_10_g), 
      myfiles     = c(Files_one_1_g,      Files_ten_10_g)
    )
     myMainFunction_speRegions_All_g(myobj_temp3 = methylBase_temp_1,     
                       path_temp3  = paste(myPath_temp_1, "/9_",  Label_one_1_g, "_vs_", Label_ten_10_g,  sep="")  ,   
                       qvalue_temp3=qvalue_temp_1,  differenceOfMethylation_temp3=differenceOfMethylation_temp_1, 
                       mergeDistance_temp3=mergeDistance_temp1,            
                        binBases_temp3=minBases_temp_1, dataFrame_temp3=dataFrame_temp1_A, numCores_temp3=numCores_temp_1 
                      ) 
}




###################################
if( (! is.null(Label_two_2_g)) && (! is.null(Label_three_3_g))   ) {
    dataFrame_temp1_A <- data.frame(
      mysampleID  = c(SampleID_two_2_g,   SampleID_three_3_g),
      mytreatment = c(Treatment_two_2_g,  Treatment_three_3_g),
      mysex       = c(Sex_two_2_g,        Sex_three_3_g),
      mytech      = c(Type_two_2_g,       Type_three_3_g), 
      myfiles     = c(Files_two_2_g,      Files_three_3_g)
    )
     myMainFunction_speRegions_All_g(myobj_temp3 = methylBase_temp_1,     
                       path_temp3  = paste(myPath_temp_1, "/12_",  Label_two_2_g, "_vs_", Label_three_3_g,  sep="")  ,   
                       qvalue_temp3=qvalue_temp_1,  differenceOfMethylation_temp3=differenceOfMethylation_temp_1, 
                       mergeDistance_temp3=mergeDistance_temp1,            
                        binBases_temp3=minBases_temp_1, dataFrame_temp3=dataFrame_temp1_A, numCores_temp3=numCores_temp_1 
                      ) 
}

if( (! is.null(Label_two_2_g)) && (! is.null(Label_four_4_g))   ) {
    dataFrame_temp1_A <- data.frame(
      mysampleID  = c(SampleID_two_2_g,   SampleID_four_4_g),
      mytreatment = c(Treatment_two_2_g,  Treatment_four_4_g),
      mysex       = c(Sex_two_2_g,        Sex_four_4_g),
      mytech      = c(Type_two_2_g,       Type_four_4_g), 
      myfiles     = c(Files_two_2_g,      Files_four_4_g)
    )
     myMainFunction_speRegions_All_g(myobj_temp3 = methylBase_temp_1,     
                       path_temp3  = paste(myPath_temp_1, "/13_",  Label_two_2_g, "_vs_", Label_four_4_g,  sep="")  ,   
                       qvalue_temp3=qvalue_temp_1,  differenceOfMethylation_temp3=differenceOfMethylation_temp_1,  
                       mergeDistance_temp3=mergeDistance_temp1,           
                        binBases_temp3=minBases_temp_1, dataFrame_temp3=dataFrame_temp1_A, numCores_temp3=numCores_temp_1 
                      ) 
}

if( (! is.null(Label_two_2_g)) && (! is.null(Label_five_5_g))   ) {
    dataFrame_temp1_A <- data.frame(
      mysampleID  = c(SampleID_two_2_g,   SampleID_five_5_g),
      mytreatment = c(Treatment_two_2_g,  Treatment_five_5_g),
      mysex       = c(Sex_two_2_g,        Sex_five_5_g),
      mytech      = c(Type_two_2_g,       Type_five_5_g), 
      myfiles     = c(Files_two_2_g,      Files_five_5_g)
    )
     myMainFunction_speRegions_All_g(myobj_temp3 = methylBase_temp_1,     
                       path_temp3  = paste(myPath_temp_1, "/14_",  Label_two_2_g, "_vs_", Label_five_5_g,  sep="")  ,   
                       qvalue_temp3=qvalue_temp_1,  differenceOfMethylation_temp3=differenceOfMethylation_temp_1,   
                       mergeDistance_temp3=mergeDistance_temp1,          
                        binBases_temp3=minBases_temp_1, dataFrame_temp3=dataFrame_temp1_A, numCores_temp3=numCores_temp_1 
                      ) 
}

if( (! is.null(Label_two_2_g)) && (! is.null(Label_six_6_g))   ) {
    dataFrame_temp1_A <- data.frame(
      mysampleID  = c(SampleID_two_2_g,   SampleID_six_6_g),
      mytreatment = c(Treatment_two_2_g,  Treatment_six_6_g),
      mysex       = c(Sex_two_2_g,        Sex_six_6_g),
      mytech      = c(Type_two_2_g,       Type_six_6_g), 
      myfiles     = c(Files_two_2_g,      Files_six_6_g)
    )
     myMainFunction_speRegions_All_g(myobj_temp3 = methylBase_temp_1,     
                       path_temp3  = paste(myPath_temp_1, "/15_",  Label_two_2_g, "_vs_", Label_six_6_g,  sep="")  ,   
                       qvalue_temp3=qvalue_temp_1,  differenceOfMethylation_temp3=differenceOfMethylation_temp_1,  
                       mergeDistance_temp3=mergeDistance_temp1,           
                        binBases_temp3=minBases_temp_1, dataFrame_temp3=dataFrame_temp1_A, numCores_temp3=numCores_temp_1 
                      ) 
}

if( (! is.null(Label_two_2_g)) && (! is.null(Label_seven_7_g))   ) {
    dataFrame_temp1_A <- data.frame(
      mysampleID  = c(SampleID_two_2_g,   SampleID_seven_7_g),
      mytreatment = c(Treatment_two_2_g,  Treatment_seven_7_g),
      mysex       = c(Sex_two_2_g,        Sex_seven_7_g),
      mytech      = c(Type_two_2_g,       Type_seven_7_g), 
      myfiles     = c(Files_two_2_g,      Files_seven_7_g)
    )
     myMainFunction_speRegions_All_g(myobj_temp3 = methylBase_temp_1,     
                       path_temp3  = paste(myPath_temp_1, "/16_",  Label_two_2_g, "_vs_", Label_seven_7_g,  sep="")  ,   
                       qvalue_temp3=qvalue_temp_1,  differenceOfMethylation_temp3=differenceOfMethylation_temp_1, 
                       mergeDistance_temp3=mergeDistance_temp1,            
                        binBases_temp3=minBases_temp_1, dataFrame_temp3=dataFrame_temp1_A, numCores_temp3=numCores_temp_1 
                      ) 
}

if( (! is.null(Label_two_2_g)) && (! is.null(Label_eight_8_g))   ) {
    dataFrame_temp1_A <- data.frame(
      mysampleID  = c(SampleID_two_2_g,   SampleID_eight_8_g),
      mytreatment = c(Treatment_two_2_g,  Treatment_eight_8_g),
      mysex       = c(Sex_two_2_g,        Sex_eight_8_g),
      mytech      = c(Type_two_2_g,       Type_eight_8_g), 
      myfiles     = c(Files_two_2_g,      Files_eight_8_g)
    )
     myMainFunction_speRegions_All_g(myobj_temp3 = methylBase_temp_1,     
                       path_temp3  = paste(myPath_temp_1, "/17_",  Label_two_2_g, "_vs_", Label_eight_8_g,  sep="")  ,   
                       qvalue_temp3=qvalue_temp_1,  differenceOfMethylation_temp3=differenceOfMethylation_temp_1,      
                       mergeDistance_temp3=mergeDistance_temp1,       
                        binBases_temp3=minBases_temp_1, dataFrame_temp3=dataFrame_temp1_A, numCores_temp3=numCores_temp_1 
                      ) 
}

if( (! is.null(Label_two_2_g)) && (! is.null(Label_nine_9_g))   ) {
    dataFrame_temp1_A <- data.frame(
      mysampleID  = c(SampleID_two_2_g,   SampleID_nine_9_g),
      mytreatment = c(Treatment_two_2_g,  Treatment_nine_9_g),
      mysex       = c(Sex_two_2_g,        Sex_nine_9_g),
      mytech      = c(Type_two_2_g,       Type_nine_9_g), 
      myfiles     = c(Files_two_2_g,      Files_nine_9_g)
    )
     myMainFunction_speRegions_All_g(myobj_temp3 = methylBase_temp_1,     
                       path_temp3  = paste(myPath_temp_1, "/18_",  Label_two_2_g, "_vs_", Label_nine_9_g,  sep="")  ,   
                       qvalue_temp3=qvalue_temp_1,  differenceOfMethylation_temp3=differenceOfMethylation_temp_1,  
                       mergeDistance_temp3=mergeDistance_temp1,           
                        binBases_temp3=minBases_temp_1, dataFrame_temp3=dataFrame_temp1_A, numCores_temp3=numCores_temp_1 
                      ) 
}


if( (! is.null(Label_two_2_g)) && (! is.null(Label_ten_10_g))   ) {
    dataFrame_temp1_A <- data.frame(
      mysampleID  = c(SampleID_two_2_g,   SampleID_ten_10_g),
      mytreatment = c(Treatment_two_2_g,  Treatment_ten_10_g),
      mysex       = c(Sex_two_2_g,        Sex_ten_10_g),
      mytech      = c(Type_two_2_g,       Type_ten_10_g), 
      myfiles     = c(Files_two_2_g,      Files_ten_10_g)
    )
     myMainFunction_speRegions_All_g(myobj_temp3 = methylBase_temp_1,     
                       path_temp3  = paste(myPath_temp_1, "/19_",  Label_two_2_g, "_vs_", Label_ten_10_g,  sep="")  ,   
                       qvalue_temp3=qvalue_temp_1,  differenceOfMethylation_temp3=differenceOfMethylation_temp_1,     
                       mergeDistance_temp3=mergeDistance_temp1,        
                        binBases_temp3=minBases_temp_1, dataFrame_temp3=dataFrame_temp1_A, numCores_temp3=numCores_temp_1 
                      ) 
}





###################################
if( (! is.null(Label_three_3_g)) && (! is.null(Label_four_4_g))   ) {
    dataFrame_temp1_A <- data.frame(
      mysampleID  = c(SampleID_three_3_g,   SampleID_four_4_g),
      mytreatment = c(Treatment_three_3_g,  Treatment_four_4_g),
      mysex       = c(Sex_three_3_g,        Sex_four_4_g),
      mytech      = c(Type_three_3_g,       Type_four_4_g), 
      myfiles     = c(Files_three_3_g,      Files_four_4_g)
    )
     myMainFunction_speRegions_All_g(myobj_temp3 = methylBase_temp_1,     
                       path_temp3  = paste(myPath_temp_1, "/23_",  Label_three_3_g, "_vs_", Label_four_4_g,  sep="")  ,   
                       qvalue_temp3=qvalue_temp_1,  differenceOfMethylation_temp3=differenceOfMethylation_temp_1,   
                       mergeDistance_temp3=mergeDistance_temp1,          
                        binBases_temp3=minBases_temp_1, dataFrame_temp3=dataFrame_temp1_A, numCores_temp3=numCores_temp_1 
                      ) 
}

if( (! is.null(Label_three_3_g)) && (! is.null(Label_five_5_g))   ) {
    dataFrame_temp1_A <- data.frame(
      mysampleID  = c(SampleID_three_3_g,   SampleID_five_5_g),
      mytreatment = c(Treatment_three_3_g,  Treatment_five_5_g),
      mysex       = c(Sex_three_3_g,        Sex_five_5_g),
      mytech      = c(Type_three_3_g,       Type_five_5_g), 
      myfiles     = c(Files_three_3_g,      Files_five_5_g)
    )
     myMainFunction_speRegions_All_g(myobj_temp3 = methylBase_temp_1,     
                       path_temp3  = paste(myPath_temp_1, "/24_",  Label_three_3_g, "_vs_", Label_five_5_g,  sep="")  ,   
                       qvalue_temp3=qvalue_temp_1,  differenceOfMethylation_temp3=differenceOfMethylation_temp_1,  
                       mergeDistance_temp3=mergeDistance_temp1,           
                        binBases_temp3=minBases_temp_1, dataFrame_temp3=dataFrame_temp1_A, numCores_temp3=numCores_temp_1 
                      ) 
}

if( (! is.null(Label_three_3_g)) && (! is.null(Label_six_6_g))   ) {
    dataFrame_temp1_A <- data.frame(
      mysampleID  = c(SampleID_three_3_g,   SampleID_six_6_g),
      mytreatment = c(Treatment_three_3_g,  Treatment_six_6_g),
      mysex       = c(Sex_three_3_g,        Sex_six_6_g),
      mytech      = c(Type_three_3_g,       Type_six_6_g), 
      myfiles     = c(Files_three_3_g,      Files_six_6_g)
    )
     myMainFunction_speRegions_All_g(myobj_temp3 = methylBase_temp_1,     
                       path_temp3  = paste(myPath_temp_1, "/25_",  Label_three_3_g, "_vs_", Label_six_6_g,  sep="")  ,   
                       qvalue_temp3=qvalue_temp_1,  differenceOfMethylation_temp3=differenceOfMethylation_temp_1,  
                       mergeDistance_temp3=mergeDistance_temp1,           
                        binBases_temp3=minBases_temp_1, dataFrame_temp3=dataFrame_temp1_A, numCores_temp3=numCores_temp_1 
                      ) 
}

if( (! is.null(Label_three_3_g)) && (! is.null(Label_seven_7_g))   ) {
    dataFrame_temp1_A <- data.frame(
      mysampleID  = c(SampleID_three_3_g,   SampleID_seven_7_g),
      mytreatment = c(Treatment_three_3_g,  Treatment_seven_7_g),
      mysex       = c(Sex_three_3_g,        Sex_seven_7_g),
      mytech      = c(Type_three_3_g,       Type_seven_7_g), 
      myfiles     = c(Files_three_3_g,      Files_seven_7_g)
    )
     myMainFunction_speRegions_All_g(myobj_temp3 = methylBase_temp_1,     
                       path_temp3  = paste(myPath_temp_1, "/26_",  Label_three_3_g, "_vs_", Label_seven_7_g,  sep="")  ,   
                       qvalue_temp3=qvalue_temp_1,  differenceOfMethylation_temp3=differenceOfMethylation_temp_1,   
                       mergeDistance_temp3=mergeDistance_temp1,          
                        binBases_temp3=minBases_temp_1, dataFrame_temp3=dataFrame_temp1_A, numCores_temp3=numCores_temp_1 
                      ) 
}

if( (! is.null(Label_three_3_g)) && (! is.null(Label_eight_8_g))   ) {
    dataFrame_temp1_A <- data.frame(
      mysampleID  = c(SampleID_three_3_g,   SampleID_eight_8_g),
      mytreatment = c(Treatment_three_3_g,  Treatment_eight_8_g),
      mysex       = c(Sex_three_3_g,        Sex_eight_8_g),
      mytech      = c(Type_three_3_g,       Type_eight_8_g), 
      myfiles     = c(Files_three_3_g,      Files_eight_8_g)
    )
     myMainFunction_speRegions_All_g(myobj_temp3 = methylBase_temp_1,     
                       path_temp3  = paste(myPath_temp_1, "/27_",  Label_three_3_g, "_vs_", Label_eight_8_g,  sep="")  ,   
                       qvalue_temp3=qvalue_temp_1,  differenceOfMethylation_temp3=differenceOfMethylation_temp_1, 
                       mergeDistance_temp3=mergeDistance_temp1,            
                        binBases_temp3=minBases_temp_1, dataFrame_temp3=dataFrame_temp1_A, numCores_temp3=numCores_temp_1 
                      ) 
}

if( (! is.null(Label_three_3_g)) && (! is.null(Label_nine_9_g))   ) {
    dataFrame_temp1_A <- data.frame(
      mysampleID  = c(SampleID_three_3_g,   SampleID_nine_9_g),
      mytreatment = c(Treatment_three_3_g,  Treatment_nine_9_g),
      mysex       = c(Sex_three_3_g,        Sex_nine_9_g),
      mytech      = c(Type_three_3_g,       Type_nine_9_g), 
      myfiles     = c(Files_three_3_g,      Files_nine_9_g)
    )
     myMainFunction_speRegions_All_g(myobj_temp3 = methylBase_temp_1,     
                       path_temp3  = paste(myPath_temp_1, "/28_",  Label_three_3_g, "_vs_", Label_nine_9_g,  sep="")  ,   
                       qvalue_temp3=qvalue_temp_1,  differenceOfMethylation_temp3=differenceOfMethylation_temp_1, 
                       mergeDistance_temp3=mergeDistance_temp1,            
                        binBases_temp3=minBases_temp_1, dataFrame_temp3=dataFrame_temp1_A, numCores_temp3=numCores_temp_1 
                      ) 
}


if( (! is.null(Label_three_3_g)) && (! is.null(Label_ten_10_g))   ) {
    dataFrame_temp1_A <- data.frame(
      mysampleID  = c(SampleID_three_3_g,   SampleID_ten_10_g),
      mytreatment = c(Treatment_three_3_g,  Treatment_ten_10_g),
      mysex       = c(Sex_three_3_g,        Sex_ten_10_g),
      mytech      = c(Type_three_3_g,       Type_ten_10_g), 
      myfiles     = c(Files_three_3_g,      Files_ten_10_g)
    )
     myMainFunction_speRegions_All_g(myobj_temp3 = methylBase_temp_1,     
                       path_temp3  = paste(myPath_temp_1, "/29_",  Label_three_3_g, "_vs_", Label_ten_10_g,  sep="")  ,   
                       qvalue_temp3=qvalue_temp_1,  differenceOfMethylation_temp3=differenceOfMethylation_temp_1,  
                       mergeDistance_temp3=mergeDistance_temp1,           
                        binBases_temp3=minBases_temp_1, dataFrame_temp3=dataFrame_temp1_A, numCores_temp3=numCores_temp_1 
                      ) 
}




 ###################################
if( (! is.null(Label_four_4_g)) && (! is.null(Label_five_5_g))   ) {
    dataFrame_temp1_A <- data.frame(
      mysampleID  = c(SampleID_four_4_g,   SampleID_five_5_g),
      mytreatment = c(Treatment_four_4_g,  Treatment_five_5_g),
      mysex       = c(Sex_four_4_g,        Sex_five_5_g),
      mytech      = c(Type_four_4_g,       Type_five_5_g), 
      myfiles     = c(Files_four_4_g,      Files_five_5_g)
    )
     myMainFunction_speRegions_All_g(myobj_temp3 = methylBase_temp_1,     
                       path_temp3  = paste(myPath_temp_1, "/34_",  Label_four_4_g, "_vs_", Label_five_5_g,  sep="")  ,   
                       qvalue_temp3=qvalue_temp_1,  differenceOfMethylation_temp3=differenceOfMethylation_temp_1, 
                       mergeDistance_temp3=mergeDistance_temp1,            
                        binBases_temp3=minBases_temp_1, dataFrame_temp3=dataFrame_temp1_A, numCores_temp3=numCores_temp_1 
                      ) 
}

if( (! is.null(Label_four_4_g)) && (! is.null(Label_six_6_g))   ) {
    dataFrame_temp1_A <- data.frame(
      mysampleID  = c(SampleID_four_4_g,   SampleID_six_6_g),
      mytreatment = c(Treatment_four_4_g,  Treatment_six_6_g),
      mysex       = c(Sex_four_4_g,        Sex_six_6_g),
      mytech      = c(Type_four_4_g,       Type_six_6_g), 
      myfiles     = c(Files_four_4_g,      Files_six_6_g)
    )
     myMainFunction_speRegions_All_g(myobj_temp3 = methylBase_temp_1,     
                       path_temp3  = paste(myPath_temp_1, "/35_",  Label_four_4_g, "_vs_", Label_six_6_g,  sep="")  ,   
                       qvalue_temp3=qvalue_temp_1,  differenceOfMethylation_temp3=differenceOfMethylation_temp_1,  
                       mergeDistance_temp3=mergeDistance_temp1,           
                        binBases_temp3=minBases_temp_1, dataFrame_temp3=dataFrame_temp1_A, numCores_temp3=numCores_temp_1 
                      ) 
}

if( (! is.null(Label_four_4_g)) && (! is.null(Label_seven_7_g))   ) {
    dataFrame_temp1_A <- data.frame(
      mysampleID  = c(SampleID_four_4_g,   SampleID_seven_7_g),
      mytreatment = c(Treatment_four_4_g,  Treatment_seven_7_g),
      mysex       = c(Sex_four_4_g,        Sex_seven_7_g),
      mytech      = c(Type_four_4_g,       Type_seven_7_g), 
      myfiles     = c(Files_four_4_g,      Files_seven_7_g)
    )
     myMainFunction_speRegions_All_g(myobj_temp3 = methylBase_temp_1,     
                       path_temp3  = paste(myPath_temp_1, "/36_",  Label_four_4_g, "_vs_", Label_seven_7_g,  sep="")  ,   
                       qvalue_temp3=qvalue_temp_1,  differenceOfMethylation_temp3=differenceOfMethylation_temp_1,    
                       mergeDistance_temp3=mergeDistance_temp1,         
                        binBases_temp3=minBases_temp_1, dataFrame_temp3=dataFrame_temp1_A, numCores_temp3=numCores_temp_1 
                      ) 
}

if( (! is.null(Label_four_4_g)) && (! is.null(Label_eight_8_g))   ) {
    dataFrame_temp1_A <- data.frame(
      mysampleID  = c(SampleID_four_4_g,   SampleID_eight_8_g),
      mytreatment = c(Treatment_four_4_g,  Treatment_eight_8_g),
      mysex       = c(Sex_four_4_g,        Sex_eight_8_g),
      mytech      = c(Type_four_4_g,       Type_eight_8_g), 
      myfiles     = c(Files_four_4_g,      Files_eight_8_g)
    )
     myMainFunction_speRegions_All_g(myobj_temp3 = methylBase_temp_1,     
                       path_temp3  = paste(myPath_temp_1, "/37_",  Label_four_4_g, "_vs_", Label_eight_8_g,  sep="")  ,   
                       qvalue_temp3=qvalue_temp_1,  differenceOfMethylation_temp3=differenceOfMethylation_temp_1,    
                       mergeDistance_temp3=mergeDistance_temp1,         
                        binBases_temp3=minBases_temp_1, dataFrame_temp3=dataFrame_temp1_A, numCores_temp3=numCores_temp_1 
                      ) 
}

if( (! is.null(Label_four_4_g)) && (! is.null(Label_nine_9_g))   ) {
    dataFrame_temp1_A <- data.frame(
      mysampleID  = c(SampleID_four_4_g,   SampleID_nine_9_g),
      mytreatment = c(Treatment_four_4_g,  Treatment_nine_9_g),
      mysex       = c(Sex_four_4_g,        Sex_nine_9_g),
      mytech      = c(Type_four_4_g,       Type_nine_9_g), 
      myfiles     = c(Files_four_4_g,      Files_nine_9_g)
    )
     myMainFunction_speRegions_All_g(myobj_temp3 = methylBase_temp_1,     
                       path_temp3  = paste(myPath_temp_1, "/38_",  Label_four_4_g, "_vs_", Label_nine_9_g,  sep="")  ,   
                       qvalue_temp3=qvalue_temp_1,  differenceOfMethylation_temp3=differenceOfMethylation_temp_1,  
                       mergeDistance_temp3=mergeDistance_temp1,           
                        binBases_temp3=minBases_temp_1, dataFrame_temp3=dataFrame_temp1_A, numCores_temp3=numCores_temp_1 
                      ) 
}


if( (! is.null(Label_four_4_g)) && (! is.null(Label_ten_10_g))   ) {
    dataFrame_temp1_A <- data.frame(
      mysampleID  = c(SampleID_four_4_g,   SampleID_ten_10_g),
      mytreatment = c(Treatment_four_4_g,  Treatment_ten_10_g),
      mysex       = c(Sex_four_4_g,        Sex_ten_10_g),
      mytech      = c(Type_four_4_g,       Type_ten_10_g), 
      myfiles     = c(Files_four_4_g,      Files_ten_10_g)
    )
     myMainFunction_speRegions_All_g(myobj_temp3 = methylBase_temp_1,     
                       path_temp3  = paste(myPath_temp_1, "/39_",  Label_four_4_g, "_vs_", Label_ten_10_g,  sep="")  ,   
                       qvalue_temp3=qvalue_temp_1,  differenceOfMethylation_temp3=differenceOfMethylation_temp_1,    
                       mergeDistance_temp3=mergeDistance_temp1,         
                        binBases_temp3=minBases_temp_1, dataFrame_temp3=dataFrame_temp1_A, numCores_temp3=numCores_temp_1 
                      ) 
}




 
################################### 
if( (! is.null(Label_five_5_g)) && (! is.null(Label_six_6_g))   ) {
    dataFrame_temp1_A <- data.frame(
      mysampleID  = c(SampleID_five_5_g,   SampleID_six_6_g),
      mytreatment = c(Treatment_five_5_g,  Treatment_six_6_g),
      mysex       = c(Sex_five_5_g,        Sex_six_6_g),
      mytech      = c(Type_five_5_g,       Type_six_6_g), 
      myfiles     = c(Files_five_5_g,      Files_six_6_g)
    )
     myMainFunction_speRegions_All_g(myobj_temp3 = methylBase_temp_1,     
                       path_temp3  = paste(myPath_temp_1, "/45_",  Label_five_5_g, "_vs_", Label_six_6_g,  sep="")  ,   
                       qvalue_temp3=qvalue_temp_1,  differenceOfMethylation_temp3=differenceOfMethylation_temp_1,   
                       mergeDistance_temp3=mergeDistance_temp1,          
                        binBases_temp3=minBases_temp_1, dataFrame_temp3=dataFrame_temp1_A, numCores_temp3=numCores_temp_1 
                      ) 
}

if( (! is.null(Label_five_5_g)) && (! is.null(Label_seven_7_g))   ) {
    dataFrame_temp1_A <- data.frame(
      mysampleID  = c(SampleID_five_5_g,   SampleID_seven_7_g),
      mytreatment = c(Treatment_five_5_g,  Treatment_seven_7_g),
      mysex       = c(Sex_five_5_g,        Sex_seven_7_g),
      mytech      = c(Type_five_5_g,       Type_seven_7_g), 
      myfiles     = c(Files_five_5_g,      Files_seven_7_g)
    )
     myMainFunction_speRegions_All_g(myobj_temp3 = methylBase_temp_1,     
                       path_temp3  = paste(myPath_temp_1, "/46_",  Label_five_5_g, "_vs_", Label_seven_7_g,  sep="")  ,   
                       qvalue_temp3=qvalue_temp_1,  differenceOfMethylation_temp3=differenceOfMethylation_temp_1,    
                       mergeDistance_temp3=mergeDistance_temp1,         
                        binBases_temp3=minBases_temp_1, dataFrame_temp3=dataFrame_temp1_A, numCores_temp3=numCores_temp_1 
                      ) 
}

if( (! is.null(Label_five_5_g)) && (! is.null(Label_eight_8_g))   ) {
    dataFrame_temp1_A <- data.frame(
      mysampleID  = c(SampleID_five_5_g,   SampleID_eight_8_g),
      mytreatment = c(Treatment_five_5_g,  Treatment_eight_8_g),
      mysex       = c(Sex_five_5_g,        Sex_eight_8_g),
      mytech      = c(Type_five_5_g,       Type_eight_8_g), 
      myfiles     = c(Files_five_5_g,      Files_eight_8_g)
    )
     myMainFunction_speRegions_All_g(myobj_temp3 = methylBase_temp_1,     
                       path_temp3  = paste(myPath_temp_1, "/47_",  Label_five_5_g, "_vs_", Label_eight_8_g,  sep="")  ,   
                       qvalue_temp3=qvalue_temp_1,  differenceOfMethylation_temp3=differenceOfMethylation_temp_1,     
                       mergeDistance_temp3=mergeDistance_temp1,        
                        binBases_temp3=minBases_temp_1, dataFrame_temp3=dataFrame_temp1_A, numCores_temp3=numCores_temp_1 
                      ) 
}

if( (! is.null(Label_five_5_g)) && (! is.null(Label_nine_9_g))   ) {
    dataFrame_temp1_A <- data.frame(
      mysampleID  = c(SampleID_five_5_g,   SampleID_nine_9_g),
      mytreatment = c(Treatment_five_5_g,  Treatment_nine_9_g),
      mysex       = c(Sex_five_5_g,        Sex_nine_9_g),
      mytech      = c(Type_five_5_g,       Type_nine_9_g), 
      myfiles     = c(Files_five_5_g,      Files_nine_9_g)
    )
     myMainFunction_speRegions_All_g(myobj_temp3 = methylBase_temp_1,     
                       path_temp3  = paste(myPath_temp_1, "/48_",  Label_five_5_g, "_vs_", Label_nine_9_g,  sep="")  ,   
                       qvalue_temp3=qvalue_temp_1,  differenceOfMethylation_temp3=differenceOfMethylation_temp_1,     
                       mergeDistance_temp3=mergeDistance_temp1,        
                        binBases_temp3=minBases_temp_1, dataFrame_temp3=dataFrame_temp1_A, numCores_temp3=numCores_temp_1 
                      ) 
}


if( (! is.null(Label_five_5_g)) && (! is.null(Label_ten_10_g))   ) {
    dataFrame_temp1_A <- data.frame(
      mysampleID  = c(SampleID_five_5_g,   SampleID_ten_10_g),
      mytreatment = c(Treatment_five_5_g,  Treatment_ten_10_g),
      mysex       = c(Sex_five_5_g,        Sex_ten_10_g),
      mytech      = c(Type_five_5_g,       Type_ten_10_g), 
      myfiles     = c(Files_five_5_g,      Files_ten_10_g)
    )
     myMainFunction_speRegions_All_g(myobj_temp3 = methylBase_temp_1,     
                       path_temp3  = paste(myPath_temp_1, "/49_",  Label_five_5_g, "_vs_", Label_ten_10_g,  sep="")  ,   
                       qvalue_temp3=qvalue_temp_1,  differenceOfMethylation_temp3=differenceOfMethylation_temp_1,    
                       mergeDistance_temp3=mergeDistance_temp1,         
                        binBases_temp3=minBases_temp_1, dataFrame_temp3=dataFrame_temp1_A, numCores_temp3=numCores_temp_1 
                      ) 
}




################################### 
if( (! is.null(Label_six_6_g)) && (! is.null(Label_seven_7_g))   ) {
    dataFrame_temp1_A <- data.frame(
      mysampleID  = c(SampleID_six_6_g,   SampleID_seven_7_g),
      mytreatment = c(Treatment_six_6_g,  Treatment_seven_7_g),
      mysex       = c(Sex_six_6_g,        Sex_seven_7_g),
      mytech      = c(Type_six_6_g,       Type_seven_7_g), 
      myfiles     = c(Files_six_6_g,      Files_seven_7_g)
    )
     myMainFunction_speRegions_All_g(myobj_temp3 = methylBase_temp_1,     
                       path_temp3  = paste(myPath_temp_1, "/56_",  Label_six_6_g, "_vs_", Label_seven_7_g,  sep="")  ,   
                       qvalue_temp3=qvalue_temp_1,  differenceOfMethylation_temp3=differenceOfMethylation_temp_1,    
                       mergeDistance_temp3=mergeDistance_temp1,         
                        binBases_temp3=minBases_temp_1, dataFrame_temp3=dataFrame_temp1_A, numCores_temp3=numCores_temp_1 
                      ) 
}

if( (! is.null(Label_six_6_g)) && (! is.null(Label_eight_8_g))   ) {
    dataFrame_temp1_A <- data.frame(
      mysampleID  = c(SampleID_six_6_g,   SampleID_eight_8_g),
      mytreatment = c(Treatment_six_6_g,  Treatment_eight_8_g),
      mysex       = c(Sex_six_6_g,        Sex_eight_8_g),
      mytech      = c(Type_six_6_g,       Type_eight_8_g), 
      myfiles     = c(Files_six_6_g,      Files_eight_8_g)
    )
     myMainFunction_speRegions_All_g(myobj_temp3 = methylBase_temp_1,     
                       path_temp3  = paste(myPath_temp_1, "/57_",  Label_six_6_g, "_vs_", Label_eight_8_g,  sep="")  ,   
                       qvalue_temp3=qvalue_temp_1,  differenceOfMethylation_temp3=differenceOfMethylation_temp_1,  
                       mergeDistance_temp3=mergeDistance_temp1,           
                        binBases_temp3=minBases_temp_1, dataFrame_temp3=dataFrame_temp1_A, numCores_temp3=numCores_temp_1 
                      ) 
}

if( (! is.null(Label_six_6_g)) && (! is.null(Label_nine_9_g))   ) {
    dataFrame_temp1_A <- data.frame(
      mysampleID  = c(SampleID_six_6_g,   SampleID_nine_9_g),
      mytreatment = c(Treatment_six_6_g,  Treatment_nine_9_g),
      mysex       = c(Sex_six_6_g,        Sex_nine_9_g),
      mytech      = c(Type_six_6_g,       Type_nine_9_g), 
      myfiles     = c(Files_six_6_g,      Files_nine_9_g)
    )
     myMainFunction_speRegions_All_g(myobj_temp3 = methylBase_temp_1,     
                       path_temp3  = paste(myPath_temp_1, "/58_",  Label_six_6_g, "_vs_", Label_nine_9_g,  sep="")  ,   
                       qvalue_temp3=qvalue_temp_1,  differenceOfMethylation_temp3=differenceOfMethylation_temp_1,   
                       mergeDistance_temp3=mergeDistance_temp1,          
                        binBases_temp3=minBases_temp_1, dataFrame_temp3=dataFrame_temp1_A, numCores_temp3=numCores_temp_1 
                      ) 
}


if( (! is.null(Label_six_6_g)) && (! is.null(Label_ten_10_g))   ) {
    dataFrame_temp1_A <- data.frame(
      mysampleID  = c(SampleID_six_6_g,   SampleID_ten_10_g),
      mytreatment = c(Treatment_six_6_g,  Treatment_ten_10_g),
      mysex       = c(Sex_six_6_g,        Sex_ten_10_g),
      mytech      = c(Type_six_6_g,       Type_ten_10_g), 
      myfiles     = c(Files_six_6_g,      Files_ten_10_g)
    )
     myMainFunction_speRegions_All_g(myobj_temp3 = methylBase_temp_1,     
                       path_temp3  = paste(myPath_temp_1, "/59_",  Label_six_6_g, "_vs_", Label_ten_10_g,  sep="")  ,   
                       qvalue_temp3=qvalue_temp_1,  differenceOfMethylation_temp3=differenceOfMethylation_temp_1,    
                       mergeDistance_temp3=mergeDistance_temp1,         
                        binBases_temp3=minBases_temp_1, dataFrame_temp3=dataFrame_temp1_A, numCores_temp3=numCores_temp_1 
                      ) 
}





###################################
if( (! is.null(Label_seven_7_g)) && (! is.null(Label_eight_8_g))   ) {
    dataFrame_temp1_A <- data.frame(
      mysampleID  = c(SampleID_seven_7_g,   SampleID_eight_8_g),
      mytreatment = c(Treatment_seven_7_g,  Treatment_eight_8_g),
      mysex       = c(Sex_seven_7_g,        Sex_eight_8_g),
      mytech      = c(Type_seven_7_g,       Type_eight_8_g), 
      myfiles     = c(Files_seven_7_g,      Files_eight_8_g)
    )
     myMainFunction_speRegions_All_g(myobj_temp3 = methylBase_temp_1,     
                       path_temp3  = paste(myPath_temp_1, "/67_",  Label_seven_7_g, "_vs_", Label_eight_8_g,  sep="")  ,   
                       qvalue_temp3=qvalue_temp_1,  differenceOfMethylation_temp3=differenceOfMethylation_temp_1,    
                       mergeDistance_temp3=mergeDistance_temp1,         
                        binBases_temp3=minBases_temp_1, dataFrame_temp3=dataFrame_temp1_A, numCores_temp3=numCores_temp_1 
                      ) 
}

if( (! is.null(Label_seven_7_g)) && (! is.null(Label_nine_9_g))   ) {
    dataFrame_temp1_A <- data.frame(
      mysampleID  = c(SampleID_seven_7_g,   SampleID_nine_9_g),
      mytreatment = c(Treatment_seven_7_g,  Treatment_nine_9_g),
      mysex       = c(Sex_seven_7_g,        Sex_nine_9_g),
      mytech      = c(Type_seven_7_g,       Type_nine_9_g), 
      myfiles     = c(Files_seven_7_g,      Files_nine_9_g)
    )
     myMainFunction_speRegions_All_g(myobj_temp3 = methylBase_temp_1,     
                       path_temp3  = paste(myPath_temp_1, "/68_",  Label_seven_7_g, "_vs_", Label_nine_9_g,  sep="")  ,   
                       qvalue_temp3=qvalue_temp_1,  differenceOfMethylation_temp3=differenceOfMethylation_temp_1,  
                       mergeDistance_temp3=mergeDistance_temp1,           
                        binBases_temp3=minBases_temp_1, dataFrame_temp3=dataFrame_temp1_A, numCores_temp3=numCores_temp_1 
                      ) 
}


if( (! is.null(Label_seven_7_g)) && (! is.null(Label_ten_10_g))   ) {
    dataFrame_temp1_A <- data.frame(
      mysampleID  = c(SampleID_seven_7_g,   SampleID_ten_10_g),
      mytreatment = c(Treatment_seven_7_g,  Treatment_ten_10_g),
      mysex       = c(Sex_seven_7_g,        Sex_ten_10_g),
      mytech      = c(Type_seven_7_g,       Type_ten_10_g), 
      myfiles     = c(Files_seven_7_g,      Files_ten_10_g)
    )
     myMainFunction_speRegions_All_g(myobj_temp3 = methylBase_temp_1,     
                       path_temp3  = paste(myPath_temp_1, "/69_",  Label_seven_7_g, "_vs_", Label_ten_10_g,  sep="")  ,   
                       qvalue_temp3=qvalue_temp_1,  differenceOfMethylation_temp3=differenceOfMethylation_temp_1,     
                       mergeDistance_temp3=mergeDistance_temp1,        
                        binBases_temp3=minBases_temp_1, dataFrame_temp3=dataFrame_temp1_A, numCores_temp3=numCores_temp_1 
                      ) 
}




 
###################################
if( (! is.null(Label_eight_8_g)) && (! is.null(Label_nine_9_g))   ) {
    dataFrame_temp1_A <- data.frame(
      mysampleID  = c(SampleID_eight_8_g,   SampleID_nine_9_g),
      mytreatment = c(Treatment_eight_8_g,  Treatment_nine_9_g),
      mysex       = c(Sex_eight_8_g,        Sex_nine_9_g),
      mytech      = c(Type_eight_8_g,       Type_nine_9_g), 
      myfiles     = c(Files_eight_8_g,      Files_nine_9_g)
    )
     myMainFunction_speRegions_All_g(myobj_temp3 = methylBase_temp_1,     
                       path_temp3  = paste(myPath_temp_1, "/78_",  Label_eight_8_g, "_vs_", Label_nine_9_g,  sep="")  ,   
                       qvalue_temp3=qvalue_temp_1,  differenceOfMethylation_temp3=differenceOfMethylation_temp_1,     
                       mergeDistance_temp3=mergeDistance_temp1,        
                        binBases_temp3=minBases_temp_1, dataFrame_temp3=dataFrame_temp1_A, numCores_temp3=numCores_temp_1 
                      ) 
}


if( (! is.null(Label_eight_8_g)) && (! is.null(Label_ten_10_g))   ) {
    dataFrame_temp1_A <- data.frame(
      mysampleID  = c(SampleID_eight_8_g,   SampleID_ten_10_g),
      mytreatment = c(Treatment_eight_8_g,  Treatment_ten_10_g),
      mysex       = c(Sex_eight_8_g,        Sex_ten_10_g),
      mytech      = c(Type_eight_8_g,       Type_ten_10_g), 
      myfiles     = c(Files_eight_8_g,      Files_ten_10_g)
    )
     myMainFunction_speRegions_All_g(myobj_temp3 = methylBase_temp_1,     
                       path_temp3  = paste(myPath_temp_1, "/79_",  Label_eight_8_g, "_vs_", Label_ten_10_g,  sep="")  ,   
                       qvalue_temp3=qvalue_temp_1,  differenceOfMethylation_temp3=differenceOfMethylation_temp_1,   
                       mergeDistance_temp3=mergeDistance_temp1,          
                        binBases_temp3=minBases_temp_1, dataFrame_temp3=dataFrame_temp1_A, numCores_temp3=numCores_temp_1 
                      ) 
}



###################################
if( (! is.null(Label_nine_9_g)) && (! is.null(Label_ten_10_g))   ) {
    dataFrame_temp1_A <- data.frame(
      mysampleID  = c(SampleID_nine_9_g,   SampleID_ten_10_g),
      mytreatment = c(Treatment_nine_9_g,  Treatment_ten_10_g),
      mysex       = c(Sex_nine_9_g,        Sex_ten_10_g),
      mytech      = c(Type_nine_9_g,       Type_ten_10_g), 
      myfiles     = c(Files_nine_9_g,      Files_ten_10_g)
    )
     myMainFunction_speRegions_All_g(myobj_temp3 = methylBase_temp_1,     
                       path_temp3  = paste(myPath_temp_1, "/89_",  Label_nine_9_g, "_vs_", Label_ten_10_g,  sep="")  ,   
                       qvalue_temp3=qvalue_temp_1,  differenceOfMethylation_temp3=differenceOfMethylation_temp_1,   
                       mergeDistance_temp3=mergeDistance_temp1,          
                        binBases_temp3=minBases_temp_1, dataFrame_temp3=dataFrame_temp1_A, numCores_temp3=numCores_temp_1 
                      ) 
}





}





 


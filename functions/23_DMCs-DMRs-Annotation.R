myDMRs_annotation_bool <- function(  myDiffDMR_55,   mySpecificLoci_55, mySpecificLoci.obj_55,  file_55, path_55  ) {
    if ( grepl(pattern="\\.Genes\\.bed", x=mySpecificLoci_55, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {
        Ann_mySpecificLoci_1A = genomation::annotateWithGeneParts( as(myDiffDMR_55,  "GRanges"),  mySpecificLoci.obj_55)
        sink( file=paste(path_55, file_55, sep="/")   )
        print(  genomation::getFeatsWithTargetsStats(  Ann_mySpecificLoci_1A,  percentage=TRUE) ) 
        cat("\n\n\n\n\n")
        print(Ann_mySpecificLoci_1A)
        sink()
        pdf( file=paste(path_55, "/", file_55, ".pdf", sep="")   )
        print(genomation::plotTargetAnnotation(Ann_mySpecificLoci_1A,  precedence=TRUE,   main=mySpecificLoci_55 ) )
        dev.off() 
    }else{
          Ann_mySpecificLoci_2 = genomation::annotateWithFeatureFlank(target = as(myDiffDMR_55, "GRanges"),
                                         feature = mySpecificLoci.obj_55$targets,  flank = mySpecificLoci.obj_55$shores,
                                         feature.name = "targets",  flank.name = "shores"
          )
          sink( file=paste(path_55, file_55, sep="/")   )
          print(  genomation::getFeatsWithTargetsStats( Ann_mySpecificLoci_2,  percentage=TRUE) ) 
          print(Ann_mySpecificLoci_2)
          sink()
          pdf( file=paste(path_55, "/", file_55, ".pdf", sep="")   )
          print(genomation::plotTargetAnnotation(Ann_mySpecificLoci_2, precedence=TRUE, main=mySpecificLoci_55))
          dev.off()  
    }  
  tryCatch(
    myTempFunction101(),
    error = function(err){"myTempFunction101_1bp_000111333"}
  )
  
}



myDMRs_annotation_g <- function(  myDiffDMR_5,   path2_5  ) {
    print(myDiffDMR_5)
    cat("\n\n\n\n")
    print(path2_5)
    if( ! file.exists(path2_5) ) { dir.create(path2_5, recursive = TRUE) }
  

    if( ! is.na(mySpecificLoci_1_g) ) {
         myDMRs_annotation_bool(myDiffDMR_55=myDiffDMR_5,   mySpecificLoci_55=mySpecificLoci_1_g, mySpecificLoci.obj_55=mySpecificLoci_1.obj_g, file_55="1_mySpecificLoci_1.txt", path_55=path2_5  )                 
    }

    if( ! is.na(mySpecificLoci_2_g) ) {
         myDMRs_annotation_bool(myDiffDMR_55=myDiffDMR_5,   mySpecificLoci_55=mySpecificLoci_2_g, mySpecificLoci.obj_55=mySpecificLoci_2.obj_g, file_55="2_mySpecificLoci_2.txt", path_55=path2_5  )                 
    }

    if( ! is.na(mySpecificLoci_3_g) ) {
         myDMRs_annotation_bool(myDiffDMR_55=myDiffDMR_5,   mySpecificLoci_55=mySpecificLoci_3_g, mySpecificLoci.obj_55=mySpecificLoci_3.obj_g, file_55="3_mySpecificLoci_3.txt", path_55=path2_5  )                 
    }

    if( ! is.na(mySpecificLoci_4_g) ) {
         myDMRs_annotation_bool(myDiffDMR_55=myDiffDMR_5,   mySpecificLoci_55=mySpecificLoci_4_g, mySpecificLoci.obj_55=mySpecificLoci_4.obj_g, file_55="4_mySpecificLoci_4.txt", path_55=path2_5  )                 
    }

    if( ! is.na(mySpecificLoci_5_g) ) {
         myDMRs_annotation_bool(myDiffDMR_55=myDiffDMR_5,   mySpecificLoci_55=mySpecificLoci_5_g, mySpecificLoci.obj_55=mySpecificLoci_5.obj_g, file_55="5_mySpecificLoci_5.txt", path_55=path2_5  )                 
    }

    if( ! is.na(mySpecificLoci_6_g) ) {
         myDMRs_annotation_bool(myDiffDMR_55=myDiffDMR_5,   mySpecificLoci_55=mySpecificLoci_6_g, mySpecificLoci.obj_55=mySpecificLoci_6.obj_g, file_55="6_mySpecificLoci_6.txt", path_55=path2_5  )                 
    }

    if( ! is.na(mySpecificLoci_7_g) ) {
         myDMRs_annotation_bool(myDiffDMR_55=myDiffDMR_5,   mySpecificLoci_55=mySpecificLoci_7_g, mySpecificLoci.obj_55=mySpecificLoci_7.obj_g, file_55="7_mySpecificLoci_7.txt", path_55=path2_5  )                 
    }

    if( ! is.na(mySpecificLoci_8_g) ) {
         myDMRs_annotation_bool(myDiffDMR_55=myDiffDMR_5,   mySpecificLoci_55=mySpecificLoci_8_g, mySpecificLoci.obj_55=mySpecificLoci_8.obj_g, file_55="8_mySpecificLoci_8.txt", path_55=path2_5  )                 
    }

    if( ! is.na(mySpecificLoci_9_g) ) {
         myDMRs_annotation_bool(myDiffDMR_55=myDiffDMR_5,   mySpecificLoci_55=mySpecificLoci_9_g, mySpecificLoci.obj_55=mySpecificLoci_9.obj_g, file_55="9_mySpecificLoci_9.txt", path_55=path2_5  )                 
    }

    if( ! is.na(mySpecificLoci_10_g) ) {
         myDMRs_annotation_bool(myDiffDMR_55=myDiffDMR_5,   mySpecificLoci_55=mySpecificLoci_10_g, mySpecificLoci.obj_55=mySpecificLoci_10.obj_g, file_55="10_mySpecificLoci_10.txt", path_55=path2_5  )                 
    }
 
 
  

    if( ! is.na(mySpecificLoci_11_g) ) {
         myDMRs_annotation_bool(myDiffDMR_55=myDiffDMR_5,   mySpecificLoci_55=mySpecificLoci_11_g, mySpecificLoci.obj_55=mySpecificLoci_11.obj_g, file_55="11_mySpecificLoci_11.txt", path_55=path2_5  )                 
    }

    if( ! is.na(mySpecificLoci_12_g) ) {
         myDMRs_annotation_bool(myDiffDMR_55=myDiffDMR_5,   mySpecificLoci_55=mySpecificLoci_12_g, mySpecificLoci.obj_55=mySpecificLoci_12.obj_g, file_55="12_mySpecificLoci_12.txt", path_55=path2_5  )                 
    }

    if( ! is.na(mySpecificLoci_13_g) ) {
         myDMRs_annotation_bool(myDiffDMR_55=myDiffDMR_5,   mySpecificLoci_55=mySpecificLoci_13_g, mySpecificLoci.obj_55=mySpecificLoci_13.obj_g, file_55="13_mySpecificLoci_13.txt", path_55=path2_5  )                 
    }

    if( ! is.na(mySpecificLoci_14_g) ) {
         myDMRs_annotation_bool(myDiffDMR_55=myDiffDMR_5,   mySpecificLoci_55=mySpecificLoci_14_g, mySpecificLoci.obj_55=mySpecificLoci_14.obj_g, file_55="14_mySpecificLoci_14.txt", path_55=path2_5  )                 
    }

    if( ! is.na(mySpecificLoci_15_g) ) {
         myDMRs_annotation_bool(myDiffDMR_55=myDiffDMR_5,   mySpecificLoci_55=mySpecificLoci_15_g, mySpecificLoci.obj_55=mySpecificLoci_15.obj_g, file_55="15_mySpecificLoci_15.txt", path_55=path2_5  )                 
    }

    if( ! is.na(mySpecificLoci_16_g) ) {
         myDMRs_annotation_bool(myDiffDMR_55=myDiffDMR_5,   mySpecificLoci_55=mySpecificLoci_16_g, mySpecificLoci.obj_55=mySpecificLoci_16.obj_g, file_55="16_mySpecificLoci_16.txt", path_55=path2_5  )                 
    }

    if( ! is.na(mySpecificLoci_17_g) ) {
         myDMRs_annotation_bool(myDiffDMR_55=myDiffDMR_5,   mySpecificLoci_55=mySpecificLoci_17_g, mySpecificLoci.obj_55=mySpecificLoci_17.obj_g, file_55="17_mySpecificLoci_17.txt", path_55=path2_5  )                 
    }

    if( ! is.na(mySpecificLoci_18_g) ) {
         myDMRs_annotation_bool(myDiffDMR_55=myDiffDMR_5,   mySpecificLoci_55=mySpecificLoci_18_g, mySpecificLoci.obj_55=mySpecificLoci_18.obj_g, file_55="18_mySpecificLoci_18.txt", path_55=path2_5  )                 
    }

    if( ! is.na(mySpecificLoci_19_g) ) {
         myDMRs_annotation_bool(myDiffDMR_55=myDiffDMR_5,   mySpecificLoci_55=mySpecificLoci_19_g, mySpecificLoci.obj_55=mySpecificLoci_19.obj_g, file_55="19_mySpecificLoci_19.txt", path_55=path2_5  )                 
    }

    if( ! is.na(mySpecificLoci_20_g) ) {
         myDMRs_annotation_bool(myDiffDMR_55=myDiffDMR_5,   mySpecificLoci_55=mySpecificLoci_20_g, mySpecificLoci.obj_55=mySpecificLoci_20.obj_g, file_55="20_mySpecificLoci_20.txt", path_55=path2_5  )                 
    }
 


 
 
  

    if( ! is.na(mySpecificLoci_21_g) ) {
         myDMRs_annotation_bool(myDiffDMR_55=myDiffDMR_5,   mySpecificLoci_55=mySpecificLoci_21_g, mySpecificLoci.obj_55=mySpecificLoci_21.obj_g, file_55="21_mySpecificLoci_21.txt", path_55=path2_5  )                 
    }

    if( ! is.na(mySpecificLoci_22_g) ) {
         myDMRs_annotation_bool(myDiffDMR_55=myDiffDMR_5,   mySpecificLoci_55=mySpecificLoci_22_g, mySpecificLoci.obj_55=mySpecificLoci_22.obj_g, file_55="22_mySpecificLoci_22.txt", path_55=path2_5  )                 
    }

    if( ! is.na(mySpecificLoci_23_g) ) {
         myDMRs_annotation_bool(myDiffDMR_55=myDiffDMR_5,   mySpecificLoci_55=mySpecificLoci_23_g, mySpecificLoci.obj_55=mySpecificLoci_23.obj_g, file_55="23_mySpecificLoci_23.txt", path_55=path2_5  )                 
    }

    if( ! is.na(mySpecificLoci_24_g) ) {
         myDMRs_annotation_bool(myDiffDMR_55=myDiffDMR_5,   mySpecificLoci_55=mySpecificLoci_24_g, mySpecificLoci.obj_55=mySpecificLoci_24.obj_g, file_55="24_mySpecificLoci_24.txt", path_55=path2_5  )                 
    }

    if( ! is.na(mySpecificLoci_25_g) ) {
         myDMRs_annotation_bool(myDiffDMR_55=myDiffDMR_5,   mySpecificLoci_55=mySpecificLoci_25_g, mySpecificLoci.obj_55=mySpecificLoci_25.obj_g, file_55="25_mySpecificLoci_25.txt", path_55=path2_5  )                 
    }

    if( ! is.na(mySpecificLoci_26_g) ) {
         myDMRs_annotation_bool(myDiffDMR_55=myDiffDMR_5,   mySpecificLoci_55=mySpecificLoci_26_g, mySpecificLoci.obj_55=mySpecificLoci_26.obj_g, file_55="26_mySpecificLoci_26.txt", path_55=path2_5  )                 
    }

    if( ! is.na(mySpecificLoci_27_g) ) {
         myDMRs_annotation_bool(myDiffDMR_55=myDiffDMR_5,   mySpecificLoci_55=mySpecificLoci_27_g, mySpecificLoci.obj_55=mySpecificLoci_27.obj_g, file_55="27_mySpecificLoci_27.txt", path_55=path2_5  )                 
    }

    if( ! is.na(mySpecificLoci_28_g) ) {
         myDMRs_annotation_bool(myDiffDMR_55=myDiffDMR_5,   mySpecificLoci_55=mySpecificLoci_28_g, mySpecificLoci.obj_55=mySpecificLoci_28.obj_g, file_55="28_mySpecificLoci_28.txt", path_55=path2_5  )                 
    }

    if( ! is.na(mySpecificLoci_29_g) ) {
         myDMRs_annotation_bool(myDiffDMR_55=myDiffDMR_5,   mySpecificLoci_55=mySpecificLoci_29_g, mySpecificLoci.obj_55=mySpecificLoci_29.obj_g, file_55="29_mySpecificLoci_29.txt", path_55=path2_5  )                 
    }

    if( ! is.na(mySpecificLoci_30_g) ) {
         myDMRs_annotation_bool(myDiffDMR_55=myDiffDMR_5,   mySpecificLoci_55=mySpecificLoci_30_g, mySpecificLoci.obj_55=mySpecificLoci_30.obj_g, file_55="30_mySpecificLoci_30.txt", path_55=path2_5  )                 
    }
 
}  










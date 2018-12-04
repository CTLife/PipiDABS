



myDMRs_annotation_g <- function(  myDiffDMR_5,   path2_5  ) {
    print(myDiffDMR_5)
    cat("\n\n\n\n")
    print(path2_5)
    if( ! file.exists(path2_5) ) { dir.create(path2_5, recursive = TRUE) }
  
    if( ! is.na(mySpecificLoci_1_g) ) {
        Ann_mySpecificLoci_1A = genomation::annotateWithGeneParts( as(myDiffDMR_5,  "GRanges"),  mySpecificLoci_1A.obj_g)
        sink( file=paste(path2_5, "1A_mySpecificLoci_1A.txt", sep="/")   )
        print(  genomation::getFeatsWithTargetsStats(  Ann_mySpecificLoci_1A,  percentage=TRUE) ) 
        cat("\n\n\n\n\n")
        print(Ann_mySpecificLoci_1A)
        sink()
        pdf( file=paste(path2_5, "1A_mySpecificLoci_1A.pdf", sep="/")   )
        print(genomation::plotTargetAnnotation(Ann_mySpecificLoci_1A,  precedence=TRUE,   main=mySpecificLoci_1_g ) )
        dev.off()
    }
   

    if( ! is.na(mySpecificLoci_1_g) ) {
        Ann_mySpecificLoci_1B = genomation::annotateWithGeneParts( as(myDiffDMR_5,  "GRanges"),  mySpecificLoci_1B.obj_g)
        sink( file=paste(path2_5, "1B_mySpecificLoci_1B.txt", sep="/")   )
        print(  genomation::getFeatsWithTargetsStats(  Ann_mySpecificLoci_1B,  percentage=TRUE) ) 
        cat("\n\n\n\n\n")
        print(Ann_mySpecificLoci_1B)
        sink()
        pdf( file=paste(path2_5, "1B_mySpecificLoci_1B.pdf", sep="/")   )
        print(genomation::plotTargetAnnotation(Ann_mySpecificLoci_1B,  precedence=TRUE,   main=mySpecificLoci_1_g) )
        dev.off()
    }
   

    if( ! is.na(mySpecificLoci_2_g) ) {
          Ann_mySpecificLoci_2 = genomation::annotateWithFeatureFlank(target = as(myDiffDMR_5, "GRanges"),
                                         feature = mySpecificLoci_2.obj_g$targets,  flank = mySpecificLoci_2.obj_g$shores,
                                         feature.name = "targets",  flank.name = "shores"
          )
          sink( file=paste(path2_5, "2_mySpecificLoci_2.txt", sep="/")   )
          print(  genomation::getFeatsWithTargetsStats( Ann_mySpecificLoci_2,  percentage=TRUE) ) 
          print(Ann_mySpecificLoci_2)
          sink()
          pdf( file=paste(path2_5, "2_mySpecificLoci_2.pdf", sep="/")   )
          print(genomation::plotTargetAnnotation(Ann_mySpecificLoci_2, precedence=TRUE, main=mySpecificLoci_2_g))
          dev.off()
    }

    

    if( ! is.na(mySpecificLoci_3_g) ) {
          Ann_mySpecificLoci_3 = genomation::annotateWithFeatureFlank(target = as(myDiffDMR_5, "GRanges"),
                                         feature = mySpecificLoci_3.obj_g$targets,  flank = mySpecificLoci_3.obj_g$shores,
                                         feature.name = "targets",  flank.name = "shores"
          )
          sink( file=paste(path2_5, "3_mySpecificLoci_3.txt", sep="/")   )
          print(  genomation::getFeatsWithTargetsStats( Ann_mySpecificLoci_3,  percentage=TRUE) ) 
          print(Ann_mySpecificLoci_3)
          sink()
          pdf( file=paste(path2_5, "3_mySpecificLoci_3.pdf", sep="/")   )
          print(genomation::plotTargetAnnotation(Ann_mySpecificLoci_3, precedence=TRUE, main=mySpecificLoci_3_g))
          dev.off()
    }

   

    if( ! is.na(mySpecificLoci_4_g) ) {
          Ann_mySpecificLoci_4 = genomation::annotateWithFeatureFlank(target = as(myDiffDMR_5, "GRanges"),
                                         feature = mySpecificLoci_4.obj_g$targets,  flank = mySpecificLoci_4.obj_g$shores,
                                         feature.name = "targets",  flank.name = "shores"
          )
          sink( file=paste(path2_5, "4_mySpecificLoci_4.txt", sep="/")   )
          print(  genomation::getFeatsWithTargetsStats( Ann_mySpecificLoci_4,  percentage=TRUE) ) 
          print(Ann_mySpecificLoci_4)
          sink()
          pdf( file=paste(path2_5, "4_mySpecificLoci_4.pdf", sep="/")   )
          print(genomation::plotTargetAnnotation(Ann_mySpecificLoci_4, precedence=TRUE, main=mySpecificLoci_4_g))
          dev.off()
    }

   

    if( ! is.na(mySpecificLoci_5_g) ) {
          Ann_mySpecificLoci_5 = genomation::annotateWithFeatureFlank(target = as(myDiffDMR_5, "GRanges"),
                                         feature = mySpecificLoci_5.obj_g$targets,  flank = mySpecificLoci_5.obj_g$shores,
                                         feature.name = "targets",  flank.name = "shores"
          )
          sink( file=paste(path2_5, "5_mySpecificLoci_5.txt", sep="/")   )
          print(  genomation::getFeatsWithTargetsStats( Ann_mySpecificLoci_5,  percentage=TRUE) ) 
          print(Ann_mySpecificLoci_5)
          sink()
          pdf( file=paste(path2_5, "5_mySpecificLoci_5.pdf", sep="/")   )
          print(genomation::plotTargetAnnotation(Ann_mySpecificLoci_5, precedence=TRUE, main=mySpecificLoci_5_g))
          dev.off()
    }



   

    if( ! is.na(mySpecificLoci_6_g) ) {
          Ann_mySpecificLoci_6 = genomation::annotateWithFeatureFlank(target = as(myDiffDMR_5, "GRanges"),
                                         feature = mySpecificLoci_6.obj_g$targets,  flank = mySpecificLoci_6.obj_g$shores,
                                         feature.name = "targets",  flank.name = "shores"
          )
          sink( file=paste(path2_5, "6_mySpecificLoci_6.txt", sep="/")   )
          print(  genomation::getFeatsWithTargetsStats( Ann_mySpecificLoci_6,  percentage=TRUE) ) 
          print(Ann_mySpecificLoci_6)
          sink()
          pdf( file=paste(path2_5, "6_mySpecificLoci_6.pdf", sep="/")   )
          print(genomation::plotTargetAnnotation(Ann_mySpecificLoci_6, precedence=TRUE, main=mySpecificLoci_6_g))
          dev.off()
    }



   

    if( ! is.na(mySpecificLoci_7_g) ) {
          Ann_mySpecificLoci_7 = genomation::annotateWithFeatureFlank(target = as(myDiffDMR_5, "GRanges"),
                                         feature = mySpecificLoci_7.obj_g$targets,  flank = mySpecificLoci_7.obj_g$shores,
                                         feature.name = "targets",  flank.name = "shores"
          )
          sink( file=paste(path2_5, "6_mySpecificLoci_7.txt", sep="/")   )
          print(  genomation::getFeatsWithTargetsStats( Ann_mySpecificLoci_7,  percentage=TRUE) ) 
          print(Ann_mySpecificLoci_7)
          sink()
          pdf( file=paste(path2_5, "6_mySpecificLoci_7.pdf", sep="/")   )
          print(genomation::plotTargetAnnotation(Ann_mySpecificLoci_7, precedence=TRUE, main=mySpecificLoci_7_g))
          dev.off()
    }



   
    if( ! is.na(mySpecificLoci_8_g) ) {
          Ann_mySpecificLoci_8 = genomation::annotateWithFeatureFlank(target = as(myDiffDMR_5, "GRanges"),
                                         feature = mySpecificLoci_8.obj_g$targets,  flank = mySpecificLoci_8.obj_g$shores,
                                         feature.name = "targets",  flank.name = "shores"
          )
          sink( file=paste(path2_5, "8_mySpecificLoci_8.txt", sep="/")   )
          print(  genomation::getFeatsWithTargetsStats( Ann_mySpecificLoci_8,  percentage=TRUE) ) 
          print(Ann_mySpecificLoci_8)
          sink()
          pdf( file=paste(path2_5, "8_mySpecificLoci_8.pdf", sep="/")   )
          print(genomation::plotTargetAnnotation(Ann_mySpecificLoci_8, precedence=TRUE, main=mySpecificLoci_8_g))
          dev.off()
    }




    if( ! is.na(mySpecificLoci_9_g) ) {
          Ann_mySpecificLoci_9 = genomation::annotateWithFeatureFlank(target = as(myDiffDMR_5, "GRanges"),
                                         feature = mySpecificLoci_9.obj_g$targets,  flank = mySpecificLoci_9.obj_g$shores,
                                         feature.name = "targets",  flank.name = "shores"
          )
          sink( file=paste(path2_5, "9_mySpecificLoci_9.txt", sep="/")   )
          print(  genomation::getFeatsWithTargetsStats( Ann_mySpecificLoci_9,  percentage=TRUE) ) 
          print(Ann_mySpecificLoci_9)
          sink()
          pdf( file=paste(path2_5, "9_mySpecificLoci_9.pdf", sep="/")   )
          print(genomation::plotTargetAnnotation(Ann_mySpecificLoci_9, precedence=TRUE, main=mySpecificLoci_9_g))
          dev.off()
    }


   

    if( ! is.na(mySpecificLoci_10_g) ) {
          Ann_mySpecificLoci_10 = genomation::annotateWithFeatureFlank(target = as(myDiffDMR_5, "GRanges"),
                                         feature = mySpecificLoci_10.obj_g$targets,  flank = mySpecificLoci_10.obj_g$shores,
                                         feature.name = "targets",  flank.name = "shores"
          )
          sink( file=paste(path2_5, "10_mySpecificLoci_10.txt", sep="/")   )
          print(  genomation::getFeatsWithTargetsStats( Ann_mySpecificLoci_10,  percentage=TRUE) ) 
          print(Ann_mySpecificLoci_10)
          sink()
          pdf( file=paste(path2_5, "10_mySpecificLoci_10.pdf", sep="/")   )
          print(genomation::plotTargetAnnotation(Ann_mySpecificLoci_10, precedence=TRUE, main=mySpecificLoci_10_g))
          dev.off()
    }




   

    if( ! is.na(mySpecificLoci_11_g) ) {
          Ann_mySpecificLoci_11 = genomation::annotateWithFeatureFlank(target = as(myDiffDMR_5, "GRanges"),
                                         feature = mySpecificLoci_11.obj_g$targets,  flank = mySpecificLoci_11.obj_g$shores,
                                         feature.name = "targets",  flank.name = "shores"
          )
          sink( file=paste(path2_5, "11_mySpecificLoci_11.txt", sep="/")   )
          print(  genomation::getFeatsWithTargetsStats( Ann_mySpecificLoci_11,  percentage=TRUE) ) 
          print(Ann_mySpecificLoci_11)
          sink()
          pdf( file=paste(path2_5, "11_mySpecificLoci_11.pdf", sep="/")   )
          print(genomation::plotTargetAnnotation(Ann_mySpecificLoci_11, precedence=TRUE, main=mySpecificLoci_11_g))
          dev.off()
    }


   

    if( ! is.na(mySpecificLoci_12_g) ) {
          Ann_mySpecificLoci_12 = genomation::annotateWithFeatureFlank(target = as(myDiffDMR_5, "GRanges"),
                                         feature = mySpecificLoci_12.obj_g$targets,  flank = mySpecificLoci_12.obj_g$shores,
                                         feature.name = "targets",  flank.name = "shores"
          )
          sink( file=paste(path2_5, "12_mySpecificLoci_12.txt", sep="/")   )
          print(  genomation::getFeatsWithTargetsStats( Ann_mySpecificLoci_12,  percentage=TRUE) ) 
          print(Ann_mySpecificLoci_12)
          sink()
          pdf( file=paste(path2_5, "12_mySpecificLoci_12.pdf", sep="/")   )
          print(genomation::plotTargetAnnotation(Ann_mySpecificLoci_12, precedence=TRUE, main=mySpecificLoci_12_g))
          dev.off()
    }


   

    if( ! is.na(mySpecificLoci_13_g) ) {
          Ann_mySpecificLoci_13 = genomation::annotateWithFeatureFlank(target = as(myDiffDMR_5, "GRanges"),
                                         feature = mySpecificLoci_13.obj_g$targets,  flank = mySpecificLoci_13.obj_g$shores,
                                         feature.name = "targets",  flank.name = "shores"
          )
          sink( file=paste(path2_5, "13_mySpecificLoci_13.txt", sep="/")   )
          print(  genomation::getFeatsWithTargetsStats( Ann_mySpecificLoci_13,  percentage=TRUE) ) 
          print(Ann_mySpecificLoci_13)
          sink()
          pdf( file=paste(path2_5, "13_mySpecificLoci_13.pdf", sep="/")   )
          print(genomation::plotTargetAnnotation(Ann_mySpecificLoci_13, precedence=TRUE, main=mySpecificLoci_13_g))
          dev.off()
    }

   

    if( ! is.na(mySpecificLoci_14_g) ) {
          Ann_mySpecificLoci_14 = genomation::annotateWithFeatureFlank(target = as(myDiffDMR_5, "GRanges"),
                                         feature = mySpecificLoci_14.obj_g$targets,  flank = mySpecificLoci_14.obj_g$shores,
                                         feature.name = "targets",  flank.name = "shores"
          )
          sink( file=paste(path2_5, "14_mySpecificLoci_14.txt", sep="/")   )
          print(  genomation::getFeatsWithTargetsStats( Ann_mySpecificLoci_14,  percentage=TRUE) ) 
          print(Ann_mySpecificLoci_14)
          sink()
          pdf( file=paste(path2_5, "14_mySpecificLoci_14.pdf", sep="/")   )
          print(genomation::plotTargetAnnotation(Ann_mySpecificLoci_14, precedence=TRUE, main=mySpecificLoci_14_g))
          dev.off()
    }


   

    if( ! is.na(mySpecificLoci_15_g) ) {
          Ann_mySpecificLoci_15 = genomation::annotateWithFeatureFlank(target = as(myDiffDMR_5, "GRanges"),
                                         feature = mySpecificLoci_15.obj_g$targets,  flank = mySpecificLoci_15.obj_g$shores,
                                         feature.name = "targets",  flank.name = "shores"
          )
          sink( file=paste(path2_5, "15_mySpecificLoci_15.txt", sep="/")   )
          print(  genomation::getFeatsWithTargetsStats( Ann_mySpecificLoci_15,  percentage=TRUE) ) 
          print(Ann_mySpecificLoci_15)
          sink()
          pdf( file=paste(path2_5, "15_mySpecificLoci_15.pdf", sep="/")   )
          print(genomation::plotTargetAnnotation(Ann_mySpecificLoci_15, precedence=TRUE, main=mySpecificLoci_15_g))
          dev.off()
    }



   

    if( ! is.na(mySpecificLoci_16_g) ) {
          Ann_mySpecificLoci_16 = genomation::annotateWithFeatureFlank(target = as(myDiffDMR_5, "GRanges"),
                                         feature = mySpecificLoci_16.obj_g$targets,  flank = mySpecificLoci_16.obj_g$shores,
                                         feature.name = "targets",  flank.name = "shores"
          )
          sink( file=paste(path2_5, "16_mySpecificLoci_16.txt", sep="/")   )
          print(  genomation::getFeatsWithTargetsStats( Ann_mySpecificLoci_16,  percentage=TRUE) ) 
          print(Ann_mySpecificLoci_16)
          sink()
          pdf( file=paste(path2_5, "16_mySpecificLoci_16.pdf", sep="/")   )
          print(genomation::plotTargetAnnotation(Ann_mySpecificLoci_16, precedence=TRUE, main=mySpecificLoci_16_g))
          dev.off()
    }


   

    if( ! is.na(mySpecificLoci_17_g) ) {
          Ann_mySpecificLoci_17 = genomation::annotateWithFeatureFlank(target = as(myDiffDMR_5, "GRanges"),
                                         feature = mySpecificLoci_17.obj_g$targets,  flank = mySpecificLoci_17.obj_g$shores,
                                         feature.name = "targets",  flank.name = "shores"
          )
          sink( file=paste(path2_5, "17_mySpecificLoci_17.txt", sep="/")   )
          print(  genomation::getFeatsWithTargetsStats( Ann_mySpecificLoci_17,  percentage=TRUE) ) 
          print(Ann_mySpecificLoci_17)
          sink()
          pdf( file=paste(path2_5, "17_mySpecificLoci_17.pdf", sep="/")   )
          print(genomation::plotTargetAnnotation(Ann_mySpecificLoci_17, precedence=TRUE, main=mySpecificLoci_17_g))
          dev.off()
    }


   

    if( ! is.na(mySpecificLoci_18_g) ) {
          Ann_mySpecificLoci_18 = genomation::annotateWithFeatureFlank(target = as(myDiffDMR_5, "GRanges"),
                                         feature = mySpecificLoci_18.obj_g$targets,  flank = mySpecificLoci_18.obj_g$shores,
                                         feature.name = "targets",  flank.name = "shores"
          )
          sink( file=paste(path2_5, "18_mySpecificLoci_18.txt", sep="/")   )
          print(  genomation::getFeatsWithTargetsStats( Ann_mySpecificLoci_18,  percentage=TRUE) ) 
          print(Ann_mySpecificLoci_18)
          sink()
          pdf( file=paste(path2_5, "18_mySpecificLoci_18.pdf", sep="/")   )
          print(genomation::plotTargetAnnotation(Ann_mySpecificLoci_18, precedence=TRUE, main=mySpecificLoci_18_g))
          dev.off()
    }



   

    if( ! is.na(mySpecificLoci_19_g) ) {
          Ann_mySpecificLoci_19 = genomation::annotateWithFeatureFlank(target = as(myDiffDMR_5, "GRanges"),
                                         feature = mySpecificLoci_19.obj_g$targets,  flank = mySpecificLoci_19.obj_g$shores,
                                         feature.name = "targets",  flank.name = "shores"
          )
          sink( file=paste(path2_5, "19_mySpecificLoci_19.txt", sep="/")   )
          print(  genomation::getFeatsWithTargetsStats( Ann_mySpecificLoci_19,  percentage=TRUE) ) 
          print(Ann_mySpecificLoci_19)
          sink()
          pdf( file=paste(path2_5, "19_mySpecificLoci_19.pdf", sep="/")   )
          print(genomation::plotTargetAnnotation(Ann_mySpecificLoci_19, precedence=TRUE, main=mySpecificLoci_19_g))
          dev.off()
    }


   

    if( ! is.na(mySpecificLoci_20_g) ) {
          Ann_mySpecificLoci_20 = genomation::annotateWithFeatureFlank(target = as(myDiffDMR_5, "GRanges"),
                                         feature = mySpecificLoci_20.obj_g$targets,  flank = mySpecificLoci_20.obj_g$shores,
                                         feature.name = "targets",  flank.name = "shores"
          )
          sink( file=paste(path2_5, "20_mySpecificLoci_20.txt", sep="/")   )
          print(  genomation::getFeatsWithTargetsStats( Ann_mySpecificLoci_20,  percentage=TRUE) ) 
          print(Ann_mySpecificLoci_20)
          sink()
          pdf( file=paste(path2_5, "20_mySpecificLoci_20.pdf", sep="/")   )
          print(genomation::plotTargetAnnotation(Ann_mySpecificLoci_20, precedence=TRUE, main=mySpecificLoci_20_g))
          dev.off()
    }



}  










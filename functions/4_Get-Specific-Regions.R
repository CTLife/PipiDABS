##############################################################################################################################################################################################
suppressPackageStartupMessages( library(genomation) )
bedFiles_g  <- list.files(path = specificRegions_g, pattern = ".bed",  full.names = TRUE  )
cat("\n\nbedFiles_g:\n")
cat(bedFiles_g, sep = "\n")
cat("\n\n\n\n\n")

number_of_bedfiles_g = length(bedFiles_g) 
cat("\n\nNumber of bed files:  ")
cat(number_of_bedfiles_g)
cat("\n\n\n\n\n")

mySpecificLoci_1_g  = NA
mySpecificLoci_2_g  = NA
mySpecificLoci_3_g  = NA
mySpecificLoci_4_g  = NA
mySpecificLoci_5_g  = NA
mySpecificLoci_6_g  = NA
mySpecificLoci_7_g  = NA
mySpecificLoci_8_g  = NA
mySpecificLoci_9_g  = NA
mySpecificLoci_10_g = NA
mySpecificLoci_11_g = NA
mySpecificLoci_12_g = NA
mySpecificLoci_13_g = NA
mySpecificLoci_14_g = NA
mySpecificLoci_15_g = NA
mySpecificLoci_16_g = NA
mySpecificLoci_17_g = NA
mySpecificLoci_18_g = NA
mySpecificLoci_19_g = NA
mySpecificLoci_20_g = NA
mySpecificLoci_21_g = NA
mySpecificLoci_22_g = NA
mySpecificLoci_23_g = NA
mySpecificLoci_24_g = NA
mySpecificLoci_25_g = NA
mySpecificLoci_26_g = NA
mySpecificLoci_27_g = NA
mySpecificLoci_28_g = NA
mySpecificLoci_29_g = NA
mySpecificLoci_30_g = NA

for(i in c(1:length(bedFiles_g)) ) {
  if(i == 1)  {mySpecificLoci_1_g  = bedFiles_g[i] }
  if(i == 2)  {mySpecificLoci_2_g  = bedFiles_g[i] }
  if(i == 3)  {mySpecificLoci_3_g  = bedFiles_g[i] }
  if(i == 4)  {mySpecificLoci_4_g  = bedFiles_g[i] }
  if(i == 5)  {mySpecificLoci_5_g  = bedFiles_g[i] }
  if(i == 6)  {mySpecificLoci_6_g  = bedFiles_g[i] }
  if(i == 7)  {mySpecificLoci_7_g  = bedFiles_g[i] }
  if(i == 8)  {mySpecificLoci_8_g  = bedFiles_g[i] }
  if(i == 9)  {mySpecificLoci_9_g  = bedFiles_g[i] }
  if(i == 10) {mySpecificLoci_10_g = bedFiles_g[i] }
  if(i == 11) {mySpecificLoci_11_g = bedFiles_g[i] }
  if(i == 12) {mySpecificLoci_12_g = bedFiles_g[i] }
  if(i == 13) {mySpecificLoci_13_g = bedFiles_g[i] }
  if(i == 14) {mySpecificLoci_14_g = bedFiles_g[i] }
  if(i == 15) {mySpecificLoci_15_g = bedFiles_g[i] }
  if(i == 16) {mySpecificLoci_16_g = bedFiles_g[i] }
  if(i == 17) {mySpecificLoci_17_g = bedFiles_g[i] }
  if(i == 18) {mySpecificLoci_18_g = bedFiles_g[i] }
  if(i == 19) {mySpecificLoci_19_g = bedFiles_g[i] }
  if(i == 20) {mySpecificLoci_20_g = bedFiles_g[i] }
  if(i == 21) {mySpecificLoci_21_g = bedFiles_g[i] }
  if(i == 22) {mySpecificLoci_22_g = bedFiles_g[i] }
  if(i == 23) {mySpecificLoci_23_g = bedFiles_g[i] }
  if(i == 24) {mySpecificLoci_24_g = bedFiles_g[i] }
  if(i == 25) {mySpecificLoci_25_g = bedFiles_g[i] }
  if(i == 26) {mySpecificLoci_26_g = bedFiles_g[i] }
  if(i == 27) {mySpecificLoci_27_g = bedFiles_g[i] }
  if(i == 28) {mySpecificLoci_28_g = bedFiles_g[i] }
  if(i == 29) {mySpecificLoci_29_g = bedFiles_g[i] }
  if(i == 30) {mySpecificLoci_30_g = bedFiles_g[i] }
}
##############################################################################################################################################################################################





##############################################################################################################################################################################################
mySpecificLoci_1.obj_g  = NA
mySpecificLoci_2.obj_g  = NA
mySpecificLoci_3.obj_g  = NA
mySpecificLoci_4.obj_g  = NA
mySpecificLoci_5.obj_g  = NA
mySpecificLoci_6.obj_g  = NA
mySpecificLoci_7.obj_g  = NA
mySpecificLoci_8.obj_g  = NA
mySpecificLoci_9.obj_g  = NA
mySpecificLoci_10.obj_g = NA
mySpecificLoci_11.obj_g = NA
mySpecificLoci_12.obj_g = NA
mySpecificLoci_13.obj_g = NA
mySpecificLoci_14.obj_g = NA
mySpecificLoci_15.obj_g = NA
mySpecificLoci_16.obj_g = NA
mySpecificLoci_17.obj_g = NA
mySpecificLoci_18.obj_g = NA
mySpecificLoci_19.obj_g = NA
mySpecificLoci_20.obj_g = NA
mySpecificLoci_21.obj_g = NA
mySpecificLoci_22.obj_g = NA
mySpecificLoci_23.obj_g = NA
mySpecificLoci_24.obj_g = NA
mySpecificLoci_25.obj_g = NA
mySpecificLoci_26.obj_g = NA
mySpecificLoci_27.obj_g = NA
mySpecificLoci_28.obj_g = NA
mySpecificLoci_29.obj_g = NA
mySpecificLoci_30.obj_g = NA


if( ! is.na(mySpecificLoci_1_g) ) {
    if ( grepl(pattern="\\.Genes\\.bed", x=mySpecificLoci_1_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {
            mySpecificLoci_1.obj_g = genomation::readTranscriptFeatures( mySpecificLoci_1_g, remove.unusual=FALSE, up.flank=2000,  down.flank=1000,  unique.prom=TRUE )    
    }else{
            mySpecificLoci_1.obj_g = genomation::readFeatureFlank( mySpecificLoci_1_g, flank=2000, feature.flank.name=c("targets",  "shores") )         
    }     
}

if( ! is.na(mySpecificLoci_2_g) ) {
    if ( grepl(pattern="\\.Genes\\.bed", x=mySpecificLoci_2_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {
            mySpecificLoci_2.obj_g = genomation::readTranscriptFeatures( mySpecificLoci_2_g, remove.unusual=FALSE, up.flank=2000,  down.flank=1000,  unique.prom=TRUE )    
    }else{
            mySpecificLoci_2.obj_g = genomation::readFeatureFlank( mySpecificLoci_2_g, flank=2000, feature.flank.name=c("targets",  "shores") )         
    }     
}

if( ! is.na(mySpecificLoci_3_g) ) {
    if ( grepl(pattern="\\.Genes\\.bed", x=mySpecificLoci_3_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {
            mySpecificLoci_3.obj_g = genomation::readTranscriptFeatures( mySpecificLoci_3_g, remove.unusual=FALSE, up.flank=2000,  down.flank=1000,  unique.prom=TRUE )    
    }else{
            mySpecificLoci_3.obj_g = genomation::readFeatureFlank( mySpecificLoci_3_g, flank=2000, feature.flank.name=c("targets",  "shores") )         
    }     
}

if( ! is.na(mySpecificLoci_4_g) ) {
    if ( grepl(pattern="\\.Genes\\.bed", x=mySpecificLoci_4_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {
            mySpecificLoci_4.obj_g = genomation::readTranscriptFeatures( mySpecificLoci_4_g, remove.unusual=FALSE, up.flank=2000,  down.flank=1000,  unique.prom=TRUE )    
    }else{
            mySpecificLoci_4.obj_g = genomation::readFeatureFlank( mySpecificLoci_4_g, flank=2000, feature.flank.name=c("targets",  "shores") )         
    }     
}

if( ! is.na(mySpecificLoci_5_g) ) {
    if ( grepl(pattern="\\.Genes\\.bed", x=mySpecificLoci_5_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {
            mySpecificLoci_5.obj_g = genomation::readTranscriptFeatures( mySpecificLoci_5_g, remove.unusual=FALSE, up.flank=2000,  down.flank=1000,  unique.prom=TRUE )    
    }else{
            mySpecificLoci_5.obj_g = genomation::readFeatureFlank( mySpecificLoci_5_g, flank=2000, feature.flank.name=c("targets",  "shores") )         
    }     
}

if( ! is.na(mySpecificLoci_6_g) ) {
    if ( grepl(pattern="\\.Genes\\.bed", x=mySpecificLoci_6_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {
            mySpecificLoci_6.obj_g = genomation::readTranscriptFeatures( mySpecificLoci_6_g, remove.unusual=FALSE, up.flank=2000,  down.flank=1000,  unique.prom=TRUE )    
    }else{
            mySpecificLoci_6.obj_g = genomation::readFeatureFlank( mySpecificLoci_6_g, flank=2000, feature.flank.name=c("targets",  "shores") )         
    }     
}

if( ! is.na(mySpecificLoci_7_g) ) {
    if ( grepl(pattern="\\.Genes\\.bed", x=mySpecificLoci_7_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {
            mySpecificLoci_7.obj_g = genomation::readTranscriptFeatures( mySpecificLoci_7_g, remove.unusual=FALSE, up.flank=2000,  down.flank=1000,  unique.prom=TRUE )    
    }else{
            mySpecificLoci_7.obj_g = genomation::readFeatureFlank( mySpecificLoci_7_g, flank=2000, feature.flank.name=c("targets",  "shores") )         
    }     
}

if( ! is.na(mySpecificLoci_8_g) ) {
    if ( grepl(pattern="\\.Genes\\.bed", x=mySpecificLoci_8_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {
            mySpecificLoci_8.obj_g = genomation::readTranscriptFeatures( mySpecificLoci_8_g, remove.unusual=FALSE, up.flank=2000,  down.flank=1000,  unique.prom=TRUE )    
    }else{
            mySpecificLoci_8.obj_g = genomation::readFeatureFlank( mySpecificLoci_8_g, flank=2000, feature.flank.name=c("targets",  "shores") )         
    }     
}

if( ! is.na(mySpecificLoci_9_g) ) {
    if ( grepl(pattern="\\.Genes\\.bed", x=mySpecificLoci_9_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {
            mySpecificLoci_9.obj_g = genomation::readTranscriptFeatures( mySpecificLoci_9_g, remove.unusual=FALSE, up.flank=2000,  down.flank=1000,  unique.prom=TRUE )    
    }else{
            mySpecificLoci_9.obj_g = genomation::readFeatureFlank( mySpecificLoci_9_g, flank=2000, feature.flank.name=c("targets",  "shores") )         
    }     
}

if( ! is.na(mySpecificLoci_10_g) ) {
    if ( grepl(pattern="\\.Genes\\.bed", x=mySpecificLoci_10_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {
            mySpecificLoci_10.obj_g = genomation::readTranscriptFeatures( mySpecificLoci_10_g, remove.unusual=FALSE, up.flank=2000,  down.flank=1000,  unique.prom=TRUE )    
    }else{
            mySpecificLoci_10.obj_g = genomation::readFeatureFlank( mySpecificLoci_10_g, flank=2000, feature.flank.name=c("targets",  "shores") )         
    }     
}

if( ! is.na(mySpecificLoci_11_g) ) {
    if ( grepl(pattern="\\.Genes\\.bed", x=mySpecificLoci_11_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {
            mySpecificLoci_11.obj_g = genomation::readTranscriptFeatures( mySpecificLoci_11_g, remove.unusual=FALSE, up.flank=2000,  down.flank=1000,  unique.prom=TRUE )    
    }else{
            mySpecificLoci_11.obj_g = genomation::readFeatureFlank( mySpecificLoci_11_g, flank=2000, feature.flank.name=c("targets",  "shores") )         
    }     
}

if( ! is.na(mySpecificLoci_12_g) ) {
    if ( grepl(pattern="\\.Genes\\.bed", x=mySpecificLoci_12_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {
            mySpecificLoci_12.obj_g = genomation::readTranscriptFeatures( mySpecificLoci_12_g, remove.unusual=FALSE, up.flank=2000,  down.flank=1000,  unique.prom=TRUE )    
    }else{
            mySpecificLoci_12.obj_g = genomation::readFeatureFlank( mySpecificLoci_12_g, flank=2000, feature.flank.name=c("targets",  "shores") )         
    }     
}

if( ! is.na(mySpecificLoci_13_g) ) {
    if ( grepl(pattern="\\.Genes\\.bed", x=mySpecificLoci_13_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {
            mySpecificLoci_13.obj_g = genomation::readTranscriptFeatures( mySpecificLoci_13_g, remove.unusual=FALSE, up.flank=2000,  down.flank=1000,  unique.prom=TRUE )    
    }else{
            mySpecificLoci_13.obj_g = genomation::readFeatureFlank( mySpecificLoci_13_g, flank=2000, feature.flank.name=c("targets",  "shores") )         
    }     
}

if( ! is.na(mySpecificLoci_14_g) ) {
    if ( grepl(pattern="\\.Genes\\.bed", x=mySpecificLoci_14_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {
            mySpecificLoci_14.obj_g = genomation::readTranscriptFeatures( mySpecificLoci_14_g, remove.unusual=FALSE, up.flank=2000,  down.flank=1000,  unique.prom=TRUE )    
    }else{
            mySpecificLoci_14.obj_g = genomation::readFeatureFlank( mySpecificLoci_14_g, flank=2000, feature.flank.name=c("targets",  "shores") )         
    }     
}

if( ! is.na(mySpecificLoci_15_g) ) {
    if ( grepl(pattern="\\.Genes\\.bed", x=mySpecificLoci_15_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {
            mySpecificLoci_15.obj_g = genomation::readTranscriptFeatures( mySpecificLoci_15_g, remove.unusual=FALSE, up.flank=2000,  down.flank=1000,  unique.prom=TRUE )    
    }else{
            mySpecificLoci_15.obj_g = genomation::readFeatureFlank( mySpecificLoci_15_g, flank=2000, feature.flank.name=c("targets",  "shores") )         
    }     
}

if( ! is.na(mySpecificLoci_16_g) ) {
    if ( grepl(pattern="\\.Genes\\.bed", x=mySpecificLoci_16_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {
            mySpecificLoci_16.obj_g = genomation::readTranscriptFeatures( mySpecificLoci_16_g, remove.unusual=FALSE, up.flank=2000,  down.flank=1000,  unique.prom=TRUE )    
    }else{
            mySpecificLoci_16.obj_g = genomation::readFeatureFlank( mySpecificLoci_16_g, flank=2000, feature.flank.name=c("targets",  "shores") )         
    }     
}

if( ! is.na(mySpecificLoci_17_g) ) {
    if ( grepl(pattern="\\.Genes\\.bed", x=mySpecificLoci_17_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {
            mySpecificLoci_17.obj_g = genomation::readTranscriptFeatures( mySpecificLoci_17_g, remove.unusual=FALSE, up.flank=2000,  down.flank=1000,  unique.prom=TRUE )    
    }else{
            mySpecificLoci_17.obj_g = genomation::readFeatureFlank( mySpecificLoci_17_g, flank=2000, feature.flank.name=c("targets",  "shores") )         
    }     
}

if( ! is.na(mySpecificLoci_18_g) ) {
    if ( grepl(pattern="\\.Genes\\.bed", x=mySpecificLoci_18_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {
            mySpecificLoci_18.obj_g = genomation::readTranscriptFeatures( mySpecificLoci_18_g, remove.unusual=FALSE, up.flank=2000,  down.flank=1000,  unique.prom=TRUE )    
    }else{
            mySpecificLoci_18.obj_g = genomation::readFeatureFlank( mySpecificLoci_18_g, flank=2000, feature.flank.name=c("targets",  "shores") )         
    }     
}

if( ! is.na(mySpecificLoci_19_g) ) {
    if ( grepl(pattern="\\.Genes\\.bed", x=mySpecificLoci_19_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {
            mySpecificLoci_19.obj_g = genomation::readTranscriptFeatures( mySpecificLoci_19_g, remove.unusual=FALSE, up.flank=2000,  down.flank=1000,  unique.prom=TRUE )    
    }else{
            mySpecificLoci_19.obj_g = genomation::readFeatureFlank( mySpecificLoci_19_g, flank=2000, feature.flank.name=c("targets",  "shores") )         
    }     
}

if( ! is.na(mySpecificLoci_20_g) ) {
    if ( grepl(pattern="\\.Genes\\.bed", x=mySpecificLoci_20_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {
            mySpecificLoci_20.obj_g = genomation::readTranscriptFeatures( mySpecificLoci_20_g, remove.unusual=FALSE, up.flank=2000,  down.flank=1000,  unique.prom=TRUE )    
    }else{
            mySpecificLoci_20.obj_g = genomation::readFeatureFlank( mySpecificLoci_20_g, flank=2000, feature.flank.name=c("targets",  "shores") )         
    }  
}

if( ! is.na(mySpecificLoci_21_g) ) {
    if ( grepl(pattern="\\.Genes\\.bed", x=mySpecificLoci_21_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {
            mySpecificLoci_21.obj_g = genomation::readTranscriptFeatures( mySpecificLoci_21_g, remove.unusual=FALSE, up.flank=2000,  down.flank=1000,  unique.prom=TRUE )    
    }else{
            mySpecificLoci_21.obj_g = genomation::readFeatureFlank( mySpecificLoci_21_g, flank=2000, feature.flank.name=c("targets",  "shores") )         
    }     
}

if( ! is.na(mySpecificLoci_22_g) ) {
    if ( grepl(pattern="\\.Genes\\.bed", x=mySpecificLoci_22_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {
            mySpecificLoci_22.obj_g = genomation::readTranscriptFeatures( mySpecificLoci_22_g, remove.unusual=FALSE, up.flank=2000,  down.flank=1000,  unique.prom=TRUE )    
    }else{
            mySpecificLoci_22.obj_g = genomation::readFeatureFlank( mySpecificLoci_22_g, flank=2000, feature.flank.name=c("targets",  "shores") )         
    }     
}

if( ! is.na(mySpecificLoci_23_g) ) {
    if ( grepl(pattern="\\.Genes\\.bed", x=mySpecificLoci_23_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {
            mySpecificLoci_23.obj_g = genomation::readTranscriptFeatures( mySpecificLoci_23_g, remove.unusual=FALSE, up.flank=2000,  down.flank=1000,  unique.prom=TRUE )    
    }else{
            mySpecificLoci_23.obj_g = genomation::readFeatureFlank( mySpecificLoci_23_g, flank=2000, feature.flank.name=c("targets",  "shores") )         
    }     
}

if( ! is.na(mySpecificLoci_24_g) ) {
    if ( grepl(pattern="\\.Genes\\.bed", x=mySpecificLoci_24_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {
            mySpecificLoci_24.obj_g = genomation::readTranscriptFeatures( mySpecificLoci_24_g, remove.unusual=FALSE, up.flank=2000,  down.flank=1000,  unique.prom=TRUE )    
    }else{
            mySpecificLoci_24.obj_g = genomation::readFeatureFlank( mySpecificLoci_24_g, flank=2000, feature.flank.name=c("targets",  "shores") )         
    }     
}

if( ! is.na(mySpecificLoci_25_g) ) {
    if ( grepl(pattern="\\.Genes\\.bed", x=mySpecificLoci_25_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {
            mySpecificLoci_25.obj_g = genomation::readTranscriptFeatures( mySpecificLoci_25_g, remove.unusual=FALSE, up.flank=2000,  down.flank=1000,  unique.prom=TRUE )    
    }else{
            mySpecificLoci_25.obj_g = genomation::readFeatureFlank( mySpecificLoci_25_g, flank=2000, feature.flank.name=c("targets",  "shores") )         
    }     
}

if( ! is.na(mySpecificLoci_26_g) ) {
    if ( grepl(pattern="\\.Genes\\.bed", x=mySpecificLoci_26_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {
            mySpecificLoci_26.obj_g = genomation::readTranscriptFeatures( mySpecificLoci_26_g, remove.unusual=FALSE, up.flank=2000,  down.flank=1000,  unique.prom=TRUE )    
    }else{
            mySpecificLoci_26.obj_g = genomation::readFeatureFlank( mySpecificLoci_26_g, flank=2000, feature.flank.name=c("targets",  "shores") )         
    }     
}

if( ! is.na(mySpecificLoci_27_g) ) {
    if ( grepl(pattern="\\.Genes\\.bed", x=mySpecificLoci_27_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {
            mySpecificLoci_27.obj_g = genomation::readTranscriptFeatures( mySpecificLoci_27_g, remove.unusual=FALSE, up.flank=2000,  down.flank=1000,  unique.prom=TRUE )    
    }else{
            mySpecificLoci_27.obj_g = genomation::readFeatureFlank( mySpecificLoci_27_g, flank=2000, feature.flank.name=c("targets",  "shores") )         
    }     
}

if( ! is.na(mySpecificLoci_28_g) ) {
    if ( grepl(pattern="\\.Genes\\.bed", x=mySpecificLoci_28_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {
            mySpecificLoci_28.obj_g = genomation::readTranscriptFeatures( mySpecificLoci_28_g, remove.unusual=FALSE, up.flank=2000,  down.flank=1000,  unique.prom=TRUE )    
    }else{
            mySpecificLoci_28.obj_g = genomation::readFeatureFlank( mySpecificLoci_28_g, flank=2000, feature.flank.name=c("targets",  "shores") )         
    }     
}

if( ! is.na(mySpecificLoci_29_g) ) {
    if ( grepl(pattern="\\.Genes\\.bed", x=mySpecificLoci_29_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {
            mySpecificLoci_29.obj_g = genomation::readTranscriptFeatures( mySpecificLoci_29_g, remove.unusual=FALSE, up.flank=2000,  down.flank=1000,  unique.prom=TRUE )    
    }else{
            mySpecificLoci_29.obj_g = genomation::readFeatureFlank( mySpecificLoci_29_g, flank=2000, feature.flank.name=c("targets",  "shores") )         
    }     
}

if( ! is.na(mySpecificLoci_30_g) ) {
    if ( grepl(pattern="\\.Genes\\.bed", x=mySpecificLoci_30_g, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) ) {
            mySpecificLoci_30.obj_g = genomation::readTranscriptFeatures( mySpecificLoci_30_g, remove.unusual=FALSE, up.flank=2000,  down.flank=1000,  unique.prom=TRUE )    
    }else{
            mySpecificLoci_30.obj_g = genomation::readFeatureFlank( mySpecificLoci_30_g, flank=2000, feature.flank.name=c("targets",  "shores") )         
    }       
}

 
##############################################################################################################################################################################################






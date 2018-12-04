

############################ These texts can be changed by user.  ###############################################
mySpecificLoci_1_g  = "/home/yp/AnnotationBED/hg38/otherRegions/test_genes.bed"   ## only for Transcript Features
my_upstream_TSS_g   = 1000   ## Define promoter from "TSS + my_upstream_TSS_g" to "TSS - my_downstream_TSS_g" (bp).
my_downstream_TSS_g = 500    ## Define promoter from "TSS + my_upstream_TSS_g" to "TSS - my_downstream_TSS_g" (bp).
mySpecificLoci_2_g = "/home/yp/AnnotationBED/ART_Twins_finalDMRs_20180929/DMRs/23_ICSI-fresh_vs_IVF-frozen/Twins/twins_hyper.bed"
mySpecificLoci_3_g = "/home/yp/AnnotationBED/ART_Twins_finalDMRs_20180929/DMRs/23_ICSI-fresh_vs_IVF-frozen/Twins/twins_hypo.bed"
mySpecificLoci_4_g = "/home/yp/AnnotationBED/ART_Twins_finalDMRs_20180929/DMRs/24_ICSI-fresh_vs_ICSI-frozen/Twins/twins_hyper.bed"
mySpecificLoci_5_g = "/home/yp/AnnotationBED/ART_Twins_finalDMRs_20180929/DMRs/24_ICSI-fresh_vs_ICSI-frozen/Twins/twins_hypo.bed"
mySpecificLoci_6_g = "/home/yp/AnnotationBED/ART_Twins_finalDMRs_20180929/DMRs/34_IVF-frozen_vs_ICSI-frozen/Twins/twins_hyper.bed"
mySpecificLoci_7_g = "/home/yp/AnnotationBED/ART_Twins_finalDMRs_20180929/DMRs/34_IVF-frozen_vs_ICSI-frozen/Twins/twins_hypo.bed"
mySpecificLoci_8_g = "/home/yp/AnnotationBED/ART_Twins_finalDMRs_20180929/DMRs/NC-IVF/4_hyperIVFfrozen_selected/1_NC_vs_IVF-fresh_hypo.bed"
mySpecificLoci_9_g = "/home/yp/AnnotationBED/ART_Twins_finalDMRs_20180929/DMRs/NC-IVF/4_hyperIVFfrozen_selected/3_NC_vs_IVF-frozen_hyper.bed"
mySpecificLoci_10_g = "/home/yp/AnnotationBED/ART_Twins_finalDMRs_20180929/DMRs/NC-IVF/4_hyperIVFfrozen_selected/13_IVF-fresh_vs_IVF-frozen_hyper.bed"
mySpecificLoci_11_g = "/home/yp/AnnotationBED/ART_Twins_finalDMRs_20180929/DMRs/NC-IVF/4_hyperIVFfrozen_selected/13_IVF-fresh_vs_IVF-frozen_hyper.bed_1_NC_vs_IVF-fresh_hypo.bed"
mySpecificLoci_12_g = "/home/yp/AnnotationBED/ART_Twins_finalDMRs_20180929/DMRs/NC-IVF/4_hyperIVFfrozen_selected/13_IVF-fresh_vs_IVF-frozen_hyper.bed_3_NC_vs_IVF-frozen_hyper.bed"                                   
mySpecificLoci_13_g = "/home/yp/AnnotationBED/ART_Twins_finalDMRs_20180929/DMRs/NC-IVF/4_hypoIVFfrozen_selected/1_NC_vs_IVF-fresh_hyper.bed"
mySpecificLoci_14_g = "/home/yp/AnnotationBED/ART_Twins_finalDMRs_20180929/DMRs/NC-IVF/4_hypoIVFfrozen_selected/3_NC_vs_IVF-frozen_hypo.bed"
mySpecificLoci_15_g = "/home/yp/AnnotationBED/ART_Twins_finalDMRs_20180929/DMRs/NC-IVF/4_hypoIVFfrozen_selected/13_IVF-fresh_vs_IVF-frozen_hypo.bed"
mySpecificLoci_16_g = "/home/yp/AnnotationBED/ART_Twins_finalDMRs_20180929/DMRs/NC-IVF/4_hypoIVFfrozen_selected/13_IVF-fresh_vs_IVF-frozen_hypo.bed_1_NC_vs_IVF-fresh_hyper.bed"
mySpecificLoci_17_g = "/home/yp/AnnotationBED/ART_Twins_finalDMRs_20180929/DMRs/NC-IVF/4_hypoIVFfrozen_selected/13_IVF-fresh_vs_IVF-frozen_hypo.bed_3_NC_vs_IVF-frozen_hypo.bed"
mySpecificLoci_18_g = NA
mySpecificLoci_19_g = NA
mySpecificLoci_20_g = NA
##################################################################################################################





########################################## Do Not Any Change ####################################################### 
mySpecificLoci_1A.obj_g = NA
mySpecificLoci_1B.obj_g = NA
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

if( ! is.na(mySpecificLoci_1_g) ) {
    mySpecificLoci_1A.obj_g = genomation::readTranscriptFeatures(mySpecificLoci_1_g, remove.unusual=FALSE, up.flank=my_upstream_TSS_g,  down.flank=my_downstream_TSS_g,  unique.prom=TRUE)         
    mySpecificLoci_1B.obj_g = genomation::readTranscriptFeatures(mySpecificLoci_1_g, remove.unusual=FALSE, up.flank=3000,  down.flank=0,  unique.prom=TRUE)         
}
if( ! is.na(mySpecificLoci_2_g) ) {
    mySpecificLoci_2.obj_g = genomation::readFeatureFlank(mySpecificLoci_2_g, flank=2000, feature.flank.name=c("targets",  "shores"))         
}
if( ! is.na(mySpecificLoci_3_g) ) {
    mySpecificLoci_3.obj_g = genomation::readFeatureFlank(mySpecificLoci_3_g, flank=2000, feature.flank.name=c("targets",  "shores"))         
}
if( ! is.na(mySpecificLoci_4_g) ) {
    mySpecificLoci_4.obj_g = genomation::readFeatureFlank(mySpecificLoci_4_g, flank=2000, feature.flank.name=c("targets",  "shores"))         
}
if( ! is.na(mySpecificLoci_5_g) ) {
    mySpecificLoci_5.obj_g = genomation::readFeatureFlank(mySpecificLoci_5_g, flank=2000, feature.flank.name=c("targets",  "shores"))         
}
if( ! is.na(mySpecificLoci_6_g) ) {
    mySpecificLoci_6.obj_g = genomation::readFeatureFlank(mySpecificLoci_6_g, flank=2000, feature.flank.name=c("targets",  "shores"))         
}
if( ! is.na(mySpecificLoci_7_g) ) {
    mySpecificLoci_7.obj_g = genomation::readFeatureFlank(mySpecificLoci_7_g, flank=2000, feature.flank.name=c("targets",  "shores"))         
}
if( ! is.na(mySpecificLoci_8_g) ) {
    mySpecificLoci_8.obj_g = genomation::readFeatureFlank(mySpecificLoci_8_g, flank=2000, feature.flank.name=c("targets",  "shores"))         
}
if( ! is.na(mySpecificLoci_9_g) ) {
    mySpecificLoci_9.obj_g = genomation::readFeatureFlank(mySpecificLoci_9_g, flank=2000, feature.flank.name=c("targets",  "shores"))         
}
if( ! is.na(mySpecificLoci_10_g) ) {
    mySpecificLoci_10.obj_g = genomation::readFeatureFlank(mySpecificLoci_10_g, flank=2000, feature.flank.name=c("targets",  "shores"))         
}
if( ! is.na(mySpecificLoci_11_g) ) {
    mySpecificLoci_11.obj_g = genomation::readFeatureFlank(mySpecificLoci_11_g, flank=2000, feature.flank.name=c("targets",  "shores"))         
}
if( ! is.na(mySpecificLoci_12_g) ) {
    mySpecificLoci_12.obj_g = genomation::readFeatureFlank(mySpecificLoci_12_g, flank=2000, feature.flank.name=c("targets",  "shores"))         
}
if( ! is.na(mySpecificLoci_13_g) ) {
    mySpecificLoci_13.obj_g = genomation::readFeatureFlank(mySpecificLoci_13_g, flank=2000, feature.flank.name=c("targets",  "shores"))         
}
if( ! is.na(mySpecificLoci_14_g) ) {
    mySpecificLoci_14.obj_g = genomation::readFeatureFlank(mySpecificLoci_14_g, flank=2000, feature.flank.name=c("targets",  "shores"))         
}
if( ! is.na(mySpecificLoci_15_g) ) {
    mySpecificLoci_15.obj_g = genomation::readFeatureFlank(mySpecificLoci_15_g, flank=2000, feature.flank.name=c("targets",  "shores"))         
}
if( ! is.na(mySpecificLoci_16_g) ) {
    mySpecificLoci_16.obj_g = genomation::readFeatureFlank(mySpecificLoci_16_g, flank=2000, feature.flank.name=c("targets",  "shores"))         
}
if( ! is.na(mySpecificLoci_17_g) ) {
    mySpecificLoci_17.obj_g = genomation::readFeatureFlank(mySpecificLoci_17_g, flank=2000, feature.flank.name=c("targets",  "shores"))         
}
if( ! is.na(mySpecificLoci_18_g) ) {
    mySpecificLoci_18.obj_g = genomation::readFeatureFlank(mySpecificLoci_18_g, flank=2000, feature.flank.name=c("targets",  "shores"))         
}
if( ! is.na(mySpecificLoci_19_g) ) {
    mySpecificLoci_19.obj_g = genomation::readFeatureFlank(mySpecificLoci_19_g, flank=2000, feature.flank.name=c("targets",  "shores"))         
}
if( ! is.na(mySpecificLoci_20_g) ) {
    mySpecificLoci_20.obj_g = genomation::readFeatureFlank(mySpecificLoci_20_g, flank=2000, feature.flank.name=c("targets",  "shores"))         
}
##################################################################################################################







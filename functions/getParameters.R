###########################################################################################################
### PipiDABS: A Comprehensive and User-Friendly Pipeline for Downstream DNA Methylation Analysis 
###           from High-throughput Bisulfite Sequencing                      
### Author: Yong Peng
### Version 0.1.0, July 30th, 2018
### Run "PipiDABS -h" to get some help.
###########################################################################################################


suppressPackageStartupMessages( library(optparse) )     # to run the script in command lines


getParameters_f <- function() {

option_list_Local <- list(   # Options list with associated default value.
  optparse::make_option(opt_str=c("-PN", "--projectName"),
			default=basename(getwd()),
			type="character",   dest="projectName",
			help="Name of the project used for the report [default: name of the current directory]."),

  optparse::make_option(opt_str=c("-A", "--author"),
			default=Sys.info()[7],
			type="character",   dest="author",
			help="Name of the report author [default: %default]."),

  optparse::make_option(opt_str=c("-SI", "--samplesInformation"),
			default="samples_information.example.txt",
			type="character",   dest="samplesInformation",
			help="Path to the design file with samples information [default: %default]."),

  optparse::make_option(opt_str=c("-CS", "--colorShape"),
			default="colorShape.example.txt",
			type="character",   dest="colorShape",
			help="Path to the design file with color or shape of each condition  [default: %default]."),

  optparse::make_option(opt_str=c("-OD", "--outDir"),
			default="BSseq_Downstream_results",
			type="character",   dest="outDir",
			help="Path to the directory containing all the analysis results [default: %default]."),

  optparse::make_option(opt_str=c("-LC", "--lowestCoverage"),
			default=5,
			type="integer",   dest="lowestCoverage",
			help="The lowest coverage of kept cytosine sites for downstream analysis [default: %default]"),

  optparse::make_option(opt_str=c("-NCS", "--numberCoveredSamples"),
			default=2,
			type="integer",   dest="numberCoveredSamples",
			help="The number of covered samples in each group. If you need to cover all samples, please set this value to 0. [default: %default]"),

  optparse::make_option(opt_str=c("-RG", "--RefGenomes"),
			default="hg38",
			type="character",   dest="RefGenomes",
			help="The reference genome of your input files, such as hg38, hg19, mm10 and mm9. [default: %default]"),

  optparse::make_option(opt_str=c("-FF", "--fileFormat"),
			default="bismarkCoverage",
			type="character",   dest="fileFormat",
			help="The format of your input files without table header. Please see the function methRead in package MethylKit for more details. [default: %default]"),

  optparse::make_option(opt_str=c("-CC", "--cytosineContext"),
			default="CpG",
			type="character",   dest="cytosineContext",
			help="Context of cytosine sites. This value should be CpG, CHG or CHH. [default: %default]"),

  optparse::make_option(opt_str=c("-Q", "--qvalue"),
			default=0.05,
			type="double",   dest="qvalue",
			help="The q-value threshold for differentially methylated sites or regions  (DMCs or DMRs). [default: %default]"),

  optparse::make_option(opt_str=c("-DOM", "--differenceOfMethylation"),
			default=0.05,
			type="double",   dest="differenceOfMethylation",
			help="Threshold of DNA methylation differences of two groups on their DMCs or DMRs. [default: %default]"),

  optparse::make_option(opt_str=c("-WS", "--windowSize"),
			default=100,
			type="integer",   dest="windowSize",
			help="An integer for the size of the tiling windows (bp). [default: %default]"),

  optparse::make_option(opt_str=c("-SS", "--stepSize"),
			default=20,
			type="integer",   dest="stepSize",
			help="An integer for the step size of tiling windows (bp). [default: %default]"),

  optparse::make_option(opt_str=c("-MD", "--mergeDistance"),
			default=202,
			type="integer",   dest="mergeDistance",
			help="DMRs will be merged if the distance of their centers is less than this value. [default: %default]"),

  optparse::make_option(opt_str=c("-MB", "--minBases"),
			default=3,
			type="integer",   dest="minBases",
			help="Minimum number of bases to be covered in a given tiling window. [default: %default]"),

  optparse::make_option(opt_str=c("-NC", "--numCores"),
			default=16,
			type="integer",   dest="numCores",
			help="Number of cores to use when processing BS-seq data. [default: %default]"),

  optparse::make_option(opt_str=c("-SR", "--SpecificRegions"),
                       action="store_true",  default=FALSE, dest="SpecificRegions",
                       help="Whether you analyze BS-seq data on some specific regions or not, such as promoters and enhancers. [default: FALSE]" ),

  optparse::make_option(opt_str=c("-MBSR", "--minBasesSpecificRegions"),
			default=3,
			type="integer",   dest="minBasesSpecificRegions",
			help="Minimum number of bases to be covered in a given specific segions, such as promoters and enhancers. [default: %default]"),
			
  optparse::make_option(opt_str=c("-FCG", "--forceCairoGraph"),
                       action="store_true",  default=FALSE, dest="forceCairoGraph",
                       help="Activate cairo type. [default: FALSE]"),

  optparse::make_option(opt_str=c("-TPC", "--topPercentageCoverage"),
			default=0,
			type="double",   dest="topPercentageCoverage",
			help="If our samples are suffering from PCR bias it would be useful to discard bases with very high read coverage. For RRBS data, we suggest that set this value to 0.5~5. If we set this value to 1, then the top 1% C sites with highest coverage will be removed for further analysis. [default: %default]")                                          

)



# now parse the command line to check which option is given and get associated values
parser_Local <- optparse::OptionParser(usage="usage: %prog [options]",
		option_list=option_list_Local, 
		description="PipiDABS: A Comprehensive and User-Friendly Pipeline for Downstream DNA Methylation Analysis of High-throughput Bisulfite Sequencing Data. Version 0.1.0, July 30th, 2018.",                             
		epilogue="For comments, bug reports etc..., please visit https://github.com/CTLife/PipiDABS or contact Yong Peng <yongp@outlook.com>"
)

opt_Local <- optparse::parse_args(parser_Local, args=commandArgs(trailingOnly=TRUE), positional_arguments=0)$options
return(opt_Local)

}





opt_g = getParameters_f()

projectName_g             <- opt_g$projectName
author_g                  <- opt_g$author
samplesInformation_g      <- opt_g$samplesInformation
colorShape_g              <- opt_g$colorShape
outDir_g                  <- opt_g$outDir
lowestCoverage_g          <- as.integer(opt_g$lowestCoverage)
numberCoveredSamples_g    <- as.integer(opt_g$numberCoveredSamples)
RefGenomes_g              <- opt_g$RefGenomes
fileFormat_g              <- opt_g$fileFormat
cytosineContext_g         <- opt_g$cytosineContext
qvalue_g                  <- opt_g$qvalue
differenceOfMethylation_g <- opt_g$differenceOfMethylation
windowSize_g              <- as.integer(opt_g$windowSize)
stepSize_g                <- as.integer(opt_g$stepSize)
mergeDistance_g           <- as.integer(opt_g$mergeDistance)
minBases_g                <- as.integer(opt_g$minBases)
numCores_g                <- as.integer(opt_g$numCores)
SpecificRegions_g         <- opt_g$SpecificRegions
minBasesSpecificRegions_g <- as.integer(opt_g$minBasesSpecificRegions)
forceCairoGraph_g         <- opt_g$forceCairoGraph
topPercentageCoverage_g   <- opt_g$topPercentageCoverage
workDir_g                 <- getwd()


options(digits=10)
if (forceCairoGraph_g) options(bitmapType="cairo")

continue_on_error <- function() {
  print( "NOTE: THERE WAS AN ERROR HERE. We are continuing because we have set 'options(error=continue_on_error())' " )
}
options( error=continue_on_error )  ## This option is very important.








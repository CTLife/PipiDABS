##############################################################################################################################################################################################
## PipiDABS: A Comprehensive and User-Friendly Pipeline for Downstream DNA Methylation Analysis of High-throughput Bisulfite Sequencing Data.                      
## Author: Yong Peng, yongp@outlook.com.
## Version 0.2.1, February 1st, 2019.
## Run "PipiDABS -h" to get some help.
##############################################################################################################################################################################################





##############################################################################################################################################################################################
suppressPackageStartupMessages( library(optparse) )  ## To run the script in command lines.

getParameters_f <- function() {
	option_list_Local <- list(   ## Options list with associated default value.  16 options.
  		optparse::make_option(opt_str=c("-SI", "--samplesInformation"),
			default="samplesInformation.example.txt",
			type="character",   dest="samplesInformation",
			help="Path to the design/target file with samples information. The table header (the first line) of this file must be 'sample_name	sample_path	group	other_feature'. For more details, please visit https://github.com/CTLife/PipiDABS. [default: %default]."),

  		optparse::make_option(opt_str=c("-CS", "--colorShape"),
			default="colorShape.example.txt",
			type="character",   dest="colorShape",
			help="Path to the design/target file with colors and shapes for each condition or group. The table header (the first line) of this file must be 'standards	type_name	color_shape'. For more details, please visit https://github.com/CTLife/PipiDABS. [default: %default]."),

  		optparse::make_option(opt_str=c("-OD", "--outDir"),
			default="outDir_BSseq_Downstream_results",
			type="character",   dest="outDir",
			help="Path to the directory containing all the analysis results. [default: %default]."),

  		optparse::make_option(opt_str=c("-LC", "--lowestCoverage"),
			default=6,
			type="integer",   dest="lowestCoverage",
			help="An integer for read counts. Cytosine bases having lower coverage than this count is discarded for further analysis. That is to say, a cytosine will be kept if its coverage >= this value. [default: %default]."),         

  		optparse::make_option(opt_str=c("-NCS", "--numberCoveredSamples"),
			default=0,
			type="integer",   dest="numberCoveredSamples",
			help="The number of covered samples in each group. If you need to cover all samples, please set this value to 0. [default: %default]."),

  		optparse::make_option(opt_str=c("-RG", "--refGenome"),
			default="hg38",
			type="character",   dest="refGenome",
			help="A string that defines the genome assembly or reference genome for your input files, such as hg38, hg19, mm10 and mm9. [default: %default]."),

  		optparse::make_option(opt_str=c("-CC", "--cytosineContext"),
			default="CpG",
			type="character",   dest="cytosineContext",
			help="Context of cytosine sites. This value should be CpG, CHG or CHH. [default: %default]."),

  		optparse::make_option(opt_str=c("-Q", "--qvalue"),
			default=0.05,
			type="double",   dest="qvalue",
			help="The q-value threshold for differentially methylated cytosines or regions  (DMCs or DMRs). [default: %default]."),

  		optparse::make_option(opt_str=c("-DOM", "--differenceOfMethylation"),
			default=0.05,
			type="double",   dest="differenceOfMethylation",
			help="Threshold of DNA methylation level differences of two groups on their DMCs or DMRs. [default: %default]."),

  		optparse::make_option(opt_str=c("-WS", "--windowSize"),
			default=200,
			type="integer",   dest="windowSize",
			help="An integer for the size of the tiling windows (bp). [default: %default]."),

  		optparse::make_option(opt_str=c("-SS", "--stepSize"),
			default=50,
			type="integer",   dest="stepSize",
			help="An integer for the step size of tiling windows (bp). [default: %default]."),

  		optparse::make_option(opt_str=c("-MB", "--minBases"),
			default=3,
			type="integer",   dest="minBases",
			help="Minimum number of bases to be covered in a given tiling window. [default: %default]."),

  		optparse::make_option(opt_str=c("-SR", "--specificRegions"),
                        default="NA_NoGenomicRegions", 
                        type="character",   dest="specificRegions",
                        help="Whether you analyze BS-seq data on some specific genomic regions or not, such as promoters and enhancers. This value is the directory of bed files with genomic regions. Please don't set this value, if no bed files need to be analyzed. [default: %default]." ),

  		optparse::make_option(opt_str=c("-MBSR", "--minBasesSpecificRegions"),
			default=3,
			type="integer",   dest="minBasesSpecificRegions",
			help="Minimum number of bases to be covered in a given specific segion, such as promoters and enhancers. The option '--specificRegions' is required. [default: %default]."),

  		optparse::make_option(opt_str=c("-TPC", "--topPercentageCoverage"),
			default=0,
			type="double",   dest="topPercentageCoverage",
			help="If our samples are suffering from PCR bias it would be useful to discard bases with very high read coverage. For RRBS data, we suggest that set this value to 0.1~5. If we set this value to 1, then the top 1% C sites with highest coverage will be removed for further analysis. [default: %default].") ,

  		optparse::make_option(opt_str=c("-NOR", "--normalize"),  
                        default="none", 
                        type="character",   dest="normalize",
                        help="Normalize coverage values between samples using a scaling factor derived from differences between mean or median of coverage distributions. A string 'none', 'mean' or 'median' which denotes no value, median or mean should be used to calculate scaling factor. [default: %default]." )                                            
	)
	
	## Now parse the command line to check which option is given and get associated values.
	parser_Local <- optparse::OptionParser(usage="usage: %prog [options]",
			option_list=option_list_Local, 
			description="PipiDABS: A Comprehensive and User-Friendly Pipeline for Downstream DNA Methylation Analysis of High-throughput Bisulfite Sequencing Data. Version 0.2.1, February 1st, 2019.",                             
			epilogue="For comments, bug reports etc..., please visit https://github.com/CTLife/PipiDABS or contact Yong Peng <yongp@outlook.com>."
	)
	opt_Local <- optparse::parse_args(parser_Local, args=commandArgs(trailingOnly=TRUE), positional_arguments=0)$options
	return(opt_Local)
}
##############################################################################################################################################################################################





##############################################################################################################################################################################################
opt_g = getParameters_f()  

samplesInformation_g      <- opt_g$samplesInformation
colorShape_g              <- opt_g$colorShape
outDir_g                  <- opt_g$outDir
lowestCoverage_g          <- as.integer(opt_g$lowestCoverage)
numberCoveredSamples_g    <- as.integer(opt_g$numberCoveredSamples)
refGenome_g               <- opt_g$refGenome
cytosineContext_g         <- opt_g$cytosineContext
qvalue_g                  <- opt_g$qvalue
differenceOfMethylation_g <- opt_g$differenceOfMethylation
windowSize_g              <- as.integer(opt_g$windowSize)
stepSize_g                <- as.integer(opt_g$stepSize)
minBases_g                <- as.integer(opt_g$minBases)
specificRegions_g         <- opt_g$specificRegions
minBasesSpecificRegions_g <- as.integer(opt_g$minBasesSpecificRegions)  
topPercentageCoverage_g   <- opt_g$topPercentageCoverage
normalize_g               <- opt_g$normalize

rm(getParameters_f)  
rm(opt_g)  

options(digits=10)
continue_on_error_g <- function() {
    print( "NOTE: THERE WAS AN ERROR HERE. We are continuing because we have set 'options(error=continue_on_error())'. " )
}
options( error=continue_on_error_g )  ## This option is very important.
##############################################################################################################################################################################################






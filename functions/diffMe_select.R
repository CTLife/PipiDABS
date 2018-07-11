




mySelectDiffMe <- function( path_temp3, file_temp3 ) {
    myOutPath = paste(path_temp3, "SelectedDiffMe", sep="/")
    if( ! file.exists(myOutPath) )  { dir.create(myOutPath, recursive = TRUE) }
    nameOfRegion = path_temp3
    sink( file=paste(myOutPath, "runLog.txt",  sep="/" ) )

    matrix1 = read.table(file= paste(nameOfRegion, file_temp3,  sep="/" ), header = TRUE, sep = "\t", quote = "\"'", dec = ".")
    dim(matrix1)
    colnames(matrix1)
    myQvalue = matrix1$qvalue
    myDiff   = matrix1$meth.diff   

    print("#####################################################")
    print("## Number of DMCs for six conditions: ")
    print(nrow( matrix1[( (myQvalue<0.001) & (abs(myDiff)>10) ),]  ) )
    print(nrow( matrix1[( (myQvalue<0.01)  & (abs(myDiff)>10) ),]  ) )
    print(nrow( matrix1[( (myQvalue<0.05)  & (abs(myDiff)>10) ),]  ) )
    print(nrow( matrix1[( (myQvalue<0.001) & (abs(myDiff)>5) ),]  ) )
    print(nrow( matrix1[( (myQvalue<0.01)  & (abs(myDiff)>5) ),]  ) )
    print(nrow( matrix1[( (myQvalue<0.05)  & (abs(myDiff)>5) ),]  ) )
    print("#####################################################")





    ## Select the DMCs.
    diff1_all = matrix1[( (myQvalue<0.05)  & (abs(myDiff)>5) ),]
    print("#####################################################")
    print("## Dimension of diff1_all : ")
    print(dim(diff1_all)  )
    print("#####################################################")
    diff1_name = c()
    for(i in c(1:nrow(diff1_all)) ) {
      diff1_name[i] = paste(nameOfRegion, i, sep="_"  )     
    }

    ############### for ChIPseeker
    diff1_ChIPseeker = cbind(diff1_all[,1:4],  diff1_name, rep(1, nrow(diff1_all)),   diff1_all[,5:7] )   
    colnames(diff1_ChIPseeker) = c("chr", "start",   "end",  "strand", "name",  "strength",  "pvalue",    "qvalue",  "meth.diff")
    diff1_ChIPseeker[,3] = diff1_ChIPseeker[,3] + 1
    diff1_ChIPseeker_hyper = diff1_ChIPseeker[ diff1_ChIPseeker$meth.diff > 0, ]
    diff1_ChIPseeker_hypo  = diff1_ChIPseeker[ diff1_ChIPseeker$meth.diff < 0, ]
    print("#####################################################")
    print("diff1_ChIPseeker:")
    print(dim(diff1_ChIPseeker)) 
    print(dim(diff1_ChIPseeker_hyper) )
    print(dim(diff1_ChIPseeker_hypo)) 
    print("#####################################################")

    ############### for BED
    diff1_BED = cbind(diff1_all[,1:3],  diff1_name )   
    colnames(diff1_BED) = c("chr", "start",   "end",  "name" )
    diff1_BED[,3] = diff1_BED[,3] + 1
    diff1_BED_hyper = diff1_BED[ diff1_ChIPseeker$meth.diff > 0, ]
    diff1_BED_hypo  = diff1_BED[ diff1_ChIPseeker$meth.diff < 0, ]
    print("#####################################################")
    print("diff1_BED:")
    print(dim(diff1_BED) )
    print(dim(diff1_BED_hyper) )
    print(dim(diff1_BED_hypo) )
    print("#####################################################")

    ############### for BED (not plus 1 for end site)
    diff1_BED_keptRaw = cbind(diff1_all[,1:3],  diff1_name )   
    colnames(diff1_BED_keptRaw) = c("chr", "start",   "end",  "name" )
    diff1_BED_keptRaw_hyper = diff1_BED_keptRaw[ diff1_ChIPseeker$meth.diff > 0, ]
    diff1_BED_keptRaw_hypo  = diff1_BED_keptRaw[ diff1_ChIPseeker$meth.diff < 0, ]
    print("#####################################################")
    print("diff1_BED_keptRaw:")
    print(dim(diff1_BED_keptRaw) ) 
    print(dim(diff1_BED_keptRaw_hyper))  
    print(dim(diff1_BED_keptRaw_hypo) ) 
    print("#####################################################")


    Part1_local = paste(myOutPath, "ChIPseeker/1-rawFiles",  sep="/" )
    if( ! file.exists(Part1_local) )  { dir.create(Part1_local, recursive = TRUE) }
    write.table(x=diff1_ChIPseeker,       file = paste(Part1_local, "diff1_all.txt", sep="/"),   append = FALSE, quote = FALSE, sep = "\t",  row.names = FALSE,  col.names = TRUE  )
    write.table(x=diff1_ChIPseeker_hyper, file = paste(Part1_local, "diff1_hyper.txt", sep="/"), append = FALSE, quote = FALSE, sep = "\t",  row.names = FALSE,  col.names = TRUE  )
    write.table(x=diff1_ChIPseeker_hypo,  file = paste(Part1_local, "diff1_hypo.txt", sep="/"),  append = FALSE, quote = FALSE, sep = "\t",  row.names = FALSE,  col.names = TRUE  )

    Part1_local = paste(myOutPath, "BED",  sep="/" )
    if( ! file.exists(Part1_local) )  { dir.create(Part1_local, recursive = TRUE) }
    write.table(x=diff1_BED,       file = paste(Part1_local, "diff1_all.bed", sep="/"),   append = FALSE, quote = FALSE, sep = "\t",  row.names = FALSE,  col.names = FALSE  )
    write.table(x=diff1_BED_hyper, file = paste(Part1_local, "diff1_hyper.bed", sep="/"), append = FALSE, quote = FALSE, sep = "\t",  row.names = FALSE,  col.names = FALSE  )
    write.table(x=diff1_BED_hypo,  file = paste(Part1_local, "diff1_hypo.bed", sep="/"),  append = FALSE, quote = FALSE, sep = "\t",  row.names = FALSE,  col.names = FALSE  )

    Part1_local =paste(myOutPath,  "BED_keptRaw",  sep="/" )
    if( ! file.exists(Part1_local) )  { dir.create(Part1_local, recursive = TRUE) }
    write.table(x=diff1_BED_keptRaw,       file = paste(Part1_local, "diff1_all.raw.bed", sep="/"),   append = FALSE, quote = FALSE, sep = "\t",  row.names = FALSE,  col.names = FALSE  )
    write.table(x=diff1_BED_keptRaw_hyper, file = paste(Part1_local, "diff1_hyper.raw.bed", sep="/"), append = FALSE, quote = FALSE, sep = "\t",  row.names = FALSE,  col.names = FALSE  )
    write.table(x=diff1_BED_keptRaw_hypo,  file = paste(Part1_local, "diff1_hypo.raw.bed", sep="/"),  append = FALSE, quote = FALSE, sep = "\t",  row.names = FALSE,  col.names = FALSE  )








    ## Select the DMCs.
    diff2_all = matrix1[( (myQvalue<0.05)  & (abs(myDiff)>10) ),]
    cat("\n\n\n\n\n")
    print("#####################################################")
    print("## Dimension of diff2_all : ")
    print(dim(diff2_all) )
    print("#####################################################")
    diff2_name = c()
    for(i in c(1:nrow(diff2_all)) ) {
      diff2_name[i] = paste(nameOfRegion, i, sep="_"  )     
    }

    ############### for ChIPseeker
    diff2_ChIPseeker = cbind(diff2_all[,1:4],  diff2_name, rep(1, nrow(diff2_all)),   diff2_all[,5:7] )   
    colnames(diff2_ChIPseeker) = c("chr", "start",   "end",  "strand", "name",  "strength",  "pvalue",    "qvalue",  "meth.diff")
    diff2_ChIPseeker[,3] = diff2_ChIPseeker[,3] + 1
    diff2_ChIPseeker_hyper = diff2_ChIPseeker[ diff2_ChIPseeker$meth.diff > 0, ]
    diff2_ChIPseeker_hypo  = diff2_ChIPseeker[ diff2_ChIPseeker$meth.diff < 0, ]
    print("#####################################################")
    print("diff2_ChIPseeker:")
    print(dim(diff2_ChIPseeker) )
    print(dim(diff2_ChIPseeker_hyper) )
    print(dim(diff2_ChIPseeker_hypo) )
    print("#####################################################")

    ############### for BED
    diff2_BED = cbind(diff2_all[,1:3],  diff2_name )   
    colnames(diff2_BED) = c("chr", "start",   "end",  "name" )
    diff2_BED[,3] = diff2_BED[,3] + 1
    diff2_BED_hyper = diff2_BED[ diff2_ChIPseeker$meth.diff > 0, ]
    diff2_BED_hypo  = diff2_BED[ diff2_ChIPseeker$meth.diff < 0, ]
    print("#####################################################")
    print("diff2_BED:")
    print(dim(diff2_BED) )
    print(dim(diff2_BED_hyper) )
    print(dim(diff2_BED_hypo) )
    print("#####################################################")

    ############### for BED (not plus 1 for end site)
    diff2_BED_keptRaw = cbind(diff2_all[,1:3],  diff2_name )   
    colnames(diff2_BED_keptRaw) = c("chr", "start",   "end",  "name" )
    diff2_BED_keptRaw_hyper = diff2_BED_keptRaw[ diff2_ChIPseeker$meth.diff > 0, ]
    diff2_BED_keptRaw_hypo  = diff2_BED_keptRaw[ diff2_ChIPseeker$meth.diff < 0, ]
    print("#####################################################")
    print("diff2_BED_keptRaw:")
    print(dim(diff2_BED_keptRaw) )
    print(dim(diff2_BED_keptRaw_hyper) )
    print(dim(diff2_BED_keptRaw_hypo) ) 
    print("#####################################################")


    Part1_local = paste(myOutPath, "ChIPseeker/1-rawFiles",  sep="/" )
    if( ! file.exists(Part1_local) )  { dir.create(Part1_local, recursive = TRUE) }
    write.table(x=diff2_ChIPseeker,       file = paste(Part1_local, "diff2_all.txt", sep="/"),   append = FALSE, quote = FALSE, sep = "\t",  row.names = FALSE,  col.names = TRUE  )
    write.table(x=diff2_ChIPseeker_hyper, file = paste(Part1_local, "diff2_hyper.txt", sep="/"), append = FALSE, quote = FALSE, sep = "\t",  row.names = FALSE,  col.names = TRUE  )
    write.table(x=diff2_ChIPseeker_hypo,  file = paste(Part1_local, "diff2_hypo.txt", sep="/"),  append = FALSE, quote = FALSE, sep = "\t",  row.names = FALSE,  col.names = TRUE  )

    Part1_local = paste(myOutPath, "BED",  sep="/" )
    if( ! file.exists(Part1_local) )  { dir.create(Part1_local, recursive = TRUE) }
    write.table(x=diff2_BED,       file = paste(Part1_local, "diff2_all.bed", sep="/"),   append = FALSE, quote = FALSE, sep = "\t",  row.names = FALSE,  col.names = FALSE  )
    write.table(x=diff2_BED_hyper, file = paste(Part1_local, "diff2_hyper.bed", sep="/"), append = FALSE, quote = FALSE, sep = "\t",  row.names = FALSE,  col.names = FALSE  )
    write.table(x=diff2_BED_hypo,  file = paste(Part1_local, "diff2_hypo.bed", sep="/"),  append = FALSE, quote = FALSE, sep = "\t",  row.names = FALSE,  col.names = FALSE  )

    Part1_local =paste(myOutPath,  "BED_keptRaw",  sep="/" )
    if( ! file.exists(Part1_local) )  { dir.create(Part1_local, recursive = TRUE) }
    write.table(x=diff2_BED_keptRaw,       file = paste(Part1_local, "diff2_all.raw.bed", sep="/"),   append = FALSE, quote = FALSE, sep = "\t",  row.names = FALSE,  col.names = FALSE  )
    write.table(x=diff2_BED_keptRaw_hyper, file = paste(Part1_local, "diff2_hyper.raw.bed", sep="/"), append = FALSE, quote = FALSE, sep = "\t",  row.names = FALSE,  col.names = FALSE  )
    write.table(x=diff2_BED_keptRaw_hypo,  file = paste(Part1_local, "diff2_hypo.raw.bed", sep="/"),  append = FALSE, quote = FALSE, sep = "\t",  row.names = FALSE,  col.names = FALSE  )

    sink() 

}










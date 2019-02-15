##############################################################################################################################################################################################
cat("##############################################\n")
samplesInformation_matrix_g = read.table(file=samplesInformation_g,   header = TRUE, sep = "\t" )
colorShape_matrix_g         = read.table(file=colorShape_g,           header = TRUE, sep = "\t" )

print( "samplesInformation_matrix_g:" )
print(  samplesInformation_matrix_g   )
cat("\n")
print( "colorShape_matrix_g:" )
print(  colorShape_matrix_g   )
cat("\n\n\n")

print("Type of variables:")
cat("\n")
print( "samplesInformation_matrix_g:" )
print( class(samplesInformation_matrix_g) )
print( mode(samplesInformation_matrix_g) )
print( typeof(samplesInformation_matrix_g) )
cat("\n")
print( "colorShape_matrix_g:" )
print( class(colorShape_matrix_g) )
print( mode(colorShape_matrix_g) )
print( typeof(colorShape_matrix_g) )
cat("\n\n\n")

print("Dimensions of variables:")
print( "dim(samplesInformation_matrix_g):" )
print(  dim(samplesInformation_matrix_g)   )
cat("\n")
print( "dim(colorShape_matrix_g):" )
print(  dim(colorShape_matrix_g)   )
cat("\n\n\n\n\n")
##############################################################################################################################################################################################





##############################################################################################################################################################################################
cat("##############################################\n")
cat("Convert dataframe to matrix......\n")
samplesInformation_matrix_g = as.matrix( samplesInformation_matrix_g )
colorShape_matrix_g         = as.matrix( colorShape_matrix_g )

print( "samplesInformation_matrix_g:" )
print(  samplesInformation_matrix_g   )
cat("\n")
print( "colorShape_matrix_g:" )
print(  colorShape_matrix_g   )
cat("\n\n\n")

print("Type of variables:")
cat("\n")
print( "samplesInformation_matrix_g:" )
print( class(samplesInformation_matrix_g) )
print( mode(samplesInformation_matrix_g) )
print( typeof(samplesInformation_matrix_g) )
cat("\n")
print( "colorShape_matrix_g:" )
print( class(colorShape_matrix_g) )
print( mode(colorShape_matrix_g) )
print( typeof(colorShape_matrix_g) )
cat("\n\n\n")

print("Dimensions of variables:")
print( "dim(samplesInformation_matrix_g):" )
print(  dim(samplesInformation_matrix_g)   )
cat("\n")
print( "dim(colorShape_matrix_g):" )
print(  dim(colorShape_matrix_g)   )
cat("\n\n\n\n\n")
##############################################################################################################################################################################################





##############################################################################################################################################################################################
cat("##############################################\n")
samplesInformation_colname_g = as.vector(colnames(samplesInformation_matrix_g))
colorShape_colname_g         = as.vector(colnames(colorShape_matrix_g))
print("samplesInformation_colname_g:")
print( samplesInformation_colname_g  )
cat("\n")
print("colorShape_colname_g:")
print( colorShape_colname_g  )
cat("\n\n\n")

if( identical( samplesInformation_colname_g, c("sample_name","sample_path","group","other_feature") ) ) {
    print("The heaer (first row) of samples_information_file is right!!!") 
}else{
    stop("Error: The heaer (first row) of samples_information_file is wrong!!!")
}

if( identical( colorShape_colname_g ,  c("standards","type_name","color_shape") ) ) {
    print("The heaer (first row) of color_shape_file is right!!!")
}else{
    stop("Error: The heaer (first row) of color_shape_file is wrong!!!")
}
cat("\n\n\n\n\n")
##############################################################################################################################################################################################





##############################################################################################################################################################################################
## For each group or feature.
cat("##############################################\n")
Label_one_1_g   = c()
Label_two_2_g   = c()
Label_three_3_g = c()
Label_four_4_g  = c()
Label_five_5_g  = c()
Label_six_6_g   = c()
Label_seven_7_g = c()
Label_eight_8_g = c()
Label_nine_9_g  = c()
Label_ten_10_g  = c()

Color_Label_one_1_g   = c()
Color_Label_two_2_g   = c()
Color_Label_three_3_g = c()
Color_Label_four_4_g  = c()
Color_Label_five_5_g  = c()
Color_Label_six_6_g   = c()
Color_Label_seven_7_g = c()
Color_Label_eight_8_g = c()
Color_Label_nine_9_g  = c()
Color_Label_ten_10_g  = c()

OtherFeature_one_1_g   = c()
OtherFeature_two_2_g   = c()
OtherFeature_three_3_g = c()
OtherFeature_four_4_g  = c()
OtherFeature_five_5_g  = c()
OtherFeature_six_6_g   = c()
OtherFeature_seven_7_g = c()
OtherFeature_eight_8_g = c()
OtherFeature_nine_9_g  = c()
OtherFeature_ten_10_g  = c()

Shape_OtherFeature_one_1_g   = c()
Shape_OtherFeature_two_2_g   = c()
Shape_OtherFeature_three_3_g = c()
Shape_OtherFeature_four_4_g  = c()
Shape_OtherFeature_five_5_g  = c()
Shape_OtherFeature_six_6_g   = c()
Shape_OtherFeature_seven_7_g = c()
Shape_OtherFeature_eight_8_g = c()
Shape_OtherFeature_nine_9_g  = c()
Shape_OtherFeature_ten_10_g  = c()

colorShape_matrix_g_part1_group = colorShape_matrix_g[colorShape_matrix_g[,1] == "group", ]
colorShape_matrix_g_part2_other = colorShape_matrix_g[colorShape_matrix_g[,1] == "other_feature", ]
colorShape_matrix_g_part2_other = as.matrix(  colorShape_matrix_g_part2_other  )
if( ncol(colorShape_matrix_g_part2_other)==1 ) {colorShape_matrix_g_part2_other = t(colorShape_matrix_g_part2_other) }

(nrow(colorShape_matrix_g_part1_group) >= 2)     ||   stop("The first column of color_shape_file is wrong! #1.")
(nrow(colorShape_matrix_g_part2_other) >= 1)     ||   stop("The first column of color_shape_file is wrong! #2.")
(nrow(colorShape_matrix_g) == nrow(colorShape_matrix_g_part1_group) + nrow(colorShape_matrix_g_part2_other))     ||   stop("The first column of color_shape_file is wrong! #3.")                    
(nrow(colorShape_matrix_g) >= 3 )                ||   stop("The first column of color_shape_file is wrong! #4.")                    
(nrow(colorShape_matrix_g_part1_group) <= 10)    ||   stop("The first column of color_shape_file is wrong! #5.")
(nrow(colorShape_matrix_g_part2_other) <= 10)    ||   stop("The first column of color_shape_file is wrong! #6.")
print( "colorShape_matrix_g_part1_group:" )
print( colorShape_matrix_g_part1_group )
cat("\n")
print( "colorShape_matrix_g_part2_other:" )
print( colorShape_matrix_g_part2_other )
cat("\n\n\n\n\n")
##############################################################################################################################################################################################





##############################################################################################################################################################################################
cat("##############################################\n")
for(i in c(1:nrow(colorShape_matrix_g_part1_group)) ) {
    if(i == 1)  {Label_one_1_g   = as.character( colorShape_matrix_g_part1_group[i,2] ); Color_Label_one_1_g   = as.character( colorShape_matrix_g_part1_group[i,3] ) }  
    if(i == 2)  {Label_two_2_g   = as.character( colorShape_matrix_g_part1_group[i,2] ); Color_Label_two_2_g   = as.character( colorShape_matrix_g_part1_group[i,3] ) }  
    if(i == 3)  {Label_three_3_g = as.character( colorShape_matrix_g_part1_group[i,2] ); Color_Label_three_3_g = as.character( colorShape_matrix_g_part1_group[i,3] ) }  
    if(i == 4)  {Label_four_4_g  = as.character( colorShape_matrix_g_part1_group[i,2] ); Color_Label_four_4_g  = as.character( colorShape_matrix_g_part1_group[i,3] ) }  
    if(i == 5)  {Label_five_5_g  = as.character( colorShape_matrix_g_part1_group[i,2] ); Color_Label_five_5_g  = as.character( colorShape_matrix_g_part1_group[i,3] ) }  
    if(i == 6)  {Label_six_6_g   = as.character( colorShape_matrix_g_part1_group[i,2] ); Color_Label_six_6_g   = as.character( colorShape_matrix_g_part1_group[i,3] ) }  
    if(i == 7)  {Label_seven_7_g = as.character( colorShape_matrix_g_part1_group[i,2] ); Color_Label_seven_7_g = as.character( colorShape_matrix_g_part1_group[i,3] ) }  
    if(i == 8)  {Label_eight_8_g = as.character( colorShape_matrix_g_part1_group[i,2] ); Color_Label_eight_8_g = as.character( colorShape_matrix_g_part1_group[i,3] ) }  
    if(i == 9)  {Label_nine_9_g  = as.character( colorShape_matrix_g_part1_group[i,2] ); Color_Label_nine_9_g  = as.character( colorShape_matrix_g_part1_group[i,3] ) }  
    if(i == 10) {Label_ten_10_g  = as.character( colorShape_matrix_g_part1_group[i,2] ); Color_Label_ten_10_g  = as.character( colorShape_matrix_g_part1_group[i,3] ) }  
}

for(i in c(1:nrow(colorShape_matrix_g_part2_other)) ) {
    if(i == 1)  {OtherFeature_one_1_g   = as.character( colorShape_matrix_g_part2_other[i,2] ); Shape_OtherFeature_one_1_g   = as.numeric( colorShape_matrix_g_part2_other[i,3] ) }  
    if(i == 2)  {OtherFeature_two_2_g   = as.character( colorShape_matrix_g_part2_other[i,2] ); Shape_OtherFeature_two_2_g   = as.numeric( colorShape_matrix_g_part2_other[i,3] ) }  
    if(i == 3)  {OtherFeature_three_3_g = as.character( colorShape_matrix_g_part2_other[i,2] ); Shape_OtherFeature_three_3_g = as.numeric( colorShape_matrix_g_part2_other[i,3] ) }  
    if(i == 4)  {OtherFeature_four_4_g  = as.character( colorShape_matrix_g_part2_other[i,2] ); Shape_OtherFeature_four_4_g  = as.numeric( colorShape_matrix_g_part2_other[i,3] ) }  
    if(i == 5)  {OtherFeature_five_5_g  = as.character( colorShape_matrix_g_part2_other[i,2] ); Shape_OtherFeature_five_5_g  = as.numeric( colorShape_matrix_g_part2_other[i,3] ) }  
    if(i == 6)  {OtherFeature_six_6_g   = as.character( colorShape_matrix_g_part2_other[i,2] ); Shape_OtherFeature_six_6_g   = as.numeric( colorShape_matrix_g_part2_other[i,3] ) }  
    if(i == 7)  {OtherFeature_seven_7_g = as.character( colorShape_matrix_g_part2_other[i,2] ); Shape_OtherFeature_seven_7_g = as.numeric( colorShape_matrix_g_part2_other[i,3] ) }  
    if(i == 8)  {OtherFeature_eight_8_g = as.character( colorShape_matrix_g_part2_other[i,2] ); Shape_OtherFeature_eight_8_g = as.numeric( colorShape_matrix_g_part2_other[i,3] ) }  
    if(i == 9)  {OtherFeature_nine_9_g  = as.character( colorShape_matrix_g_part2_other[i,2] ); Shape_OtherFeature_nine_9_g  = as.numeric( colorShape_matrix_g_part2_other[i,3] ) }  
    if(i == 10) {OtherFeature_ten_10_g  = as.character( colorShape_matrix_g_part2_other[i,2] ); Shape_OtherFeature_ten_10_g  = as.numeric( colorShape_matrix_g_part2_other[i,3] ) }  
}

print("The 2 features and their colors or shapes:")
cat("\n\nLabel:\n")
print( Label_one_1_g   )
print( Label_two_2_g   )
print( Label_three_3_g )
print( Label_four_4_g  )
print( Label_five_5_g  )
print( Label_six_6_g   )
print( Label_seven_7_g )
print( Label_eight_8_g )
print( Label_nine_9_g  )
print( Label_ten_10_g  )
cat("\n\nColor:\n")
print( Color_Label_one_1_g   )
print( Color_Label_two_2_g   )
print( Color_Label_three_3_g )
print( Color_Label_four_4_g  )
print( Color_Label_five_5_g  )
print( Color_Label_six_6_g   )
print( Color_Label_seven_7_g )
print( Color_Label_eight_8_g )
print( Color_Label_nine_9_g  )
print( Color_Label_ten_10_g  )
cat("\n\nOtherFeature:\n")
print( OtherFeature_one_1_g   )
print( OtherFeature_two_2_g   )
print( OtherFeature_three_3_g )
print( OtherFeature_four_4_g  )
print( OtherFeature_five_5_g  )
print( OtherFeature_six_6_g   )
print( OtherFeature_seven_7_g )
print( OtherFeature_eight_8_g )
print( OtherFeature_nine_9_g  )
print( OtherFeature_ten_10_g  )
cat("\n\nShape_OtherFeature:\n")
print( Shape_OtherFeature_one_1_g   )
print( Shape_OtherFeature_two_2_g   )
print( Shape_OtherFeature_three_3_g )
print( Shape_OtherFeature_four_4_g  )
print( Shape_OtherFeature_five_5_g  )
print( Shape_OtherFeature_six_6_g   )
print( Shape_OtherFeature_seven_7_g )
print( Shape_OtherFeature_eight_8_g )
print( Shape_OtherFeature_nine_9_g  )
print( Shape_OtherFeature_ten_10_g  )
cat("\n\n\n\n\n\n\n")
##############################################################################################################################################################################################





##############################################################################################################################################################################################
## For each sample of each group.
cat("##############################################\n")
Files_one_1_g   = c()
Files_two_2_g   = c()
Files_three_3_g = c()
Files_four_4_g  = c()
Files_five_5_g  = c()
Files_six_6_g   = c()
Files_seven_7_g = c()
Files_eight_8_g = c()
Files_nine_9_g  = c()
Files_ten_10_g  = c()
  
ShortName_one_1_g   = c()  ## Short name of files, their pathes are removed.
ShortName_two_2_g   = c()
ShortName_three_3_g = c()
ShortName_four_4_g  = c()
ShortName_five_5_g  = c()
ShortName_six_6_g   = c()
ShortName_seven_7_g = c()
ShortName_eight_8_g = c()
ShortName_nine_9_g  = c()
ShortName_ten_10_g  = c()

Sex_one_1_g   = c() 
Sex_two_2_g   = c()
Sex_three_3_g = c() 
Sex_four_4_g  = c()
Sex_five_5_g  = c()
Sex_six_6_g   = c()
Sex_seven_7_g = c()
Sex_eight_8_g = c() 
Sex_nine_9_g  = c()
Sex_ten_10_g  = c()

Type_one_1_g   = c() 
Type_two_2_g   = c()  
Type_three_3_g = c() 
Type_four_4_g  = c() 
Type_five_5_g  = c() 
Type_six_6_g   = c() 
Type_seven_7_g = c() 
Type_eight_8_g = c() 
Type_nine_9_g  = c() 
Type_ten_10_g  = c() 

Treatment_one_1_g   = c() 
Treatment_two_2_g   = c() 
Treatment_three_3_g = c()  
Treatment_four_4_g  = c() 
Treatment_five_5_g  = c() 
Treatment_six_6_g   = c() 
Treatment_seven_7_g = c() 
Treatment_eight_8_g = c() 
Treatment_nine_9_g  = c() 
Treatment_ten_10_g  = c() 

SampleID_one_1_g   = Type_one_1_g 
SampleID_two_2_g   = Type_two_2_g
SampleID_three_3_g = Type_three_3_g 
SampleID_four_4_g  = Type_four_4_g
SampleID_five_5_g  = Type_five_5_g
SampleID_six_6_g   = Type_six_6_g 
SampleID_seven_7_g = Type_seven_7_g
SampleID_eight_8_g = Type_eight_8_g 
SampleID_nine_9_g  = Type_nine_9_g
SampleID_ten_10_g  = Type_ten_10_g
 
for(i in c(1:nrow(samplesInformation_matrix_g)) ) {
    myTempVec1 = as.vector( unlist(samplesInformation_matrix_g[i,] ) )  
    if( (! is.null(Label_one_1_g)) && ( myTempVec1[3] == Label_one_1_g ) )  {
            ShortName_one_1_g[length(ShortName_one_1_g)+1] = myTempVec1[1]
            myTempVec1[2] = gsub(pattern="/$", replacement="", x=myTempVec1[2], ignore.case = FALSE, perl = TRUE )   
            Files_one_1_g[length(Files_one_1_g)+1] = paste(myTempVec1[2], myTempVec1[1], sep="/")
            Type_one_1_g[length(Type_one_1_g)+1] = myTempVec1[3]  
            Sex_one_1_g[length(Sex_one_1_g)+1] = myTempVec1[4]  
            Treatment_one_1_g[length(Treatment_one_1_g)+1] = 1
            SampleID_one_1_g[length(SampleID_one_1_g)+1] = paste(myTempVec1[3], "_",    length(SampleID_one_1_g)+1, sep="") 
    }
    if( (! is.null(Label_two_2_g)) && ( myTempVec1[3] == Label_two_2_g ) )  {
            ShortName_two_2_g[length(ShortName_two_2_g)+1] = myTempVec1[1]
            myTempVec1[2] = gsub(pattern="/$", replacement="", x=myTempVec1[2], ignore.case = FALSE, perl = TRUE )   
            Files_two_2_g[length(Files_two_2_g)+1] = paste(myTempVec1[2], myTempVec1[1], sep="/")
            Type_two_2_g[length(Type_two_2_g)+1] = myTempVec1[3]  
            Sex_two_2_g[length(Sex_two_2_g)+1] = myTempVec1[4]  
            Treatment_two_2_g[length(Treatment_two_2_g)+1] = 2
            SampleID_two_2_g[length(SampleID_two_2_g)+1] = paste(myTempVec1[3], "_",    length(SampleID_two_2_g)+1, sep="") 
    }
    if( (! is.null(Label_three_3_g)) && ( myTempVec1[3] == Label_three_3_g ) )  {
            ShortName_three_3_g[length(ShortName_three_3_g)+1] = myTempVec1[1]
            myTempVec1[2] = gsub(pattern="/$", replacement="", x=myTempVec1[2], ignore.case = FALSE, perl = TRUE )   
            Files_three_3_g[length(Files_three_3_g)+1] = paste(myTempVec1[2], myTempVec1[1], sep="/")
            Type_three_3_g[length(Type_three_3_g)+1] = myTempVec1[3]  
            Sex_three_3_g[length(Sex_three_3_g)+1] = myTempVec1[4]  
            Treatment_three_3_g[length(Treatment_three_3_g)+1] = 3
            SampleID_three_3_g[length(SampleID_three_3_g)+1] = paste(myTempVec1[3], "_",    length(SampleID_three_3_g)+1, sep="") 
    }
    if( (! is.null(Label_four_4_g)) && ( myTempVec1[3] == Label_four_4_g ) )  {
            ShortName_four_4_g[length(ShortName_four_4_g)+1] = myTempVec1[1]
            myTempVec1[2] = gsub(pattern="/$", replacement="", x=myTempVec1[2], ignore.case = FALSE, perl = TRUE )   
            Files_four_4_g[length(Files_four_4_g)+1] = paste(myTempVec1[2], myTempVec1[1], sep="/")
            Type_four_4_g[length(Type_four_4_g)+1] = myTempVec1[3]  
            Sex_four_4_g[length(Sex_four_4_g)+1] = myTempVec1[4]  
            Treatment_four_4_g[length(Treatment_four_4_g)+1] = 4
            SampleID_four_4_g[length(SampleID_four_4_g)+1] = paste(myTempVec1[3], "_",    length(SampleID_four_4_g)+1, sep="") 
    }
    if( (! is.null(Label_five_5_g)) && ( myTempVec1[3] == Label_five_5_g ) )  {
            ShortName_five_5_g[length(ShortName_five_5_g)+1] = myTempVec1[1]
            myTempVec1[2] = gsub(pattern="/$", replacement="", x=myTempVec1[2], ignore.case = FALSE, perl = TRUE )   
            Files_five_5_g[length(Files_five_5_g)+1] = paste(myTempVec1[2], myTempVec1[1], sep="/")
            Type_five_5_g[length(Type_five_5_g)+1] = myTempVec1[3]  
            Sex_five_5_g[length(Sex_five_5_g)+1] = myTempVec1[4]  
            Treatment_five_5_g[length(Treatment_five_5_g)+1] = 5
            SampleID_five_5_g[length(SampleID_five_5_g)+1] = paste(myTempVec1[3], "_",    length(SampleID_five_5_g)+1, sep="") 
    }
    if( (! is.null(Label_six_6_g)) && ( myTempVec1[3] == Label_six_6_g ) )  {
            ShortName_six_6_g[length(ShortName_six_6_g)+1] = myTempVec1[1]
            myTempVec1[2] = gsub(pattern="/$", replacement="", x=myTempVec1[2], ignore.case = FALSE, perl = TRUE )   
            Files_six_6_g[length(Files_six_6_g)+1] = paste(myTempVec1[2], myTempVec1[1], sep="/")
            Type_six_6_g[length(Type_six_6_g)+1] = myTempVec1[3]  
            Sex_six_6_g[length(Sex_six_6_g)+1] = myTempVec1[4]  
            Treatment_six_6_g[length(Treatment_six_6_g)+1] = 6
            SampleID_six_6_g[length(SampleID_six_6_g)+1] = paste(myTempVec1[3], "_",    length(SampleID_six_6_g)+1, sep="") 
    }
    if( (! is.null(Label_seven_7_g)) && ( myTempVec1[3] == Label_seven_7_g ) )  {
            ShortName_seven_7_g[length(ShortName_seven_7_g)+1] = myTempVec1[1]
            myTempVec1[2] = gsub(pattern="/$", replacement="", x=myTempVec1[2], ignore.case = FALSE, perl = TRUE )   
            Files_seven_7_g[length(Files_seven_7_g)+1] = paste(myTempVec1[2], myTempVec1[1], sep="/")
            Type_seven_7_g[length(Type_seven_7_g)+1] = myTempVec1[3]  
            Sex_seven_7_g[length(Sex_seven_7_g)+1] = myTempVec1[4]  
            Treatment_seven_7_g[length(Treatment_seven_7_g)+1] = 7
            SampleID_seven_7_g[length(SampleID_seven_7_g)+1] = paste(myTempVec1[3], "_",    length(SampleID_seven_7_g)+1, sep="") 
    }
    if( (! is.null(Label_eight_8_g)) && ( myTempVec1[3] == Label_eight_8_g ) )  {
            ShortName_eight_8_g[length(ShortName_eight_8_g)+1] = myTempVec1[1]
            myTempVec1[2] = gsub(pattern="/$", replacement="", x=myTempVec1[2], ignore.case = FALSE, perl = TRUE )   
            Files_eight_8_g[length(Files_eight_8_g)+1] = paste(myTempVec1[2], myTempVec1[1], sep="/")
            Type_eight_8_g[length(Type_eight_8_g)+1] = myTempVec1[3]  
            Sex_eight_8_g[length(Sex_eight_8_g)+1] = myTempVec1[4]  
            Treatment_eight_8_g[length(Treatment_eight_8_g)+1] = 8
            SampleID_eight_8_g[length(SampleID_eight_8_g)+1] = paste(myTempVec1[3], "_",    length(SampleID_eight_8_g)+1, sep="") 
    }
    if( (! is.null(Label_nine_9_g)) && ( myTempVec1[3] == Label_nine_9_g ) )  {
            ShortName_nine_9_g[length(ShortName_nine_9_g)+1] = myTempVec1[1]
            myTempVec1[2] = gsub(pattern="/$", replacement="", x=myTempVec1[2], ignore.case = FALSE, perl = TRUE )   
            Files_nine_9_g[length(Files_nine_9_g)+1] = paste(myTempVec1[2], myTempVec1[1], sep="/")
            Type_nine_9_g[length(Type_nine_9_g)+1] = myTempVec1[3]  
            Sex_nine_9_g[length(Sex_nine_9_g)+1] = myTempVec1[4]  
            Treatment_nine_9_g[length(Treatment_nine_9_g)+1] = 9
            SampleID_nine_9_g[length(SampleID_nine_9_g)+1] = paste(myTempVec1[3], "_",    length(SampleID_nine_9_g)+1, sep="") 
    }
    if( (! is.null(Label_ten_10_g)) && ( myTempVec1[3] == Label_ten_10_g ) ) {
            ShortName_ten_10_g[length(ShortName_ten_10_g)+1] = myTempVec1[1]
            myTempVec1[2] = gsub(pattern="/$", replacement="", x=myTempVec1[2], ignore.case = FALSE, perl = TRUE )   
            Files_ten_10_g[length(Files_ten_10_g)+1] = paste(myTempVec1[2], myTempVec1[1], sep="/")
            Type_ten_10_g[length(Type_ten_10_g)+1] = myTempVec1[3]  
            Sex_ten_10_g[length(Sex_ten_10_g)+1] = myTempVec1[4]  
            Treatment_ten_10_g[length(Treatment_ten_10_g)+1] = 10
            SampleID_ten_10_g[length(SampleID_ten_10_g)+1] = paste(myTempVec1[3], "_",    length(SampleID_ten_10_g)+1, sep="") 
    }
}

ShortName_All_vector_g <- c(
  ShortName_one_1_g,
  ShortName_two_2_g,
  ShortName_three_3_g,
  ShortName_four_4_g,
  ShortName_five_5_g,
  ShortName_six_6_g,
  ShortName_seven_7_g,
  ShortName_eight_8_g,
  ShortName_nine_9_g,
  ShortName_ten_10_g
)
ShortName_All_list_g <- as.list( ShortName_All_vector_g )

Files_All_vector_g <- c(
  Files_one_1_g,
  Files_two_2_g,
  Files_three_3_g,
  Files_four_4_g,
  Files_five_5_g,
  Files_six_6_g,
  Files_seven_7_g,
  Files_eight_8_g,
  Files_nine_9_g,
  Files_ten_10_g
)
Files_All_list_g <- as.list( Files_All_vector_g )

Sex_All_vector_g <- c(
  Sex_one_1_g,
  Sex_two_2_g,
  Sex_three_3_g,
  Sex_four_4_g,
  Sex_five_5_g,
  Sex_six_6_g,
  Sex_seven_7_g,
  Sex_eight_8_g,
  Sex_nine_9_g,
  Sex_ten_10_g
)
Sex_All_list_g <- as.list( Sex_All_vector_g )

Type_All_vector_g <- c(
  Type_one_1_g,
  Type_two_2_g,
  Type_three_3_g,
  Type_four_4_g,
  Type_five_5_g,
  Type_six_6_g,
  Type_seven_7_g,
  Type_eight_8_g,
  Type_nine_9_g,
  Type_ten_10_g
)
Type_All_list_g <- as.list( Type_All_vector_g )

Treatment_All_vector_g <- c(
  Treatment_one_1_g,
  Treatment_two_2_g,
  Treatment_three_3_g,
  Treatment_four_4_g,
  Treatment_five_5_g,
  Treatment_six_6_g,
  Treatment_seven_7_g,
  Treatment_eight_8_g,
  Treatment_nine_9_g,
  Treatment_ten_10_g
)
Treatment_All_list_g <- as.list( Treatment_All_vector_g )

SampleID_All_vector_g <- c(
  SampleID_one_1_g,
  SampleID_two_2_g,
  SampleID_three_3_g,
  SampleID_four_4_g,
  SampleID_five_5_g,
  SampleID_six_6_g,
  SampleID_seven_7_g,
  SampleID_eight_8_g,
  SampleID_nine_9_g,
  SampleID_ten_10_g
)
SampleID_All_list_g <- as.list( SampleID_All_vector_g )

cat("Length of variables: \n")
print( length( ShortName_All_vector_g ) )
print( length( ShortName_All_list_g ) )
print( length( Files_All_vector_g ) )
print( length( Files_All_list_g ) )
print( length( Sex_All_vector_g ) )
print( length( Sex_All_list_g ) )
print( length( Type_All_vector_g ) )
print( length( Type_All_list_g ) )
print( length( Treatment_All_vector_g ) )
print( length( Treatment_All_list_g ) )
print( length( SampleID_All_vector_g ) )
print( length( SampleID_All_list_g ) )
cat("\n\n\n\n\n\n")
##############################################################################################################################################################################################





##############################################################################################################################################################################################
cat("##############################################\n")
allSamples_info_g = cbind(ShortName_All_vector_g, Files_All_vector_g,  Sex_All_vector_g,  
                          Type_All_vector_g,  Treatment_All_vector_g,  SampleID_All_vector_g)

write.table(allSamples_info_g , 
            file = paste(Part2_g,   "1_allSamples-info.txt",  sep="/"), 
            append =FALSE, quote = FALSE, sep = "\t", eol = "\n", na = "NA", dec = ".", 
            row.names = FALSE,  col.names = TRUE, qmethod = c("escape", "double"),  fileEncoding = "")

mySex_All_shape_g  = c( Shape_OtherFeature_one_1_g,  Shape_OtherFeature_two_2_g, Shape_OtherFeature_three_3_g, Shape_OtherFeature_four_4_g,
                        Shape_OtherFeature_five_5_g, Shape_OtherFeature_six_6_g, Shape_OtherFeature_seven_7_g, Shape_OtherFeature_eight_8_g,
                        Shape_OtherFeature_nine_9_g, Shape_OtherFeature_ten_10_g  ) 
myTech_All_color_g = c( Color_Label_one_1_g,  Color_Label_two_2_g, Color_Label_three_3_g, Color_Label_four_4_g,
                        Color_Label_five_5_g, Color_Label_six_6_g, Color_Label_seven_7_g, Color_Label_eight_8_g,
                        Color_Label_nine_9_g, Color_Label_ten_10_g )
print("length of two vectors")
cat("\n")
print("mySex_All_shape_g:")
print( length(mySex_All_shape_g) )
cat("\n")
print("myTech_All_color_g:")
print( length(myTech_All_color_g) )
cat("\n\n\n") 

sink( file = paste(Part2_g,  "2_twoVectors.txt",  sep="/") )
    print("mySex_All_shape_g:")
    print( mySex_All_shape_g )
    cat("\n\n\n") 
    print("myTech_All_color_g:")
    print( myTech_All_color_g )
    cat("\n\n\n") 
sink() 
 
MySex_Shape_g <- function(  mySex_vector   ) {
  mySex_shape2  = mySex_vector
  for(i in c(1:length(mySex_shape2)) ) {
    if( (! is.null(OtherFeature_one_1_g))   && (mySex_shape2[i] == OtherFeature_one_1_g)    ) { mySex_shape2[i] =  Shape_OtherFeature_one_1_g    }
    if( (! is.null(OtherFeature_two_2_g))   && (mySex_shape2[i] == OtherFeature_two_2_g)    ) { mySex_shape2[i] =  Shape_OtherFeature_two_2_g    }
    if( (! is.null(OtherFeature_three_3_g)) && (mySex_shape2[i] == OtherFeature_three_3_g)  ) { mySex_shape2[i] =  Shape_OtherFeature_three_3_g  }
    if( (! is.null(OtherFeature_four_4_g))  && (mySex_shape2[i] == OtherFeature_four_4_g)   ) { mySex_shape2[i] =  Shape_OtherFeature_four_4_g   }
    if( (! is.null(OtherFeature_five_5_g))  && (mySex_shape2[i] == OtherFeature_five_5_g)   ) { mySex_shape2[i] =  Shape_OtherFeature_five_5_g   }
    if( (! is.null(OtherFeature_six_6_g))   && (mySex_shape2[i] == OtherFeature_six_6_g)    ) { mySex_shape2[i] =  Shape_OtherFeature_six_6_g    }
    if( (! is.null(OtherFeature_seven_7_g)) && (mySex_shape2[i] == OtherFeature_seven_7_g)  ) { mySex_shape2[i] =  Shape_OtherFeature_seven_7_g  }
    if( (! is.null(OtherFeature_eight_8_g)) && (mySex_shape2[i] == OtherFeature_eight_8_g)  ) { mySex_shape2[i] =  Shape_OtherFeature_eight_8_g  }
    if( (! is.null(OtherFeature_nine_9_g))  && (mySex_shape2[i] == OtherFeature_nine_9_g)   ) { mySex_shape2[i] =  Shape_OtherFeature_nine_9_g   }
    if( (! is.null(OtherFeature_ten_10_g))  && (mySex_shape2[i] == OtherFeature_ten_10_g)   ) { mySex_shape2[i] =  Shape_OtherFeature_ten_10_g   }
  }
  mySex_shape2 = as.numeric(mySex_shape2)
  names(mySex_shape2) = mySex_vector
  return(mySex_shape2)
}
 
MyTech_color_g <- function(  myTech_vector  ) {
  myTech_color  = myTech_vector
  for(i in c(1:length(myTech_color)) ) {
    if( (! is.null(Label_one_1_g))   && (myTech_color[i] == Label_one_1_g)   ) { myTech_color[i] = Color_Label_one_1_g   }
    if( (! is.null(Label_two_2_g))   && (myTech_color[i] == Label_two_2_g)   ) { myTech_color[i] = Color_Label_two_2_g   }
    if( (! is.null(Label_three_3_g)) && (myTech_color[i] == Label_three_3_g) ) { myTech_color[i] = Color_Label_three_3_g }
    if( (! is.null(Label_four_4_g))  && (myTech_color[i] == Label_four_4_g)  ) { myTech_color[i] = Color_Label_four_4_g  }
    if( (! is.null(Label_five_5_g))  && (myTech_color[i] == Label_five_5_g)  ) { myTech_color[i] = Color_Label_five_5_g  }
    if( (! is.null(Label_six_6_g))   && (myTech_color[i] == Label_six_6_g)   ) { myTech_color[i] = Color_Label_six_6_g   }
    if( (! is.null(Label_seven_7_g)) && (myTech_color[i] == Label_seven_7_g) ) { myTech_color[i] = Color_Label_seven_7_g }
    if( (! is.null(Label_eight_8_g)) && (myTech_color[i] == Label_eight_8_g) ) { myTech_color[i] = Color_Label_eight_8_g }
    if( (! is.null(Label_nine_9_g))  && (myTech_color[i] == Label_nine_9_g)  ) { myTech_color[i] = Color_Label_nine_9_g  }
    if( (! is.null(Label_ten_10_g))  && (myTech_color[i] == Label_ten_10_g)  ) { myTech_color[i] = Color_Label_ten_10_g  }
  }
  names(myTech_color) = myTech_vector
  return(myTech_color)
}

MyTech_number_g <- function(  myTech_vector  ) {
  myTech_color  = myTech_vector
  for(i in c(1:length(myTech_color)) ) {
    if( (! is.null(Label_one_1_g))   && (myTech_color[i] == Label_one_1_g)   ) { myTech_color[i] = 1 }
    if( (! is.null(Label_two_2_g))   && (myTech_color[i] == Label_two_2_g)   ) { myTech_color[i] = 2 }
    if( (! is.null(Label_three_3_g)) && (myTech_color[i] == Label_three_3_g) ) { myTech_color[i] = 3 }
    if( (! is.null(Label_four_4_g))  && (myTech_color[i] == Label_four_4_g)  ) { myTech_color[i] = 4 }
    if( (! is.null(Label_five_5_g))  && (myTech_color[i] == Label_five_5_g)  ) { myTech_color[i] = 5 }
    if( (! is.null(Label_six_6_g))   && (myTech_color[i] == Label_six_6_g)   ) { myTech_color[i] = 6 }
    if( (! is.null(Label_seven_7_g)) && (myTech_color[i] == Label_seven_7_g) ) { myTech_color[i] = 7 }
    if( (! is.null(Label_eight_8_g)) && (myTech_color[i] == Label_eight_8_g) ) { myTech_color[i] = 8 }
    if( (! is.null(Label_nine_9_g))  && (myTech_color[i] == Label_nine_9_g)  ) { myTech_color[i] = 9 }
    if( (! is.null(Label_ten_10_g))  && (myTech_color[i] == Label_ten_10_g)  ) { myTech_color[i] = 10}
  }
  return(myTech_color)
}

sink( paste(Part2_g, "/3_All-Files.txt",  sep="")  )
    cat("\n\n Files_one_1_g:\n")
    print( Files_one_1_g  )
    cat("\n\n Files_two_2_g:\n")
    print( Files_two_2_g  )
    cat("\n\n Files_three_3_g:\n")
    print( Files_three_3_g  )
    cat("\n\n Files_four_4_g:\n")
    print( Files_four_4_g  )
    cat("\n\n Files_five_5_g:\n")
    print( Files_five_5_g  )
    cat("\n\n Files_six_6_g:\n")
    print( Files_six_6_g  )
    cat("\n\n Files_seven_7_g:\n")
    print( Files_seven_7_g  )
    cat("\n\n Files_eight_8_g:\n")
    print( Files_eight_8_g  )
    cat("\n\n Files_nine_9_g:\n")
    print( Files_nine_9_g  )
    cat("\n\n Files_ten_10_g:\n")
    print( Files_ten_10_g  )
sink()  

sink( paste(Part2_g, "/4_All-Files-shortName.txt",  sep="")  )
    cat("\n\n ShortName_one_1_g:\n")
    print( ShortName_one_1_g  )
    cat("\n\n ShortName_two_2_g:\n")
    print( ShortName_two_2_g  )
    cat("\n\n ShortName_three_3_g:\n")
    print( ShortName_three_3_g  )
    cat("\n\n ShortName_four_4_g:\n")
    print( ShortName_four_4_g  )
    cat("\n\n ShortName_five_5_g:\n")
    print( ShortName_five_5_g  )
    cat("\n\n ShortName_six_6_g:\n")
    print( ShortName_six_6_g  )
    cat("\n\n ShortName_seven_7_g:\n")
    print( ShortName_seven_7_g  )
    cat("\n\n ShortName_eight_8_g:\n")
    print( ShortName_eight_8_g  )
    cat("\n\n ShortName_nine_9_g:\n")
    print( ShortName_nine_9_g  )
    cat("\n\n ShortName_ten_10_g:\n")
    print( ShortName_ten_10_g  )
sink()  

sink( paste(Part2_g, "/5_Number-of-files-of-each-category.txt",  sep="")  )
    print( length( Files_one_1_g ) )
    print( length( Files_two_2_g  ) )
    print( length( Files_three_3_g ) )
    print( length( Files_four_4_g  ) )
    print( length( Files_five_5_g  ) )
    print( length( Files_six_6_g ) )
    print( length( Files_seven_7_g  ) )
    print( length( Files_eight_8_g ) )
    print( length( Files_nine_9_g  ) )
    print( length( Files_ten_10_g  ) )
    cat("\n\n\n\n\n")  
    print( length( ShortName_one_1_g ) )
    print( length( ShortName_two_2_g  ) )
    print( length( ShortName_three_3_g ) )
    print( length( ShortName_four_4_g  ) )
    print( length( ShortName_five_5_g  ) )
    print( length( ShortName_six_6_g ) )
    print( length( ShortName_seven_7_g  ) )
    print( length( ShortName_eight_8_g ) )
    print( length( ShortName_nine_9_g  ) )
    print( length( ShortName_ten_10_g  ) )
sink() 
##############################################################################################################################################################################################




 



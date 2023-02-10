# =========================================================
# R script to generate core genome rarefaction curves.
# Script by Leonardos Mageiros, February 2014
# =========================================================
# The input format is shown in sample_input.csv included
# in this archive. 1=gene present/0=gene absent; 
# 1 column= 1 genome.
# =========================================================
# Requests should be made to s.k.sheppard@swansea.ac.uk 
# Visit our lab website: http://www.sheppardlab.com/
# =========================================================
# Associated publication: Meric et al. (2014) A reference 
# pan-genome approach to comparative bacterial genomics: 
# identification of novel epidemiological markers in 
# pathogenic Campylobacter
# =========================================================


#Define working directory
setwd("C:/work/development/R");

#read the input file. 
#The input file must have no header row and column.
#rows corespond to genes, columns corespond to isolates
data <- read.table("sample_input.csv", sep="\t");
transposed_data <- t(data);#we transpose the table to run a for loop row-wise
nr_rows <- nrow(transposed_data);
nr_cols <- ncol(transposed_data);
nr_iterations <- 100; #the number of iterations 

#the matrix which will hold the results
core_results <- matrix(data=NA,nrow=nr_rows,ncol=nr_iterations);
#a vector the holds the updated results of each iteration
presence_line <- matrix(data=0,nrow=1,ncol=nr_cols);

# the times that we will run our calculation
for(times in 1:nr_iterations){

  #for all the rows of the table
  for (i in 1: nr_rows){
    sum <- 0;
   if(i==1){ #for the first row
     for(j in 1: nr_cols){#calculate the number of genes
       presence_line[1,j]<-transposed_data[i,j]
       sum <- sum + transposed_data[i,j]; 
     }
     #and store the first result
     core_results[i,times] <- sum
     next;
   }
    
   #for every consecutive row, for every gene  
   for(j in 1: nr_cols){
    #if the gene was and is present count it
    if(presence_line[1,j] && transposed_data[i,j]){
      presence_line[1,j] <- 1;
    }# else dont
    else{presence_line[1,j] <- 0;}
   }
    #count the total number of present genes
   for(j in 1: nr_cols){
     sum <- sum + presence_line[1,j]; 
   }
    #store the result
   core_results[i,times] <- sum;
   
  }
  #suffle the matrix and repeat the procedure
  transposed_data <- transposed_data[sample(nrow(transposed_data),nrow(transposed_data)), ]
}

#print the results in a tab delemeter file
write.table(core_results, file = "core_genome.txt", sep="\t");



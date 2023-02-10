# Built for use with the tANI distance and associated methodology
# Hence matrices are not symmetrical and need to be averaged 


# If you are not using the tANI script you will need to change these inputs. 
# Make sure your bootstrapped matrices have a unique file extension i.e. .matrix
ANI_orig_file <- "Distance_.orig"
bootstrap_suffix <- "*.matrix"

library(ape)
library(phangorn)
library(MASS)
library(Matrix)
library(reshape2)
library(OpenMx)
library(stringr)

# Read in table with DDH info
ANI_orig <- read.table(file = ANI_orig_file,sep="\t",header=TRUE,stringsAsFactors=FALSE)

#Grab diag for later overwrite
ANI_orig_diag <- diag2vec(ANI_orig)	#Pull out the diagonal of the ANI matrix

# Transpose matrix to flip triangles
t_ANI_orig <- t(ANI_orig)

# Average the two triangle
Comb_ANI_orig <- (ANI_orig + t_ANI_orig) / 2

# Reassert the diagonal
diag(Comb_ANI_orig) <- ANI_orig_diag

#write fastme.bal tree for the distance, write to file, and draw in the plotter
tree_orig <- fastme.bal(as.matrix(Comb_ANI_orig), nni = TRUE, spr = TRUE, tbr = TRUE)
write.tree(tree_orig,file="BestTree.tre")
plot(tree_orig)

# Read in the bootstrap trees
file_list <- list.files(pattern = bootstrap_suffix)
BS_set <- list()
for(a in 1:length(file_list)){
  BS_set[[a]] <- as.matrix(read.table(file = file_list[a], sep = "\t", row.names = 1, header = TRUE, stringsAsFactors = FALSE))
}

for(b in 1:length(BS_set)){
  
  BS_current <- BS_set[[b]]
  #Grab diag for later overwrite
  ANI_BS_diag <- diag2vec(BS_current)	#Pull out the diagonal of the ANI matrix
  
  # Transpose matrix to flip triangles
  t_ANI_BS <- t(BS_current)
  
  # Average the two triangle
  Comb_ANI_BS <- (BS_current + t_ANI_BS) / 2
  
  # Reassert the diagonal
  diag(Comb_ANI_BS) <- ANI_BS_diag
  
  bs_tree <- fastme.bal(Comb_ANI_BS, nni = TRUE, spr = TRUE, tbr = TRUE)
  write.tree(bs_tree,file="BootstrapTrees.tre",append=TRUE)
}

#Apply Bootstraps to best tree

#bring in the bootsrapped trees from the file
bs_trees <- read.tree(file="BootstrapTrees.tre")

#bring in the bootsrapped trees from the file
tree_orig <- read.tree(file="BestTree.tre")	

#map the trees as splits with frequencies of splits
clade_map <- as.splits(bs_trees)

#Plots the bootstrap splits onto best tree
write.tree(plotBS(tree_orig,bs_trees,p=1),file="BestTree_wSupport.tre")



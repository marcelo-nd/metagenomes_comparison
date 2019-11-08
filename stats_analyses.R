install.packages('devtools')

library(devtools)

install_github("pmartinezarbizu/pairwiseAdonis/pairwiseAdonis")

library(pairwiseAdonis)

#read data

genes_cog <- read.csv("C:/Users/marce/OneDrive/datos_rev/cog_counts_per_family.csv")
  
genes_ko <- read.csv("C:/Users/marce/OneDrive/datos_rev/genes_counts_per_family.csv")
  
genes_mods <- read.csv("C:/Users/marce/OneDrive/datos_rev/mods_counts_per_family.csv")
  
genes_paths <- read.csv("C:/Users/marce/OneDrive/datos_rev/paths_counts_per_family.csv")
  
genes_metab <- read.csv("C:/Users/marce/OneDrive/datos_rev/metab_counts_per_family.csv")


pairwise.adonis(genes_cog[,3:22],genes_cog$family_origin)

pairwise.adonis(genes_ko[,3:22],genes_ko$family_origin)

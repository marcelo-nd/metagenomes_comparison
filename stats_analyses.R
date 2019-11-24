install.packages('devtools')

library(devtools)

install_github("pmartinezarbizu/pairwiseAdonis/pairwiseAdonis")

library(pairwiseAdonis)



# Lista de genes por familia para permanova

genes_ko <- read.csv("C:/Users/marce/OneDrive/datos_rev/genes_counts_per_family.csv")

# lista de categorías cog por familia para permanova y pruebas de t

genes_cog <- read.csv("C:/Users/marce/OneDrive/datos_rev/cog_counts_per_family.csv")

# lista de categorías de metabolismo del kegg por familia para permanova y pruebas de t

genes_metab <- read.csv("C:/Users/marce/OneDrive/datos_rev/metab_counts_per_family.csv")

# lista de pathways del kegg por familia para permanova y pruebas de t

genes_paths <- read.csv("C:/Users/marce/OneDrive/datos_rev/paths_counts_per_family.csv")

# lista de modulos del kegg por familia para permanova y pruebas de t

genes_mods <- read.csv("C:/Users/marce/OneDrive/datos_rev/mods_counts_per_family.csv")



#permanovas a todos los niveles

pairwise.adonis(genes_ko[,3:2708],genes_ko$family_origin)

pairwise.adonis(genes_cog[,3:22],genes_cog$family_origin)

pairwise.adonis(genes_metab[,3:13],genes_metab$family_origin)

pairwise.adonis(genes_paths[,3:124],genes_paths$family_origin)

pairwise.adonis(genes_mods[,3:147],genes_mods$family_origin)


######### Pruebas t 

# función para sacar sólo las pruebas t significativas ajustadas por multiples comparaciones con correción BH

get_significant_tests <- function(tests_list){
  pvalues <- c()
  significant_tests <- c()
  
  for (pair_test in 1:length(tests_list)) {
    pvalues <- c(pvalues, tests_list[[pair_test]]$p.value)
  }
  #print(pvalues)
  
  pvalues <- p.adjust(pvalues, method = "BH")
  
  #print(pvalues)
  
  for (pval in 1:length(pvalues)) {
    if (!is.nan(pvalues[pval]) && pvalues[pval] <= 0.05) {
      #print(tests_list[pval])
      #print(pvalues[pval])
      significant_tests <- c(significant_tests,tests_list[pval])
      #tests_list[counter]
    }
  }
  return(significant_tests)
}

# función para sacar sólo las pruebas t significativas (unadjusted)

get_significant_tests <- function(tests_list){
  pvalues <- as.list()
  significant_tests <- as.list()
  
  for (pair_test in 1:length(tests_list)) {
    #print(pair_test)
    if (!is.nan(tests_list[[pair_test]]$p.value) && tests_list[[pair_test]]$p.value <= 0.05) {
      print(tests_list[pair_test])
      #tests_list[counter]
    }
    
  }
}

#### Clostridiaceae

clost_cog <- genes_cog[genes_cog$family_origin == "Clostridiaceae_engineered" | genes_cog$family_origin == "Clostridiaceae_hosted",]

clost_mods <- genes_mods[genes_mods$family_origin == "Clostridiaceae_engineered" | genes_mods$family_origin == "Clostridiaceae_hosted",]

clost_paths <- genes_paths[genes_paths$family_origin == "Clostridiaceae_engineered" | genes_paths$family_origin == "Clostridiaceae_hosted",]

clost_metab <- genes_metab[genes_metab$family_origin == "Clostridiaceae_engineered" | genes_metab$family_origin == "Clostridiaceae_hosted",]


# pruebas t entre "hosted" y "engineered" para todos los niveles de clasificación

clost_cog_tests <- lapply(clost_cog[,3:22], function(x) tryCatch(wilcox.test(x ~ clost_paths$family_origin, correct=FALSE), error=function(e) NULL))

clost_mods_tests <- lapply(clost_mods[,3:147], function(x) tryCatch(wilcox.test(x ~ clost_paths$family_origin, correct=FALSE), error=function(e) NULL))

clost_paths_tests <- lapply(clost_paths[,3:124], function(x) tryCatch(wilcox.test(x ~ clost_paths$family_origin, correct=FALSE), error=function(e) NULL))

clost_metab_tests <- lapply(clost_metab[,3:13], function(x) tryCatch(wilcox.test(x ~ clost_paths$family_origin, correct=FALSE), error=function(e) NULL))


get_significant_tests(clost_cog_tests)

get_significant_tests(clost_mods_tests)

get_significant_tests(clost_paths_tests)

get_significant_tests(clost_metab_tests)



#### Lactobacillaceae

lac_cog <- genes_cog[genes_cog$family_origin == "Lactobacillaceae_engineered" | genes_cog$family_origin == "Lactobacillaceae_hosted",]

lac_mods <- genes_mods[genes_mods$family_origin == "Lactobacillaceae_engineered" | genes_mods$family_origin == "Lactobacillaceae_hosted",]

lac_paths <- genes_paths[genes_paths$family_origin == "Lactobacillaceae_engineered" | genes_paths$family_origin == "Lactobacillaceae_hosted",]

lac_metab <- genes_metab[genes_metab$family_origin == "Lactobacillaceae_engineered" | genes_metab$family_origin == "Lactobacillaceae_hosted",]


# pruebas t entre "hosted" y "engineered" para todos los niveles de clasificación

lac_cog_tests <- lapply(lac_cog[,3:22], function(x) tryCatch(wilcox.test(x ~ lac_paths$family_origin, correct=FALSE), error=function(e) NULL))

lac_mods_tests <- lapply(lac_mods[,3:147], function(x) tryCatch(wilcox.test(x ~ lac_paths$family_origin, correct=FALSE), error=function(e) NULL))

lac_paths_tests <- lapply(lac_paths[,3:124], function(x) tryCatch(wilcox.test(x ~ lac_paths$family_origin, correct=FALSE), error=function(e) NULL))

lac_metab_tests <- lapply(lac_metab[,3:13], function(x) tryCatch(wilcox.test(x ~ lac_paths$family_origin, correct=FALSE), error=function(e) NULL))



get_significant_tests(lac_cog_tests)

get_significant_tests(lac_mods_tests)

get_significant_tests(lac_paths_tests)

get_significant_tests(lac_metab_tests)


#### Ruminococcaceae

rum_cog <- genes_cog[genes_cog$family_origin == "Ruminococcaceae_engineered" | genes_cog$family_origin == "Ruminococcaceae_hosted",]

rum_mods <- genes_mods[genes_mods$family_origin == "Ruminococcaceae_engineered" | genes_mods$family_origin == "Ruminococcaceae_hosted",]

rum_paths <- genes_paths[genes_paths$family_origin == "Ruminococcaceae_engineered" | genes_paths$family_origin == "Ruminococcaceae_hosted",]

rum_metab <- genes_metab[genes_metab$family_origin == "Ruminococcaceae_engineered" | genes_metab$family_origin == "Ruminococcaceae_hosted",]


# pruebas t entre "hosted" y "engineered" para todos los niveles de clasificación

rum_cog_tests <- lapply(rum_cog[,3:22], function(x) tryCatch(wilcox.test(x ~ rum_paths$family_origin, correct=FALSE), error=function(e) NULL))

rum_mods_tests <- lapply(rum_mods[,3:147], function(x) tryCatch(wilcox.test(x ~ rum_paths$family_origin, correct=FALSE), error=function(e) NULL))

rum_paths_tests <- lapply(rum_paths[,3:124], function(x) tryCatch(wilcox.test(x ~ rum_paths$family_origin, correct=FALSE), error=function(e) NULL))

rum_metab_tests <- lapply(rum_metab[,3:13], function(x) tryCatch(wilcox.test(x ~ rum_paths$family_origin, correct=FALSE), error=function(e) NULL))



get_significant_tests(rum_cog_tests)

get_significant_tests(rum_mods_tests)

get_significant_tests(rum_paths_tests)

get_significant_tests(rum_metab_tests)

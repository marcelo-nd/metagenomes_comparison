library(dplyr)

# Función para hacer subset de la línea de texto desde la derecha.
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}


# función para obtener vector con todos los genes de un archivo de genes, kegg ids. Input: archivo txt
get_genes_list <- function(genes_file){
  # vector para guardar la lista de genes en kegg id
  genes_vector <- c()
  
  # leer todas las líneas del archivo (da una lista)
  current_file_lines_list <- readLines(genes_file)
  
  for(linea in current_file_lines_list){
    # tomar el nombre de cada gen.
    current_split_line <- strsplit(linea, split = "\t")
    if (length(current_split_line[[1]]) == 2) {
      # añadir el nombre de cada gen al vector de genes.
      genes_vector <- c(as.character(genes_vector), as.character(current_split_line[[1]][2]))
    }
  }
  return(genes_vector)
}


## Función que lee todos los archivos de anotaciones de MAGs convertidas a keggs ids en un path
# y devuelve un df [genes kegg ids, MAGs]

get_all_mags_genes <- function(path_to_files){
  #lista de todos los archivos en el path
  kegg_files <- list.files(path=path_to_files)
  
  #data frame para guardar [genes, MAGs(OTUs)]
  mags_genes <- data.frame(genes=character(), stringsAsFactors = FALSE)
  
  # procesar rodos los archivos en el path
  for(cfile in kegg_files){
    # excluir los archivos log
    if(substrRight(cfile, 4) == ".txt"){
      print(cfile)
      # nombre de MAGs partir del nombre del archivo
      current_mag_id <- substr(cfile, 1, nchar(cfile)-4)
      #debug print(current_mag_id)
      # obtener lista de genes del archivo txt
      current_gene_list <- get_genes_list(paste(path_to_files, cfile, sep = ""))
      #print("lista de genes")
      
      #print(current_gene_list)
      #print(length(current_gene_list))
      
      # añadir nombre del mag y lista de genes 
      current_genes_df <- as.data.frame(table(current_gene_list), stringsAsFactors = FALSE)
      
      #debug print(current_genes_df[current_genes_df$current_gene_list == "K10831",])
      #debug print("dataframe de frecuencias")
      #print(current_genes_df)
      # renombrar columnas
      colnames(current_genes_df) <- c("genes", current_mag_id)
      
      # debug print(current_genes_df[current_genes_df$genes == "K10831",])
      
      #summary(current_genes_df)
      
      # unir el df de la muestra actual con el general.
      mags_genes <- full_join(mags_genes, current_genes_df, by = "genes")
    }
  }
  #convert NAs to 0s
  mags_genes[is.na(mags_genes)] <- 0
  return(mags_genes)
}

# función para generar los metadatos de origen y familia para todos los Mags en un folder

generate_mags_metadata <- function(path_to_files, pFamily, pOrigin){
  #lista de todos los archivos en el path
  kegg_files <- list.files(path=path_to_files) 
  
  mags_metadata <- data.frame(mag_id = character(), family = character(), origin = character(), stringsAsFactors = FALSE)
  
  counter <- 1
  
  for(cfile in kegg_files){
    if(substrRight(cfile, 4) == ".txt"){
      mags_metadata[counter, "mag_id"] <- substr(cfile, 1, nchar(cfile)-4)
      mags_metadata[counter, "family"] <- pFamily
      mags_metadata[counter, "origin"] <- pOrigin
      counter <- counter + 1
    }
  }
  return(mags_metadata)
}


### generate genes tables for each group

clos_hosted_genes <-get_all_mags_genes("C:/Users/marce/OneDrive/converted_anot/clos_hosted_gl/")

clos_eng_genes <- get_all_mags_genes("C:/Users/marce/OneDrive/converted_anot/clos_eng_gl/")

rum_hosted_genes <- get_all_mags_genes("C:/Users/marce/OneDrive/converted_anot/rum_hosted_gl/")

rum_eng_genes <- get_all_mags_genes("C:/Users/marce/OneDrive/converted_anot/rum_eng_gl/")

lac_hosted_genes <- get_all_mags_genes("C:/Users/marce/OneDrive/converted_anot/lac_hosted_gl/")

lac_eng_genes <- get_all_mags_genes("C:/Users/marce/OneDrive/converted_anot/lac_eng_gl/")


# joining all dfs

genes_df <- full_join(clos_hosted_genes, clos_eng_genes, by = "genes")

genes_df <- full_join(genes_df, rum_hosted_genes, by = "genes")

genes_df <- full_join(genes_df, rum_eng_genes, by = "genes")

genes_df <- full_join(genes_df, lac_hosted_genes, by = "genes")

genes_df <- full_join(genes_df, lac_eng_genes, by = "genes")

genes_df[is.na(genes_df)] <- 0

genes_df

write.table(genes_df, file = "C:/Users/marce/Desktop/genes_df.txt", col.names = TRUE, row.names = FALSE, quote=FALSE)


### generate metadata

clos_hosted_metadata <-generate_mags_metadata("C:/Users/marce/OneDrive/converted_anot/clos_hosted_gl/", "Clostridiaceae", "hosted")

clos_eng_metadata <- generate_mags_metadata("C:/Users/marce/OneDrive/converted_anot/clos_eng_gl/", "Clostridiaceae", "engineered")

rum_hosted_metadata <- generate_mags_metadata("C:/Users/marce/OneDrive/converted_anot/rum_hosted_gl/", "Ruminococcaceae", "hosted")

rum_eng_metadata <- generate_mags_metadata("C:/Users/marce/OneDrive/converted_anot/rum_eng_gl/", "Ruminococcaceae", "engineered")

lac_hosted_metadata <- generate_mags_metadata("C:/Users/marce/OneDrive/converted_anot/lac_hosted_gl/", "Lactobacillaceae", "hosted")

lac_eng_metadata <- generate_mags_metadata("C:/Users/marce/OneDrive/converted_anot/lac_eng_gl/", "Lactobacillaceae", "engineered")


genes_metadata <- bind_rows(clos_hosted_metadata, clos_eng_metadata,
                            rum_hosted_metadata, rum_eng_metadata,
                            lac_hosted_metadata, lac_eng_metadata)

genes_metadata


write.table(genes_metadata, file = "C:/Users/marce/Desktop/genes_metadata.txt", col.names = TRUE, row.names = FALSE, quote=FALSE)

###################################################################################################################################
###################################################################################################################################
###################################################################################################################################

clos_hosted_genes <-get_all_mags_genes("C:/Users/marce/Downloads/converted_anot/clos_hosted_gl/")

clos_eng_genes <- get_all_mags_genes("C:/Users/marce/Downloads/converted_anot/clos_eng_gl/")

rum_hosted_genes <- get_all_mags_genes("C:/Users/marce/Downloads/converted_anot/rum_hosted_gl/")

rum_eng_genes <- get_all_mags_genes("C:/Users/marce/Downloads/converted_anot/rum_eng_gl/")

lac_hosted_genes <- get_all_mags_genes("C:/Users/marce/Downloads/converted_anot/lac_hosted_gl/")

lac_eng_genes <- get_all_mags_genes("C:/Users/marce/Downloads/converted_anot/lac_eng_gl/")


clos_genes <- full_join(clos_hosted_genes, clos_eng_genes, by = "genes")

clos_genes[is.na(clos_genes)] <- 0

write.table(clos_genes, file = "C:/Users/marce/Desktop/clos_genes.txt", col.names = TRUE, row.names = FALSE, quote=FALSE)


rum_genes <- full_join(rum_hosted_genes, rum_eng_genes, by = "genes")

rum_genes[is.na(rum_genes)] <- 0

write.table(rum_genes, file = "C:/Users/marce/Desktop/rum_genes.txt", col.names = TRUE, row.names = FALSE, quote=FALSE)



lac_genes <- full_join(lac_hosted_genes, lac_eng_genes, by = "genes")

lac_genes[is.na(lac_genes)] <- 0

write.table(lac_genes, file = "C:/Users/marce/Desktop/lac_genes.txt", col.names = TRUE, row.names = FALSE, quote=FALSE)





### generate metadata

clos_hosted_metadata <-generate_mags_metadata("C:/Users/marce/Downloads/converted_anot/clos_hosted_gl/", "Clostridiaceae", "hosted")

clos_eng_metadata <- generate_mags_metadata("C:/Users/marce/Downloads/converted_anot/clos_eng_gl/", "Clostridiaceae", "engineered")

rum_hosted_metadata <- generate_mags_metadata("C:/Users/marce/Downloads/converted_anot/rum_hosted_gl/", "Ruminococcaceae", "hosted")

rum_eng_metadata <- generate_mags_metadata("C:/Users/marce/Downloads/converted_anot/rum_eng_gl/", "Ruminococcaceae", "engineered")

lac_hosted_metadata <- generate_mags_metadata("C:/Users/marce/Downloads/converted_anot/lac_hosted_gl/", "Lactobacillaceae", "hosted")

lac_eng_metadata <- generate_mags_metadata("C:/Users/marce/Downloads/converted_anot/lac_eng_gl/", "Lactobacillaceae", "engineered")




clos_metadata <- bind_rows(clos_hosted_metadata, clos_eng_metadata)

write.table(clos_metadata, file = "C:/Users/marce/Desktop/clos_metadata.txt", col.names = TRUE, row.names = FALSE, quote=FALSE)


rum_metadata <- bind_rows(rum_hosted_metadata, rum_eng_metadata)

write.table(rum_metadata, file = "C:/Users/marce/Desktop/rum_metadata.txt", col.names = TRUE, row.names = FALSE, quote=FALSE)



lac_metadata <- bind_rows(lac_hosted_metadata, lac_eng_metadata)

write.table(lac_metadata, file = "C:/Users/marce/Desktop/lac_metadata.txt", col.names = TRUE, row.names = FALSE, quote=FALSE)







###################################################################################################
###################################################################################################
###################################################################################################
#TESTS
# Probando la extracción de los genes
lista_test <- get_genes_list("C:/Users/marce/Downloads/converted_anot/lac_eng_gl/ASM386229.txt")
lista_test

# checando un gen 
for (i in lista_test) {
  if (i == "K10831") {
    print("true")
  }
  
}

# checando un gen en un archivo en los df generados
lac_eng_genes[lac_eng_genes$genes =="K10831", "ASM386229"]

sum(lac_eng_genes[lac_eng_genes$ASM386229 > 0,]$ASM386229)

# checando el df final

genes_df[genes_df$genes =="K10831",]

sum(genes_df[genes_df$ASM386229 > 0,]$ASM386229)

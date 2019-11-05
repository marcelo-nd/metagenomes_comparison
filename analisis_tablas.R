library(dplyr)

# Función para hacer subset de la línea de texto desde la derecha.
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}



###############################################################################################################################################
###############################################################################################################################################

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
  print 
  
  # procesar rodos los archivos en el path
  for(cfile in kegg_files){
    # excluir los archivos log
    if(substrRight(cfile, 4) == ".txt"){
      print(cfile)
      # nombre de MAGs partir del nombre del archivo
      current_mag_id <- substr(cfile, 1, nchar(cfile)-4)
      print(current_mag_id)
      # obtener lista de genes del archivo txt
      current_gene_list <- get_genes_list(paste(path_to_files, cfile, sep = ""))
      print("lista de genes")
      print(current_gene_list)
      
      # añadir nombre del mag y lista de genes 
      current_genes_df <- as.data.frame(table(current_gene_list), stringsAsFactors = FALSE)
      print("dataframe de frecuencias")
      print(current_genes_df)
      # renombrar columnas
      colnames(current_genes_df) <- c("genes", current_mag_id)
      
      # unir el df de la muestra actual con el general.
      mags_genes <- full_join(mags_genes, current_genes_df, by = "genes")
    }
  }
  mags_genes[is.na(mags_genes)] <- 0
  return(mags_genes)
}





clos_hosted_genes <-get_all_mags_genes("C:/Users/marce/OneDrive/converted_anot/clos_hosted_gl/")

clos_eng_genes <- get_genes_list("C:/Users/marce/OneDrive/converted_anot/clos_eng_gl/")

rum_hosted_genes <- get_genes_list("C:/Users/marce/OneDrive/converted_anot/rum_hosted_gl/")

rum_eng_genes <- get_genes_list("C:/Users/marce/OneDrive/converted_anot/rum_eng_gl/")

lac_hosted_genes <- get_genes_list("C:/Users/marce/OneDrive/converted_anot/lac_hosted_gl/")

lac_eng_genes <- get_genes_list("C:/Users/marce/OneDrive/converted_anot/lac_eng_gl/")


#########################################################################################################################################################################
############################################################################################################################################################









lac_eng_tab

clos_df_joined <- full_join(clos_hosted_tab, clos_eng_tab, by = "genes")

clos_df_joined

rum_df_joined <- full_join(rum_hosted_tab, rum_eng_tab, by = "genes")

rum_df_joined

lac_df_joined <- full_join(lac_eng_tab, lac_hosted_tab, by = "genes")

lac_df_joined



final_df <- full_join(clos_df_joined, rum_df_joined, by = "genes")

final_df <- full_join(final_df, lac_df_joined, by = "genes")

final_df



write.table(final_df, file = "C:/Users/marce/Desktop/genes_df.txt", col.names = TRUE, row.names = FALSE, quote=FALSE)

# Función para hacer subset de la línea de texto desde la derecha.
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

# Función que lee todos los archivos de anotaciones y devuelve vector con todos los genes.
get_genes_list <- function(path_to_files){
  # lista de archivos
  kegg_files <- list.files(path=path_to_files)
  genes_vector <- c()
  # procesar rodos los archivos en el path
  for(cfile in kegg_files){
    # excluit los archivos log
    if(substrRight(cfile, 4) == ".txt"){
      print(cfile)
      # leer cada línea de los archivos
      current_file_lines_list <- readLines(paste(path_to_files, cfile, sep = ""))
      for(linea in current_file_lines_list){
        # tomar el nombre de cada gen.
        current_split_line <- strsplit(linea, split = "\t")
        if (length(current_split_line[[1]]) == 2) {
          print(current_split_line[[1]])
          # añadir el nombre de cada gen al vector de genes.
          genes_vector <- c(as.character(genes_vector), as.character(current_split_line[[1]][2]))
        }
      }
    }
  }
  return(genes_vector)
}



clost_hosted_genes <- get_genes_list("C:/Users/marce/OneDrive/converted_anot/clos_hosted_gl/")

summary(clost_hosted_genes)

clost_eng_genes <- get_genes_list("C:/Users/marce/OneDrive/converted_anot/clos_eng_gl/")

summary(clost_eng_genes)

rum_hosted_genes <- get_genes_list("C:/Users/marce/OneDrive/converted_anot/rum_hosted_gl/")

summary(rum_hosted_genes)

rum_eng_genes <- get_genes_list("C:/Users/marce/OneDrive/converted_anot/rum_eng_gl/")

summary(rum_eng_genes)

lac_hosted_genes <- get_genes_list("C:/Users/marce/OneDrive/converted_anot/lac_hosted_gl/")

summary(lac_hosted_genes)

lac_eng_genes <- get_genes_list("C:/Users/marce/OneDrive/converted_anot/lac_eng_gl/")

summary(lac_eng_genes)

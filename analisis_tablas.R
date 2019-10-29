substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}


get_genes_list <- function(path_to_files){
  kegg_files <- list.files(path=path_to_files)
  genes_vector <- c()
  for(cfile in kegg_files){
    if(substrRight(cfile, 4) == ".txt"){
      #print(paste("C:/Users/marce/OneDrive/converted_anot/clos_eng_gl/", cfile, sep = ""))
      print(cfile)
      current_file_lines_list <- readLines(paste(path_to_files, cfile, sep = ""))
      for(linea in current_file_lines_list){
        current_split_line <- strsplit(linea, split = "\t")
        print(current_split_line[[1]])
        if (length(current_split_line[[1]]) == 2) {
          print(current_split_line[[1]])
          genes_vector <- c(as.character(genes_vector), as.character(current_split_line[[1]][2]))
        }
      }
    }
  }
  return(genes_vector)
}



clost_eng_genes <- get_genes_list("C:/Users/marce/OneDrive/converted_anot/clos_eng_gl/")

length(clost_eng_genes)
















clost_eng_kegg_files <- list.files(path="C:/Users/marce/OneDrive/converted_anot/clos_eng_gl/")

genes_vector <- c()

for(cfile in clost_eng_kegg_files){
  if(substrRight(cfile, 4) == ".txt"){
    #print(paste("C:/Users/marce/OneDrive/converted_anot/clos_eng_gl/", cfile, sep = ""))
    print(cfile)
    current_file_lines_list <- readLines(paste("C:/Users/marce/OneDrive/converted_anot/clos_eng_gl/", cfile, sep = ""))
    for(linea in current_file_lines_list){
      current_split_line <- strsplit(linea, split = "\t")
      print(current_split_line[[1]])
      if (length(current_split_line[[1]]) == 2) {
        print(current_split_line[[1]])
        genes_vector <- c(as.character(genes_vector), as.character(current_split_line[[1]][2]))
      }
    }
  }
}

genes_vector

length(genes_vector)








test <- read.table("C:/Users/marce/OneDrive/converted_anot/clos_eng_gl/ASM151250.txt", fill=TRUE, row.names = NULL, sep = "\t")
test


lista <-  readLines("C:/Users/marce/OneDrive/converted_anot/clos_eng_gl/ASM151250.txt")

for(i in lista){
  print(strsplit(i, split = "\t"))
}

for(i in lista){
  print(i)
}

as.character(strsplit("EIBFANAO_02751\tK01696", split = "\t")[[1]][2])

length(strsplit("EIBFANAO_02751\tK01696", split = "\t")[[1]])


table(clos_hosted$V2)

#files 



for(cfile in clost_eng_kegg_files){
  if(substrRight(cfile, 4) == ".txt"){
    #print(paste("c:/users/marce/onedrive/converted_anot/clos_eng_gl/", cfile, sep = ""))
    print(cfile)
    current_file_df <- read.table(paste("c:/users/marce/onedrive/converted_anot/clos_eng_gl/", cfile, sep = ""), fill=TRUE, row.names = NULL)
    print(length(current_file_df$V2))
    #print(current_file_df$V2)
    genes_vector <- c(as.character(genes_vector), as.character(current_file_df$V2))
    #print(genes_vector)
  }

}

938+780+768

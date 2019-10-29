library(readxl)

get_path_lists <- function(dataF, p_path){
  index <- 1
  r_data <- data.frame(paths=character(), stringsAsFactors = FALSE)
  for(i in dataF$id){
    #print(paste("/srv/home/anavarro/genomes_annotation/clostridiaceae/", i))
    
    var1 <- paste(p_path, i, sep = "")
    
    var2 <- paste(var1, "/", sep = "")
    
    var3 <- paste(var2, i, sep = "")
    
    var4 <- paste(var3, "/", sep = "")
    
    var5 <- paste(var4, i, sep = "")
    
    var6 <- paste(var5, ".gbk", sep = "")
    
    r_data[index, ] <- as.list(var6)
    
    index <- index + 1
  }
  return(r_data)
}

clos_hosted <- read_xlsx(path = "C:/Users/marce/OneDrive/genomas_assembly.xlsx", sheet = "clos_hosted")
clos_hosted_df <- get_path_lists(clos_hosted, "/srv/home/anavarro/genomes_annotation/clostridiaceae/hosted/")
clos_hosted_df

clos_eng <- read_xlsx(path = "C:/Users/marce/OneDrive/genomas_assembly.xlsx", sheet = "clos_eng")
clos_eng_df <- get_path_lists(clos_eng, "/srv/home/anavarro/genomes_annotation/clostridiaceae/eng/")
clos_eng_df

rum_hosted <- read_xlsx(path = "C:/Users/marce/OneDrive/genomas_assembly.xlsx", sheet = "rum_hosted")
rum_hosted_df <- get_path_lists(rum_hosted, "/srv/home/anavarro/genomes_annotation/ruminococcaceae/hosted/")
rum_hosted_df

rum_eng <- read_xlsx(path = "C:/Users/marce/OneDrive/genomas_assembly.xlsx", sheet = "rum_eng")
rum_eng_df <- get_path_lists(rum_eng, "/srv/home/anavarro/genomes_annotation/ruminococcaceae/eng/")
rum_eng_df

lac_hosted <- read_xlsx(path = "C:/Users/marce/OneDrive/genomas_assembly.xlsx", sheet = "lac_hosted")
lac_hosted_df <- get_path_lists(lac_hosted, "/srv/home/anavarro/genomes_annotation/lactobacillaceae/hosted/")
lac_hosted_df

lac_eng <- read_xlsx(path = "C:/Users/marce/OneDrive/genomas_assembly.xlsx", sheet = "lac_eng")
lac_eng_df <- get_path_lists(lac_eng, "/srv/home/anavarro/genomes_annotation/lactobacillaceae/eng/")
lac_eng_df

write.table(clos_hosted_df, "C:/Users/marce/Desktop/clos_hosted.txt", col.names = FALSE, row.names = FALSE, quote=FALSE)
write.table(clos_eng_df, "C:/Users/marce/Desktop/clos_eng.txt", col.names = FALSE, row.names = FALSE, quote=FALSE)
write.table(rum_hosted_df, "C:/Users/marce/Desktop/rum_hosted.txt", col.names = FALSE, row.names = FALSE, quote=FALSE)
write.table(rum_eng_df, "C:/Users/marce/Desktop/rum_eng.txt", col.names = FALSE, row.names = FALSE, quote=FALSE)
write.table(lac_hosted_df, "C:/Users/marce/Desktop/lac_hosted.txt", col.names = FALSE, row.names = FALSE, quote=FALSE)
write.table(lac_eng_df, "C:/Users/marce/Desktop/lac_eng.txt", col.names = FALSE, row.names = FALSE, quote=FALSE)

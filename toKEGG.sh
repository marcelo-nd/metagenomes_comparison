#!/bin/bash
#leer tabla
input="/srv/home/anavarro/genomes_path_lists/lac_eng.txt"
# leer linea por linea
while IFS= read -r line
do
# ejecutar script python
  echo $line
  ID_NUMBER=$(echo $line | cut -d'/' -f 8)
  OUT_NAME="/srv/home/anavarro/converted_anot/lac_eng_gl/$ID_NUMBER.txt"
  python3 /srv/home/anavarro/prokka_convertions/db/prokka2kegg.py -i $line -d /srv/home/anavarro/prokka_convertions/db/idmapping_KO.tab.gz -o $OUT_NAME
done < "$input"
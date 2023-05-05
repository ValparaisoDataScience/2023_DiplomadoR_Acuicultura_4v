# -----------------------------  
# R script: Práctica análisis de secuencias con Biostring  
# Autor: Tu nombre  
# Fecha: Completar
# Curso: Genes y genomas
# -----------------------------

library(Biostrings)

# Crea objeto DNAString
abo_seq <- DNAString("ATGGCCGAGGTGTTGCGGACGCTGGCCGGAAAACCAAAATGCCACGCGGTCCGGAACCCGTGA")
abo_seq

# Transcripción de ADN a ARN
rna_seq <- RNAString(dna_seq)
rna_seq

# Traduce de ARN a proteína o de ADN a proteína.
aa_seq <-translate(dna_seq)
aa_seq


# Importa secuencia de a.a. en formato fasta
prot <- readAAStringSet(file="229E_Proteina_S.fasta")

prot

# Nombre de la secuencia
names(prot)

# Tamaño de la secuencia
length(prot)

width(prot)

# Frecuencia de aminoácidos de la secuencia
alphabetFrequency(prot)

# Seleccionar nucleótidos del gen
subseq(prot, start=10, end=150)




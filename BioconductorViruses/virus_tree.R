# Alex Van Plantinga
# May 28 2020
# Virus tree


library("rentrez")
library("Biostrings")
library("GenomicRanges")
library("clusterProfiler")
library("DOSE")
library("org.Hs.eg.db")
library("treeio")
library("ape")
library("Biostrings")
library("ggplot2")
library("ggtree")
library("msa")
library("seqinr")
library("phylobase")



# https://www.sciencemag.org/news/2020/01/mining-coronavirus-genomes-clues-outbreak-s-origins
virus_accession_numbers = c("MT525950", "NC_045512", "MT528599", 
                            "AY278489", "AY278741.1", "Z86099.2", 
                            "AB618031.1", "CY121680", "CY098755", 
                            "MF797870", " KT029139.1", "MH481611", "AF033819.3")

virus_sequences = read.GenBank(virus_accession_numbers)

# https://bioinformaticshome.com/bioinformatics_tutorials/R/phylogeny_estimation.html
setwd("C:\\Users\\Lenovo\\Epidemic\\BioconductorViruses\\virus_sequences")

for(i in 1:length(virus_accession_numbers)){
  write.dna(read.GenBank(virus_accession_numbers[i]), 
            file = paste(virus_accession_numbers[i], ".fasta", sep=""), 
            format = "fasta", append =
              FALSE, nbcol = 6, colsep = " ", colw = 10)
  i = i+1
}

#virusSeq <- readAAStringSet("virus_sequences1.fasta")
nfiles <- length(dir())
seqdat <- vector("list", nfiles)

for(i in 1:nfiles){
  seqdat[[i]] <- read.fasta(file=dir()[i])
}

label <- sapply(1:nfiles, function(k) unlist(strsplit(dir()[k], "[.]"))[1])
names(seqdat) <- label

seqdat_join <- lapply(seqdat, function(k) paste(toupper(unlist(k)),collapse="") )

#enumerate all 5-mers
dna <- c("A","C","G","T")
kmer5 <- expand.grid(dna, dna, dna, dna, dna)
kmer5 <- apply(kmer5, 1, function(k) paste(k, collapse=""))

library(textmineR)
#function for counting all possible kmers (k=5) given a single dna string
kmercount <- function(data){
  sapply(1:length(kmer5), function(k)
    length(unlist(gregexpr2(kmer5[k], data)))
  )}

#vector of counts for all possible kmers (k=5) for all viral sequences
kmer_features <- lapply(seqdat_join, function(k) kmercount(k))


#Collect k-mer counts into a data frame
M <- do.call(rbind, kmer_features)

#taxonomic labels
virus_names = c("COVID19Italy", "COVID19Wuhan", "COVID19USA", "sars1a",
                "sars1b", "herpes2", "herpes1", 
                "swine_flu_H1N1", "bird_flu_H5N1", "WNV", "MERS", "Ebola", "HIV")

n = gsub("\\..*","", virus_accession_numbers)

taxonomy <- data.frame(rownames(M), virus_names[order(n)])

colnames(taxonomy) <- c("Virus","Family")

#Simplify virus species names
#virusnames <- sapply(1:nrow(taxonomy), function(k){
#  chop <- unlist(strsplit(as.character(taxonomy$Virus)[k],"_")) 
#  chop[length(chop)]}
#)

rownames(M) <- virus_names[order(n)]

tipcolor <- c("red","blue","darkviolet")[unclass(taxonomy$Family)]

#The correct input for CalcJSDivergence is the (unnormalised) count vector
JSdist <- CalcJSDivergence(M)
tree = bionj(JSdist)

#make negative branches positive
tree$edge.length = abs(tree$edge.length)

plot.phylo(tree, type="unrooted", cex=0.8, tip.color=tipcolor,
           rotate.tree=95)

plot.phylo(tree, cex=0.8, tip.color=tipcolor,
           rotate.tree=95)

plot(tree, main="Viral RNA Difference Tree", cex = 0.8)

# see node numbering three lines down 
tree = groupClade(tree, .node = c(14, 15, 17,  19, 20, 21, 23, 24))

ggtree(tree, aes(color=group, linetype=group), 
       layout='slanted',  branch.length= 'none' ) + 
  coord_flip()+
  geom_tiplab(size=3.6, aes(angle=90)) + 
  ggtitle("Viral RNA Difference Tree") + 
  #geom_text(aes(label = node)) + # number the nodes before final plotting to configure groupClade
   ggplot2::xlim(0, 10) + 
  scale_color_manual(labels = c("SARS1", "MERS", "Covid19", 
                                "Ebola", "HIV", "Influenza", 
                                "West Nile Virus", "Herpes"), 
                     values = c( "black", "navy","red" , 
                                 "purple", "darkgreen", "coral3",
                                 "magenta", "orange") ) +
  guides(color = guide_legend(
    override.aes = list(linetype = c('solid','solid','solid', 
                                     'solid', 'solid', "solid", 
                                     'solid', "solid"))),
         linetype = FALSE)



ggtree(tree, aes(color=group, linetype=group), 
       layout='circular',  branch.length= 'none' ) + 
  geom_tiplab(size=3.6) + 
  ggtitle("Viral RNA Difference Tree") + 
  #geom_text(aes(label = node)) + # number the nodes before final plotting to configure groupClade
  ggplot2::xlim(0, 10) + 
  scale_color_manual(labels = c("SARS1", "MERS", "Covid19", 
                                "Ebola", "HIV", "Influenza", 
                                "West Nile Virus", "Herpes"), 
                     values = c( "black", "navy","red" , 
                                 "purple", "darkgreen", "coral3",
                                 "magenta", "orange") ) +
  guides(color = guide_legend(
    override.aes = list(linetype = c('solid','solid','solid', 
                                     'solid', 'solid', "solid", 
                                     'solid', "solid"))),
    linetype = FALSE)



# Now that we know how closely related some viruses are
# try making a circular tree of only SARS viruses


# https://www.sciencemag.org/news/2020/01/mining-coronavirus-genomes-clues-outbreak-s-origins

# accession numbers copied from:
# https://www.ncbi.nlm.nih.gov/genbank/sars-cov-2-seqs/

virus_accession_numbers = c("MT525950", "NC_045512", "MT517437",
                            "LC542976", "MT509959", "MT510999",
                            "MT066156",  "MT079847", "MT509657",
                            "MT106053", "MT093571","MT517430",
                            "MT152824", "MT511696", "MT511077", 
                            "MT509685", "MT510643", "MT507274",
                            "MT506897", 
                            "MT482145", "MT470150", "MT459908",
                            "MT457400", "MT451835", "MT450927",
                             "MT419821", "MT412265",
                            "MT394864", "LR757995",
                            "LC547520", "LR757997", "MN938390")

#taxonomic labels
virus_names = c("Italy1", "Wuhan", "Taiwan",
                "Japan1", "India1", "Netherlands1",
                "Italy2", "China", "India2",
                "USACA", "Sweden", "CzechRep",
                "USAWA", "USAFL", "Poland", 
                "USAMI", "Russia", "Jamaica",
                "USAIL",
                "USAVA", "France", "Greece",
                "Netherlands2", "Australia1", "Australia2",
               "PuertoRico", "USACT", 
                "Germany", "Wuhan2",
               "Japan2", "Wuhan3", "ChinaShenzen")


virus_sequences = read.GenBank(virus_accession_numbers)

# https://bioinformaticshome.com/bioinformatics_tutorials/R/phylogeny_estimation.html

setwd("C:\\Users\\Lenovo\\Epidemic\\BioconductorViruses\\virus_sequences2")
for(i in 1:length(virus_accession_numbers)){
  write.dna(read.GenBank(virus_accession_numbers[i]), 
            file = paste(virus_accession_numbers[i], ".fasta", sep=""), 
            format = "fasta", append =
              FALSE, nbcol = 6, colsep = " ", colw = 10)
 i = i+1
 print(i)
}

#virusSeq <- readAAStringSet("virus_sequences1.fasta")
nfiles <- length(dir())
seqdat <- vector("list", nfiles)

for(i in 1:nfiles){
  seqdat[[i]] <- read.fasta(file=dir()[i])
}

label <- sapply(1:nfiles, function(k) unlist(strsplit(dir()[k], "[.]"))[1])
names(seqdat) <- label

seqdat_join <- lapply(seqdat, function(k) paste(toupper(unlist(k)),collapse="") )

#enumerate all 5-mers
dna <- c("A","C","G","T")
kmer5 <- expand.grid(dna, dna, dna, dna, dna)
kmer5 <- apply(kmer5, 1, function(k) paste(k, collapse=""))

library(textmineR)
#function for counting all possible kmers (k=5) given a single dna string
kmercount <- function(data){
  sapply(1:length(kmer5), function(k)
    length(unlist(gregexpr2(kmer5[k], data)))
  )}

#vector of counts for all possible kmers (k=5) for all viral sequences
kmer_features <- lapply(seqdat_join, function(k) kmercount(k))


#Collect k-mer counts into a data frame
M <- do.call(rbind, kmer_features)

n = gsub("\\..*","", virus_accession_numbers)

taxonomy <- data.frame(rownames(M), virus_names[order(n)])

colnames(taxonomy) <- c("Virus","Family")

#Simplify virus species names
#virusnames <- sapply(1:nrow(taxonomy), function(k){
#  chop <- unlist(strsplit(as.character(taxonomy$Virus)[k],"_")) 
#  chop[length(chop)]}
#)

rownames(M) <- virus_names[order(n)]

tipcolor <- c("red","blue","darkviolet")[unclass(taxonomy$Family)]

#The correct input for CalcJSDivergence is the (unnormalised) count vector
JSdist <- CalcJSDivergence(M)
tree = bionj(JSdist)

#tree$tip.label = virus_names[which(n %in% rownames(M))]
plot(tree, main="Viral RNA Difference Tree", cex = 0.8)

plot.phylo(tree, type="unrooted", cex=0.6, tip.color=tipcolor,
           rotate.tree=140)



# https://guangchuangyu.github.io/ggtree-book/chapter-ggtree.html
#ggtree(virusTree,  layout='circular') + geom_tiplab(size=3, aes(angle=angle)) + theme(legend.position="none")+ aes(color=I(color))
ggtree(tree,  layout='circular',  branch.length='none' ) + 
  ggtitle("Covid19 Cladogram Circle Plot") + 
  geom_tiplab(size=3.6, aes(angle=angle)) + 
  theme(legend.position="none") + 
  #ggplot2::xlim(0.00008, 0.01) +
  theme(text = element_text(size=10))

ggtree(tree,  layout='slanted',  branch.length="none" ) + coord_flip()+ 
  ggtitle("Covid19 Cladogram Slanted Plot") + 
  geom_tiplab(size=3.6, aes(angle=90)) + 
  theme(legend.position="none") + 
  ggplot2::xlim(0,20) +
  theme(text = element_text(size=10))

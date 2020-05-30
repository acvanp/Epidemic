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
                            "MF797870")

virus_sequences = read.GenBank(virus_accession_numbers)

# https://bioinformaticshome.com/bioinformatics_tutorials/R/phylogeny_estimation.html
setwd("C:\\Users\\Lenovo\\Epidemic\\Bioconductor\\virus_sequences")

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
virus_names = c("sars2Italy", "sars2Wuhan", "sars2USA", "sars1a",
                "sars1b", "herpes2", "herpes1", 
                "swine_flu_H1N1", "bird_flu_H5N1", "WNV")

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

plot.phylo(tree, type="unrooted", cex=0.8, tip.color=tipcolor,
           rotate.tree=95)

plot.phylo(tree, cex=0.8, tip.color=tipcolor,
           rotate.tree=95)

plot(tree, main="Viral RNA Difference Tree", cex = 0.8)

tree = groupClade(tree, .node = c(11,13, 15, 17, 18))
ggtree(tree, aes(color=group, linetype=group)) + 
  ggtitle("Viral RNA Difference Tree") + 
  geom_tiplab(aes(subset=(group==1))) +
  geom_tiplab(aes(subset=(group==2))) +
  geom_tiplab(aes(subset=(group==3))) +
  geom_tiplab(aes(subset=(group==4))) +
  geom_tiplab(aes(subset=(group==5))) + 
  ggplot2::xlim(0, 0.29) + 
  scale_color_manual(labels = c("Covid19", "SARS1", "Influenza", "West Nile Virus", "Herpes"), 
                     values = c("black",  "navy", "purple", "magenta", "orange") ) +
  guides(color = guide_legend(override.aes = list(linetype = c('solid','solid', 'solid', 'solid', "solid"))),
         linetype = FALSE)




# Now that we know how closely related some viruses are
# try making a circular tree of only SARS viruses


# https://www.sciencemag.org/news/2020/01/mining-coronavirus-genomes-clues-outbreak-s-origins
virus_accession_numbers = c("MT525950", "NC_045512", "MT528599",
                            "LC542976", 
                            "MT066156",  "MT079847",
                            "MT106053", "MT093571","MT517430",
                            "MT152824", "MT511696", "MT511077", 
                            "MT509685", "MT510643")

#taxonomic labels
virus_names = c("Italya", "Wuhan", "USA", 
                "Japan",
                "Italyb", "China",
                "USACA", "Sweden", "CzechRep",
                "USAWA", "USAFL", "Poland", 
                "USAMI", "Russia")


virus_sequences = read.GenBank(virus_accession_numbers)

# https://bioinformaticshome.com/bioinformatics_tutorials/R/phylogeny_estimation.html

setwd("C:\\Users\\Lenovo\\Epidemic\\BioconductorViruses\\virus_sequences2")
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


#####################

virus_locations = c("USA: CA", "USA: WA", 
                    "USA: FL", "USA: NY",
                    "Wuhan",  "China",
                    "Italy", "Germany",
                    "UK", "France",
                    "Spain", "Canada",
                    "Iran", "Russia",
                    "India", "Brazil",
                    "Japan")

ll = c()

search_hits = c()

accession_number_list = c()

for(i in 1:length(virus_locations)){
  t = paste("SARS-CoV-2 AND Human[Organism]AND ", virus_locations[i], sep = "")
  m = entrez_search(db = "nuccore", term = t, retmax = 2)
  search_hits[i] = m$count
  l = length(m$id)
  m = entrez_fetch(db="nuccore", id=m$ids, rettype="fasta")
  
  for(k in 1:l){
  if(k == 1){n = gsub('^.*>\\s*|\\s* .*$', '', m) 
  accession_number_list = append(accession_number_list, n)}else({
    n = gsub(paste0(n,'^.*>\\s*|\\s* .*$'), '', m);
    accession_number_list = append(accession_number_list, n)
  })
  }
  
  ll = append(ll, m)
}

accession_number_list = gsub('>','',accession_number_list)

setwd("C:\\Users\\Lenovo\\Epidemic\\BioconductorViruses\\virus_sequences3")
write(ll, "viruses.fasta", sep="\n")
viruss_COI_seqinr_format <- read.fasta("viruses.fasta")

virusSeq <- readAAStringSet("viruses.fasta")
virusAln <- msa(virusSeq)
## use default substitution matrix

virusAln

virusAln2 <- msaConvert(virusAln, type="seqinr::alignment")

d <- dist.alignment(virusAln2, "identity")

virusTree <- nj(d)

nameslist = c()

for(i in 1:length(virus_locations)){
  if(search_hits[i] > 2){
    nameslist=append(nameslist, rep(virus_locations[i],2))}else(
      nameslist=append(nameslist, rep(virus_locations[i], search_hits[i]))
    )
}

df = data.frame(accession_number_list, nameslist)

virusTree$tip.label = nameslist

plot(virusTree, main="Covid19 Virus RNA by Location Plot", cex = 0.8)

#virusTree = groupOTU(virusTree, virusTree$tip.label)

# https://guangchuangyu.github.io/ggtree-book/chapter-ggtree.html
#ggtree(virusTree,  layout='circular') + geom_tiplab(size=3, aes(angle=angle)) + theme(legend.position="none")+ aes(color=I(color))
ggtree(virusTree,  layout='circular') + geom_tiplab(size=3, aes(angle=angle)) + theme(legend.position="none")

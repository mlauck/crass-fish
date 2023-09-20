# Phylomaker

# devtools::install_github("GabrielNakamura/FishPhyloMaker", ref = "main")
library(FishPhyloMaker)
library(rfishbase)

# example
data(neotropical_comm)
data_comm <- neotropical_comm[, -c(1, 2)] # removing latitude and longitude

# load traits data
allarid <- read.csv("Data/fish_traits.csv", header = TRUE)

# # replace space with underscore
# allarid$<-gsub(" ", "_", df1$x1)

# # make a vector of fish names
fish <- allarid[,1]

# what sort of data are they?
str(fish)
fish2 <- as.factor(fish)

# # keep only unique
# fish2 <- as.data.frame(unique(fish))

# finds family and order of species in Fishbase
taxon_data <- FishTaxaMaker(data_comm, allow.manual.insert = TRUE)
taxon_data2 <- FishTaxaMaker(fish2, allow.manual.insert = TRUE)


res_phylo <- FishPhyloMaker(data = taxon_data$Taxon_data_FishPhyloMaker,
                            insert.base.node = TRUE, 
                            return.insertions = TRUE, 
                            progress.bar = TRUE)

# The output has two objects, a phylogenetic tree that can be directly plot with the following code:
plot(res_phylo$Phylogeny, cex = 0.7)

# And a data frame indicating at which level the species was inserted (one of the six categories detailed above).
res_phylo$Insertions_data

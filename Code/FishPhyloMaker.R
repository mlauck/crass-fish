# Phylomaker

# devtools::install_github("GabrielNakamura/FishPhyloMaker", ref = "main")
library(FishPhyloMaker)
library(rfishbase)

# example
data(neotropical_comm)
data_comm <- neotropical_comm[, -c(1, 2)] # removing latitude and longitude

# load traits data
allarid <- read.csv("Data/fish_traits.csv", header = TRUE)

# # make a vector of fish names
fish <- allarid[,1]

# # replace space with underscore
fish2 <- gsub(" ", "_", fish)

# # what sort of data are they?
# str(fish)
# fish2 <- as.factor(fish)

# # keep only unique
fish3 <- unique(fish2)

# finds family and order of species in Fishbase
# example
taxon_data <- FishTaxaMaker(data_comm, allow.manual.insert = TRUE)

# our data
taxon_data2 <- FishTaxaMaker(fish2, allow.manual.insert = TRUE)

## Families and orders
# > taxon_data2 <- FishTaxaMaker(fish2, allow.manual.insert = TRUE)
# Joining with `by = join_by(Subfamily, GenCode, FamCode)`
# Joining with `by = join_by(FamCode)`
# Joining with `by = join_by(Order, Ordnum, Class, ClassNum)`
# Joining with `by = join_by(Class, ClassNum)`
# Joining with `by = join_by(Subfamily, GenCode, FamCode)`
# Joining with `by = join_by(FamCode)`
# Joining with `by = join_by(Order, Ordnum, Class, ClassNum)`
# Joining with `by = join_by(Class, ClassNum)`
# tell the Family of  Oncorhynchus clarkii henshawi
# Salmonidae
# tell the Order of  Oncorhynchus clarkii henshawi
# Salmoniformes
# tell the Family of  Oncorhynchus clarkii utah
# Salmonidae
# tell the Order of  Oncorhynchus clarkii utah
# Salmoniformes
# tell the Family of  Phoxinus grumi belimiauensis
# Leuciscidae
# tell the Order of  Phoxinus grumi belimiauensis
# Cypriniformes
# tell the Family of  Phoxinus czekanowskiifresh
# Leuciscidae
# tell the Order of  Phoxinus czekanowskiifresh
# Cypriniformes
# tell the Family of  Gila alutacea
# Cyprinidae
# tell the Order of  Gila alutacea
# Cypriniformes
# tell the Family of  Pantosteus plebeius
# Catostomidae
# tell the Order of  Pantosteus plebeius
# Cypriniformes
# tell the Family of  Cyprinodon macularis
# Cyprinodontidae
# tell the Order of  Cyprinodon macularis
# Cyprinodontiformes
# tell the Family of  Cyprinodon pisterti
# Cyprinodontidae
# tell the Order of  Cyprinodon pisterti
# Cyprinodontiformes
# tell the Family of  NA
# 
# tell the Order of  NA
# 
# tell the Family of  Siphateles bicolor mohavensis
# Leuciscidae
# tell the Order of  Siphateles bicolor mohavensis
# Cypriniformes
# tell the Family of  NA
# 
# tell the Order of  NA
# 
# tell the Family of  NA
# 
# tell the Order of  NA
# 
# tell the Family of  NA
# 
# tell the Order of  NA
# 
# tell the Family of  NA
# 
# tell the Order of  NA
# 
# tell the Family of  NA
# 
# tell the Order of  NA
# 
# tell the Family of  Gila coriacea
# Cyprinidae
# tell the Order of  Gila coriacea
# Cypriniformes
# tell the Family of  NA
# 
# tell the Order of  NA
# 
# tell the Family of  NA
# 
# tell the Order of  NA
# 
# tell the Family of  NA
# 
# tell the Order of  NA
# 
# tell the Family of  NA
# 
# tell the Order of  NA
# 
# tell the Family of  Pantosteus plebeius
# Catostomidae
# tell the Order of  Pantosteus plebeius
# Cypriniformes
# tell the Family of  NA
# 
# tell the Order of  NA
# 
# tell the Family of  NA
# 
# tell the Order of  NA
# 
# tell the Family of  NA
# 
# tell the Order of  NA
# 
# tell the Family of  NA
# 
# tell the Order of  NA


# Cyprinidon macularis
## 
res_phylo <- FishPhyloMaker(data = taxon_data$Taxon_data_FishPhyloMaker,
                            insert.base.node = TRUE, 
                            return.insertions = TRUE, 
                            progress.bar = TRUE)

res_phylo2 <- FishPhyloMaker(data = taxon_data2$Taxon_data_FishPhyloMaker,
                            insert.base.node = TRUE, 
                            return.insertions = TRUE, 
                            progress.bar = TRUE)

# The output has two objects, a phylogenetic tree that can be directly plot with the following code:
plot(res_phylo2$Phylogeny, cex = 0.7)

# And a data frame indicating at which level the species was inserted (one of the six categories detailed above).
res_phylo2$Insertions_data

## plot tree
library(ape)

if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("ggtree")
library(ggtree)
tree.PR <- res_phylo$Phylogeny

tree.PR <- ape::makeNodeLabel(tree.PR)
phylo <- tree.PR

rm.famNames <- which(table(taxon_data$f) == 1) # monotipic families
names.fam <- setdiff(unique(taxon_data$f), names(rm.famNames)) # removing monotipic families from the names 

for (i in 1:length(names.fam)) {
  set <- subset(taxon_data, f == names.fam[i])
  phylo <- ape::makeNodeLabel(phylo, "u", nodeList = list(Fam_name = set$s))
  
  phylo$node.label[which(phylo$node.label == 
                           "Fam_name") ] <- paste(set$f[1])
}

pos.node <- unlist(lapply(names.fam, function(x){
  which(phylo$node.label == x) + length(phylo$tip.label)
}))

df.phylo <- data.frame(Fam.names = names.fam,
                       node.number = pos.node)

plot.base <- ggtree(phylo) + theme_tree2()
plot1 <- revts(plot.base) + scale_x_continuous(labels=abs)


PR.PG <- plot1 + geom_hilight(data = df.phylo, aes(node = node.number, fill = Fam.names), 
                              alpha = .6) +
  scale_fill_viridis(discrete = T, name = "Family names")


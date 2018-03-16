
# Andrew & Regis
# Nov 2017
# Doing an adonis analysis of the positions of the animals on the first princiapal component axes


# load data ---------------------------------------------------------------


library(fwdata)
library(plotly)
library(tidyverse)
library(vegan)



# define functions --------------------------------------------------------

calculate_adonis_taxo_level <- function(taxon, .taxa_below_taxon, .first_four_axes){
  
  # join the taxa with enough information for an adonis with the first four PCAs
  below_taxon_complete <- .taxa_below_taxon %>% 
    select(species_id, taxon_name, taxon_level, family, ord) %>% 
    left_join(.first_four_axes) %>% 
    drop_na(Axis.1)
  
  # axis scores
  axis_values <- as.matrix(below_taxon_complete[,c("Axis.1", "Axis.2", "Axis.3", "Axis.4")])
  
  ff <- sprintf("axis_values ~ %s", taxon)
  res <- adonis(as.formula(ff), data = below_taxon_complete, method = "euclidian")

  return(res)
  
}

# load the full dataset This is the very last version of the PCA  -- the species
# scores on axes 1 and 4 of the PCA according to the most recent data version.
axes.raw <- read.table("Current Results - 0.7.7/RES_pca_individuals_0.7.7.txt",
                       header = TRUE, row.names = 1)

fwd <- fw_data("0.7.7")
str(fwd, max.level = 1)


first_four_axes <- axes.raw %>% 
  rownames_to_column("species_id") %>% 
  select(species_id, Axis.1:Axis.4)

# put in the taxonomic information

# first filter for all taxa identified to the level of below family: everything
# which was either genus or species

glimpse(fwd$traits)

# each taxonomic level has a little number to make it easy to filter by "rank of
# taxonomic group"


# hey so what is the distribution of lowest taxonomic levels anyway?

fwd$traits %>%
  group_by(taxon_number, taxon_level) %>% 
  tally %>% 
  arrange(taxon_number)


# Give me every morphospecies which was identified below family 
taxa_below_family <- fwd$traits %>% 
  select(species_id:taxon_number) %>% 
  filter(!(taxon_number <= 9))



# to conduct an adonis we need to put this info together with the axis scores:


taxa_below_family %>% 
  select(species_id, taxon_name, taxon_level, family) %>% 
  left_join(first_four_axes) %>% 
  ggplot(aes(x  = Axis.1, y = Axis.2, colour = family))+ geom_point() + guides(colour = FALSE)

# # Why is there missing data? 
# at_least_to_genera %>% 
#   select(species_id, taxon_name, taxon_level, genus) %>% 
#   left_join(first_four_axes) %>% 
#   visdat::vis_miss(.)
# 
# # Who are these sad animals? 
# at_least_to_genera %>% 
#   select(species_id, taxon_name, taxon_level, genus) %>% 
#   left_join(first_four_axes) %>% 
#   filter(is.na(Axis.1))

genus_adonis <- calculate_adonis_taxo_level("family", taxa_below_family, first_four_axes)

summary(genus_adonis)
genus_adonis



# order level -------------------------------------------------------------


# Give me every morphospecies which was identified below order
taxa_below_order <- fwd$traits %>% 
  select(species_id:taxon_number) %>% 
  filter(!(taxon_number <= 8 ))

order_adonis <- calculate_adonis_taxo_level("ord", taxa_below_order, first_four_axes)

summary(order_adonis)
order_adonis




# 3D plots of the dots ----------------------------------------------------

family_axis <- taxa_below_family %>% 
  select(species_id, taxon_name, taxon_level, family) %>% 
  left_join(first_four_axes)



family_axis %>% glimpse %>% 
  plot_ly(x = ~ Axis.1, y = ~Axis.2, z = ~Axis.3) %>% 
  add_markers(color = ~family)


family_axis %>% glimpse %>% 
  plot_ly(x = ~ Axis.2, y = ~Axis.3, z = ~Axis.4) %>% 
  add_markers(color = ~family)

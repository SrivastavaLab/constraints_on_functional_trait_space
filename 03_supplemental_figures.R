
## load libraries and data

library(fwdata)
library(dplyr)
library(ggplot2)
library(ggmap)
library(ggrepel)

# source("PCA_with_NA.R")

library(fwdata)
fw_versions(local = TRUE)
fwd <- fw_data("0.7.7")


SAm <- c(left = -120, bottom = -56, right = -34, top = 30)
# sa_map <- get_stamenmap(SAm, zoom = 3, maptype = "toner")


sa_map <- readRDS("../CESAB-detritivore-predator-dropbox/analysis_output/sa_stamenmap.rds")

sa_map %>% 
  ggmap() + 
  geom_count(data = fwd$visits %>% 
               select(lat = latitude, lon =longitude),
             pch = 21, fill = "red", alpha = 0.8)

ggsave("supplemental_figures/map_number_of_visits.png")

# change scale of size to be larger


sa_map %>% 
  ggmap() + 
  geom_bin2d(data = fwd$visits %>% 
               select(lat = latitude, lon =longitude), binwidth = c(1, 1)) + 
  scale_fill_continuous(high = "red")



# taxa ranked by their number of morphospecies ----------------------------

glimpse(fwd$traits)

# list of species ids in each dataset:

taxa_by_site <- fwd$abundance %>% 
  select(dataset_id, species_id, bwg_name) %>% distinct %>% 
  left_join(fwd$traits %>% select(species_id, taxon_level)) %>% 
  group_by(dataset_id, taxon_level) %>% 
  tally %>% 
  mutate(n = n / sum(n))

taxa_by_site$taxon_level %>% unique %>% dput


taxa_num <- 1:11
names(taxa_num) <- c("species_name","genus", "tribe", "subfamily", "family","subord", "ord", "subclass", "class","phylum", 
    NA) 
taxa_num

taxa_by_site %>% 
  mutate(taxa_num = taxa_num[taxon_level]) %>% 
  ggplot(aes(x = taxa_num, y = n, group = dataset_id)) + geom_line()

library(ggjoy)

taxa_by_site %>% 
  ungroup %>% 
  left_join(fwd$datasets %>% select(dataset_id, name)) %>% 
  mutate(taxa_num = taxa_num[taxon_level],
         taxon_ord = forcats::fct_reorder(taxon_level, taxa_num)) %>% 
  ggplot(aes(x = taxon_ord, height = n,y = name, group = dataset_id)) + 
  geom_ridgeline(scale = 3.2, alpha = 0.6) +
  theme_minimal(base_size = 14) +
  theme(axis.text.y = element_text(vjust = 0),
        axis.text.x = element_text(angle = -45, hjust = 0)) 

ggsave("supplemental_figures/taxonomic_identifications.png")

# table of datasets -------------------------------------------------------

# main characteristics of sampling sites (location, environment) and sampled bromeliads (species identity, sampling effort)

fwd$datasets %>% 
  select(country, `field site` = location, year)

# bromeliads sampling and species identity
library(tidyr)
library(purrr)
fwd$bromeliads %>% group_by(visit_id) %>% 
  nest %>% 
  mutate(start_date = map_chr(data, ~.x$collection_date %>% min(na.rm = TRUE) %>% as.character),
         end_date = map_chr(data, ~.x$collection_date %>% max(na.rm = TRUE) %>% as.character),
         n_broms = map_dbl(data, nrow),
         brom_spp = map_chr(data, ~ unique(.x$species) %>% paste0(collapse = "; "))) %>% 
  # drop the `data` column
  keep(is_atomic) %>% 
  left_join(fwd$visits %>% select(visit_id, latitude, longitude)) %>% 
  readr::write_csv("supplemental_figures/visit_information_table.csv")


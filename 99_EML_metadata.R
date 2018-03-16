# writing EML for the trait table

library(EML)
library(tibble)
library(fwdata)
library(dplyr)

# first, I need to find the thing I'm actually working on! What precisely am I documenting??

latest_data <- fw_data("0.7.7")

# take the minimum taxonomic columns we need, the species_id, and the first few traits to document.
# convert the trait values to characters
trait_data <- latest_data$traits %>% 
  select(species_id, taxon_name, taxon_number, AS1:BF4) %>% 
  mutate(taxon_number = as.integer(taxon_number)) %>% 
  mutate_at(.vars = vars(AS1:BF4), .funs = funs(as.character))

readr::write_csv(trait_data, "Current Results - 0.7.7/trait_data.csv")

# set it as physical
physical <- set_physical("Current Results - 0.7.7/trait_data.csv")

# an important question -- should it be treated as a factor, with the "0" and
# "1" defined as levels? I actually think this would be easier! Otherwise I would have to create a new kind of "unit". 

attributes <- frame_data(
  ~ attributeName,  ~ unit,              ~ numberType, ~ attributeDefinition,                                                 ~ definition, 
  "species_id",     "dimensionless",     NA,           "Unique id for every species",                                         "Species ID code",       
  "taxon_name",     NA,                  NA,           "Taxonomic name of species",                                           "Taxonomic category",       
  "taxon_number",   "dimensionless",     "real",       "Rank of taxonomic category relative to others in this dataset",       "Rank of category",         
  "AS1",            "dimensionless",     NA,           "aquatic as egg",                                                      "aquatic as egg",
  "AS2",            "dimensionless",     NA,           "aquatic as larva",                                                    "aquatic as larva",
  "AS3",            "dimensionless",     NA,           "aquatic as nymph",                                                    "aquatic as nymph",
  "AS4",            "dimensionless",     NA,           "aquatic as adult",                                                    "aquatic as adult",
  "BS1",            NA,                  NA,           "Maximum body size <= 0.25 cm",                                        "Maximum body size",
  "BS2",            NA,                  NA,           "Maximum body size 0.25-0.5 cm",                                       "Maximum body size",
  "BS3",            NA,                  NA,           "Maximum body size 0.5-1 cm",                                          "Maximum body size",
  "BS4",            NA,                  NA,           "Maximum body size 1-2 cm",                                            "Maximum body size",
  "BS5",            NA,                  NA,           "Maximum body size >2 cm",                                             "Maximum body size",
  "DM1",            NA,                  NA,           "Dispersal: passive",                                                  "Dispersal mode",
  "DM2",            NA,                  NA,           "Dispersal: active",                                                   "Dispersal mode",
  "FD1",            NA,                  NA,           "microorganisms" ,                                                     "Food",
  "FD2",            NA,                  NA,           "detritus <1mm",                                                       "Food",
  "FD3",            NA,                  NA,           "dead plant litter",                                                   "Food",
  "FD4",            NA,                  NA,           "living microphytes",                                                  "Food",
  "FD5",            NA,                  NA,           "living leaf tissue",                                                  "Food",
  "FD6",            NA,                  NA,           "dead animals < 1mm",                                                  "Food",
  "FD7",            NA,                  NA,           "living microinvertebrates",                                           "Food",
  "FD8",            NA,                  NA,           "living macroinvertebrates",                                           "Food",
  "FG1",            NA,                  NA,           "deposit feeder" ,                                                     "Feeding group",
  "FG2",            NA,                  NA,           "shredder" ,                                                           "Feeding group",
  "FG3",            NA,                  NA,           "scraper",                                                             "Feeding group",
  "FG4",            NA,                  NA,           "filter-feeder",                                                       "Feeding group",
  "FG5",            NA,                  NA,           "piercer",                                                             "Feeding group",
  "FG6",            NA,                  NA,           "predator",                                                            "Feeding group",
  "RE1",            NA,                  NA,           "Ovoviviparity",                                                       "Reproduction",
  "RE2",            NA,                  NA,           "isolated eggs, free",                                                 "Reproduction",
  "RE3",            NA,                  NA,           "isolated eggs, cemented",                                             "Reproduction",
  "RE4",            NA,                  NA,           "clutches, cemented" ,                                                 "Reproduction",
  "RE5",            NA,                  NA,           "clutches, free" ,                                                     "Reproduction",
  "RE6",            NA,                  NA,           "clutches, in vegetation",                                             "Reproduction",
  "RE7",            NA,                  NA,           "clutches, terrestrial",                                               "Reproduction",
  "RE8",            NA,                  NA,           "asexual reproduction",                                                "Reproduction",
  "RF1",            NA,                  NA,           "eggs, statoblasts",                                                   "Resistance form",
  "RF2",            NA,                  NA,           "cocoons",                                                             "Resistance form",
  "RF3",            NA,                  NA,           "diapause or dormancy",                                                "Resistance form",
  "RF4",            NA,                  NA,           "none",                                                                "Resistance form",
  "LO1",            NA,                  NA,           "flight" ,                                                             "Locomotion",
  "LO2",            NA,                  NA,           "surface swimmer" ,                                                    "Locomotion",
  "LO3",            NA,                  NA,           "full water swimmer",                                                  "Locomotion",
  "LO4",            NA,                  NA,           "crawler",                                                             "Locomotion",
  "LO5",            NA,                  NA,           "burrower",                                                            "Locomotion",
  "LO6",            NA,                  NA,           "interstitial",                                                        "Locomotion",
  "LO7",            NA,                  NA,           "tube builder",                                                        "Locomotion",
  "RM1",            NA,                  NA,           "integument",                                                          "Respiration mode",
  "RM2",            NA,                  NA,           "gill",                                                                "Respiration mode",
  "RM3",            NA,                  NA,           "plastron",                                                            "Respiration mode",
  "RM4",            NA,                  NA,           "siphon or spiracle" ,                                                 "Respiration mode",
  "RM5",            NA,                  NA,           "hydrostatic vesicle",                                                 "Respiration mode",
  "MD1",            NA,                  NA,           "none",                                                                "Morphological defense",
  "MD2",            NA,                  NA,           "elongate tubercle",                                                   "Morphological defense",
  "MD3",            NA,                  NA,           "hairs",                                                               "Morphological defense",
  "MD4",            NA,                  NA,           "sclerotized spines",                                                  "Morphological defense",
  "MD5",            NA,                  NA,           "dorsal plates",                                                       "Morphological defense",
  "MD6",            NA,                  NA,           "sclerotized exoskeleton",                                             "Morphological defense",
  "MD7",            NA,                  NA,           "shell" ,                                                              "Morphological defense",
  "MD8",            NA,                  NA,           "case or tube" ,                                                       "Morphological defense",
  "CP1",            NA,                  NA,           "less than 21 days",                                                   "Cohort production interval",
  "CP2",            NA,                  NA,           "21 to 60 days",                                                       "Cohort production interval",
  "CP3",            NA,                  NA,           "more than 60 days",                                                   "Cohort production interval",
  "BF1",            NA,                  NA,           "flat elongate",                                                       "Body form",
  "BF2",            NA,                  NA,           "flat ovoid",                                                          "Body form",
  "BF3",            NA,                  NA,           "cylindrical elongate",                                                "Body form",
  "BF4",            NA,                  NA,            "cylindrical ovoid",                                                  "Body form"
  )            

fuzzy_factor <- frame_data(
  ~code, ~definition,
  "0",   "no affinity",
  "1",   "weak affinity",
  "2",   "moderate affinity",
  "3",   "strong affinity"
)

factors <- expand.grid(
  attributeName = c("AS1", "AS2", "AS3", "AS4", "BS1", "BS2", "BS3", "BS4", "BS5", "DM1", "DM2", "FD1", 
                    "FD2", "FD3", "FD4", "FD5", "FD6", "FD7", "FD8", "FG1", "FG2", 
                    "FG3", "FG4", "FG5", "FG6", "RE1", "RE2", "RE3", "RE4", "RE5", 
                    "RE6", "RE7", "RE8", "RF1", "RF2", "RF3", "RF4", "LO1", "LO2", 
                    "LO3", "LO4", "LO5", "LO6", "LO7", "RM1", "RM2", "RM3", "RM4", 
                    "RM5", "MD1", "MD2", "MD3", "MD4", "MD5", "MD6", "MD7", "MD8", 
                    "CP1", "CP2", "CP3", "BF1", "BF2", "BF3", "BF4"),
  code = c("0", "1", "2", "3"),
  stringsAsFactors = FALSE
) %>% 
  arrange(attributeName) %>% 
  left_join(fuzzy_factor, by = "code")

# create attributes
attributeList <- set_attributes(attributes = attributes, factors = factors,
                                col_classes = c("character", "character", "numeric", 
                                                "factor", "factor", "factor", "factor",
                                                "factor", "factor", "factor", "factor",
                                                "factor", "factor", "factor", "factor",
                                                "factor", "factor", "factor", "factor",
                                                "factor", "factor", "factor", "factor",
                                                "factor", "factor", "factor", "factor",
                                                "factor", "factor", "factor", "factor",
                                                "factor", "factor", "factor", "factor",
                                                "factor", "factor", "factor", "factor",
                                                "factor", "factor", "factor", "factor",
                                                "factor", "factor", "factor", "factor",
                                                "factor", "factor", "factor", "factor",
                                                "factor", "factor", "factor", "factor",
                                                "factor", "factor", "factor", "factor",
                                                "factor", "factor", "factor", "factor",
                                                "factor", "factor", "factor", "factor"))


# create dataTable

dataTable <- new("dataTable",
                 entityName = "test_trait_data.csv",
                 entityDescription = "Fuzzy traits of bromeliad insects",
                 physical = physical,
                 attributeList = attributeList)


# coverage ----------------------------------------------------------------

# taxonomic coverage could be a challenge!
?set_taxonomicCoverage
# according to that i might actually be able to use the unique taxonomy of the
# species (from the "pretty taxonomy" of Regis) to create all this.
latlong <- latest_data$visits %>% 
  select(latitude, longitude) %>% 
  distinct

minlat <- min(latlong$latitude, na.rm = TRUE)
maxlat <- max(latlong$latitude, na.rm = TRUE)
minlon <- min(latlong$longitude, na.rm = TRUE)
maxlon <- max(latlong$longitude, na.rm = TRUE)

latest_data$visits %>% select(contains("elevation")) %>% distinct %>% 
{c(min(.$min_elevation, na.rm = TRUE), max(.$max_elevation, na.rm = TRUE))}

# need to get the pretty taxonomy -- apparently in table S2
library(readxl)
pretty_taxonomy <- read_excel("Manuscript/Supplementary Table S2.xlsx", skip = 3, na = "NA")

unique_pretty <- pretty_taxonomy %>% select(domain:species, taxon_name) %>% distinct

# reconfigure to match the example

names(unique_pretty)
library(purrr)
library(stringr)

# just cosmetic -- changing the column names to match the spelling int he
# examples, and also replacing underscores with spaces in taxon_name.
unique_species <- unique_pretty %>% 
  select(kingdom, phylum, class, order = ord, family, genus, taxon_name) %>% 
  {set_names(., toupper(names(.)))} %>% 
  rename(genusSpecies = TAXON_NAME) %>% 
  mutate(genusSpecies = if_else(str_detect(genusSpecies, "_"), true = str_replace(genusSpecies, "_", " "), false = NA_character_))
# Also dropping species names for any group which has no info at that level



# save these to objects and add to coverage

geographicDescription <- "These insects were collected by scientists studying bromeliads in a wide variety of habitats. This dataset does not contain any spatially explict observations. Geographic coverage includes everywhere that animals were collected."

# date coverage = approximate date of CESAB funding
coverage <- 
  set_coverage(begin = '2015-11-30', end = '2018-11-01',
               sci_names = as.data.frame(unique_species),
               geographicDescription = geographicDescription,
               west = minlon, east = maxlon, 
               north = maxlat, south = minlat,
               altitudeMin = 0, altitudeMaximum = 3150,
               altitudeUnits = "meter")


# methods -----------------------------------------------------------------

methods <- set_methods("Manuscript/fuzzy_code_methods.md")


# people ------------------------------------------------------------------

# the example
# "Aaron Ellison <fakeaddress@email.com> [cre]"

regis_per <- as.person("Régis Céréghino <regis.cereghino@univ-tlse3.fr> [cre]")
regis <- as(regis_per, "creator")

others <- c(
  as.person("Valério D. Pillar [ctb]"),
  as.person("Diane S. Srivastava [ctb]"),
  as.person("Paula M. Omena [ctb]"),
  as.person("A. Andrew M. MacDonald [ctb]"),
  as.person("Ignacio M. Barberis [ctb]"),
  as.person("Bruno Corbara [ctb]"),
  as.person("L. Melissa Guzman [ctb]"),
  as.person("Céline Leroy [ctb]"),
  as.person("Fabiola Ospina Bautista [ctb]"),
  as.person("Gustavo Q. Romero [ctb]"),
  as.person("M. Kurtis Trzcinski [ctb]"),
  as.person("Pavel Kratina [ctb]"),
  as.person("Vanderlei J. Debastiani [ctb]"),
  as.person("Ana Z. Gonçalves [ctb]"),
  as.person("Nicholas A.C. Marino [ctb]"),
  as.person("Vinicius F. Farjalla [ctb]"),
  as.person("Barbara A. Richardson [ctb]"),
  as.person("Michael J. Richardson [ctb]"),
  as.person("Olivier Dézerald [ctb]"),
  as.person("Benjamin Gilbert [ctb]"),
  as.person("Jana Petermann [ctb]"),
  as.person("Stanislas Talaga [ctb]"),
  as.person("Gustavo C. O. Piccoli [ctb]"),
  as.person("Merlijin Jocqué [ctb]"),
  as.person("Guillermo Monter [ctb]")
)

associatedParty <- as(others, "associatedParty")

ECOLAB_address <- new("address",
                  deliveryPoint = "118 route de Narbonne, ECOLAB, Université de Toulouse",
                  city = "Toulouse",
                  administrativeArea = "Occitanie",
                  postalCode = "31062",
                  country = "France")

contact <- 
  new("contact",
      individualName = regis@individualName,
      electronicMail = regis@electronicMailAddress,
      address = ECOLAB_address,
      organizationName = "Ecolab",
      phone = "33 5 61 55 84 36")

publisher <- new("publisher",
                 organizationName = "CESAB FUNCTIONALWEBS",
                 address = ECOLAB_address)

# keywords ----------------------------------------------------------------

keywordSet <-
  new("keywordSet",
      keywordThesaurus = "T-SITA Thesaurus for Soil Invertebrate Trait-based Approaches",
      keyword = c("Invertebrate",
                  "Morphology",
                  "Behaviour",
                  "Protection"))

# other information -------------------------------------------------------

pubDate <- "2012" 

title <- "Constraints on the functional trait space of aquatic invertebrates in bromeliads"

abstract <- "We examined the ecological strategies and constraints underlying the realized trait space of aquatic invertebrates,
using data on 12 functional traits of 852 taxa collected in tank bromeliads from Mexico to Argentina.
The major axes of trait variation represented life history strategies optimizing resource use,
and anti-predator adaptations.
There was evidence for trophic, habitat, defence and life history niche axes.
Bromeliad invertebrates only occupied 17-24% of the potential space within these dimensions,
due to greater concentrations than predicted under uniform or normal distributions. 
Trait combinations aggregated taxa by family and then by order, suggesting that niche conservatism 
is a widespread mechanism in the diversification of ecological strategies. "  

intellectualRights <- "This dataset is released to the public and may be freely
downloaded. Please keep the designated Contact person informed of any
plans to use the dataset. Consultation or collaboration with the original
investigators is strongly encouraged. Publications and data products
that make use of the dataset must include proper acknowledgement. For
more information on the Bromeliad Working Group data access, and for other data about this fauna, please
see: www.zoology.ubc.ca/~srivast/bwg"



# les petits oignons -------------------------------------------------------

dataset <- new("dataset",
               title = title,
               creator = regis,
               pubDate = pubDate,
               intellectualRights = intellectualRights,
               abstract = abstract,
               associatedParty = associatedParty,
               keywordSet = keywordSet,
               coverage = coverage,
               contact = contact,
               methods = methods,
               dataTable = dataTable)

eml <- new("eml",
           packageId = "b0d4f447-18f5-4872-9dbd-f14389f9b91d",  # from ,
           system = "uuid", # type of identifier
           dataset = dataset)
write_eml(eml, "eml.xml")
eml_validate("eml.xml")

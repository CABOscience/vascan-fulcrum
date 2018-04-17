### Script extract VASCAN data and re-arrange
# Etienne Laliberte, 2018-04-17
# modify VASCAN to import into Fulcrum


# load libraries
library(dplyr)

# read vascan data
taxon <- read.delim('dwca-vascan-v37/taxon.txt')
description <- read.delim('dwca-vascan-v37/description.txt')
distribution <- read.delim('dwca-vascan-v37/distribution.txt')
resourcerelationship <- read.delim('dwca-vascan-v37/resourcerelationship.txt')
vernacularname <- read.delim('dwca-vascan-v37/vernacularname.txt')

# only keep a subset of taxon
taxon.sub <- taxon %>%
  filter(taxonomicStatus == 'accepted',
         !taxonRank %in% c('class', 'order', 'section', 'series', 'subclass', 'subgenus', 'subsection', 'subtribe', 'superorder'))

# check number of rows
nrow(taxon.sub) # 9643... max 10,000 records per import in Fulcrum 

# check that id = taxonID across all records
all(taxon.sub$id == taxon.sub$taxonID)

# check number of rows of vernacular names
nrow(vernacularname)

# extract french vernacular names
verna.sub.fr <- vernacularname %>%
  filter(isPreferredName == 'true',
         language == 'FR') %>%
  semi_join(taxon.sub, by = 'id') %>%
  rename(taxonID = id,
         vernaculaNameFR = vernacularName) %>%
  select(taxonID, vernaculaNameFR)
nrow(verna.sub.fr) # 7291

# extract english vernacular names
verna.sub.en <- vernacularname %>%
  filter(isPreferredName == 'true',
         language == 'EN') %>%
  semi_join(taxon.sub, by = 'id') %>%
  rename(taxonID = id,
         vernaculaNameEN = vernacularName) %>%
  select(taxonID, vernaculaNameEN)
nrow(verna.sub.en) # 7768

# extract habit
description.sub <- description %>%
  semi_join(taxon.sub, by = 'id') %>%
  rename(taxonID = id)

# get subset of distribution
distribution.sub <- distribution %>%
  semi_join(taxon.sub, by = 'id') %>%
  rename(taxonID = id)

# collapse distribution by taxon ID
distribution.collapsed <- distribution.sub %>%
  group_by(taxonID) %>%
  summarise(localities = toString(unique(locality)))


# add French and English vernacular names, and description
taxon.sub2 <- taxon.sub %>%
  left_join(verna.sub.en, by = 'taxonID') %>%
  left_join(verna.sub.fr, by = 'taxonID') %>%
  left_join(description.sub, by = 'taxonID') %>%
  left_join(distribution.collapsed, by = 'taxonID') %>%
  mutate(VASCAN_version = 37)

# create csv
write.csv(taxon.sub2, file = 'vascan_taxon_fulcrum_v37.csv', row.names = F,
          na = '')

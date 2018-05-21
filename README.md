# london_postcode
postcode variables (and shape) to match with rightmove

# Setup
setwd('~/R/gis-london/maps-in-R')
x = c("ggmap", "rgdal", "rgeos", "maptools", "dplyr", "tidyr", "tmap", "stringr")
# install.packages(x)
lapply(x, library, character.only = TRUE)   

# Load data
london = readOGR(dsn = "postcode_data/Districts.shp")          # http://www.opendoorlogistics.com/downloads/
head(london@data)                                              # summary of first two data points 
sapply(london@data, class)                                     # class of data/column

london_dat = read.csv("postcode_data/london-postcodes.csv", 
                      stringsAsFactors = FALSE)                # https://www.doogal.co.uk/london_postcodes.php
names(london_dat) = tolower(names(london_dat))

# Total pop
london_dat %>% 
  select(population) %>% 
  na.omit() %>%
  sum()

# Aggregate data by right move pc
london_dat$postcode_3d = str_split_fixed(london_dat$postcode, " ", 2)[,1]  # create post to match rightmove
london_dat$population[is.na(london_dat$population)] <- 0                   # na to 0
london_dat$households[is.na(london_dat$households)] <- 0                   # na to 0

pc_pop = london_dat %>% 
  group_by(postcode_3d) %>%
  summarise(pop = sum(population))                                         # pop by pc

pc_hh = london_dat %>% 
  group_by(postcode_3d) %>%
  summarise(hh_count = sum(households))                                    # hh count by pc

pc_zones = london_dat %>% 
  group_by(postcode_3d) %>% 
  count(london.zone) %>% 
  mutate(prop = prop.table(n)) %>%
  select(-n) %>% spread(london.zone, prop) %>% 
  mutate_all(funs(replace(., is.na(.), 0)))                                # % of 6D_pcs in each zone

pc_deprivation = london_dat %>% 
  group_by(postcode_3d) %>% 
  summarise(deprivation = mean(index.of.multiple.deprivation))             # multiple deprivation index by zone

all_pc_dat = plyr::join_all(list(pc_pop, 
                              pc_hh, 
                              pc_deprivation, 
                              pc_zones), by='postcode_3d', type='left')    # merge data

all_pc_dat = all_pc_dat %>% 
  select(name = postcode_3d, everything())                                 # rename postcode column

# Join shapefile & data
london@data = data.frame(london@data, all_pc_dat[match(london@data[, "name"],
                                                        all_pc_dat[, "name"]), ])
london@data = london@data %>% na.omit()

# check data
head(london@data)
sapply(london@data, class)
london@proj4string
plot(london)

sel = london$hh_count > 10

plot(london[sel, ])

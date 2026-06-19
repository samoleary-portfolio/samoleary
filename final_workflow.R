library(tidycensus)
library(tidyverse)
library(sf)
library(tigris)
library(purrr)
library(igraph)
library(tmap)
#######Data Aquisition######
usc_raw<- get_acs(
  geography = "county",
  variables = c(population = "B01003_001",
                commuting = "B08015_001"),  # Total population
  year = 2023,
  survey = "acs5",
  geometry = TRUE,
  progress_bar = FALSE,
  output = "wide",
  
)
cz_shapes <- st_read("cz_shapes.shp")

msa_raw<- get_acs(
  geography = "metropolitan statistical area/micropolitan statistical area",
  variables = c(population = "B01003_001"),
  # Total population
  year = 2023,
  survey = "acs5",
  geometry = TRUE,
  progress_bar = FALSE,
  output = "wide"
)

######Initial filters and analysis######
usc_raw <- usc_raw %>% mutate(micro  = populationE > 50000)
usc <- usc_raw %>% filter(populationE > 50000)

# county analysis for adjacency to areas greater than 500,000 ( significant us cities) 
big_areas_c <- usc %>% filter(populationE >= 500000)
neighbors_c <- st_touches(usc, big_areas_c)
noborderc <-  usc %>% mutate(keep = populationE >= 500000 | lengths(neighbors_c) > 0)


#eliminate rest and hawaii
uscMAP <- noborderc %>% 
  filter(keep == TRUE) %>%
  filter(GEOID != 15003)

######Centroid Model#######
#centroids of uscMAP
uscMAP_centroids <-  uscMAP%>% st_centroid(uscMAP)
usc_centroids <- st_centroid(usc_raw)

#identify the CZs which contain a centroid  of a county in uscMAP
present_czs <- st_contains(cz_shapes,uscMAP_centroids)

#eliminate those that don't 
cent_cz_shapes <- cz_shapes %>% mutate(keep = lengths(present_czs) > 0) 
tr_cz_shapes <- cent_cz_shapes %>% filter(keep == TRUE)

usc_centroids_tr <- usc_centroids %>% # spatial join of usc_centroids and cz_shapes
  st_join(tr_cz_shapes, join = st_within)
ctr_in <- st_within(usc_centroids_tr,tr_cz_shapes)
usc_centroids_tr1 <-  usc_centroids_tr  %>%
  mutate(contained = lengths(ctr_in)> 0)

uscMAP_cent <- st_join(usc_raw,usc_centroids_tr,st_within, left = TRUE)

intersections <- st_intersects(usc_raw, usc_centroids_tr1) # preserves binaries in the spatial join 
uscMAP_cent$contained_a <- sapply(intersections, function(idx) {
  any(usc_centroids_tr1$contained[idx] == 1)
})
uscMAP_cent_raw <- uscMAP_cent %>% filter(contained_a == TRUE)

######Cleaning up Small Areas#####
big_areas_c <- usc_raw %>% filter(populationE >= 500000)

c_big_areas_c <- usc_raw %>% filter(populationE >= 500000)
c_neighbors_c <- st_touches(uscMAP_cent_raw, big_areas_c)
uscMAP_cent <-  uscMAP_cent_raw %>% mutate(keep = populationE.x >= 500000 | lengths(c_neighbors_c) > 0)

small_area_c<- uscMAP_cent_raw %>% filter(populationE.x <= 250000)
s_neighbors_c <- st_touches(uscMAP_cent_raw, small_area_c)

uscMAP_cent <-  uscMAP_cent %>% mutate(small = ((keep = FALSE) | lengths(s_neighbors_c)>1) & populationE.x < 75000)
uscMAP_cent <- uscMAP_cent %>% filter(small == FALSE)
tmap_mode("view")
tm_shape(uscMAP_cent) +
  tm_fill(col = "populationE.x",
          breaks = c(0, 250000, 500000, 1000000, 2000000, 4000000, 8000000),
          labels = c("<250k", "250k–500k", "500k–1M", "1M–2M", "2M–4M", "4M-8M"),
          palette = "Blues")

######Cleaning up Large Areas####
county_area<- st_area(uscMAP_cent$geometry)
uscMAP_cent <- uscMAP_cent%>%
  mutate(area = county_area)
uscMAP_cent <-uscMAP_cent %>%
  mutate(areaKM = area/1000000) 
uscMAP_cent <-uscMAP_cent %>%
  mutate(areaKM = as.numeric(areaKM))%>%
  mutate(popdens_km = populationE.x/areaKM)

#isolates the top 10% of counties by area 
uscMAP_cent <- uscMAP_cent %>%
  mutate(area_bool = area >= (quantile(area, .95)))


# reasoning: low population density and large size interfering with analyis
mega_counties <- uscMAP_cent %>%
  filter(GEOID.x != "06071",
         GEOID.x != "32023",
         GEOID.x != "06027",
         GEOID.x != "06065",
         GEOID.x != "04012",
         GEOID.x != "06025",
         GEOID.x != "04027",
         GEOID.x != "06051",
         GEOID.x != "04015",
         GEOID.x != "53007",
         GEOID.x != "53077"
  )



######Dissolving County Geometry#####
mega_id <- mega_counties %>%
  mutate(
    # adjacency list: which other counties touch each one
    neighbors = st_touches(geometry),
    
    # build graph + find connected components
    group_id = {
      g <- graph_from_adj_list(neighbors)
      comps <- components(g)$membership
      comps
    }
  )
# need to drop column 
mega_id$neighbors <-NULL

mega_shapes <- mega_id %>%
  group_by(group_id) %>%
  summarise(
    # sum any numeric columns you want
    population = sum(populationE.x, na.rm = TRUE),
    # combine geometries into one polygon per CZ
    geometry = st_union(geometry),
    .groups = "drop"
  )

mega_shapes <- mega_shapes %>%filter(population >1500000)
######Adjacency to Megas#########
usc_borders <- st_make_valid(usc_raw)
mega_shapes <- st_make_valid(mega_shapes)
#checks for borders 
county_mega_adjacency <- st_touches(usc_borders, mega_shapes)
usc_mega_adjacency <- usc_raw%>%
  mutate(border_mega = lengths(county_mega_adjacency)>0) %>%
  mutate(more_mega = lengths(county_mega_adjacency)>=1)




######MSA Analysis#####

msa <- msa_raw %>%
  filter(populationE >= 50000)

msa_centroid <- st_centroid(msa)

msa_in_mega <-st_contains(mega_shapes, msa_centroid)

#ensures mega's have more than 1 MSA 
mega_shapes <- mega_shapes%>%
  mutate(n_msa = lengths(msa_in_mega) >1)

#counties in msa
county_in_msa<- st_within(usc_centroids,msa)
msa_id <- usc_raw %>% mutate(msa_bool = lengths(county_in_msa)>0)
#need to create pop_bool, an aggreagate polycentric check, and 
mega_shapes <- mega_shapes%>%  #maybe on mega id ?
  mutate(pop_bool = population >  (quantile(population, .5))) %>%
  mutate(poly_bool = pop_bool == FALSE | n_msa == FALSE) %>%
  mutate(mega_bool = poly_bool == FALSE)
######Creation of a united county dataset with all variables#####

mega_id<- st_drop_geometry(mega_id)
mega_shapes <- st_drop_geometry(mega_shapes)
mega_id <- mega_id %>%
  rename("GEOID" = "GEOID.x")

usc_mega_indication <- left_join(usc_raw, mega_id, by = "GEOID")
usc_mega_indication <- usc_mega_indication %>% mutate(group_id =
                                                        ifelse(is.na(group_id), 0, group_id))
reg_step <- left_join( usc_mega_indication,mega_shapes, by = "group_id" )
reg_step <- reg_step %>%mutate(n_msa = case_when(n_msa = is.na(n_msa) ~ FALSE, TRUE ~ n_msa))
reg_step <- reg_step %>%mutate(area_bool = case_when(area_bool = is.na(area_bool) ~ FALSE, TRUE ~ area_bool))
reg_step <- reg_step %>%mutate(mega_bool = case_when(mega_bool = is.na(mega_bool) ~ FALSE, TRUE ~ mega_bool))
reg_step <- reg_step %>%mutate(pop_bool = case_when(pop_bool = is.na(pop_bool) ~ FALSE, TRUE ~ pop_bool))
mega_regression_raw <- reg_step %>%mutate(poly_bool = case_when(poly_bool = is.na(poly_bool) ~ FALSE, TRUE ~ poly_bool))
msa_id<- st_drop_geometry(msa_id)


mega_regression_raw <- left_join(reg_step,msa_id, by = "GEOID")
mega_regression_raw <- mega_regression_raw %>%mutate(poly_bool = case_when(poly_bool = is.na(poly_bool) ~ FALSE, TRUE ~ poly_bool))



usc_mega_adjacency<- st_drop_geometry(usc_mega_adjacency)

mega_regression_raw <- left_join(mega_regression_raw, usc_mega_adjacency, by = "GEOID")

#clean that shit 

test <- function(mega_regression_raw) {
  mega_regression_raw[, colSums(is.na(mega_regression_raw)) == 0]
}
mega_regression_raw <- test(mega_regression_raw)
mega_regression_raw <- mega_regression_raw%>%
  mutate(NAME.y.y = NULL) %>%
  mutate(populationE.y.y = NULL) %>%
  mutate(commutingE.x = NULL) %>%
  mutate(populationE.x = NULL) %>%
  mutate(commutingM.x = NULL) %>%
  mutate(commutingE.y.y = NULL) %>%
  mutate(commutingM.y.y = NULL) %>%
  mutate(commutingM = NULL) %>%
  mutate(NAME.x = NULL ) %>%
  mutate(micro.y.y= NULL )%>%
  mutate(micro.x = NULL )
#####Writing files####

st_write(mega_regression_raw,"mega_regression_raw_sc.gpkg") 
st_write(mega_shapes, "final_mega_shapes_sc.gpkg")


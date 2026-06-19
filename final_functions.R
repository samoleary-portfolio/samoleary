library(methods)
install.packages("R6")
library(R6)
library(tidycensus)
library(sf)
library(tidyverse)
library(tmap)
library(igraph)

#(done in functions_2 file)need function that checks environment for files, and runs the tidycensus functions if not to save time
#   added steps to the aquire census, clean_cz and verify_mega function
# self$ is currently before all the things it needs to be before, but I think i need to do some 
#intitialization to fix it. 
Megapolitan <- R6Class("Megapolitan",public = list(
  # acquires county census population for a given year 
  acquire_census = function(surv_year){
    ver1 <- exists("usc_raww")
    if(ver1 == FALSE){
      usc_raww <<- get_acs(
        geography = "county",
        variables = c(population = "B01003_001",
                      commuting = "B08015_001"),  # Total population
        year = surv_year,
        survey = "acs5",
        geometry = TRUE,
        progress_bar = FALSE,
        output = "wide",
      )
      usc_raw <<- usc_raww %>% mutate(micro  = populationE > 50000)
      usc <<- usc_raw %>% filter(populationE > 50000) 
    } #checks if it needs to be run, otherwise continuing 
    else{}},
  #cleans and collates CZs 
  clean_cz = function(surv_year,cz_file){
    ver2 <- exists("cz_shapes")
    if(ver2 == FALSE){
      commuting_zones <- read.csv("2020-commuting-zones.csv")
      usc_ran<- get_acs(
        geography = "county",
        variables = c(population = "B01003_001",
                      commuting = "B08015_001"),  # Total population
        year = surv_year,
        survey = "acs5",
        geometry = TRUE,
      )
      usc <- usc_ran %>%
        select(GEOID, NAME,variable, estimate) %>%
        tidyr::pivot_wider(
          names_from = variable,
          values_from = estimate
        )
      commuting_zones <- commuting_zones %>%
        rename("GEOID" = "FIPStxt") 
      commuting_zones<- commuting_zones %>% mutate(GEOID = as.numeric(GEOID))
      usc<- usc %>% mutate(GEOID = as.numeric(GEOID))
      commuting_zones<- commuting_zones %>% mutate(GEOID = as.character(GEOID))
      usc<- usc %>% mutate(GEOID = as.character(GEOID))
      usc_commuting<- full_join(usc, commuting_zones, by = "GEOID") 
      #####visualizing and combining commuting zones ####
      usc_commuting <- usc_commuting %>%
        mutate(PreliminaryCZ2020 = as.factor(PreliminaryCZ2020))
      cz_shapes <<- usc_commuting %>%
        group_by(PreliminaryCZ2020) %>%
        summarise(
          population = sum(population, na.rm = TRUE),
          geometry = st_union(geometry),
          .groups = "drop"
        )
    } #gets and cleans the files if they are not already there 
    else{}
  },
  #returns counties 500k+ and those adjacent to it (uscMAP)
  adjacency_model = function(surv_year){
    self$acquire_census(surv_year)
    big_areas_c <- usc %>% filter(populationE >= 500000)
    neighbors_c <- st_touches(usc, big_areas_c)
    noborderc <-  usc %>% mutate(keep = populationE >= 500000 | lengths(neighbors_c) > 0)
    uscMAP <<- noborderc %>% 
      filter(keep == TRUE) %>%
      filter(GEOID != 15003)
  },
  #returns counties 500k+ 
  selection_model = function(surv_year){
    self$acquire_census(surv_year)
    noborderc_select <- usc %>% mutate(keep = populationE >= 500000 )
    uscMAP <<- noborderc_select %>% 
      filter(keep == TRUE) %>%
      filter(GEOID != 15003)
  }, 
  #returns the counties within the commuting zones of a previously selected county of either model 
  centroid_model = function(surv_year,model= "def"){
    self$clean_cz(surv_year)
    if (model == 'selection' )
      self$selection_model(surv_year)
    else (self$adjacency_model(surv_year))
    #centroids of uscMAP
    
    uscMAP_centroids <-  uscMAP%>% st_centroid(uscMAP)
    usc_centroids <<- st_centroid(usc_raww)
    
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
    
    uscMAP_cent <<- st_join(usc_raw,usc_centroids_tr,st_within, left = TRUE)
    
    intersections <- st_intersects(usc_raw, usc_centroids_tr1) # preserves binaries in the spatial join 
    uscMAP_cent$contained_a <- sapply(intersections, function(idx) {
      any(usc_centroids_tr1$contained[idx] == 1)
    })
    uscMAP_cent_raw <<- uscMAP_cent %>% filter(contained_a == TRUE)
  }, #default adjacency
  #filters small and large counties 
  small_and_large = function(surv_year,model= "def"){
    self$centroid_model(surv_year)
    big_areas_c <- usc_raw %>% filter(populationE >= 500000)
    c_big_areas_c <- usc_raw %>% filter(populationE >= 500000)
    c_neighbors_c <- st_touches(uscMAP_cent_raw, big_areas_c)
    uscMAP_cent <-  uscMAP_cent_raw %>% mutate(keep = populationE.x >= 500000 | lengths(c_neighbors_c) > 0)
    small_area_c<- uscMAP_cent_raw %>% filter(populationE.x <= 250000)
    s_neighbors_c <- st_touches(uscMAP_cent_raw, small_area_c)
    uscMAP_cent <-  uscMAP_cent %>% mutate(small = ((keep = FALSE) | lengths(s_neighbors_c)>1) & populationE.x < 75000)
    uscMAP_cent <- uscMAP_cent %>% filter(small == FALSE)
    mega_counties <<- uscMAP_cent %>%
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
  }, 
  #makes the megapolitan regions 
  mega_maker = function(surv_year,model= "def"){
    self$small_and_large(surv_year)
    mega_id <- mega_counties %>%
      mutate(
        # adjacency list: which other counties touch each one
        neighbors = st_touches(geometry),
        
        # build graph + find connected components
        group_id = {
          g <- graph_from_adj_list(neighbors)
          comps <- components(g)$membership
          comps
        },
        group_co = as.numeric(group_id),
      ) 
    # need to drop column 
    mega_id$neighbors <-NULL
    mega_id <<- mega_id
    mega_shapes <<- mega_id %>%
      group_by(group_id) %>%
      summarise(
        # sum any numeric columns you want
        population = sum(populationE.x, na.rm = TRUE),
        county_count = n(),
        # combine geometries into one polygon per CZ
        geometry = st_union(geometry),
        .groups = "drop"
      )
    
  },
  #post hoc testing of the megapolitan regions 
  verify_mega = function(surv_year,model= "def"){
    ver3 <- exists("msa_raw") # checks for msa_related files 
    if(ver3 == FALSE){
      msa_raw<- get_acs(
        geography = "metropolitan statistical area/micropolitan statistical area",
        variables = c(population = "B01003_001"),
        # Total population
        year = surv_year,
        survey = "acs5",
        geometry = TRUE,
        progress_bar = FALSE,
        output = "wide"
      )} # gets the MSAs if needed 
    else{}
    self$mega_maker(surv_year)
    mega_shapes <- mega_shapes %>%filter(population >1500000)
    msa <- msa_raw %>%
      filter(populationE >= 50000)
    
    msa_centroid <- st_centroid(msa)
    
    msa_in_mega <-st_contains(mega_shapes, msa_centroid)
    
    #ensures mega's have more than 1 MSA 
    mega_shapes <- mega_shapes%>%
      mutate(n_msa = lengths(msa_in_mega) >1)
    
    #counties in msa
    county_in_msa<- st_within(usc_centroids,msa)
    msa_id <<- usc_raw %>% mutate(msa_bool = lengths(county_in_msa)>0)
    #need to create pop_bool, an aggreagate polycentric check, and 
    mega_shapes <- mega_shapes%>%  #maybe on mega id ?
      mutate(pop_bool = population >  (quantile(population, .5))) %>%
      mutate(poly_bool = pop_bool == FALSE | n_msa == FALSE) %>%
      mutate(mega_bool = poly_bool == FALSE)
    final_mega_shapes <<- mega_shapes %>%
      filter(county_count!=1)
    semester_mega_shapes <<- mega_shapes
    
  },
  #creates a regression ready data set of US megapolitan counties 
  regression_clean = function(surv_year,model= "def"){
    self$verify_mega(surv_year)
    mega_id<- st_drop_geometry(mega_id)
    mega_shapes <- st_drop_geometry(final_mega_shapes)
    mega_id <- mega_id %>%
      rename("GEOID" = "GEOID.x")
    
    usc_mega_indication <- left_join(usc_raw, mega_id, by = "GEOID")
    usc_mega_indication <- usc_mega_indication %>% mutate(group_id =
                                                            ifelse(is.na(group_id), 0, group_id))
    reg_step <- left_join( usc_mega_indication,mega_shapes, by = "group_id" )
    reg_step <- reg_step %>%mutate(n_msa = case_when(n_msa = is.na(n_msa) ~ FALSE, TRUE ~ n_msa))
    reg_step <- reg_step %>%mutate(mega_bool = case_when(mega_bool = is.na(mega_bool) ~ FALSE, TRUE ~ mega_bool))
    reg_step <- reg_step %>%mutate(pop_bool = case_when(pop_bool = is.na(pop_bool) ~ FALSE, TRUE ~ pop_bool))
    mega_regression_raw <- reg_step %>%mutate(poly_bool = case_when(poly_bool = is.na(poly_bool) ~ FALSE, TRUE ~ poly_bool))
    msa_id<- st_drop_geometry(msa_id)
    mega_regression_raw <- left_join(reg_step,msa_id, by = "GEOID")
    mega_regression_raw <- mega_regression_raw %>%mutate(poly_bool = case_when(poly_bool = is.na(poly_bool) ~ FALSE, TRUE ~ poly_bool))
    
    #clean that shit 
    
    test <- function(mega_regression_raw) {
      mega_regression_raw[, colSums(is.na(mega_regression_raw)) == 0]
    }
    mega_regression_raw <- test(mega_regression_raw)
    mega_regression_raw <<- mega_regression_raw%>%
      mutate(NAME.y.y = NULL) %>%
      mutate(populationE.y.y = NULL) %>%
      mutate(commutingE.x = NULL) %>%
      mutate(populationE.x = NULL) %>%
      mutate(commutingM.x = NULL) %>%
      mutate(commutingE.y.y = NULL) %>%
      mutate(commutingM.y.y = NULL) %>%
      mutate(commutingM = NULL) %>%
      mutate(micro.y.y= NULL )%>%
      mutate(micro.x = NULL )
  }
))
mega <-Megapolitan$new()
mega$acquire_census(2017)

mega$regression_clean(2023)

tmap_mode("view")
tm_shape(semester_mega_shapes) +
  tm_fill(col = "population",
          breaks = c(0, 250000, 500000, 1000000, 2000000, 4000000, 8000000),
          labels = c("<250k", "250k–500k", "500k–1M", "1M–2M", "2M–4M", "4M-8M"),
          palette = "Blues")
?mega$adjacency_model


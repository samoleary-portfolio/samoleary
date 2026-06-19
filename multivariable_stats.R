install.packages("sjPlot")
library(tidyverse)
library(rstatix)
library(ggpubr) #
library(tidycensus)
library(sf)
library(data.table)
library(gtsummary) # cite 
library(ggplot2)
library(ggstatsplot) #cite 
library(jtools) #cite 
library(flextable) #cite 

#fix female bach share, total variable has to be wrong
#####getting set up #####
options(scipen = 999)
acs_vars <- load_variables(2023, "acs5")
mega_regression_raw <- st_read("final_mega_regression_raw_sc.gpkg") 
mega_regression <-st_drop_geometry(mega_regression_raw)
#find correct labor force vars 
econ_predictiors<- get_acs(
  geography = "county",
  variables = c(med_income = "B19013_001",
                med_home = "B25077_001",
                gross_rent = "B25031_001",
                laborf = "B23025_003",
                work_pop = "B23022_001",
                FIRE = "C24050_009",
                med_earn_fem = "B24012_038",
                med_earn_male = "B24012_002",
                male_total25 = "B15002_002",
                female_total25 = "B15002_019",
                male_bach = "B15002_015",
                female_bach = "B15002_032"
  ),
  year = 2023,
  survey = "acs5",
  geometry = TRUE,
  progress_bar = FALSE,
  output = "wide"
)
write.csv(econ_predictiors,"econ_predictors.csv")
st_write(econ_predictiors, "econ_predictors.gpkg")
mega_regression_vars <- left_join(mega_regression, econ_predictiors, by = "GEOID")
emp18<- get_acs(geography = "county",
                variable = c(laborf_18 = "B23025_003",
                             work_pop_18 = "B23022_001"),
                year = 2018,
                survey = "acs5",
               output = "wide")

emp_analysis <- left_join(mega_regression_vars, emp18, by = "GEOID")


# rename/group variables 
mega_regression_vars <- mega_regression_vars %>%
  select(-ends_with("M"))
mega_regression_vars<- mega_regression_vars %>%
  drop_na()
#still don't work, work on later 
mega_regression_vars<- mega_regression_vars %>%
  mutate(lbf_rate = (laborfE/work_popE) *100) %>%
 # mutate(lbf_rate_18 = (laborf_18E/work_pop_18E) *100) %>%
  mutate(norm_fire = FIREE/work_popE *100) %>%
  mutate(male_bach_share = male_bachE / male_total25E * 100)%>%
  mutate(female_bach_share = female_bachE / female_total25E * 100)



#####clean the data and create case when
#lost some counties somewhere, need to figure it out eventually 
mega_regression <- mega_regression_vars %>%
  mutate(STATEFP = str_pad(substr(GEOID, 1, 2), 2, pad = "0")) %>%
  filter(!STATEFP %in% c(
    "02", # Alaska
    "15", # Hawaii
    "72", # Puerto Rico
    "60", # American Samoa
    "66", # Guam
    "69", # Northern Mariana Islands
    "78"  # U.S. Virgin Islands
  ))
#msa bool 
mega_regression <- mega_regression %>% #need to work on this 
  mutate(ism_bool = msa_bool == TRUE & mega_bool == FALSE & poly_bool == FALSE)
mega_regression <- mega_regression %>%
  mutate(type = case_when(
    mega_bool == TRUE ~ "Large Megapolitan",
    poly_bool == TRUE ~ "Small Megapolitan",
    msa_bool == TRUE ~ "Isolated Metro",
    msa_bool == FALSE ~ "Non Metro"
    
  ))
mega_regression <- mega_regression%>% mutate(
  type = factor(
    type, 
    levels = c("Non Metro", "Isolated Metro", "Small Megapolitan", "Large Megapolitan")
  )
)

mega_regression <- mega_regression %>%
  rename("median_income" ="med_incomeE",
         "median_home_value" ="med_homeE",
         "gross_rent" ="gross_rentE",
         "female_median_income" = "med_earn_femE",
         "male_median_income" = "med_earn_maleE",
         )
########regionality predictor##########
mega_regression <- mega_regression %>%
  mutate(
    state_fips = substr(GEOID, 1, 2),
    region = case_when(
    state_fips %in% c("09", "23", "25", "33", "44", "50", "34", "36", "42") ~ "Northeast",
    state_fips %in% c("17", "18", "19", "20", "26", "27", "29", "31", "38", "39", "46", "55") ~ "Midwest",
    state_fips %in% c("01", "05", "10", "11", "12", "13", "21", "24", "28", "37", "40", "45", "47", "48", "51", "54","22") ~ "South",
    state_fips %in% c( "04", "06", "08", "16", "30", "32", "35", "41", "49", "53", "56") ~ "West"
  ))
mega_regression %>% ggboxplot(x="region", y= "median_income")
write.csv(mega_regression,"mega_regression.csv")

###### median incom stats  ####

medinc <- mega_regression %>%
  group_by(type) %>%
  shapiro_test(median_income)

medinck <- mega_regression %>%
  kruskal_test(median_income~type)

medincd <- mega_regression %>%
  dunn_test(median_income~type)
medincd <- medincd %>% add_xy_position(x = "type")

ggboxplot(mega_regression, x = "type", y = "median_income", outlier.shape = NA)+
  stat_pvalue_manual(medincd, step.increase = .05)+
  labs(
    title = "Median Income by County Type",
    subtitle= get_test_label(medinck),
    caption = get_pwc_label(medincd),
    x = "County Type",
    y = "Median Income"
  )




###### median home value ######

medhome <- mega_regression %>%
  group_by(type) %>%
  shapiro_test(median_home_value)

medhomek <- mega_regression %>%
  kruskal_test(median_home_value~type)

medhomed <- mega_regression %>%
  dunn_test(median_home_value~type)
medhomed <- medhomed %>% add_xy_position(x = "type")

ggboxplot(mega_regression, x = "type", y = "median_home_value", outlier.shape = NA)+
  stat_pvalue_manual(medhomed, step.increase = .05)+
  labs(
    title = "Median Home Value by County Type",
    subtitle= get_test_label(medhomek),
    caption = get_pwc_label(medhomed),
    x = "County Type",
    y = "Median Home Value"
  )


#####gross rent ####

medinc <- mega_regression %>%
  group_by(type) %>%
  shapiro_test(gross_rent)

grossk <- mega_regression %>%
  kruskal_test(gross_rent~type)

grossd <- mega_regression %>%
  dunn_test(gross_rent~type)
grossd <- grossd %>% add_xy_position(x = "type")

ggboxplot(mega_regression, x = "type", y = "gross_rent", outlier.shape = NA)+
  stat_pvalue_manual(grossd, step.increase = .05)+
  labs(
    title = "Gross Rent by County Type",
    subtitle= get_test_label(grossk),
    caption = get_pwc_label(grossd),
    x = "County Type",
    y = "Gross Rent"
  )

#########lbf rate ######
#need to fix scale and lbf variable 
lbf <- mega_regression %>%
  group_by(type) %>%
  shapiro_test(lbf_rate)

lbfk <- mega_regression %>%
  kruskal_test(lbf_rate~type)

lbfd <- mega_regression %>%
  dunn_test(lbf_rate~type)
lbfd <- lbfd %>% add_xy_position(x = "type")


ggboxplot(mega_regression, x = "type", y = "lbf_rate", ylim = c(0, 100))+
  stat_pvalue_manual(lbfd, step.increase = .05)+
  labs(
    title = "Labor Force Participation Rate by County Type",
    subtitle= get_test_label(lbfk),
    caption = get_pwc_label(lbfd),
    x = "County Type",
    y = "Labor Force Participation Rate "
  )

#########fire####### 
#fix scale
fire <- mega_regression %>%
  group_by(type) %>%
  shapiro_test(norm_fire)

firek <- mega_regression %>%
  kruskal_test(norm_fire~type)

fired <- mega_regression %>%
  dunn_test(norm_fire~type)
fired <- grossd %>% add_xy_position(x = "type")


ggboxplot(mega_regression, x = "type", y = "norm_fire", ylim = c(0, 100))+
  stat_pvalue_manual(fired, step.increase = .05)+
  labs(
    title = "Percent FIRE by County Type",
    subtitle= get_test_label(firek),
    caption = get_pwc_label(fired),
    x = "County Type",
    y = "Percent FIRE"
  )
  
#####Single varible stat test for small megapolitan#####

fire_poly<- mega_regression %>%
  group_by(poly_bool) %>%
  shapiro_test(norm_fire)
firet <- mega_regression %>%
  t_test(norm_fire~poly_bool)

ggboxplot(mega_regression, x = "poly_bool", y = "norm_fire")+
  labs(
    title = "Percent FIRE by County Type",
    subtitle= get_test_label(firet),
    x = "Small Megapolitan or Not",
    y = "Percent FIRE"
  )




#########male income#####

male_med <- mega_regression %>%
  group_by(type) %>%
  shapiro_test(male_median_income)

male_medk <- mega_regression %>%
  kruskal_test(male_median_income~type)

male_medd <- mega_regression %>%
  dunn_test(male_median_income~type)
male_medd <- male_medd %>% add_xy_position(x = "type")


ggboxplot(mega_regression, x = "type", y = "male_median_income")+
  stat_pvalue_manual(male_medd, step.increase = .05)+
  labs(
    title = "Male Median Income by County Type",
    subtitle= get_test_label(male_medk),
    caption = get_pwc_label(male_medd),
    x = "County Type",
    y = "Male Median Income "
  )

######fem income####
fem_med <- mega_regression %>%
  group_by(type) %>%
  shapiro_test(female_median_income)

fem_medk <- mega_regression %>%
  kruskal_test(female_median_income~type)

fem_medd <- mega_regression %>%
  dunn_test(female_median_income~type)
fem_medd <- fem_medd %>% add_xy_position(x = "type")


ggboxplot(mega_regression, x = "type", y = "female_median_income")+
  stat_pvalue_manual(fem_medd, step.increase = .05)+
  labs(
    title = "Female Median Income by County Type",
    subtitle= get_test_label(fem_medk),
    caption = get_pwc_label(fem_medd),
    x = "County Type",
    y = "Female Median Income "
  )
#####t test small mega for female income ####
fem_inc_poly<- mega_regression %>%
  group_by(poly_bool) %>%
  shapiro_test(female_median_income)
fem_inct <- mega_regression %>%
  t_test(female_median_income~poly_bool)

ggboxplot(mega_regression, x = "poly_bool", y = "norm_fire")+
  labs(
    title = "Percent FIRE by County Type",
    subtitle= get_test_label(fem_inct),
    x = "Small Megapolitan or Not",
    y = "female median income"
  )
###### male education#####
male_edu <- mega_regression %>%
  group_by(type) %>%
  shapiro_test(male_bach_share)

male_eduk <- mega_regression %>%
  kruskal_test(male_bach_share~type)

male_edud <- mega_regression %>%
  dunn_test(male_bach_share~type)
male_edud <- male_edud %>% add_xy_position(x = "type")


ggboxplot(mega_regression, x = "type", y = "male_bach_share",ylim = c(0, 100))+
  stat_pvalue_manual(male_edud, step.increase = .05)+
  labs(
    title = "Share of Men with Bachelors' Degrees by County Type",
    subtitle= get_test_label(male_eduk),
    caption = get_pwc_label(male_edud),
    x = "County Type",
    y = "Share of Men with Bachelors' Degrees"
  )

###### Female education ####
fem_edu <- mega_regression %>%
  group_by(type) %>%
  shapiro_test(female_bach_share)

fem_eduk <- mega_regression %>%
  kruskal_test(female_bach_share~type)

fem_edud <- mega_regression %>%
  dunn_test(female_bach_share~type)
fem_edud <- fem_edud %>% add_xy_position(x = "type")


ggboxplot(mega_regression, x = "type", y = "female_bach_share",ylim = c(0, 100))+
  stat_pvalue_manual(fem_edud, step.increase = .05)+
  labs(
    title = "Share of Women with Bachelors' Degrees by County Type",
    subtitle= get_test_label(fem_eduk),
    caption = get_pwc_label(fem_edud),
    x = "County Type",
    y = "Share of Women with Bachelors' Degrees"
  )
######regression#####
median_income <- lm(median_income ~ mega_bool + poly_bool + msa_bool, mega_regression )
home<- lm(median_home_value ~ mega_bool + poly_bool + msa_bool, mega_regression )
gross_rent <- lm(gross_rent~ mega_bool + poly_bool + msa_bool, mega_regression)
norm_fire <- lm(norm_fire ~ mega_bool + poly_bool + msa_bool, mega_regression)
lbf_rate <- lm(lbf_rate ~ mega_bool + poly_bool + msa_bool, mega_regression)
male_bach <- lm(male_bach_share ~ mega_bool + poly_bool + msa_bool, mega_regression)
fem_bach <- lm(female_bach_share ~ mega_bool + poly_bool + msa_bool, mega_regression)
fem_inc <- lm(female_median_income ~ mega_bool + poly_bool + msa_bool, mega_regression)
male_inc <- lm(male_median_income ~ mega_bool + poly_bool + msa_bool, mega_regression)
export_summs(median_income,home, gross_rent, norm_fire, lbf_rate,
             model.names = c("Median Income","Median Home Value","Gross Rent", "Percent FIRE",
                             "LBF Rate"),
             coefs = c("Large Megapolitan" = "mega_boolTRUE",
                       "Small Megapolitan" = "poly_boolTRUE",
                       "Metropolitan" = "msa_boolTRUE"),
             to.file = "docx",
             omit.coefs = NULL,
             file.name = "regression_table_econ.docx",
             error_format = "({std.error})",
             stars = c('*' = 0.05, '**' = 0.01, '***' = 0.001), 
             digits = 3)
export_summs(
             male_bach, fem_bach, fem_inc, male_inc,
             model.names = c("Male Bachelor", "Female Bachelor",
                             "Female Income", "Male Income"),
             coefs = c("Large Megapolitan" = "mega_boolTRUE",
                       "Small Megapolitan" = "poly_boolTRUE",
                       "Metropolitan" = "msa_boolTRUE"),
             to.file = "docx",
             omit.coefs = NULL,
             file.name = "regression_table_gender.docx",
             error_format = "({std.error})",
             stars = c('*' = 0.05, '**' = 0.01, '***' = 0.001), 
             digits = 3)


######revised regression ######
mega_regression <- mega_regression%>%
  mutate(adj_median_income = median_income/1000) %>%
  mutate(adj_home_value = median_home_value/1000)
#median_income
model1<- lm(median_income ~ adj_home_value +gross_rent+ norm_fire +lbf_rate+male_bach_share+
              female_bach_share, mega_regression)
model2<-lm(median_income ~ adj_home_value +gross_rent+ norm_fire +lbf_rate+male_bach_share+
             female_bach_share +mega_bool,mega_regression)
model3<-lm(median_income ~ adj_home_value +gross_rent+ norm_fire +lbf_rate+male_bach_share+
             female_bach_share +mega_bool + poly_bool + ism_bool,mega_regression)
model4<-lm(median_income ~ adj_home_value +gross_rent+ norm_fire +lbf_rate+male_bach_share+
             female_bach_share +mega_bool + poly_bool + ism_bool + region,mega_regression)
med_reg<- export_summs(model1,model2,model3,model4,to.file = "word")

#median_home_value
home1<- lm(median_home_value ~ median_income +gross_rent+ norm_fire +lbf_rate+male_bach_share+
              female_bach_share, mega_regression)
home2<-lm(median_home_value ~ median_income  +gross_rent+ norm_fire +lbf_rate+male_bach_share+
             female_bach_share +mega_bool,mega_regression)
home3<-lm(median_home_value ~ median_income +gross_rent+ norm_fire +lbf_rate+male_bach_share+
             female_bach_share +mega_bool + poly_bool + msa_bool,mega_regression)
home4<-lm(median_home_value ~ median_income  +gross_rent+ norm_fire +lbf_rate+male_bach_share+
             female_bach_share +mega_bool + poly_bool + msa_bool + region,mega_regression)


home_reg<- export_summs(home1,home2,home3,home4,to.file = "word")
grosstest <- lm(gross_rent ~median_home_value +median_income + norm_fire +lbf_rate+male_bach_share+
                  female_bach_share +type,mega_regression)
# gross rent

gross1<- lm(gross_rent ~adj_home_value +adj_median_income + norm_fire +lbf_rate+male_bach_share+
             female_bach_share, mega_regression)
gross2<-lm(gross_rent ~adj_home_value +adj_median_income + norm_fire +lbf_rate+male_bach_share+
            female_bach_share +mega_bool,mega_regression)
gross3<-lm(gross_rent ~adj_home_value +adj_median_income + norm_fire +lbf_rate+male_bach_share+
            female_bach_share + mega_bool +poly_bool +ism_bool ,mega_regression)
gross4<-lm( gross_rent ~adj_home_value +adj_median_income  + norm_fire +lbf_rate+male_bach_share+
            female_bach_share + mega_bool+ poly_bool + ism_bool +region,mega_regression)

gross_reg <- export_summs(gross1,gross2,gross3,gross4,to.file = "word")

######appendix#####
usc <- st_read("usc.shp") # fix filter 
ap_counties <- usc %>% filter(GEOID = "06071",
                     GEOID = "32023",
                     GEOID = "06027",
                     GEOID = "06065",
                     GEOID  = "04012",
                     GEOID  = "06025",
                     GEOID  = "04027",
                     GEOID  = "06051",
                     GEOID = "04015",
                     GEOID = "53007",
                     GEOID = "53077"
)

######summary table for type #####

type_summary <- mega_regression %>%
  group_by(type) %>%
  summarize(median_income,median_home_value,home_ratio, gross_rent,rent_ratio,lbf_rate,norm_fire, male_bach_share,female_bach_share, populationE)%>%
  tbl_summary(by = type) |> add_overall()
type_flex <- type_summary %>% as_flex_table()
type_flex <- type_flex %>% save_as_docx(type_summary, path ="z_final_table.docx")
getwd()






######economic limits of megapolitan areas######
mega_regression <- mega_regression %>%
  mutate(rent_ratio= gross_rent/(median_income/12)) %>%
  mutate(home_ratio= median_home_value/median_income)

rent <- mega_regression %>%
  group_by(type) %>%
  shapiro_test(rent_ratio)

rentk <- mega_regression %>%
  kruskal_test(rent_ratio~type)

rentd <- mega_regression %>%
  dunn_test(rent_ratio~type)
rentd <- rentd %>% add_xy_position(x = "type")

ggboxplot(mega_regression, x = "type", y = "rent_ratio",outlier.shape = NA, ylim = c(0,.90))+
  stat_pvalue_manual(rentd)+
  labs(
    title = "rent ratio by County Type",
    subtitle= get_test_label(rentk),
    caption = get_pwc_label(rentd),
    x = "County Type",
    y = "Ratio of Gross Monthly rent to median income "
  )

homer <- mega_regression %>%
  group_by(type) %>%
  shapiro_test(home_ratio)

homerk <- mega_regression %>%
  kruskal_test(home_ratio~type)

homerd <- mega_regression %>%
  dunn_test(home_ratio~type)
homerd <- homerd %>% add_xy_position(x = "type")

ggboxplot(mega_regression, x = "type", y = "home_ratio",outlier.shape = NA)+
  stat_pvalue_manual(homerd)+
  labs(
    title = "Home ratio by County Type",
    subtitle= get_test_label(homerk),
    caption = get_pwc_label(homerd),
    x = "County Type",
    y = "Ratio of Gross Monthly rent to median income "
  )


###### change in employment####
emp_analysis <- emp_analysis %>%
  mutate(lbf_rate = (laborfE/work_popE) *100)%>%
  mutate(lbf_rate_18 = (laborf_18E/work_pop_18E) *100)%>%
  mutate(emp_change = lbf_rate - lbf_rate_18)
emp_analysis <- emp_analysis %>%
  select(-ends_with("M"))
emp_analysis<- emp_analysis %>%
  drop_na()
emp_analysis <- emp_analysis %>%
  mutate(STATEFP = str_pad(substr(GEOID, 1, 2), 2, pad = "0")) %>%
  filter(!STATEFP %in% c(
    "02", # Alaska
    "15", # Hawaii
    "72", # Puerto Rico
    "60", # American Samoa
    "66", # Guam
    "69", # Northern Mariana Islands
    "78"  # U.S. Virgin Islands
  ))
#msa bool 
emp_analysis <- emp_analysis %>% #need to work on this 
  mutate(ism_bool = msa_bool == TRUE & mega_bool == FALSE & poly_bool == FALSE)
emp_analysis <- emp_analysis %>%
  mutate(type = case_when(
    mega_bool == TRUE ~ "Large Megapolitan",
    poly_bool == TRUE ~ "Small Megapolitan",
    msa_bool == TRUE ~ "Isolated Metro",
    msa_bool == FALSE ~ "Non Metro"
    
  ))
emp_analysis <- emp_analysis%>% mutate(
  type = factor(
    type, 
    levels = c("Non Metro", "Isolated Metro", "Small Megapolitan", "Large Megapolitan")
  )
)

emp <- emp_analysis %>%
  group_by(type) %>%
  shapiro_test(emp_change)

empk <- emp_analysis %>%
  kruskal_test(emp_change~type)

empd <- emp_analysis %>%
  dunn_test(emp_change~type)
empd <- empd %>% add_xy_position(x = "type")

ggboxplot(emp_analysis, x = "type", y = "emp_change")+
  stat_pvalue_manual(empd, step.increase = .05)+
  labs(
    title = "Employment differences by County Type",
    subtitle= get_test_label(empk),
    caption = get_pwc_label(empd),
    x = "County Type",
    y = "Median Income"
  )






  




                
            

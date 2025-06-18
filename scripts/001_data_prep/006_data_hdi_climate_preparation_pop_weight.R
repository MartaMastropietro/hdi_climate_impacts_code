

################################################################################
################################################################################

### using original variables


rm(list=ls())
source("utils/libraries.R")


# output dir
out_dir<-"output"
if(!dir.exists(out_dir)){dir.create(out_dir)}


### years of analysis
years<-1990:2020


### HDI and components import 
data_dir <- "data/hdi_data"
data_out_all <- read_csv(file.path(data_dir,"data_hdi_original_components_1990_2022.csv"), show_col_types = FALSE)

### data climate and extremes
data_dir <- "data/climate_data/era5/"
data_cl <- read_csv(file.path(data_dir, "data_climate_gdl_pop_weight_1950_2023_updated.csv"), show_col_types = FALSE)

### unify 
data <- dplyr::inner_join(data_cl, data_out_all) # if gdl not present in data, we dont add useless NA

### add mean hist cl 

###  cut years
data<-data[which(data$year<=range(years)[2] & data$year>=range(years)[1]), ]

### Inequality data for dividing income groups 
#  WIID_28NOV2023 <- read_excel("data/data_inequality/downloaded/WIID_28NOV2023/WIID_28NOV2023.xlsx")
#  data_ineq <- unique(WIID_28NOV2023[, c(3,47,50)])
#  data_ineq$incomegroup[which(data_ineq$c3=="VEN")]<-"Upper middle income" # 2022 world bank classification
#  colnames(data_ineq)<- c("iso3", "region_wb", "income_group")
#  
#  ### Sub-national shape file (hdi + components data)
#  gdl_shape_file <- sf::st_read("data/hdi_data/GDL Shapefiles V6.2/GDL Shapefiles V6.2 large.shp")
#  gdl_continent<-gdl_shape_file[, c(1,2)]
#  gdl_lon_lat<-gdl_continent
#  gdl_lon_lat<- sf::st_make_valid(gdl_lon_lat)
#  gdl_lon_lat$centroid<-sf::st_centroid(gdl_lon_lat)
#  gdl_lon_lat$lon<-sf::st_coordinates(gdl_lon_lat$centroid)[1]
#  gdl_lon_lat$lat<-sf::st_coordinates(gdl_lon_lat$centroid)[2]
#  gdl_lon_lat$centroid<-NULL
#  
#  gdl_continent$geometry<-NULL
#  gdl_lon_lat$geometry<-NULL
#  
#  iso_continent<-gdl_shape_file[, c(2,3)]
#  iso_continent$geometry<-NULL
#  iso_continent<-unique(iso_continent)
#  colnames(iso_continent)<-c("continent", "iso3")
#  
#  ### Countries borders 
#  countries<-giscoR::gisco_get_countries()
#  #simplify data countries
#  countries<-countries[,c(3,6)]
#  countries<-sf::st_as_sf(countries)

### output vars manipulation



# add diff log (growth rate) vars
data <- data %>% 
  arrange(gdlcode, year) %>% 
  group_by(gdlcode) %>%
  dplyr::mutate(gr_mys = lmys- dplyr::lag(lmys, n = 1, default = NA))%>% 
  dplyr::mutate(gr_eys = leys - dplyr::lag(leys, n = 1, default = NA))%>% 
  dplyr::mutate(gr_gnipc = lgnipc - dplyr::lag(lgnipc, n = 1, default = NA))%>% 
  dplyr::mutate(gr_leb= lleb - dplyr::lag(lleb, n = 1, default = NA))%>% 
  dplyr::mutate(lag_gr_mys = dplyr::lag(gr_mys , n = 1, default = NA))%>% 
  dplyr::mutate(lag_gr_eys =  dplyr::lag(gr_eys, n = 1, default = NA))%>% 
  dplyr::mutate(lag_gr_gnipc =  dplyr::lag(gr_gnipc, n = 1, default = NA))%>% 
  dplyr::mutate(lag_gr_leb= dplyr::lag(gr_leb, n = 1, default = NA))%>% 
  ungroup()

# add lag vars
data <- data %>% 
  arrange(gdlcode, year) %>% 
  group_by(gdlcode) %>%
  dplyr::mutate(lag_log_leb = dplyr::lag(lleb, n = 1, default = NA)  )%>% 
  dplyr::mutate(lag_log_eys =  dplyr::lag(leys, n = 1, default = NA)  )%>% 
  dplyr::mutate(lag_log_mys =   dplyr::lag(lmys, n = 1, default = NA) )%>%
  dplyr::mutate(lag_log_gni_pc =   dplyr::lag(lgnipc, n = 1, default = NA) )%>% 
  ungroup()


### climate vars manipulation

# climate sq

data<-data %>% mutate(
  TM_2 = TM^2,
  TVAR_2 = TVAR^2,
  TN_2 = TN^2,
  TX_2 = TX^2,
  RR_2 = RR^2,
  HW_2= HW^2 ,
  CW_2= CW^2 ,
  SPEI_2= SPEI^2,
  SPI_2=SPI^2,
  RX_2=RX^2,
  PEXT_2=PEXT^2, 
  PET_2=PET^2,
  WD_2=WD^2
)

# add vars climate, lagged
data <- data %>% 
  arrange(gdlcode, year) %>% 
  group_by(gdlcode) %>%
  dplyr::mutate(lag_TM =  dplyr::lag(TM, n = 1, default = NA),
                lag_TVAR = dplyr::lag(TVAR, n = 1, default = NA), 
                lag_TN = dplyr::lag(TN , n = 1, default = NA), 
                lag_TX = dplyr::lag(TX , n = 1, default = NA), 
                lag_RR = dplyr::lag( RR, n = 1, default = NA), 
                lag_HW = dplyr::lag( HW, n = 1, default = NA), 
                lag_CW = dplyr::lag( CW, n = 1, default = NA), 
                lag_SPEI = dplyr::lag(SPEI , n = 1, default = NA), 
                lag_SPI = dplyr::lag(SPI , n = 1, default = NA), 
                lag_RX = dplyr::lag( RX, n = 1, default = NA), 
                lag_PEXT = dplyr::lag( PEXT, n = 1, default = NA), 
                lag_PET= dplyr::lag(PET , n = 1, default = NA), 
                lag_WD = dplyr::lag( WD, n = 1, default = NA)
  )%>% 
  ungroup()

# add diff vars climate 
data <- data %>% 
  arrange(gdlcode, year) %>% 
  group_by(gdlcode) %>%
  dplyr::mutate(diff_TM = TM - dplyr::lag(TM, n = 1, default = NA),
                diff_TVAR = TVAR - dplyr::lag(TVAR, n = 1, default = NA), 
                diff_TN = TN -  dplyr::lag(TN , n = 1, default = NA), 
                diff_TX = TX - dplyr::lag(TX , n = 1, default = NA), 
                diff_RR = RR - dplyr::lag( RR, n = 1, default = NA), 
                diff_HW = HW - dplyr::lag( HW, n = 1, default = NA), 
                diff_CW = CW - dplyr::lag( CW, n = 1, default = NA), 
                diff_SPEI = SPEI - dplyr::lag(SPEI , n = 1, default = NA), 
                diff_SPI = SPI - dplyr::lag(SPI , n = 1, default = NA), 
                diff_RX = RX - dplyr::lag( RX, n = 1, default = NA), 
                diff_PEXT = PEXT - dplyr::lag( PEXT, n = 1, default = NA), 
                diff_PET= PET - dplyr::lag(PET , n = 1, default = NA), 
                diff_WD = WD - dplyr::lag( WD, n = 1, default = NA)
  )%>% 
  ungroup()

# diff of vars squared
data <- data %>% 
  arrange(gdlcode, year) %>% 
  group_by(gdlcode) %>%
  dplyr::mutate(diff_TM_2 = TM_2 - dplyr::lag(TM_2, n = 1, default = NA),
                diff_TVAR_2 = TVAR_2 - dplyr::lag(TVAR_2, n = 1, default = NA), 
                diff_TN_2 = TN_2 -  dplyr::lag(TN_2 , n = 1, default = NA), 
                diff_TX_2 = TX_2 - dplyr::lag( TX_2 , n = 1, default = NA), 
                diff_RR_2 = RR_2 - dplyr::lag( RR_2, n = 1, default = NA), 
                diff_HW_2 = HW_2 - dplyr::lag( HW_2, n = 1, default = NA), 
                diff_CW_2 = CW_2 - dplyr::lag( CW_2, n = 1, default = NA), 
                diff_SPEI_2 = SPEI_2 - dplyr::lag(SPEI_2 , n = 1, default = NA), 
                diff_SPI_2 = SPI_2 - dplyr::lag(SPI_2 , n = 1, default = NA), 
                diff_RX_2 = RX_2 - dplyr::lag( RX_2, n = 1, default = NA), 
                diff_PEXT_2 = PEXT_2 - dplyr::lag( PEXT_2, n = 1, default = NA), 
                diff_PET_2 = PET_2 - dplyr::lag(PET_2 , n = 1, default = NA), 
                diff_WD_2 = WD_2 - dplyr::lag( WD_2 , n = 1, default = NA)
  )%>% 
  ungroup()

# add diff vars climate, lagged

data <- data %>% 
  arrange(gdlcode, year) %>% 
  group_by(gdlcode) %>%
  dplyr::mutate(lag_diff_TM = dplyr::lag(diff_TM, n = 1, default = NA),
                lag_diff_TVAR =  dplyr::lag(diff_TVAR, n = 1, default = NA), 
                lag_diff_TN = dplyr::lag(diff_TN , n = 1, default = NA), 
                lag_diff_TX = dplyr::lag(diff_TX , n = 1, default = NA), 
                lag_diff_RR = dplyr::lag( diff_RR, n = 1, default = NA), 
                lag_diff_HW = dplyr::lag( diff_HW, n = 1, default = NA), 
                lag_diff_CW = dplyr::lag( diff_CW, n = 1, default = NA), 
                lag_diff_SPEI = dplyr::lag(diff_SPEI , n = 1, default = NA), 
                lag_diff_SPI = dplyr::lag(diff_SPI , n = 1, default = NA), 
                lag_diff_RX = dplyr::lag( diff_RX, n = 1, default = NA), 
                lag_diff_PEXT = dplyr::lag( diff_PEXT, n = 1, default = NA), 
                lag_diff_PET= dplyr::lag(diff_PET , n = 1, default = NA), 
                lag_diff_WD = dplyr::lag( diff_WD, n = 1, default = NA)
  )%>% 
  ungroup()

### save dataset in model output folder, to have data and model together 

data_sub<-data[which(data$Level=="Subnat"),]
write.csv(data_sub, file.path(out_dir,"data_hdi_original_comp_climate_pop_weight_1990_2020.csv"), row.names = FALSE)

data_nat<-data[-which(data$Level=="Subnat"),]
write.csv(data_nat, file.path(out_dir,"data_hdi_original_comp_climate_country_pop_weight_1990_2020.csv"), row.names = FALSE)


################################################################################
################################################################################

### using original variables, cut too many na regions


rm(list=ls())
source("utils/libraries.R")


# output dir
out_dir<-"output"
if(!dir.exists(out_dir)){dir.create(out_dir)}


### years of analysis
years<-1990:2021


### HDI and components import 
data_dir <- "data/hdi_data"
data_out_all <- read_csv(file.path(data_dir,"data_hdi_original_components_1990_2022_less_na.csv"), show_col_types = FALSE)

### data climate and extremes
data_dir <- "data/climate_data/era5/"
data_cl <- read_csv(file.path(data_dir, "data_climate_gdl_pop_weight_1950_2023_updated.csv"), show_col_types = FALSE)



### unify 
data <- dplyr::inner_join(data_cl, data_out_all) # if gdl not present in data, we dont add useless NA

###  cut years
data<-data[which(data$year<=range(years)[2] & data$year>=range(years)[1]), ]

### Inequality data for dividing income groups 
#  WIID_28NOV2023 <- read_excel("data/data_inequality/downloaded/WIID_28NOV2023/WIID_28NOV2023.xlsx")
#  data_ineq <- unique(WIID_28NOV2023[, c(3,47,50)])
#  data_ineq$incomegroup[which(data_ineq$c3=="VEN")]<-"Upper middle income" # 2022 world bank classification
#  colnames(data_ineq)<- c("iso3", "region_wb", "income_group")
#  
#  ### Sub-national shape file (hdi + components data)
#  gdl_shape_file <- sf::st_read("data/hdi_data/GDL Shapefiles V6.2/GDL Shapefiles V6.2 large.shp")
#  gdl_continent<-gdl_shape_file[, c(1,2)]
#  gdl_lon_lat<-gdl_continent
#  gdl_lon_lat<- sf::st_make_valid(gdl_lon_lat)
#  gdl_lon_lat$centroid<-sf::st_centroid(gdl_lon_lat)
#  gdl_lon_lat$lon<-sf::st_coordinates(gdl_lon_lat$centroid)[1]
#  gdl_lon_lat$lat<-sf::st_coordinates(gdl_lon_lat$centroid)[2]
#  gdl_lon_lat$centroid<-NULL
#  
#  gdl_continent$geometry<-NULL
#  gdl_lon_lat$geometry<-NULL
#  
#  iso_continent<-gdl_shape_file[, c(2,3)]
#  iso_continent$geometry<-NULL
#  iso_continent<-unique(iso_continent)
#  colnames(iso_continent)<-c("continent", "iso3")
#  
#  ### Countries borders 
#  countries<-giscoR::gisco_get_countries()
#  #simplify data countries
#  countries<-countries[,c(3,6)]
#  countries<-sf::st_as_sf(countries)

### output vars manipulation



# add diff log (growth rate) vars
data <- data %>% 
  arrange(gdlcode, year) %>% 
  group_by(gdlcode) %>%
  dplyr::mutate(gr_mys = lmys- dplyr::lag(lmys, n = 1, default = NA))%>% 
  dplyr::mutate(gr_eys = leys - dplyr::lag(leys, n = 1, default = NA))%>% 
  dplyr::mutate(gr_gnipc = lgnipc - dplyr::lag(lgnipc, n = 1, default = NA))%>% 
  dplyr::mutate(gr_leb= lleb - dplyr::lag(lleb, n = 1, default = NA))%>% 
  dplyr::mutate(lag_gr_mys = dplyr::lag(gr_mys , n = 1, default = NA))%>% 
  dplyr::mutate(lag_gr_eys =  dplyr::lag(gr_eys, n = 1, default = NA))%>% 
  dplyr::mutate(lag_gr_gnipc =  dplyr::lag(gr_gnipc, n = 1, default = NA))%>% 
  dplyr::mutate(lag_gr_leb= dplyr::lag(gr_leb, n = 1, default = NA))%>% 
  ungroup()

# add lag vars
data <- data %>% 
  arrange(gdlcode, year) %>% 
  group_by(gdlcode) %>%
  dplyr::mutate(lag_log_leb = dplyr::lag(lleb, n = 1, default = NA)  )%>% 
  dplyr::mutate(lag_log_eys =  dplyr::lag(leys, n = 1, default = NA)  )%>% 
  dplyr::mutate(lag_log_mys =   dplyr::lag(lmys, n = 1, default = NA) )%>%
  dplyr::mutate(lag_log_gni_pc =   dplyr::lag(lgnipc, n = 1, default = NA) )%>% 
  ungroup()


### climate vars manipulation

# climate sq

data<-data %>% mutate(
  TM_2 = TM^2,
  TVAR_2 = TVAR^2,
  TN_2 = TN^2,
  TX_2 = TX^2,
  RR_2 = RR^2,
  HW_2= HW^2 ,
  CW_2= CW^2 ,
  SPEI_2= SPEI^2,
  SPI_2=SPI^2,
  RX_2=RX^2,
  PEXT_2=PEXT^2, 
  PET_2=PET^2,
  WD_2=WD^2
)

# add vars climate, lagged
data <- data %>% 
  arrange(gdlcode, year) %>% 
  group_by(gdlcode) %>%
  dplyr::mutate(lag_TM =  dplyr::lag(TM, n = 1, default = NA),
                lag_TVAR = dplyr::lag(TVAR, n = 1, default = NA), 
                lag_TN = dplyr::lag(TN , n = 1, default = NA), 
                lag_TX = dplyr::lag(TX , n = 1, default = NA), 
                lag_RR = dplyr::lag( RR, n = 1, default = NA), 
                lag_HW = dplyr::lag( HW, n = 1, default = NA), 
                lag_CW = dplyr::lag( CW, n = 1, default = NA), 
                lag_SPEI = dplyr::lag(SPEI , n = 1, default = NA), 
                lag_SPI = dplyr::lag(SPI , n = 1, default = NA), 
                lag_RX = dplyr::lag( RX, n = 1, default = NA), 
                lag_PEXT = dplyr::lag( PEXT, n = 1, default = NA), 
                lag_PET= dplyr::lag(PET , n = 1, default = NA), 
                lag_WD = dplyr::lag( WD, n = 1, default = NA)
  )%>% 
  ungroup()

# add diff vars climate 
data <- data %>% 
  arrange(gdlcode, year) %>% 
  group_by(gdlcode) %>%
  dplyr::mutate(diff_TM = TM - dplyr::lag(TM, n = 1, default = NA),
                diff_TVAR = TVAR - dplyr::lag(TVAR, n = 1, default = NA), 
                diff_TN = TN -  dplyr::lag(TN , n = 1, default = NA), 
                diff_TX = TX - dplyr::lag(TX , n = 1, default = NA), 
                diff_RR = RR - dplyr::lag( RR, n = 1, default = NA), 
                diff_HW = HW - dplyr::lag( HW, n = 1, default = NA), 
                diff_CW = CW - dplyr::lag( CW, n = 1, default = NA), 
                diff_SPEI = SPEI - dplyr::lag(SPEI , n = 1, default = NA), 
                diff_SPI = SPI - dplyr::lag(SPI , n = 1, default = NA), 
                diff_RX = RX - dplyr::lag( RX, n = 1, default = NA), 
                diff_PEXT = PEXT - dplyr::lag( PEXT, n = 1, default = NA), 
                diff_PET= PET - dplyr::lag(PET , n = 1, default = NA), 
                diff_WD = WD - dplyr::lag( WD, n = 1, default = NA)
  )%>% 
  ungroup()

# diff of vars squared
data <- data %>% 
  arrange(gdlcode, year) %>% 
  group_by(gdlcode) %>%
  dplyr::mutate(diff_TM_2 = TM_2 - dplyr::lag(TM_2, n = 1, default = NA),
                diff_TVAR_2 = TVAR_2 - dplyr::lag(TVAR_2, n = 1, default = NA), 
                diff_TN_2 = TN_2 -  dplyr::lag(TN_2 , n = 1, default = NA), 
                diff_TX_2 = TX_2 - dplyr::lag( TX_2 , n = 1, default = NA), 
                diff_RR_2 = RR_2 - dplyr::lag( RR_2, n = 1, default = NA), 
                diff_HW_2 = HW_2 - dplyr::lag( HW_2, n = 1, default = NA), 
                diff_CW_2 = CW_2 - dplyr::lag( CW_2, n = 1, default = NA), 
                diff_SPEI_2 = SPEI_2 - dplyr::lag(SPEI_2 , n = 1, default = NA), 
                diff_SPI_2 = SPI_2 - dplyr::lag(SPI_2 , n = 1, default = NA), 
                diff_RX_2 = RX_2 - dplyr::lag( RX_2, n = 1, default = NA), 
                diff_PEXT_2 = PEXT_2 - dplyr::lag( PEXT_2, n = 1, default = NA), 
                diff_PET_2 = PET_2 - dplyr::lag(PET_2 , n = 1, default = NA), 
                diff_WD_2 = WD_2 - dplyr::lag( WD_2 , n = 1, default = NA)
  )%>% 
  ungroup()

# add diff vars climate, lagged

data <- data %>% 
  arrange(gdlcode, year) %>% 
  group_by(gdlcode) %>%
  dplyr::mutate(lag_diff_TM = dplyr::lag(diff_TM, n = 1, default = NA),
                lag_diff_TVAR =  dplyr::lag(diff_TVAR, n = 1, default = NA), 
                lag_diff_TN = dplyr::lag(diff_TN , n = 1, default = NA), 
                lag_diff_TX = dplyr::lag(diff_TX , n = 1, default = NA), 
                lag_diff_RR = dplyr::lag( diff_RR, n = 1, default = NA), 
                lag_diff_HW = dplyr::lag( diff_HW, n = 1, default = NA), 
                lag_diff_CW = dplyr::lag( diff_CW, n = 1, default = NA), 
                lag_diff_SPEI = dplyr::lag(diff_SPEI , n = 1, default = NA), 
                lag_diff_SPI = dplyr::lag(diff_SPI , n = 1, default = NA), 
                lag_diff_RX = dplyr::lag( diff_RX, n = 1, default = NA), 
                lag_diff_PEXT = dplyr::lag( diff_PEXT, n = 1, default = NA), 
                lag_diff_PET= dplyr::lag(diff_PET , n = 1, default = NA), 
                lag_diff_WD = dplyr::lag( diff_WD, n = 1, default = NA)
  )%>% 
  ungroup()

### save dataset in model output folder, to have data and model together 

data_sub<-data[which(data$Level=="Subnat"),]
write.csv(data_sub, file.path(out_dir,"data_hdi_original_comp_climate_pop_weight_1990_2020_less_na.csv"), row.names = FALSE)

data_nat<-data[-which(data$Level=="Subnat"),]
write.csv(data_nat, file.path(out_dir,"data_hdi_original_comp_climate_country_pop_weight_1990_2020_less_na.csv"), row.names = FALSE)




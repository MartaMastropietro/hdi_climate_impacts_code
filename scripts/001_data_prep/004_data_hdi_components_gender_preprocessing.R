
### data of each original component 
rm(list=ls())

source("utils/libraries.R")

data <- read_csv("data/hdi_data/downloaded/GDL-Expected-years-schooling-data.csv")
data<-reshape2::melt(data, id.vars = c("Country", "Continent", "ISO_Code",  "Level","GDLCODE", "Region" ), variable.name = "year")
data<-data[, c("ISO_Code", "Level","GDLCODE", "year", "value")]
colnames(data)<- c("iso3", "Level","gdlcode", "year", "eys")
data_eys<-data
data_eys$leys<-log(data_eys$eys)

data <- read_csv("data/hdi_data/downloaded/GDL-Mean-years-schooling-data.csv")
data<-reshape2::melt(data, id.vars = c("Country", "Continent", "ISO_Code",  "Level","GDLCODE", "Region" ), variable.name = "year")
data<-data[, c("ISO_Code", "Level","GDLCODE", "year", "value")]
colnames(data)<- c("iso3", "Level","gdlcode", "year", "mys")
data_mys<-data
data_mys$lmys<-log(data_mys$mys)


data <-read_csv("data/hdi_data/downloaded/GDL-LGNIPC-in-1000-US-Dollars-(2011-PPP)-data.csv")
data<-reshape2::melt(data, id.vars = c("Country", "Continent", "ISO_Code",  "Level","GDLCODE", "Region" ), variable.name = "year")
data<-data[, c("ISO_Code","Level", "GDLCODE", "year", "value")]
colnames(data)<- c("iso3","Level", "gdlcode", "year", "lgnipc")
data_lgnipc<-data

data <- read_csv("data/hdi_data/downloaded/gdl_subnational_original_variables/GDL-Life-expectancy-data.csv")
data<-reshape2::melt(data, id.vars = c("Country", "Continent", "ISO_Code",  "Level","GDLCODE", "Region" ), variable.name = "year")
data<-data[, c("ISO_Code",  "Level","GDLCODE", "year", "value")]
colnames(data)<- c("iso3", "Level", "gdlcode", "year", "leb")
data_leb<-data
data_leb$lleb<-log(data_leb$leb)


data<-inner_join(data_eys,data_mys)
data<-inner_join(data,data_lgnipc)
data<-inner_join(data,data_leb)

data_na<-unique(data%>%group_by(gdlcode)%>%mutate(na_mys=sum(is.na(mys)), 
                                  na_eys=sum(is.na(eys)), 
                                  na_leb=sum(is.na(leb)), 
                                  na_gni=sum(is.na(lgnipc)) ) %>% select(iso3,gdlcode, na_mys, na_eys, na_leb, na_gni))
hist(data_na$na_mys)
hist(data_na$na_eys)
hist(data_na$na_leb)
hist(data_na$na_gni)

too_much_na_codes<-unique(data_na$gdlcode[which(data_na$na_mys>=15 |data_na$na_eys>=15 |data_na$na_gni>=15 |data_na$na_leb>=15 )])

data_less_na<-data[-which(data$gdlcode %in% too_much_na_codes), ]

### save 
out_dir<-"data/hdi_data"
write.csv(data, file=file.path(out_dir, "data_hdi_original_components_1990_2022.csv"), row.names = FALSE)
write.csv(data_less_na, file=file.path(out_dir, "data_hdi_original_components_1990_2022_less_na.csv"), row.names = FALSE)


### extraction raster bricks

rm(list=ls())
source("utils/libraries.R")

gdl_shape_file <- sf::st_read("data/hdi_data/downloaded/GDL Shapefiles V6.2/GDL Shapefiles V6.2 large.shp")
gdl_shape_file$geometry<-NULL
gdl_shape_file<-gdl_shape_file[, c("gdlcode", "iso_code")]
colnames(gdl_shape_file)<-c("gdlcode", "iso3")
write.csv(gdl_shape_file, "data/gdlcodes_iso.csv", row.names = FALSE)

# variables we want 
data_dir<-"data/climate_data/era5/downloaded"
out_dir<-"data/climate_data/era5"

file_var_names<-c( "HW",   # heatwaves
                   "CW",    # coldwaves
                   "PET",  # aridity
                   "PEXT", 
                   "RR", 
                   "RX", 
                   "SNX", 
                   "TM", 
                   "TN", 
                   "TVAR", 
                   "TX", 
                   "WD", 
                   "SPEI", 
                   "SPI"
)
years<-1950:2023

# retrieve names of netcdf
names_tab<- data.frame(file_var=file_var_names, nc_var=NA)
for (v in file_var_names){
  nc<-nc_open(file.path(data_dir, paste0("ERA5_025_" , v , "_2015.nc")))
  print(names(nc[['var']])[1])
  names_tab$nc_var[names_tab$file_var==v]<-names(nc[['var']])[1]
}

# all_rast<-list()
# for (v in file_var_names){
#   for (y in years){
#     
#     file_name<-file.path(data_dir, paste0("ERA5_025_" , v , "_", y, ".nc"))
#     print(nc_open(file_name))
#     var<-names_tab$nc_var[names_tab$file_var==v]
#     
#     all_rast[[paste0(v)]][[paste0(y)]]<-stack(file_name, varname=var)
#     
#   }
# }


# lets assign crs meanwhile
all_rast<-list()
for (v in file_var_names){
  for (y in years){
    
    file_name<-file.path(data_dir, paste0("ERA5_025_" , v , "_", y, ".nc"))
    nc<-nc_open(file_name)
    var<-names_tab$nc_var[names_tab$file_var==v]
    
    lon <- ncvar_get(nc, "lon")
    lat <- ncvar_get(nc, "lat", verbose = F)
    var.array <- ncvar_get(nc, var) # store the data in a 2-dimensional array
    fillvalue <- ncatt_get(nc, var, "_FillValue")
    var.array[which(var.array == fillvalue$value)] <- NA
    r <- raster(t(var.array), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs="EPSG:4326")
    r<-flip(r, "y")
    all_rast[[paste0(v)]][[paste0(y)]] <- r
  }
}

library(maps)
map("world", add = TRUE)

for (v in c("TM", "RR")){
  for ( y in years)
    plot(all_rast[[paste0(v)]][[paste0(y)]], main=paste0(v))
  map("world", add = TRUE)
}


### save the list as RData
save(all_rast, file=file.path(data_dir,"climate_data_1950_2023.RData"))

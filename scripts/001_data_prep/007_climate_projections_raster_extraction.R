### extraction raster bricks, different extraction in different contexts

rm(list=ls())
source("utils/libraries.R")


data_dir<-"data/climate_data/projections"
out_dir<-"data/climate_data/projections"

ssp_names<-c("ssp126", "ssp119", "ssp245", "ssp370", "ssp585")

models_names<-c("CAN_ESM5", "CNRM_CM6", "CNRM_ESM2", 
                "EC_EARTH3", "GFDL_ESM4", "IPSL_CM6A",
                "MIRO_C6", "MPI_ESM1" ,  "MRI_ESM2", 
                "UK_ESM1") 
models_names_lower_case<-c("canesm5_r1i1p1f1", "cnrm-cm6-1_r1i1p1f2", "cnrm-esm2-1_r1i1p1f2",
                           "ec-earth3_r1i1p1f1", "gfdl-esm4_r1i1p1f1", "ipsl-cm6a-lr_r1i1p1f1",
                           "miroc6_r1i1p1f1" , "mpi-esm1-2-hr_r1i1p1f1", "mri-esm2-0_r1i1p1f1", 
                           "ukesm1-0-ll_r1i1p1f2")


### variables we want, first set 

var_names<-c("PEXT", 
             "RR", 
             "TM", 
             "TVAR", 
             "WD")
years<-2015:2100

for (v in var_names){
  for (mod in models_names){
    for (ssp in ssp_names){
      gc()
      
      all_rast<-list()
      
      for (y in years){
        
        
        file_name<-file.path(data_dir,mod,"Annual", paste0(mod, "_" , ssp ,"_", v , "_", y, ".nc"))
        
        if(file.exists(file_name)){
          nc<-nc_open(file_name)
          var<-names(nc[['var']])[1]
          
          lon <- ncvar_get(nc, "lon")
          lat <- ncvar_get(nc, "lat", verbose = F)
          var.array <- ncvar_get(nc, var) # store the data in a 2-dimensional array
          fillvalue <- ncatt_get(nc, var, "_FillValue")
          var.array[which(var.array == fillvalue$value)] <- NA
          r <- raster(t(var.array), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs="EPSG:4326")
          #r<-flip(r, "y")
          all_rast[[paste0(v)]][[paste0(y)]] <- r
          
          nc_close(nc)
        }
        
        
      }
      
      if(file.exists(file_name)){ # it is sufficient that the last year is checked, since all projections go up to 2100
        ### save the list as RData
        save(all_rast, file=file.path(data_dir, paste0("projections_",v,"_", mod, "_", ssp, ".RData")))
        print(paste0("saved: ",paste0("projections_",v,"_", mod, "_", ssp, ".RData")))
      }
      
    } 
  }
}

################################################################################

### variables we want, second set  
### here years from 1881, we need from 2015 -> from index 135 to 220

rm(list=ls())
source("utils/libraries.R")


data_dir<-"data/climate_data/projections"
out_dir<-"data/climate_data/projections"

ssp_names<-c("ssp126", "ssp119", "ssp245", "ssp370", "ssp585")

models_names<-c("CAN_ESM5", "CNRM_CM6", "CNRM_ESM2", 
                "EC_EARTH3", "GFDL_ESM4", "IPSL_CM6A",
                "MIRO_C6", "MPI_ESM1" ,  "MRI_ESM2", 
                "UK_ESM1") 
models_names_lower_case<-c("canesm5_r1i1p1f1", "cnrm-cm6-1_r1i1p1f2", "cnrm-esm2-1_r1i1p1f2",
                           "ec-earth3_r1i1p1f1", "gfdl-esm4_r1i1p1f1", "ipsl-cm6a-lr_r1i1p1f1",
                           "miroc6_r1i1p1f1" , "mpi-esm1-2-hr_r1i1p1f1", "mri-esm2-0_r1i1p1f1", 
                           "ukesm1-0-ll_r1i1p1f2")

indeces<-c(135:220)

var_names<-c("HWd_LARG", 
             "RRX_RR5d", 
             "SPI_SEV", 
             "SPEI_SEV")

new_var_names<-c("HW", 
                 "RX",
                 "SPI",
                 "SPEI")



for (v in var_names){
  for (mod_low in models_names_lower_case){
    for (ssp in ssp_names){
      gc()
      
      v_new<-new_var_names[which(var_names==v)]
      mod<-models_names[which(models_names_lower_case==mod_low)]
      file_name<-file.path(data_dir, mod, "Annual", paste0( v , "_",mod_low, "_" , ssp ,"_05_land", ".nc"))
      
      if(file.exists(file_name)){
        
        nc<-nc_open(file_name)
        
        print(nc)
        
        var<-names(nc[['var']])[1]
        
        lon <- ncvar_get(nc, "lon")
        lat <- ncvar_get(nc, "lat", verbose = F)
        
        var.array <- ncvar_get(nc, var) # store the data in a 2-dimensional array
        fillvalue <- ncatt_get(nc, var, "_FillValue")
        var.array[which(var.array == fillvalue$value)] <- NA
        
        all_rast<-list()
        
        for ( i in indeces ){
          y<-i+1880
          temp<-var.array[,,i]
          r <- raster(t(temp), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs="EPSG:4326")
          #r<-flip(r, "y")
          all_rast[[paste0(v_new)]][[paste0(y)]] <- r
        }
        
        save(all_rast, file=file.path(data_dir, paste0("projections_",v_new,"_", mod, "_", ssp, ".RData")))
        print(paste0("saved: ",paste0("projections_",v_new,"_", mod, "_", ssp, ".RData")))
        nc_close(nc)
      }
      
    }
  }
}

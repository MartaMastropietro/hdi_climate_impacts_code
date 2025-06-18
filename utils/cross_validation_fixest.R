### function for performing panel crossvalidation: 

cross_validation_fixest_1year = function (data_dem, f, y){
  data_train<-data_dem[ data_dem$year <y, ]
  data_test<-data_dem[ data_dem$year ==y, ]
  m_cross_val <- fixest::feols(f, data_train)
  pred<-predict(m_cross_val, data_test)
  return (sqrt(sum((data_test[[o]]-pred)^2)/length(pred)))
  
}

cross_validation_fixest_3year = function (data_dem, f, y){
  
  data_train<-data_dem[ data_dem$year <y, ]
  data_test<-data_dem[ data_dem$year ==y, ]
  m_cross_val <- fixest::feols(f, data_train)
  pred<-predict(m_cross_val, data_test)
  e1= (sqrt(sum((data_test[[o]]-pred)^2, na.rm=TRUE)/length(pred)))
  
  data_train<-data_dem[ data_dem$year <(y-1), ]
  data_test<-data_dem[ data_dem$year >=(y-1), ]
  m_cross_val <- fixest::feols(f, data_train)
  pred<-predict(m_cross_val, data_test)
  e2= (sqrt(sum((data_test[[o]]-pred)^2, na.rm=TRUE)/length(pred)))
  
  data_train<-data_dem[ data_dem$year <(y-2), ]
  data_test<-data_dem[ data_dem$year >=(y-2), ]
  m_cross_val <- fixest::feols(f, data_train)
  pred<-predict(m_cross_val, data_test)
  e3= (sqrt(sum((data_test[[o]]-pred)^2, na.rm=TRUE)/length(pred)))
  
  return ((e1+e2+e3)/3)
}



cross_validation_fixest = function (data_dem, f){
  
  e<-list()
  years<-unique(data_dem$year)
  for (y in 1:length(years)){ #nmax useful if we have lags, corresponds to max n lags in our reg to avoid using first years 
    data_train<-data_dem[ data_dem$year !=years[y], ]
    data_test<-data_dem[ data_dem$year ==years[y], ]
    m_cross_val <- fixest::feols(f, data_train)
    pred<-predict(m_cross_val, data_test)
    e[[length(e)+1]]<-sqrt(mean((data_test[[o]]-pred)^2, na.rm=TRUE))
    
  }
  err= sqrt(mean((unlist(e)^2), na.rm=TRUE))
  
  return(err)
}


# 
#  rm(list=ls())
#  
#  # xlsx write
#  require(openxlsx)
#  
#  # libraries parallel
#  library(parallel)
#  library(foreach)
#  library(doParallel)
#  library(fixest)
#  library(modelsummary)
#  
#  # libraries needed
#  library(dplyr)
#  library(readr)
#  library(ggplot2)
#  
#  # output dir
#  out_dir<-"output/models"
#  if(!dir.exists(out_dir)){dir.create(out_dir)}
#  out_dir<-"output/models/components_tp_extr_burke_models"
#  if(!dir.exists(out_dir)){dir.create(out_dir)}
#  
#  data<- read_csv("output/data_hdi_climate_gdl_pop_weight_1990_2021.csv")
#  
#  ### vcov: regional, dk, iso3
#  fvcov_dk<-function(x) vcov(x, "DK")
#  fvcov_iso<-function(x) vcov(x, cluster~iso3)
#  
#  
#  pan_id<-c('gdlcode', 'year')
#  
#  # setup for lag models 
#  varns=c('diff_TM','diff_RR', "diff_TVAR", "diff_HW", "diff_RX", "diff_PEXT", "diff_WD", "diff_SPI", "diff_SPEI", "diff_PET")
#  modns=c('TM','RR', "TVAR", "HW", "RX", "PEXT", "WD", "SPI", "SPEI", "PET")
#  
#  adap=c('lag_log_gni_pc','lag_log_gni_pc', 'lag_log_gni_pc','lag_log_gni_pc', 'lag_log_gni_pc','lag_log_gni_pc', 'lag_log_gni_pc','lag_log_gni_pc', 'lag_log_gni_pc','lag_log_gni_pc')
#  
#  for (i in 1:length(varns)){
#    data[paste(modns[i],'_i_',varns[i],sep='')] <- data[modns[i]] * data[varns[i]]
#  } 
#  
#  for (i in 1:length(modns)){ # works better with level interaction
#    data[paste(adap[i],'_i_',varns[i],sep='')] <- data[adap[i]] * data[varns[i]]
#  } 
#  
#  N_temp<-8
#  N_rain<-6
#  N<-6
#  
#  ### for each variable, create up to 10 lags 
#  for ( i in 1:10){
#    for (v in 1:length(varns)){
#      
#      data[paste0("lag_",i,"_",varns[v])]<-lag(as.formula(paste0(varns[v], "~gdlcode+year")), i, data)
#      data[paste0("lag_",i,"_",modns[v])]<-lag(as.formula(paste0(modns[v], "~gdlcode+year")), i, data)
#      data[paste0("lag_",i,"_",paste(modns[v],'_i_',varns[v],sep=''))]<-lag(as.formula(paste0(paste(modns[v],'_i_',varns[v],sep=''), "~gdlcode+year")), i, data)
#      data[paste0("lag_",i,"_",paste(adap[v],'_i_',varns[v],sep=''))]<-lag(as.formula(paste0(paste(adap[v],'_i_',varns[v],sep=''), "~gdlcode+year")), i, data)
#      
#    }
#  }
#  
#  
#  ext="HW"
#  o="diff_hdi"
#  i="gdlcode + iso3[year] + iso3[year^2] + year"
#  r <- paste0("diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR + ", paste0("diff_",ext), " + ", paste0(ext,"_i_","diff_",ext))
#  f <- as.formula(paste( o, "~", r, "|" ,i ))
#  m <- fixest::feols(f, data, panel.id=pan_id)
#  summary(m)
#  
#  
#  # in the function
#  
#  
#  data_train<-data_dem[ data_dem$year <2020, ]
#  data_test<-data_dem[ data_dem$year ==2020, ]
#  m_cross_val <- fixest::feols(f, data_train)
#  pred<-predict(m_cross_val, data_test)
#  sqrt(sum((data_test[[o]]-pred)^2)/length(pred))
#  
#  data_dem <- fixest::demean(as.formula(paste0(o , "+", r , "~", i )), data = data, na.rm=FALSE)
#  f <- as.formula(paste( o, "~", r ,"-1" ))
#  data_dem$year<-data$year
#  cross_validation_fixest_1year(data_dem=data_dem, f=f, y=2020)
#  cross_validation_fixest_3year(data_dem=data_dem, f=f, y=2020)
#  
#  
#  data_dem=data_dem 
#  f=f 
#  y=2020
#  
#  data_train<-data_dem[ data_dem$year <y, ]
#  data_test<-data_dem[ data_dem$year ==y, ]
#  m_cross_val <- fixest::feols(f, data_train)
#  pred<-predict(m_cross_val, data_test)
#  e1= (sqrt(sum((data_test[[o]]-pred)^2)/length(pred)))
#  
#  data_train<-data_dem[ data_dem$year <(y-1), ]
#  data_test<-data_dem[ data_dem$year >=(y-1), ]
#  m_cross_val <- fixest::feols(f, data_train)
#  pred<-predict(m_cross_val, data_test)
#  e2= (sqrt(sum((data_test[[o]]-pred)^2)/length(pred)))
#  
#  data_train<-data_dem[ data_dem$year <(y-2), ]
#  data_test<-data_dem[ data_dem$year >=(y-2), ]
#  m_cross_val <- fixest::feols(f, data_train)
#  pred<-predict(m_cross_val, data_test)
#  e3= (sqrt(sum((data_test[[o]]-pred)^2)/length(pred)))
#  
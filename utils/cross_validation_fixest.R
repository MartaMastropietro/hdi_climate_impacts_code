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




### example on how to use 
#
#   
#  i <- "gdlcode + year + iso3[year] +iso3[year^2]" # fixed effects part 
#  
#  pan_id<-c('gdlcode', 'year') # id and year 
#  
#  r=paste0("TM + TM_2 + RR + RR_2 + ", cl_var, "+", cl_var, "_2") # climate part 
#  
#  o= "gr_gnipc" # output
#    
#  f= as.formula(paste( o, "~", r, "|" ,i ))
#  m = fixest::feols(f, data , panel.id = pan_id)
#  
#  # cross val
#  data_dem <- fixest::demean(as.formula(paste0(o , "+", r , "~", i )), data = data, na.rm=FALSE)
#  fcv <- as.formula(paste( o, "~", r ,"-1" ))
#  data_dem$year<-data$year
#  
#  loocv<-cross_validation_fixest(data_dem=data_dem, f=fcv)
#  l3ocv<-cross_validation_fixest_3year(data_dem=data_dem, f=fcv, y=2019) # pass last year of data
#  aic<-AIC(m)
#  bic<-BIC(m)
#  wr2<-fixest::r2(m,type='wr2')

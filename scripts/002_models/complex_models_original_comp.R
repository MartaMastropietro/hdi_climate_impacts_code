### rationale: tm, rr controls, but discard non significant lags for each variable. add extremes

rm(list=ls())
source("utils/libraries.R")

# xlsx write
require(openxlsx)

# libraries needed
library(dplyr)
library(readr)
library(ggplot2)
library(plm)

library(gt)

# output dir
out_dir<-"output/models"
if(!dir.exists(out_dir)){dir.create(out_dir)}
out_dir<-"output/models/original_components_tp_extr_complex_models_try"
if(!dir.exists(out_dir)){dir.create(out_dir)}
out_dir<-"output/models/original_components_tp_extr_complex_models_try"
if(!dir.exists(out_dir)){dir.create(out_dir)}

data<- read_csv("output/data_hdi_original_comp_climate_pop_weight_1990_2020_less_na.csv")
data_country <- read_csv("output/data_hdi_original_comp_climate_country_pop_weight_1990_2020_less_na.csv")
all_controls <- read_csv("data/controls/all_controls.csv")

data<-left_join(data, all_controls)

library(slider)
data <- data %>%
  arrange(gdlcode, year) %>%  
  group_by(gdlcode) %>%
  mutate(
    TM_mean = mean(TM),
    RR_mean = mean(RR),
    TVAR_mean = mean(TVAR),
    HW_mean = mean(HW),
    RX_mean = mean(RX),
    PEXT_mean = mean(PEXT),
    WD_mean = mean(WD),
    SPI_mean = mean(SPI),
    SPEI_mean = mean(SPEI),
    PET_mean = mean(PET),
    # Compute 10-year moving averages
    TM_10y_mean = slide_dbl(TM, mean, .before = 9, .complete = TRUE),
    RR_10y_mean = slide_dbl(RR, mean, .before = 9, .complete = TRUE),
    TVAR_10y_mean = slide_dbl(TVAR, mean, .before = 9, .complete = TRUE),
    HW_10y_mean = slide_dbl(HW, mean, .before = 9, .complete = TRUE),
    RX_10y_mean = slide_dbl(RX, mean, .before = 9, .complete = TRUE),
    PEXT_10y_mean = slide_dbl(PEXT, mean, .before = 9, .complete = TRUE),
    WD_10y_mean = slide_dbl(WD, mean, .before = 9, .complete = TRUE),
    SPI_10y_mean = slide_dbl(SPI, mean, .before = 9, .complete = TRUE),
    SPEI_10y_mean = slide_dbl(SPEI, mean, .before = 9, .complete = TRUE),
    PET_10y_mean = slide_dbl(PET, mean, .before = 9, .complete = TRUE)
  )


data_country<-left_join(data_country, all_controls)

### vcov: regional, dk, iso3
fvcov_dk<-function(x) vcov(x, "DK")
fvcov_iso<-function(x) vcov(x, cluster~iso3)


pan_id<-c('gdlcode', 'year')

# setup for lag models 
varns=c('diff_TM','diff_RR', "diff_TVAR", "diff_HW", "diff_RX", "diff_PEXT", "diff_WD", "diff_SPI", "diff_SPEI", "diff_PET")

modns=c('TM','RR', "TVAR", "HW", "RX", "PEXT", "WD", "SPI", "SPEI", "PET")

# historical mean vars 
mean_modns=c('mTM','mRR', "mTVAR", "mHW", "mRX", "mPEXT", "mWD", "mSPI", "mSPEI", "mPET")

adap=c('lag_log_gni_pc','lag_log_gni_pc', 'lag_log_gni_pc','lag_log_gni_pc', 'lag_log_gni_pc','lag_log_gni_pc', 'lag_log_gni_pc','lag_log_gni_pc', 'lag_log_gni_pc','lag_log_gni_pc')

for (i in 1:length(varns)){
  data[paste(modns[i],'_i_',varns[i],sep='')] <- data[modns[i]] * data[varns[i]]
} 

for (i in 1:length(varns)){ # 
  data[paste(adap[i],'_i_',varns[i],sep='')] <- data[adap[i]] * data[varns[i]]
} 


for (i in 1:length(modns)){ # 
  data[paste(adap[i],'_i_',modns[i],sep='')] <- data[adap[i]] * data[modns[i]]
} 

for (i in 1:length(varns)){ # 
  data[paste("TM",'_i_',varns[i],sep='')] <- data["TM"] * data[varns[i]]
} 

for (i in 1:length(varns)){ # 
  data[paste("RR",'_i_',varns[i],sep='')] <- data["RR"] * data[varns[i]]
} 

for (i in 1:length(varns)){ # 
  data[paste("TM",'_i_',modns[i],sep='')] <- data["TM"] * data[modns[i]]
} 

for (i in 1:length(varns)){ # 
  data[paste("RR",'_i_',modns[i],sep='')] <- data["RR"] * data[modns[i]]
} 



### for each variable, create up to 10 lags 
for ( i in 1:10){
  for (v in 1:length(varns)){
    
    data[paste0("lag_",i,"_",varns[v])]<-fixest::lag_fml(as.formula(paste0(varns[v], "~gdlcode+year")), i, data)
    data[paste0("lag_",i,"_",modns[v])]<-fixest::lag_fml(as.formula(paste0(modns[v], "~gdlcode+year")), i, data)
    data[paste0("lag_",i,"_",paste(modns[v],'_i_',varns[v],sep=''))]<-fixest::lag_fml(as.formula(paste0(paste(modns[v],'_i_',varns[v],sep=''), "~gdlcode+year")), i, data)
    data[paste0("lag_",i,"_",paste("TM",'_i_',varns[v],sep=''))]<-fixest::lag_fml(as.formula(paste0(paste("TM",'_i_',varns[v],sep=''), "~gdlcode+year")), i, data)
    data[paste0("lag_",i,"_",paste("RR",'_i_',varns[v],sep=''))]<-fixest::lag_fml(as.formula(paste0(paste("RR",'_i_',varns[v],sep=''), "~gdlcode+year")), i, data)
    data[paste0("lag_",i,"_",paste(adap[v],'_i_',varns[v],sep=''))]<-fixest::lag_fml(as.formula(paste0(paste(adap[v],'_i_',varns[v],sep=''), "~gdlcode+year")), i, data)
    data[paste0("lag_",i,"_",paste(adap[v],'_i_',modns[v],sep=''))]<-fixest::lag_fml(as.formula(paste0(paste(adap[v],'_i_',modns[v],sep=''), "~gdlcode+year")), i, data)
    
  }
}

# country data

for (i in 1:length(varns)){
  data_country[paste(modns[i],'_i_',varns[i],sep='')] <- data_country[modns[i]] * data_country[varns[i]]
} 

for (i in 1:length(varns)){ # 
  data_country[paste(adap[i],'_i_',varns[i],sep='')] <- data_country[adap[i]] * data_country[varns[i]]
} 


for (i in 1:length(modns)){ # 
  data_country[paste(adap[i],'_i_',modns[i],sep='')] <- data_country[adap[i]] * data_country[modns[i]]
} 

for (i in 1:length(varns)){ # 
  data_country[paste("TM",'_i_',varns[i],sep='')] <- data_country["TM"] * data_country[varns[i]]
} 

for (i in 1:length(varns)){ # 
  data_country[paste("RR",'_i_',varns[i],sep='')] <- data_country["RR"] * data_country[varns[i]]
} 

for (i in 1:length(varns)){ # 
  data_country[paste("TM",'_i_',modns[i],sep='')] <- data_country["TM"] * data_country[varns[i]]
} 

for (i in 1:length(varns)){ # 
  data_country[paste("RR",'_i_',modns[i],sep='')] <- data_country["RR"] * data_country[varns[i]]
} 



### for each variable, create up to 10 lags 
for ( i in 1:10){
  for (v in 1:length(varns)){
    
    data_country[paste0("lag_",i,"_",varns[v])]<-fixest::lag_fml(as.formula(paste0(varns[v], "~gdlcode+year")), i, data_country)
    data_country[paste0("lag_",i,"_",modns[v])]<-fixest::lag_fml(as.formula(paste0(modns[v], "~gdlcode+year")), i, data_country)
    data_country[paste0("lag_",i,"_",paste(modns[v],'_i_',varns[v],sep=''))]<-fixest::lag_fml(as.formula(paste0(paste(modns[v],'_i_',varns[v],sep=''), "~gdlcode+year")), i, data_country)
    data_country[paste0("lag_",i,"_",paste("TM",'_i_',varns[v],sep=''))]<-fixest::lag_fml(as.formula(paste0(paste("TM",'_i_',varns[v],sep=''), "~gdlcode+year")), i, data_country)
    data_country[paste0("lag_",i,"_",paste("RR",'_i_',varns[v],sep=''))]<-fixest::lag_fml(as.formula(paste0(paste("RR",'_i_',varns[v],sep=''), "~gdlcode+year")), i, data_country)
    data_country[paste0("lag_",i,"_",paste(adap[v],'_i_',varns[v],sep=''))]<-fixest::lag_fml(as.formula(paste0(paste(adap[v],'_i_',varns[v],sep=''), "~gdlcode+year")), i, data_country)
    data_country[paste0("lag_",i,"_",paste(adap[v],'_i_',modns[v],sep=''))]<-fixest::lag_fml(as.formula(paste0(paste(adap[v],'_i_',modns[v],sep=''), "~gdlcode+year")), i, data_country)
    
  }
}


### all with this fe spec 
i <- "gdlcode + year + iso3[year] +iso3[year^2]"

################################################################################
### one at the time extreme variables addition
################################################################################

### models edu -> mean years of schooling age 25+

o<-"gr_eys"

models<-list()

# diff model
r <- "diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR  "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

# diff model + wd
r <- "diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR +
WD +  WD_2"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

# diff model + pext
r <- "diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR +
PEXT + PEXT_2 + TM_i_PEXT"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m


# diff model + rx
r <- "diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR +
RX + TM_i_RX"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

# diff model + tvar
r <- "diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR +
TVAR + TM_i_TVAR"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m



# wd
r <- "WD +  WD_2 "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

# pext
r <- "PEXT + PEXT_2 + TM_i_PEXT"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

# rx
r <- "RX +  TM_i_RX"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

# tvar
r <- "TVAR +  TM_i_TVAR"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m


modelsummary(models, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir, paste0(o,"_complex_models.html")))
modelsummary(models, vcov=lapply(models, FUN=fvcov_dk), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir,  paste0(o,"_complex_models_dk.html")))
modelsummary(models, vcov=lapply(models, FUN=fvcov_iso), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir,  paste0(o,"_complex_models_iso.html")))


modelsummary(models, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.2e"),  gof_map = NA, output=file.path(out_dir, paste0(o,"_complex_models.tex")))
modelsummary(models, vcov=lapply(models, FUN=fvcov_dk), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"),  gof_map = NA, fmt = fmt_sprintf("%.2e"), output=file.path(out_dir,  paste0(o,"_complex_models_dk.tex")))
modelsummary(models, vcov=lapply(models, FUN=fvcov_iso), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"),  gof_map = NA, fmt = fmt_sprintf("%.2e"), output=file.path(out_dir,  paste0(o,"_complex_models_iso.tex")))

# tab <- modelsummary(models, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"),  output = "gt" ) %>%
#   tab_spanner(label = o) 
# gt::gtsave(tab, filename =file.path(out_dir, paste0(o,"_complex_models.html")) )

################################################################################

### models health -> life exp birth 

o<-"gr_leb"

models<-list()

# burke model
r <- "TM + TM_2 + RR + RR_2 "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
 
# diff model
r <- "diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR  "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

# diff model + HW
r <- "diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR + 
diff_HW + TM_i_diff_HW"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

# diff model + WD
r <- "diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR + 
diff_WD + TM_i_diff_WD"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

# diff model + RX
r <- "diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR + 
diff_RX + TM_i_diff_RX"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

# diff model + TVAR
r <- "diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR + 
diff_TVAR + TVAR_i_diff_TVAR"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m


# HW
r <- "diff_HW + TM_i_diff_HW"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m


#  WD
r <- "diff_WD + TM_i_diff_WD"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

#  RX
r <- "diff_RX + TM_i_diff_RX"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m


#  TVAR
r <- "diff_TVAR + TVAR_i_diff_TVAR"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m



modelsummary(models, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir, paste0(o,"_complex_models.html")))
modelsummary(models, vcov=lapply(models, FUN=fvcov_dk), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir,  paste0(o,"_complex_models_dk.html")))
modelsummary(models, vcov=lapply(models, FUN=fvcov_iso), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir,  paste0(o,"_complex_models_iso.html")))


modelsummary(models, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.2e"),  gof_map = NA, output=file.path(out_dir, paste0(o,"_complex_models.tex")))
modelsummary(models, vcov=lapply(models, FUN=fvcov_dk), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"),  gof_map = NA, fmt = fmt_sprintf("%.2e"), output=file.path(out_dir,  paste0(o,"_complex_models_dk.tex")))
modelsummary(models, vcov=lapply(models, FUN=fvcov_iso), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"),  gof_map = NA, fmt = fmt_sprintf("%.2e"), output=file.path(out_dir,  paste0(o,"_complex_models_iso.tex")))

# tab <- modelsummary(models, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"),  output = "gt" ) %>%
#   tab_spanner(label = o) 
# gt::gtsave(tab, filename =file.path(out_dir, paste0(o,"_complex_models.html")) )

################################################################################

### models gnipc

o<-"gr_gnipc"

models<-list()

# burke model
r <- "TM + TM_2 + RR + RR_2 "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m


# diff model -> keep this, as we use this for all others, for easier comparison
r <- "diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR +l(diff_TM,1) + l(TM_i_diff_TM,1)   "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m


# diff + wd

r <- "diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR  +
     l(diff_TM,1) + l(TM_i_diff_TM,1)  + 
      diff_WD + RR_i_diff_WD "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m


# diff + pext

r <- "diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR  +
  l(diff_TM,1) + l(TM_i_diff_TM,1)  + 
      diff_PEXT + TM_i_diff_PEXT"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m



# diff + hw

r <- "diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR  +
  l(diff_TM,1) + l(TM_i_diff_TM,1)  + 
      diff_HW + TM_i_diff_HW"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m


r <- "TM + TM_2 + RR + RR_2 +
diff_HW + TM_i_diff_HW"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m


r <- "diff_WD + RR_i_diff_WD "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

r <- "diff_PEXT + TM_i_diff_PEXT"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

r <- "diff_HW + TM_i_diff_HW"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m




modelsummary(models, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir, paste0(o,"_complex_models.html")))
modelsummary(models, vcov=lapply(models, FUN=fvcov_dk), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir,  paste0(o,"_complex_models_dk.html")))
modelsummary(models, vcov=lapply(models, FUN=fvcov_iso), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir,  paste0(o,"_complex_models_iso.html")))


modelsummary(models, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.2e"),  gof_map = NA, output=file.path(out_dir, paste0(o,"_complex_models.tex")))
modelsummary(models, vcov=lapply(models, FUN=fvcov_dk), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"),  gof_map = NA, fmt = fmt_sprintf("%.2e"), output=file.path(out_dir,  paste0(o,"_complex_models_dk.tex")))
modelsummary(models, vcov=lapply(models, FUN=fvcov_iso), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"),  gof_map = NA, fmt = fmt_sprintf("%.2e"), output=file.path(out_dir,  paste0(o,"_complex_models_iso.tex")))


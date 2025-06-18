
rm(list=ls())

library(readr)
library(dplyr)
library(ggplot2)

library(RColorBrewer)

out_dir<-"output/projections/original_comp/pop_weight_gdlcode/final_proj"
if(!dir.exists(out_dir)){dir.create(out_dir)}

#lags number 
N<-"_mix_"
#N<-8

out_dir<-file.path(out_dir, paste0("N",N,"lags"))
if(!dir.exists(out_dir)){dir.create(out_dir)}

out_dir<-file.path(out_dir, "tp_extr_sep")
if(!dir.exists(out_dir)){dir.create(out_dir)}

variables_sep<-c("_tp", "_extr")

out_dir_pop<-"output/projections/original_comp/pop_weight_gdlcode"
out_dir_lag<-file.path(out_dir_pop, "lag_models")
out_dir_lag<-file.path(out_dir_pop, "lag_models", "tp_extr_sep")
if(!dir.exists(out_dir_lag)){dir.create(out_dir_lag)}

out_variables<-c( "leb", "eys", "gnipc"  )

out_dir_comp<-"output/projections/original_comp" 

specifications<-c("mean_mod") #   ,"mean_mod_spec")
types<-c("all_extr_tp") #  , "all_vars_adap")#, "all_vars_controls", "all_vars_autoreg")

vars_in_proj<-c( "all_extr_tp", "extr_only")


spec_type<-"dlm"

effect<-"growth_eff"

spec<-specifications[1]
type<-types[1]
vars<-vars_in_proj[1]



quant05 <- function(x){
  quantile(x, probs=0.05, na.rm=TRUE)
}
quant95 <- function(x){
  quantile(x, probs=0.95, na.rm=TRUE)
}
quant10 <- function(x){
  quantile(x, probs=0.10, na.rm=TRUE)
}
quant90 <- function(x){
  quantile(x, probs=0.90, na.rm=TRUE)
}


################################################################################
################################################################################
################################################################################



# maps: median diff or perc diff (med_values_deltas) selecting 90% and 2/3 models from the subnat (deltas boot intervals)

out_dir_maps<-file.path(out_dir, "maps")
if(!dir.exists(out_dir_maps)){dir.create(out_dir_maps)}

gdl_shape_file <- sf::st_read("data/hdi_data/downloaded/GDL Shapefiles V6.2/GDL Shapefiles V6.2 large.shp")
gdl_shape_file<-gdl_shape_file[, c("gdlcode", "geometry")]
gdl_shape_file<-sf::st_as_sf(gdl_shape_file)


library(ggplot2)
library(ggpubr)
library(ggpattern)

for (v in variables_sep){
  #### income_index
  
  gc()
  out_var<-"income_index"
  out_var_plot<-"Income index"
  glob_data<-arrow::read_feather(file.path(out_dir_lag,paste0(out_var,v,".feather")))
  
  glob_data <- glob_data %>%
    mutate(same_sign_90 = sign(delta_q90) == sign(delta_q10)) %>%
    group_by(gdlcode, year, ssp) %>%
    summarise(sign_90 = as.integer(mean(same_sign_90) >= 2/3), .groups = "drop") %>%
    right_join(glob_data, by = c("gdlcode", "year", "ssp"))  
  
  glob_data <- glob_data %>%
    mutate(same_sign_perc_90 = sign(perc_delta_q90) == sign(perc_delta_q10)) %>%
    group_by(gdlcode, year, ssp) %>%
    summarise(sign_perc_90 = as.integer(mean(same_sign_perc_90) >= 2/3), .groups = "drop") %>%
    right_join(glob_data, by = c("gdlcode", "year", "ssp"))  
  
  data_median<-glob_data%>%group_by(gdlcode, year, ssp) %>%
    mutate(median_delta_mean=median(delta_mean, na.rm=TRUE),
           median_perc_delta_mean=median(perc_delta_mean, na.rm=TRUE),
           median_cc_mean=median(cc_mean, na.rm=TRUE))%>%
    select(gdlcode, year, ssp, iso3, 
           sign_90,sign_perc_90,
           median_delta_mean,median_perc_delta_mean,median_cc_mean,income_index)%>%distinct()
  
  rm(glob_data)
  gc()
  years<-2050#c(2030,2050,2080,2100)
  
  for (sc in unique(data_median$ssp)){
    for (y in years){
      
      gc()
      plot_map<-data_median%>%
        filter(ssp %in% sc)%>%
        filter(year %in% y)
      
      plot_map<-inner_join(plot_map, gdl_shape_file)
      plot_map<-sf::st_as_sf(plot_map)
      
      # # 90 , absolute changes
      g<-ggplot(plot_map)+geom_sf( aes(fill=median_delta_mean*100), color = 'black', lwd=0.08)+ theme_bw()+ scale_fill_gradient2() + 
        labs(fill=paste0(out_var_plot,"\nchange") )+ # ggtitle(paste0("Effect wrt baseline, ",sc,", year ", y)) + 
        geom_sf_pattern(data =plot_map %>% filter(sign_90 == 0), 
                        pattern = "stripe",pattern_fill = "gray50",color='black', lwd=0.08,pattern_colour = NA,
                        fill = "white", size=0.1, pattern_spacing=0.01 )  + guides(pattern="non-sign") 
      ggsave(filename=file.path(out_dir_maps, paste0("med_mod_sign_90_", out_var, "_", sc ,"_", y,v,"_dam.png")), g, width=6, height=3)
      # 
      # 90 , perc changes
      g<-ggplot(plot_map)+geom_sf( aes(fill=median_perc_delta_mean*100), color = 'black', lwd=0.08)+ theme_bw()+ scale_fill_gradient2() + 
        labs(fill=paste0(out_var_plot ,"\nperc. change") )+ # ggtitle(paste0("Effect wrt baseline, ",sc,", year ", y)) + 
        geom_sf_pattern(data =plot_map %>% filter(sign_perc_90 == 0), 
                        pattern = "stripe",pattern_fill = "gray50",color='black', lwd=0.08,pattern_colour = NA,
                        fill = "white", size=0.1, pattern_spacing=0.01 )  + guides(pattern="non-sign") 
      ggsave(filename=file.path(out_dir_maps, paste0("med_mod_sign_90_", out_var, "_", sc ,"_", y,v, "_perc_dam.png")), g, width=6, height=3)
      
    }
  }
  
  
  
  #### lifex_index
  
  gc()
  out_var<-"lifex_index"
  out_var_plot<-"Lifex index"
  glob_data<-arrow::read_feather(file.path(out_dir_lag,paste0(out_var,v,".feather")))
  
  glob_data <- glob_data %>%
    mutate(same_sign_90 = sign(delta_q90) == sign(delta_q10)) %>%
    group_by(gdlcode, year, ssp) %>%
    summarise(sign_90 = as.integer(mean(same_sign_90) >= 2/3), .groups = "drop") %>%
    right_join(glob_data, by = c("gdlcode", "year", "ssp"))  
  
  glob_data <- glob_data %>%
    mutate(same_sign_perc_90 = sign(perc_delta_q90) == sign(perc_delta_q10)) %>%
    group_by(gdlcode, year, ssp) %>%
    summarise(sign_perc_90 = as.integer(mean(same_sign_perc_90) >= 2/3), .groups = "drop") %>%
    right_join(glob_data, by = c("gdlcode", "year", "ssp"))  
  
  data_median<-glob_data%>%group_by(gdlcode, year, ssp) %>%
    mutate(median_delta_mean=median(delta_mean, na.rm=TRUE),
           median_perc_delta_mean=median(perc_delta_mean, na.rm=TRUE),
           median_cc_mean=median(cc_mean, na.rm=TRUE))%>%
    select(gdlcode, year, ssp, iso3, 
           sign_90,sign_perc_90,
           median_delta_mean,median_perc_delta_mean,median_cc_mean,lifex_index)%>%distinct()
  
  rm(glob_data)
  gc()
  years<-2050#c(2030,2050,2080,2100)
  
  for (sc in unique(data_median$ssp)){
    for (y in years){
      
      gc()
      plot_map<-data_median%>%
        filter(ssp %in% sc)%>%
        filter(year %in% y)
      
      plot_map<-inner_join(plot_map, gdl_shape_file)
      plot_map<-sf::st_as_sf(plot_map)
      
      # # 90 , absolute changes
      g<-ggplot(plot_map)+geom_sf( aes(fill=median_delta_mean*100), color = 'black', lwd=0.08)+ theme_bw()+ scale_fill_gradient2() + 
        labs(fill=paste0(out_var_plot,"\nchange") )+ # ggtitle(paste0("Effect wrt baseline, ",sc,", year ", y)) + 
        geom_sf_pattern(data =plot_map %>% filter(sign_90 == 0), 
                        pattern = "stripe",pattern_fill = "gray50",color='black', lwd=0.08,pattern_colour = NA,
                        fill = "white", size=0.1, pattern_spacing=0.01 )  + guides(pattern="non-sign") 
      ggsave(filename=file.path(out_dir_maps, paste0("med_mod_sign_90_", out_var, "_", sc ,"_", y,v,"_dam.png")), g, width=6, height=3)
      # 
      # 90 , perc changes
      g<-ggplot(plot_map)+geom_sf( aes(fill=median_perc_delta_mean*100), color = 'black', lwd=0.08)+ theme_bw()+ scale_fill_gradient2() + 
        labs(fill=paste0(out_var_plot ,"\nperc. change") )+ # ggtitle(paste0("Effect wrt baseline, ",sc,", year ", y)) + 
        geom_sf_pattern(data =plot_map %>% filter(sign_perc_90 == 0), 
                        pattern = "stripe",pattern_fill = "gray50",color='black', lwd=0.08,pattern_colour = NA,
                        fill = "white", size=0.1, pattern_spacing=0.01 )  + guides(pattern="non-sign") 
      ggsave(filename=file.path(out_dir_maps, paste0("med_mod_sign_90_", out_var, "_", sc ,"_", y,v, "_perc_dam.png")), g, width=6, height=3)
      
    }
  }
  
  
  #### edu_index
  
  gc()
  out_var<-"edu_index"
  out_var_plot<-"Edu index"
  glob_data<-arrow::read_feather(file.path(out_dir_lag,paste0(out_var,v,".feather")))
  
  glob_data <- glob_data %>%
    mutate(same_sign_90 = sign(delta_q90) == sign(delta_q10)) %>%
    group_by(gdlcode, year, ssp) %>%
    summarise(sign_90 = as.integer(mean(same_sign_90) >= 2/3), .groups = "drop") %>%
    right_join(glob_data, by = c("gdlcode", "year", "ssp"))  
  
  glob_data <- glob_data %>%
    mutate(same_sign_perc_90 = sign(perc_delta_q90) == sign(perc_delta_q10)) %>%
    group_by(gdlcode, year, ssp) %>%
    summarise(sign_perc_90 = as.integer(mean(same_sign_perc_90) >= 2/3), .groups = "drop") %>%
    right_join(glob_data, by = c("gdlcode", "year", "ssp"))  
  
  data_median<-glob_data%>%group_by(gdlcode, year, ssp) %>%
    mutate(median_delta_mean=median(delta_mean, na.rm=TRUE),
           median_perc_delta_mean=median(perc_delta_mean, na.rm=TRUE),
           median_cc_mean=median(cc_mean, na.rm=TRUE))%>%
    select(gdlcode, year, ssp, iso3, 
           sign_90,sign_perc_90,
           median_delta_mean,median_perc_delta_mean,median_cc_mean,edu_index)%>%distinct()
  
  rm(glob_data)
  gc()
  years<-2050#c(2030,2050,2080,2100)
  
  for (sc in unique(data_median$ssp)){
    for (y in years){
      
      gc()
      plot_map<-data_median%>%
        filter(ssp %in% sc)%>%
        filter(year %in% y)
      
      plot_map<-inner_join(plot_map, gdl_shape_file)
      plot_map<-sf::st_as_sf(plot_map)
      
      # # 90 , absolute changes
      g<-ggplot(plot_map)+geom_sf( aes(fill=median_delta_mean*100), color = 'black', lwd=0.08)+ theme_bw()+ scale_fill_gradient2() + 
        labs(fill=paste0(out_var_plot,"\nchange") )+ # ggtitle(paste0("Effect wrt baseline, ",sc,", year ", y)) + 
        geom_sf_pattern(data =plot_map %>% filter(sign_90 == 0), 
                        pattern = "stripe",pattern_fill = "gray50",color='black', lwd=0.08,pattern_colour = NA,
                        fill = "white", size=0.1, pattern_spacing=0.01 )  + guides(pattern="non-sign") 
      ggsave(filename=file.path(out_dir_maps, paste0("med_mod_sign_90_", out_var, "_", sc ,"_", y,v,"_dam.png")), g, width=6, height=3)
      # 
      # 90 , perc changes
      g<-ggplot(plot_map)+geom_sf( aes(fill=median_perc_delta_mean*100), color = 'black', lwd=0.08)+ theme_bw()+ scale_fill_gradient2() + 
        labs(fill=paste0(out_var_plot ,"\nperc. change") )+ # ggtitle(paste0("Effect wrt baseline, ",sc,", year ", y)) + 
        geom_sf_pattern(data =plot_map %>% filter(sign_perc_90 == 0), 
                        pattern = "stripe",pattern_fill = "gray50",color='black', lwd=0.08,pattern_colour = NA,
                        fill = "white", size=0.1, pattern_spacing=0.01 )  + guides(pattern="non-sign") 
      ggsave(filename=file.path(out_dir_maps, paste0("med_mod_sign_90_", out_var, "_", sc ,"_", y,v, "_perc_dam.png")), g, width=6, height=3)
      
    }
  }
  
  
  
}


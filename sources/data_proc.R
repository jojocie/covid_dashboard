library(tidyverse)
library(zoo)
#library("rstudioapi") 
#print(dirname(getActiveDocumentContext()$path))
#print(dirname(sys.frame(2)$ofile))
# 
# thisFile <- function() {
#   cmdArgs <- commandArgs(trailingOnly = FALSE)
#   needle <- "--file="
#   match <- grep(needle, cmdArgs)
#   if (length(match) > 0) {
#     # Rscript
#     return(normalizePath(sub(needle, "", cmdArgs[match])))
#   } else {
#     # 'source'd via R console
#     return(normalizePath(sys.frames()[[1]]$ofile))
#   }
# }
# this_file_path = thisFile()
# this_file_path = dirname(this_file_path)
# setwd(this_file_path)
#setwd("~/Documents/covid_projects/covid_dashboard/")

compute_rolled_data = function(df,day_lag = 1,daily_average = TRUE){
  rolling_positives = df$P %>% rollsum(day_lag)
  rolling_tests = df$T %>% rollsum(day_lag)
  
  if (daily_average){
    rolling_positives = rolling_positives/ day_lag
    rolling_tests = rolling_tests / day_lag
    
    
  }
  if (day_lag > 1) {
    days = df$jour[-(1:(day_lag-1))]
  } else {
    days = df$jour
  }
  
  return(list(days = days, rolling_positives=rolling_positives,rolling_tests=rolling_tests))
  
}

get_fr_data = function(day_lag=1){
  sp_pos_quot_fra = read.csv2("data/sp-pos-quot-fra.csv")
  sp_pos_quot_fra$jour = as.Date.factor(sp_pos_quot_fra$jour)
  sp_pos_quot_fra$cl_age90 = as.factor(sp_pos_quot_fra$cl_age90)
  sp_pos_quot_fra_age_groupped = sp_pos_quot_fra %>% filter(cl_age90 == "0")
  returned_df = compute_rolled_data(sp_pos_quot_fra_age_groupped,day_lag = day_lag)
  return(returned_df)

}
#sp_pos_heb_fra = read.csv2("donnees_gouvs/sp-pos-heb-fra-2020-07-15-19h15.csv")

get_dep_data = function(dep_val = 75, day_lag = 1){
  
  sp_pos_quot_dep = read.csv2("data/sp-pos-quot-dep.csv")
  sp_pos_quot_dep$jour = as.Date.factor(sp_pos_quot_dep$jour)
  sp_pos_quot_dep$dep = as.character(sp_pos_quot_dep$dep)
  sp_pos_quot_dep_groupped = sp_pos_quot_dep %>% filter(dep == dep_val) %>%  filter(cl_age90==0) 
  returned_df = compute_rolled_data(sp_pos_quot_dep_groupped,day_lag = day_lag)
  return(returned_df)
  
}


get_hosp_data = function(){
  col_to_keep = c('hosp','rea','rad','dc')
  donnees_hosp = read.csv2("data/donnees-hospitalieres-covid19.csv")
  donnees_hosp$jour = as.Date.factor(donnees_hosp$jour)
  
  # Un défaut a été détecté sur la base de données pour le 24/03/2020
  donnees_hosp = donnees_hosp %>% filter(dep!='') %>%droplevels()
  
  # On utilise uniquement les données non genrées (hommes + femmes). La colonne sexe n'a donc plus d'interêt
  donnees_hosp_mixed = donnees_hosp%>%filter(sexe == 0) %>% droplevels() %>% select(-sexe)
  all_dep = unique(donnees_hosp_mixed$dep)
  
  
  # Certaines dates sont marquées en NA. Pourtant on peut supposer (et vérifier sur certain départements) que ces dates sont certainement la continuité des précédentes.
  for (dep_itt in all_dep){
    donnees_hosp_mixed$jour[donnees_hosp_mixed$dep == dep_itt] = na.fill(donnees_hosp_mixed$jour[donnees_hosp_mixed$dep == dep_itt],'extend')
  }
  

  fr_donnees_hosp_mixed = data.frame()
  for (day_itt in unique(donnees_hosp_mixed$jour)){
    s = donnees_hosp_mixed %>% filter(jour == day_itt) %>% select(col_to_keep) %>%colSums()
    
    line_to_add = cbind(data.frame(dep = 'France' ,jour = as.Date(day_itt )),t(s))
    
    fr_donnees_hosp_mixed = rbind(fr_donnees_hosp_mixed,line_to_add)
    
    
    
  }  
  
  
  donnees_hosp_mixed = rbind(donnees_hosp_mixed %>% select(c('dep','jour',col_to_keep)),fr_donnees_hosp_mixed)
  donnees_hosp_mixed$diff_dc = 0
  all_dep = c(all_dep,'France')
  for (dep_itt in all_dep) {  
    sort_index = order(donnees_hosp_mixed$jour[donnees_hosp_mixed$dep == dep_itt])
    
    donnees_hosp_mixed[donnees_hosp_mixed$dep == dep_itt,] = donnees_hosp_mixed[donnees_hosp_mixed$dep == dep_itt,][sort_index,]
    n_days = dim(donnees_hosp_mixed[donnees_hosp_mixed$dep == dep_itt,])[1]
    donnees_hosp_mixed$diff_dc[donnees_hosp_mixed$dep == dep_itt][-1] = donnees_hosp_mixed$dc[donnees_hosp_mixed$dep == dep_itt][-1] - 
      donnees_hosp_mixed$dc[donnees_hosp_mixed$dep == dep_itt][-n_days]
    
    
    
  }
  
  return(donnees_hosp_mixed)
  
  
}

prossess_hosp_graph = function (donnees_hosp_mixed,dep_itt = 'France',k = 7, alignement = 'center'){

  
  ref_jour = as.Date('2020/05/11')
  
  
  
  get_rolled_hosp_data = function(df, selected_dep, k, alignement){
    
    dates = df %>% filter(dep==selected_dep) %>% pull(jour)
    if (alignement == 'right'){
      dates = dates[-(1:(k-1))]
    }else if (alignement == 'center'){
      dates = dates[ceiling(k/2):(length(dates) - floor(k/2))]
      
    }
    

    cat('k = ', k ,'\n')
    df_rolled = data.frame(hosp = df %>% filter(dep==selected_dep) %>% pull(hosp) %>% rollmean(k))# option moving average
    df_rolled$rea = df %>% filter(dep==selected_dep) %>% pull(rea) %>% rollmean(k)
    df_rolled$dc = df %>% filter(dep==selected_dep) %>% pull(dc) %>% rollmean(k)
    df_rolled$diff_dc = df %>% filter(dep==selected_dep) %>% pull(diff_dc) %>% rollmean(k)
    print(length(df_rolled$diff_dc))
    df_rolled$jour = dates
    return(df_rolled)
    
  }
  
  
  donnees_hosp_mixed_rolled = get_rolled_hosp_data(donnees_hosp_mixed,dep_itt,k, alignement)
  
  # hosp_normed_factor = donnees_hosp_mixed %>% filter(jour == ref_jour & dep == dep_itt) %>% pull(hosp)/100
  # 
  # rea_normed_factor = donnees_hosp_mixed %>% filter(jour == ref_jour & dep == dep_itt) %>% pull(rea)/100
  # 
  # diff_dc_normed_factor = donnees_hosp_mixed %>% filter(jour == ref_jour & dep == dep_itt) %>% pull(diff_dc)/100
  # if (diff_dc_normed_factor == 0){
  #   diff_dc_normed_factor =1/ 100
  # }
  # 
  # donnees_hosp_mixed_rolled_normed = data_frame(hosp = donnees_hosp_mixed_rolled$hosp/ hosp_normed_factor )
  # donnees_hosp_mixed_rolled_normed$rea = donnees_hosp_mixed_rolled$rea / rea_normed_factor
  # donnees_hosp_mixed_rolled_normed$diff_dc = donnees_hosp_mixed_rolled$diff_dc / diff_dc_normed_factor
  # donnees_hosp_mixed_rolled_normed$jour = donnees_hosp_mixed_rolled$jour
  # 
  # plot(donnees_hosp_mixed_rolled_normed$jour,donnees_hosp_mixed_rolled_normed$hosp,type = 'l')
  # par(new = TRUE)
  # plot(donnees_hosp_mixed_rolled_normed$jour,donnees_hosp_mixed_rolled_normed$rea,type = 'l', col="orange")
  # par(new = TRUE)
  # plot(donnees_hosp_mixed_rolled_normed$jour,donnees_hosp_mixed_rolled_normed$diff_dc,type = 'l', col="red")
  
  
  
  

    graph_indicators = list(
      data=list(
        list(
          x=as.list(donnees_hosp_mixed_rolled$jour),
          y=as.list(donnees_hosp_mixed_rolled$hosp),
          name = "hosp",
          line = list(color = "#1f77b4")
        ),
        
        list(
          x=as.list(donnees_hosp_mixed_rolled$jour),
          y = as.list(donnees_hosp_mixed_rolled$rea),
          name = "rea",
          yaxis='y2',
          line = list(color = "#ff7f0e")
        ),
        
        list(
          x=as.list(donnees_hosp_mixed_rolled$jour),
          y = as.list(donnees_hosp_mixed_rolled$diff_dc),
          name = "diff_dc",
          yaxis = 'y3',
          line = list(color = "#d62728")
        )
        
      ),
      
      layout = list(title='Données hospitalières (moyenne cumulée)',
                    xaxis = list(domain=list(0.16, 1)),
                    yaxis = list(title = 'hospitalisations',
                                 hoverformat = ".0f",
                                 titlefont=list(color="#1f77b4"),
                                 tickfont=list(color="#1f77b4"),
                                 showgrid = FALSE
                                 ),
                    yaxis2 = list(title = 'réanimations', hoverformat = ".0f",
                                  titlefont=list(color="#ff7f0e"),
                                  tickfont=list(color="#ff7f0e"),
                                  anchor="free",
                                  overlaying="y",
                                  side="left",
                                  position=0.08,
                                  showgrid = FALSE),
                    yaxis3 = list(title = 'Décès',
                                  hoverformat = ".1f",
                                  titlefont=list(color="#d62728"),
                                  tickfont=list(color="#d62728"),
                                  anchor="x",
                                  overlaying="y",
                                  side="right",
                                  showgrid = FALSE
                                  )
                                
                    )
    )
    
    graph_severity = list(
      data=list(
        list(
          x=as.list(donnees_hosp_mixed_rolled$jour),
          y=as.list(donnees_hosp_mixed_rolled$rea/ (donnees_hosp_mixed_rolled$hosp)),
          name = "ration",
          line = list(color = "#1f77b4")
        
      )),
      
      layout = list(title='Proportion de réanimation parmi les personnes hospitalisées',
                    xaxis = list(domain=list(0.16, 1)),
                    yaxis = list(title = 'rea / hosp',
                                 hoverformat = ".2%",
                                 titlefont=list(color="#1f77b4"),
                                 tickfont=list(color="#1f77b4"),
                                 showgrid = FALSE
                    )

                    
      )
    )
  
    return(list(graph_indicators,graph_severity))
  
}






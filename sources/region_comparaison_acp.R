library(FactoMineR)
print(getwd())

load("intermediate_data/territoires_france_infos.Rdata")


get_dep_acp_var_selection_dropdown = function(){
  
  dd = dccChecklist(
    options=list(
      list(label = "Décès", value = "dc"),
      list(label = "Hospitalisation", value = "hosp"),
      list(label = "Réanimation", value = "rea")
    ),
    value = list("dc"),
    id = "dep_acp_var_selection_dropdown"
    )
  return(dd)
}

get_dep_acp_computation_selection_dropdown = function(){
  
  dd = dccChecklist(
    options=list(
      list(label = "Population", value = "pop"),
      list(label = "Densité", value = "density"),
      list(label = "Superficie", value = "sup")
    ),
    value = list("pop"),
    id = "dep_acp_computation_selection_dropdown"
  )
  return(dd)
}


compute_acp_data = function(territoires_france_infos ,donnees_hosp_mixed,min_date = as.Date("2020/01/01"), max_date = as.Date("2025/04/13"),interest_variables = c("dc","hosp","rea"),computations=c('pop')){
  #computations : list in c("pop","density","sup")
  #computations = list("pop","density",'sup')
  computations = as.vector(computations)
  interest_pca_variables = c()
  territoires_france_infos_loc = territoires_france_infos %>% filter(territoires_france_infos$CODDEP %in% donnees_hosp_mixed$dep)
  print(class(min_date))
  print(max_date)
  for(var_itt in interest_variables){
    territoires_france_infos_loc[[var_itt]] = 0
    for (dep_itt in territoires_france_infos_loc$CODDEP){
      # nbr = donnees_hosp_mixed %>% filter(jour >= min_date & jour <= max_date & dep == dep_itt) %>% pull(var_itt) %>% sum()
      dep_data = donnees_hosp_mixed %>% filter(dep == dep_itt)
      jours_min_diff = abs(dep_data$jour - min_date)
      jours_max_diff = abs(dep_data$jour - max_date)
      
      jour_min_idx = which.min(jours_min_diff)
      jour_max_idx = which.min(jours_max_diff)
      
      nbr = dep_data[[var_itt]][jour_max_idx] - dep_data[[var_itt]][jour_min_idx]
      
      
      territoires_france_infos_loc[territoires_france_infos_loc$CODDEP==dep_itt,var_itt] = nbr
    }
    
    if ("pop" %in% computations){
      var_name = paste0(var_itt,"_pop")
      interest_pca_variables = c(interest_pca_variables,var_name)
      territoires_france_infos_loc[[var_name]] = territoires_france_infos_loc[[var_itt]] / territoires_france_infos_loc$PMUN
    }
    if ("sup" %in% computations){
      var_name = paste0(var_itt,"_sup")
      interest_pca_variables = c(interest_pca_variables,var_name)
      territoires_france_infos_loc[[var_name]] = territoires_france_infos_loc[[var_itt]] / territoires_france_infos_loc$SUPERFICIE
    }
    
    if ('density' %in% computations){
      var_name = paste0(var_itt,"_density")
      interest_pca_variables = c(interest_pca_variables,var_name)
      territoires_france_infos_loc[[var_name]] = territoires_france_infos_loc[[var_itt]] * territoires_france_infos_loc$SUPERFICIE/ territoires_france_infos_loc$PMUN**2
    }
    
  }
  
  X = as.data.frame(territoires_france_infos_loc%>%select(interest_pca_variables))
  rownames(X) = territoires_france_infos_loc$CODDEP
  res_pca = PCA(X,graph = FALSE, ncp = 2)
  return(res_pca)
}



get_dep_pca_date_picker = function(){
  date_picker = dccDatePickerRange(
    id='dep_pca_date_picker',
    display_format = "DD/MM/YYYY",
    #min_date_allowed=as.Date('1995-8-5'),
    #max_date_allowed=as.Date('2017-9-19'),
    #initial_visible_month=as.Date('2017-8-5'),
    start_date = as.Date('2020-01-01'),
    end_date = Sys.Date()
  )
  return(date_picker)
  
}


get_dep_pca_figure = function(res_pca){
  print("debut_get_pca_fig")
  cat("dimX = ",dim(res_pca$call$X),'\n')
  cat("dimind = ",dim(res_pca$ind$coord),'\n')
  cat("dimvar = ",dim(res_pca$var$coord),'\n')
  var_names = colnames(res_pca$call$X)
  ind_name = rownames(res_pca$call$X)
  ind_x = res_pca$ind$coord[,1]

  
  
  

  if(dim(res_pca$call$X)[2] == 1){
    print("Une seule variable")
    var_y = list(0)
    var_x = list(1)
    ind_y = rep(0,length(ind_x))
    
  } else {
    ind_y = res_pca$ind$coord[,2]
    var_y = res_pca$var$coord[,2]
    var_x = res_pca$var$coord[,1]
  }
    
  ind_fig = list(data = list(list(x = as.list(as.vector(ind_x)),
                                  y = as.list(as.vector(ind_y)),
                                  text = as.list(ind_name),
                                  mode = "markers"
                                  )
                            ),
                layout = list(#title='Dep PCA individuals',
                              annotations = list(
                                list(x = 0.2 , y = 1.05, text = "Dep PCA ind", showarrow = F, xref='paper', yref='paper')
                              )
                              #height = "300px",
                              #width = "300px"
                              )
                )
  var_fig = list(data = list(list(x = as.list(as.vector(var_x)),
                                  y = as.list(as.vector(var_y)),
                                  text = as.list(var_names),
                                  type = 'scatter',
                                  mode = "markers+text",
                                  textposition = 'top center'
                                  #xaxis="x2"
                                  #yaxis="y1"
                                  )
                             ),
                 layout = list(#title='Dep PCA variables',
                               annotations = list(
                                 list(x = 0.2 , y = 1.05, text = "Dep PCA vars", showarrow = F, xref='paper', yref='paper')
                               ),
                               shapes = list(list(type = 'circle', xref='x', x0=-1, x1=1,yref='y', y0=-1,y1=1))
                               #xaxis = list(scaleanchor="x", scaleratio=1),
                               #yaxis = list(scaleanchor="x1", scaleratio=1)
                               #height = "300px",
                               #width = "300px"
                               )
                )
  fig = subplot(ind_fig,var_fig)
  
  fig = fig %>% layout(yaxis2 = list(scaleanchor="x2", scaleratio=1))
  # fig %>% layout(annotations = list(
  #   list(x = 0.2 , y = 1.05, text = "ind figure", showarrow = F, xref='paper', yref='paper'),
  #   list(x = 0.8 , y = 1.05, text = "var figure", showarrow = F, xref='paper', yref='paper'))
  # )
  return(fig)
         
  
  
}



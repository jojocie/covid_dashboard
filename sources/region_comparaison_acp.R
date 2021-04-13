library(FactoMineR)
print(getwd())

load("intermediate_data/territoires_france_infos.Rdata")





compute_acp_data = function(territoires_france_infos ,donnees_hosp_mixed,min_date = as.Date("2021/02/01"), max_date = as.Date("2022/11/30"),interest_variables = c("dc","hosp","rea")){
  interest_pca_variables = c()
  territoires_france_infos_loc = territoires_france_infos %>% filter(territoires_france_infos$CODDEP %in% donnees_hosp_mixed$dep)
  
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
    
    
    
    var_name = paste0(var_itt,"_pop")
    interest_pca_variables = c(interest_pca_variables,var_name)
    territoires_france_infos_loc[[var_name]] = territoires_france_infos_loc[[var_itt]] / territoires_france_infos_loc$PMUN
    var_name = paste0(var_itt,"_sup")
    interest_pca_variables = c(interest_pca_variables,var_name)
    territoires_france_infos_loc[[var_name]] = territoires_france_infos_loc[[var_itt]] / territoires_france_infos_loc$SUPERFICIE
    var_name = paste0(var_itt,"_density")
    interest_pca_variables = c(interest_pca_variables,var_name)
    territoires_france_infos_loc[[var_name]] = territoires_france_infos_loc[[var_itt]] * territoires_france_infos_loc$SUPERFICIE/ territoires_france_infos_loc$PMUN**2
    
    res_pca = PCA(territoires_france_infos_loc%>%select(interest_pca_variables),graph = FALSE, ncp = 2)
    return(res_pca)
    
  }
}


get_dep_pca_figure = function(res_pca){
  
  var_names = colnames(res_pca$call$X)
  ind_name = rownames(res_pca$call$X)
  ind_fig = list(data = list(list(x = as.list(as.vector(res_pca$ind$coord[,1])),
                                  y = as.list(as.vector(res_pca$ind$coord[,2])),
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
  var_fig = list(data = list(list(x = as.list(as.vector(res_pca$var$coord[,1])),
                                  y = as.list(as.vector(res_pca$var$coord[,2])),
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



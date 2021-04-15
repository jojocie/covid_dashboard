#install.packages('httpuv', dependencies = TRUE)
#install.packages('dash')

rm(list = ls())



thisFile <- function() {
  cmdArgs <- commandArgs(trailingOnly = FALSE)
  needle <- "--file="
  match <- grep(needle, cmdArgs)
  if (length(match) > 0) {
    # Rscript
    return(normalizePath(sub(needle, "", cmdArgs[match])))
  } else {
    # 'source'd via R console
    return(normalizePath(sys.frames()[[1]]$ofile))
  }
}
this_file_path = thisFile()
this_file_path = dirname(this_file_path)
setwd(this_file_path)
#setwd("~/Documents/covid_projects/covid_dashboard/") # backup setwd


source("sources/data_proc.R")
source("sources/region_comparaison_acp.R")

dep_list = load("sources/dep_list")
library(dash)
library(plotly)
library(dashCoreComponents)
library(dashHtmlComponents)




location_data = get_fr_data()
donnees_hosp_mixed = get_hosp_data()

res_pca1 = compute_acp_data(territoires_france_infos,donnees_hosp_mixed,max_date = Sys.Date())
now = timestamp()

app <- Dash$new()




get_positive_figure = function(location_data, title_suffix = ""){
  return(
    list(
      data=list(
        list(
          x=as.list(location_data$days),
          y=as.list(location_data$rolling_positives)
        )
        
      ),
      layout = list(title=paste('Nombre de testés positifs', title_suffix),
                    yaxis = list(hoverformat = ".0f"))
      
      # layout = list(title='Nombre de testés positifs',
      #               yaxis = list(hoverformat = ".0f"))
    )
  )
  
}



get_tested_figure = function(location_data, title_suffix = ""){
  return(
    list(
      data=list(
        list(
          x=as.list(location_data$days),
          y=as.list(location_data$rolling_tests)
        )
        
      ),
      layout = list(title=paste('Nombre de test effectué', title_suffix),
                    yaxis = list(hoverformat = ".0f"))
      # 
      # layout = list(title='Nombre de test effectué',
      #               yaxis = list(hoverformat = ".0f"))
    )
  )
  
}


get_ratio_figure = function(location_data){
  return(
    list(
      data=list(
        list(
          x=as.list(location_data$days),
          y=as.list(location_data$rolling_positives/location_data$rolling_tests)
        )
        
      ),
      layout = list(title='Représentation de la proportion de testés positif',
                    yaxis = list(hoverformat = ".2%"))
    )
  )
  
}


app$layout(
  
  htmlDiv(
    list(
      
      htmlH1('Evolution COVID'),
      htmlP(children = list("Ce dashboard présente l'évolution de plusieurs indicateurs de l'état de l'épidémie en France.", htmlBr(),
                            'Les données sont mise à jour au moins une fois tous les 15 jours et des nouvelles fonctionnalités seront ajoutées.',htmlBr(),
                            'contact : ', htmlA('jlalo@hotmail.fr', href = "mailto:jlalo@hotmail.fr")),
            style = list("font-size" = "130%")),
      

      
      
      htmlH2("Comparaison de l'évolution épidémique département par département"),
      htmlP(children = list("Construction de l'analyse en composantes principales selon plusieurs indicateurs épidémique 
                            qui peuvent être choisi à l'aide des boites de sélection. Les indicateurs se distinguent par 
                            les grandeurs qu'ils réprésente ainsi que par les calculs effectués pour normaliser la grandeur 
                            d'un département à un autre. Les grandeurs proposées sont : ", 
                            htmlUl(list(htmlLi("Différence du nombre de personne hospitalisé entre les deux dates séléctionnées"),
                                        htmlLi("Différence du nombre de personne en réanimation entre les deux dates séléctionnées"),
                                        htmlLi("Nombre de morts entre les deux dates sélectionnées")
                            )
                            ),
                            "Les normalisations proposées sont",
                            htmlUl(list(htmlLi("Par population"),
                                        htmlLi("Par superficie"),
                                        htmlLi("Par densité")))
      )
      ),
      
      htmlBr(),
      htmlDiv(
        #children = list(get_dep_pca_date_picker(),get_dep_acp_var_selection_dropdown(),get_dep_acp_computation_selection_dropdown()),
        children = htmlTable(
          children = list(htmlTr(list(htmlTd("Sélection de la date", style = list("text-align" = 'center')),
                                      htmlTd("Sélection des grandeurs", style = list("text-align" = 'center')),
                                      htmlTd("Sélection des calculs", style = list("text-align" = 'center'))
          )
          ),
          htmlTr(list(htmlTd(get_dep_pca_date_picker(), style = list('padding' = '0px,20px,0px,20px','width' = "20%")),
                      htmlTd(get_dep_acp_var_selection_dropdown(), style = list('padding' = '0px,20px,0px,20px','width' = "20%")),
                      htmlTd(get_dep_acp_computation_selection_dropdown(), style = list('padding' = '0px,20px,0px,20px','width' = "20%"))
          )
          )
          )
        ),
        style = list("text-align" = 'center', 'width' = '100%')
      ),
      htmlDiv(
        children = dccGraph(
          id = 'dep_pca',
          figure = get_dep_pca_figure(res_pca1)
        ),
        style = list("margin" = "50px")
      ),
      htmlH2("Evolution des indicateurs au cours du temps"),
      htmlTable(htmlTr(list(
        
        htmlTd(
          list(
            htmlDiv("Selectionner un lieu géographique (France ou n°de département) :", style = list("margin-bottom" = "15px")),
            dccDropdown(
              options=dropdown_list,
              value="France",
              id="location_selector"
            )
          ) , style = list("width" = "35%")), 
        htmlTd(list(
          htmlDiv("Selectionnner le nombre de jour utilisé pour la moyenne mobile :", style = list("margin-bottom" = "15px")),
          dccSlider(
            id = 'day_lag',
            min = 1,
            max = 21,
            step = 1,
            marks=0:21,
            value = 7
          ))
        ))) , style = list("width" = "70%")),
      htmlH2("Résultats des tests PCR"),
      htmlP("Ces graphiques présentent l'évolution de la situation concernant les tests PCR effectués"),
      htmlP(children = "Source des donnees :"), 
      htmlA(children =   "https://www.data.gouv.fr/en/datasets/donnees-relatives-aux-resultats-des-tests-virologiques-covid-19/" ,href = "https://www.data.gouv.fr/en/datasets/donnees-relatives-aux-resultats-des-tests-virologiques-covid-19/"),
      
      htmlDiv(
        dccGraph(
          id = 'graph_pos'
          #figure=get_positive_figure(location_data_data)
        ),
        # style = list("width" = "80%", "text-align" = 'center')
        style = list("margin" = "50px")
        
      ),
      htmlDiv(
        dccGraph(
          id = 'graph_tested'
        ),
        # style = list("width" = "80%", "text-align" = 'center')
        style = list("margin" = "50px")
        
      ),
      htmlDiv(
        dccGraph(
          id = 'graph_ratio'
        ),
       # style = list("width" = "80%", "text-align" = 'center')
       style = list("margin" = "50px")
        
      ),
      htmlH2(children = "Données Hospitalières"),
      htmlP(children = list("Ce graphique montre l'évolution de la situation hospitalières au cours du temps")),
      
      htmlP("Source des données hospitalières : "),
      htmlA('https://www.data.gouv.fr/en/datasets/donnees-hospitalieres-relatives-a-lepidemie-de-covid-19/', href = 'https://www.data.gouv.fr/en/datasets/donnees-hospitalieres-relatives-a-lepidemie-de-covid-19/'),
      htmlP(list("Les graphiques sont supperposés pour mieux visualiser les interactions entre les indicateurs hospitaliers : ",
                 htmlUl(list(htmlLi("Nombre de personnes hospitalisées"), htmlLi("Nombre de personnes en réanimations") , htmlLi("Nombre de décès quotidien à l'hopital (hors EPHAD et EMS)"))))),
    
      
      htmlDiv(
        dccGraph(
          id = 'graph_hosp'
        ),
        # style = list("width" = "80%", "text-align" = 'center')
        style = list("margin" = "50px")
        
      ),
      
      
      htmlDiv(
        dccGraph(
          id = 'graph_severity'
        ),
        # style = list("width" = "80%", "text-align" = 'center')
        style = list("margin" = "50px")
        
      )

      

    )
  )
)

app$callback(
  
  output = output(id='dep_pca', property = 'figure'),
  params = list(input(id = 'dep_pca_date_picker', property = 'start_date'),
                input(id = 'dep_pca_date_picker', property = 'end_date'),
                input(id = 'dep_acp_var_selection_dropdown', property = 'value'),
                input(id = "dep_acp_computation_selection_dropdown", property = 'value')),
  function(start_date, end_date,var_selection, computation_selection){
    
    #print(var_selection)
    #print(computation_selection)
    #print(class(computation_selection))
    res_pca = compute_acp_data(territoires_france_infos,donnees_hosp_mixed,min_date = as.Date(start_date),max_date = as.Date(end_date),interest_variables = var_selection, computations=computation_selection)
    return(get_dep_pca_figure(res_pca))
  }
)
app$callback(
  output=list(output(id='graph_pos', property='figure'),
              output(id='graph_tested', property='figure'),
              output(id='graph_ratio', property='figure'),
              output(id='graph_hosp', property='figure'),
              output(id='graph_severity', property = 'figure')
              ),
  params=list(input(id='day_lag', property='value'),
              input(id='location_selector', property = 'value')),
  function(input_value,selected_location) {
    
    suffix_title = ""
    sprintf("You've entered \"%s\"", input_value)
    if (class(input_value) == "list"){
      #print("list")
      input_value = 7
    }
    
    if (input_value != 1){
      suffix_title = "(Moyenne cumulée)"
    }
    
    if (class(selected_location) == 'list'){
      #print('location_list')
      selected_location == 'France'
    }
    
    print(selected_location)
    if (selected_location == 'France'){
      location_data = get_fr_data(day_lag = input_value)
    } else {
      location_data = get_dep_data(dep_val = selected_location, day_lag = input_value)
    }
    
    hosp_fig = prossess_hosp_graph(donnees_hosp_mixed = donnees_hosp_mixed, dep_itt = selected_location,k = input_value,alignement = 'center')

    assign("now", timestamp(), envir = .GlobalEnv)
    return(append(list(get_positive_figure(location_data,suffix_title) , get_tested_figure(location_data,suffix_title), get_ratio_figure(location_data)), hosp_fig))
  })
app$run_server(host = '0.0.0.0')
#app$run_server()
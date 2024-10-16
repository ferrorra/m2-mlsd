library(shinythemes)
library(shiny)
library(Hmisc)
library(UsingR)
library(reshape2)
library(ggpubr)
library(shinydashboard)
library(fastDummies)
library(tidyverse)
library(RColorBrewer)
library(DT)
library(plyr)
library(plotly)
library(reader)
library("plotrix")
library(CatEncoders)
library(MLmetrics)
library(ROSE)
library(DMwR)
library(ggplot2)
library(GGally)
library(readxl)
library(tidyr)
library(dplyr)
library("partykit")
library(nortest)
library(mvnormtest)
library(MASS)
library(shinyLP)
library(class)
library(gmodels)
library(rattle)
library(ranger)
library(klaR)
library(kernlab)
#library(micad)                       #package for anomaly detection
library(e1071)
library(anomaly)
library(NeuralNetTools)
library(neuralnet)
library(nnet)
library(mclust)
library(rsconnect)
library(packrat)

source('./Server moduls/Univariee.R')
source('./Server moduls/Bivaree.R')
source('./Server moduls/Qnt_Qlt.R')
source('./Server moduls/Qlt_Qlt.R')
source('./Server moduls/exploration.R')
source('./Server moduls/dataset.R')
source('./Server moduls/Missing.R')
source('./Server moduls/Modele.R')
source('./Server moduls/RUS.R')

library(ROCR)
library(CatEncoders)
library(boot)
library(DAAG)
library(class)
library(plotrix)
library(KNNShiny)
library(tidyverse)
library(pROC)
library(caTools)
library(randomForest)
library(PRROC)
library(rpart)
library(shinycssloaders)
library(NbClust) 
library(shinydashboardPlus)
library(dashboardthemes)
library(VIM)
library(epitools)
library(survival)
library(naniar)
library(data.table)
library(markdown)
library(ggfortify)
library(survminer)
library(psych)
library(factoextra)
library(FactoMineR)
library(dlookr)
library(summarytools)
library(purrr)
library(openxlsx)
library(BSDA)
library(hrbrthemes)
library(performance)
library(gt)
library(broom)
library(gtsummary)
library(fontawesome)
library(aod)
library(DescTools)
library(tidyverse)
library(ggeffects)
library(effects)
library(pscl)
library(shinyalert)
library(shinycustomloader)
library(periscope)
library(viridis)
library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(knitr)
library(fresh)
library(DT)
library(plyr)
library(plotly)
#library("ElemStatLearn")
library(readxl)
library(zoo)
library(tidyr)
library(dplyr)
library(mice)
library(caret)
library(datasets)
library(plotrix)
library(KNNShiny)
library(tidyverse)
library(ROCR)
library(pROC)
library(caTools)
library(randomForest)
library(plyr)
library(corrplot)
library(rpart.plot)
#library("ElemStatLearn")
library(dplyr)
library(ggfortify)
library(survminer)
library(psych)
library(factoextra)
library(FactoMineR)
library(summarytools)
library(glmnet)
library(BSDA)
library(hrbrthemes)
library(performance)
library(broom)
library(pscl)
library(shinyalert)
library(see)
library(shinycustomloader)
library(periscope)
library(heatmaply)
library(corrr)
library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(parameters)
library(ggforce)
library(DataExplorer)
library(vov)
library(Metrics)
options(warn=-1)
library(shiny)
library(shinydashboard)
library(shinythemes)
library(fontawesome)
library(shinycssloaders)

shinyApp(
  ui = dashboardPage(
    dashboardHeader(title = "Projet Dashboard M2- Amelioration M1"),
    
    dashboardSidebar(
      selectInput("sep", label = "Séparateur", 
                  choices = c("point-virgule" = ";", 
                              "virgule" = ",", 
                              "espace" = " ", 
                              "tabulation" = "\t", 
                              "point" = ".")),
      checkboxInput("Header", label = "Noms de variables présents", value = FALSE),
      
      fileInput(inputId = "file", label = "Veuillez choisir votre fichier",
                accept = c("text/plain", ".csv", ".data", ".xls", ".tsv")),
      sidebarMenu(
        menuItem("Accueil", tabName = "accueill", icon = icon("home")),
        menuItem("Dataset", tabName = "overview", icon = icon("table"),
                 menuSubItem("Overview", tabName = "Dataset", icon = icon("list-alt")),
                 menuSubItem("Informations données", tabName = "f", icon = icon("list-alt")),
                 menuSubItem("Summary détaillé", tabName = "Sm", icon = icon("chart-bar"))
        ),
        menuItem("Prétraitement des données", tabName = "Pd", icon = icon("table"),
                 menuSubItem("Valeurs manquantes", tabName = "imp", icon=icon("exclamation-triangle")),
                 menuSubItem("Valeurs abérrantes", tabName = "out", icon=icon("exclamation-triangle")),
                 menuSubItem("Normalisation", tabName = "nm", icon=icon("balance-scale")),
                 menuSubItem("Dummification", tabName = "dm", icon=icon("dice")),
                 menuSubItem("Déséquilibre des classes", tabName = "rb", icon=icon("balance-scale-left"))
        ),
        menuItem("Analyse Univariée", tabName="datanalys", icon=icon("chart-line"),
                 menuSubItem("Quantitative et Qualitative", tabName = "Unv", icon=icon("list"))
        ),
        menuItem("Analyse Bivariée", tabName="b", icon=icon("chart-pie"),
                 menuSubItem("Quantitative VS Quantitative", tabName = "Bv", icon=icon("dice-two")),
                 menuSubItem("Quantitative VS Qualitative", tabName = "vs", icon=icon("chart-pie")),
                 menuSubItem("Qualitative VS Qualitative", tabName = "vs2", icon=icon("chart-pie"))
        ),
        menuItem("Analyse Multivariée", tabName = "exp", icon = icon("search"),
                 menuSubItem("Matrice de Corrélation", tabName = "cr", icon=icon("project-diagram")),
                 menuSubItem("Cercle de Corrélation", tabName = "crce", icon=icon("circle"))
        ),
        menuItem("Modeles", tabName="ml", icon=icon("tasks")),
        # New menu item for the report
        menuItem("Rapport", tabName = "report", icon = icon("file-alt"),
                 menuSubItem("Dataset 1", tabName = "report1", icon = icon("file")),
                 menuSubItem("Dataset 2", tabName = "report2", icon = icon("file")),
                 menuSubItem("Dataset 3", tabName = "report3", icon = icon("file"))
        )
      ),
      downloadButton("Download", "Télécharger Dataset"),
      actionButton("button", "Rénitialisation Dataset")
    ),
    
    
    dashboardBody(
      
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
        tags$script(src = "https://ajax.googleapis.com/ajax/libs/webfont/1.6.26/webfont.js"),
        tags$script(HTML("
          WebFont.load({
            google: {
              families: ['Montserrat']
            }
          });
        "))
      ),
      
      titlePanel(div(class = "typing-animation", "Advanced Data Analysis Dashboard")),
      

      tabItems(
        tabItem(tabName = "accueill",
                fluidPage(
                  tabPanel("Description", 
                           box(width = 12, title="Rapport",status = "success",solidHeader = TRUE,
                           htmlOutput("rapport")
                           )
                           
                  )
                )
        ),

        tabItem(tabName = "imp",
                fluidPage(
                  column(12,
                         
                         
                         tabBox(
                           width = 12,
                           tabPanel("Variable Quantitative",
                                    uiOutput('quantlistM'),
                                    radioButtons("imputation", "Méthode d'imputation",
                                                 c(
                                                   "Imputation par la moyenne" = "Im",
                                                   "Imputation par la mediane" = "Id",
                                                   "Imputation par interpolation" = "Ip",
                                                   "Supprimer la colonne (si le pourcentage est > 30 %"="Sc",
                                                   "Visulalisation des valeurs manquantes"="vsl"
                                                   
                                                 ),selected = FALSE),
                                    conditionalPanel(
                                      condition = "input.imputation == 'Im'",
                                      plotOutput("resultat_Im")
                                    ),
                                    
                                    conditionalPanel(
                                      condition = "input.imputation == 'vsl'",
                                      plotOutput("visualisation_manquantes")
                                    ),
                                    conditionalPanel(
                                      condition = "input.imputation == 'Sc'",
                                      textOutput("resultat_Sc"),
                                      tags$head(tags$style("#resultat_Sc{color:  green;
                                                           font-size: 20px;
                                                           font-style: italic;
                                                           }"
                                      )
                                      )
                                      ),
                                    conditionalPanel(
                                      condition = "input.imputation == 'Id'",
                                      plotOutput("resultat_Id")
                                    ),
                                    conditionalPanel(
                                      condition = "input.imputation == 'Ip'",
                                      plotOutput("resultat_Ip")
                                    )
                                    
                                    
                                    
                                      ),
                           tabPanel("Variable Qualitative",
                                    uiOutput('qualistM'),
                                    radioButtons("imputationQ", "Méthode d'imputation",
                                                 c("Imputation par le mode" = "mode",
                                                   "Suppression de la colonne"="sup",
                                                   "Visulalisation des valeurs manquantes"="vslQ"
                                                   
                                                   
                                                 ),selected = FALSE),
                                    conditionalPanel(
                                      condition = "input.imputationQ == 'mode'",
                                      plotOutput("resultat_mode")
                                    ),
                                    conditionalPanel(
                                      condition = "input.imputationQ == 'vslQ'",
                                      plotOutput("visualisation_manquantesQ")
                                    ),
                                    conditionalPanel(
                                      condition = "input.imputationQ == 'sup'",
                                      textOutput("resultat_sup"),
                                      tags$head(tags$style("#resultat_sup{color:  green;
                                                           font-size: 20px;
                                                           font-style: italic;
                                                           }"
                                      )
                                      )
                                      ),
                                    conditionalPanel(
                                      condition = "input.imputationQ == 'r'"
                                      
                                    )
                                    
                                      )
                           )
                         
                         
                         
                         
                         
                  )
                  
                  
      )
                  ),
      
      tabItem(tabName = "out",
              fluidPage(
                column(12,
                       tabBox(
                         width = 12,
                         tabPanel("Traitement des outliers",
                                  uiOutput('OutList'),
                                  textOutput("outlier"),
                                  radioButtons("Out", "Vous voulez supprimer les outliers ?",
                                               c("Non" = "Non",
                                                 "Oui" = "Oui",
                                                 "Affichage des outliers"="Af"
                                                 
                                               )),
                                  #actionButton("deleteRows", "spl"),
                                  DT::dataTableOutput("tab_outliers"),
                                  conditionalPanel(
                                    condition = "input.Out == 'Oui'",
                                    textOutput("resultat_Oui")
                                  )
                         )
                       )
                )
              )
      ),
      tabItem(tabName = "nm",
              fluidPage(
                column(12,
                       tabBox(width = 12,
                              tabPanel("Normalisation",
                                       uiOutput('NList'),
                                       radioButtons("normalisation", "Méthodes de normalisation",
                                                    c("Normalisation Min-Max" = "Nm",
                                                      "Normalisation du score z"="Nz",
                                                      "Visualisation des données"="Vsd"
                                                      
                                                      
                                                      
                                                    ),selected = FALSE)),
                              conditionalPanel(
                                condition = "input.normalisation == 'Nm'",
                                textOutput("resultat_Nm"),
                                tags$head(tags$style("#resultat_Nm{color:  green;
                                                     font-size: 20px;
                                                     font-style: italic;
                                                     }"
                                  )
                                ),
                                plotOutput("visualisation_Nm")
                                ),
                              conditionalPanel(
                                condition = "input.normalisation == 'Nz'",
                                textOutput("resultat_Nz"),
                                tags$head(tags$style("#resultat_Nz{color:  green;
                                                     font-size: 20px;
                                                     font-style: italic;
                                                     }"
                                  )
                                ),
                                plotOutput("visualisation_Nz")
                                ),
                              conditionalPanel(
                                condition = "input.normalisation == 'Vsd'",
                                plotOutput("visualisation_N")
                                
                              )
                              
                                )
                              )
                       )
      ),
      tabItem(tabName = "dm",
              fluidPage(
                column(12,
                       tabBox(width = 12,
                              tabPanel("Dummification",
                                       uiOutput('dmList'),
                                       
                                       radioButtons("dm", "Voulez vous appliquer la dummification ?",
                                                    c("Non" = "Non",
                                                      "Oui" = "Oui"
                                                      
                                                      
                                                    )),
                                       
                                       conditionalPanel(
                                         condition = "input.dm == 'Oui'",
                                         textOutput("resultat_dm"),
                                         tags$head(tags$style("#resultat_dm{color:  green;
                                                              font-size: 20px;
                                                              font-style: italic;
                                                              }"
                                           ) ),
                                         textOutput("resultat_dm2"),
                                         tags$head(tags$style("#resultat_dm2{color: red;
                                                              font-size: 20px;
                                                              font-style: italic;
                                                              }"
                                           ) )
                                         
                                         
                                         
                                         
                                         
                                         )
                                         )
                              
                                         ))
              )
      ),
      tabItem(tabName = "rb",
              fluidPage(
                column(
                  12,
                  tabBox(width = 12,
                         tabPanel("Rééquilibrage de classes",
                                  uiOutput('reList'),
                                  
                                  radioButtons("re", "la méthode de réechantillonnage",
                                               c("visualisez la distribition de la classe"="Vc",
                                                 "over-sampling" = "Over",
                                                 "under-sampling" = "Under"
                                                 
                                                 
                                                 
                                               ),selected = FALSE),
                                  
                                  conditionalPanel(
                                    condition = "input.re == 'Vc'",
                                    plotOutput("Vc")
                                    
                                  ),
                                  conditionalPanel(
                                    condition = "input.re == 'Over'",
                                    textOutput("resultat_Over"),
                                    textOutput("resultat_Over2"),
                                    plotOutput("Vc_Over"),
                                    tags$head(tags$style("#resultat_Over{color:  green;
                                                         font-size: 20px;
                                                         font-style: italic;
                                                         }"
                                      ) ),
                                    tags$head(tags$style("#resultat_Over2{color:  red;
                                                         font-size: 20px;
                                                         font-style: italic;
                                                         }"
                                      ) )
                                    
                                    ),
                                  conditionalPanel(
                                    condition = "input.re == 'Under'",
                                    textOutput("resultat_Under"),
                                    textOutput("resultat_Under2"),
                                    plotOutput("Vc_Under"),
                                    tags$head(tags$style("#resultat_Under{color:  green;
                                                         font-size: 20px;
                                                         font-style: italic;
                                                         }"
                                      ) ),
                                    tags$head(tags$style("#resultat_Under2{color:  red;
                                                         font-size: 20px;
                                                         font-style: italic;
                                                         }"
                                      ) )
                                    
                                    ),
                                  conditionalPanel(
                                    condition = "input.re == 'Both'",
                                    textOutput("resultat_Both"),
                                    textOutput("resultat_Both2"),
                                    tags$head(tags$style("#resultat_Both{color:  green;
                                                         font-size: 20px;
                                                         font-style: italic;
                                                         }"
                                      ) ),
                                    tags$head(tags$style("#resultat_Both2{color:  red;
                                                         font-size: 20px;
                                                         font-style: italic;
                                                         }"
                                      ) )
                                    
                                    )
                                  
                                  
                                    )
                                  )
                                  )
                                    )
                                    ),
      tabItem(tabName = "f",
              fluidPage(
                column( 
                  width = 12,
                  tabBox(
                    width = 12,
                    tabPanel("Variables",
                             tableOutput("variable_characteristics"),
                             textOutput("rania")
   
                    )
                             )
                             )
                    )
      ),
      tabItem(tabName = "Sm",
              fluidPage(
                fluidRow(
                  column(12,
                         htmlOutput("sumariz1"))
                )
              )
      ),
      tabItem(tabName = "cr",
              fluidPage(
                column(12,
                      tabBox(width = 12,
                              tabPanel("Heatmap correlation",
                                      textOutput("soustxt"),
                                      textOutput("Theatmap"),
                                      tags$head(tags$style("#Theatmap{color: red;
                                                            font-size: 20px;
                                                            font-style: italic;
                                                            }"
                                        )
                                      ),
                                      plotlyOutput("heatmap")
                                      )
                              
                                      )
                              )
                
                )
      ),
      tabItem(tabName = "crce",
              fluidPage(
                column(12,
                      tabBox(width = 12,
                              tabPanel("Cercle de Corrélation",
                                      fluidRow(
                                        column(12, align = "center",
                                                h3("Cercle de Corrélation", style = "color: #0073B7;"),
                                                br(),
                                                textOutput("circleText"),
                                                br(),
                                                tags$head(tags$style("#Tcercle{
                                                                      color: #FF6347;
                                                                      font-size: 18px;
                                                                      font-style: italic;
                                                                      text-align: center;
                                                                      }"
                                                ))
                                        )
                                      ),
                                      fluidRow(
                                        column(12, align = "center",
                                                plotlyOutput("correlationCirclePlot", height = "600px")
                                        )
                                      )
                              )
                      )
                )
              )
      ),
      tabItem(tabName = "Dataset",
              fluidPage(
                
                fluidRow(column(width = 10, infoBoxOutput("nbrlignes", width = 12)),
                         column(width = 10, infoBoxOutput("nbrcolonnes", width = 12))),
                
                # Row for Data Table and Data Info
                fluidRow(
                  column(
                    width = 12,
                    box(title = "Table", solidHeader = TRUE, width = 12,
                        collapsible = TRUE, div(DT::dataTableOutput("table1"))
                    )
                  )
                ),
                # Row for Info Boxes and Data Description
                fluidRow(
                  column(
                    width = 12,
                    box(
                      title = "Résumé Statistique", solidHeader = TRUE, width = 12, status = "primary",
                      collapsible = TRUE,
                      div(
                        style = "overflow-x: scroll;",  # This adds a horizontal scrollbar if content overflows
                        DT::dataTableOutput("data_desc")
                      )
                    )
                  )
                )
                
                
              )
      ),      
      tabItem(tabName = "Unv",
        fluidPage(
          tags$head(
            tags$style(HTML("
              .box {border-radius: 5px; box-shadow: 0 2px 5px rgba(0,0,0,0.1);}
              .box-header {border-radius: 5px 5px 0 0;}
              .plot-container {height: 400px;}
              .select-container {padding: 15px; background-color: #f8f9fa; border-radius: 5px; margin-bottom: 15px;}
            "))
          ),
          fluidRow(  
            column( 
              width = 12,
              tabBox(
                width = 12,
                title = "Variables Analysis",
                id = "tabset1", 
                tabPanel("Quantitative", value = "tab_quantitative",
                  div(class = "select-container",
                    uiOutput('quantlist')
                  ),
                  verbatimTextOutput(outputId = "summary")
                ),
                tabPanel("Qualitative", value = "tab_qualitative",
                  div(class = "select-container",
                    uiOutput('qualist')
                  )
                ),
                tabPanel("Churn Analysis", value = "tab_churn",
                  div(class = "select-container",
                    fluidRow(
                      column(width = 6,
                        selectInput("churn_var", "Select Churn Variable", choices = NULL)
                      ),
                      column(width = 6,
                        selectInput("churn_compare_var", "Select Comparison Variable", choices = NULL)
                      )
                    )
                  )
                )
              )
            )
          ),
          conditionalPanel(
            condition = "input.tabset1 == 'tab_quantitative'",
            fluidRow(
              column(
                width = 6,
                box(
                  width = NULL,
                  title = "Diagramme en bâtons des effectifs", status = "primary", solidHeader = TRUE,
                  collapsible = TRUE,
                  div(class = "plot-container", plotOutput(outputId = "effectifsDiag"))
                )
              ),
              column(
                width = 6,
                box(
                  width = NULL,
                  title = "Boîte à moustaches", status = "info", solidHeader = TRUE,
                  collapsible = TRUE,
                  div(class = "plot-container", plotOutput(outputId = "boiteMoustaches"))
                )
              )
            ),
            fluidRow(
              column(
                width = 6,
                box(
                  width = NULL,
                  title = "Histogramme d'effectifs", status = "success", solidHeader = TRUE,
                  collapsible = TRUE,
                  div(class = "plot-container", plotOutput(outputId = "effectifsHist"))
                )
              ),
              column(
                width = 6,
                box(
                  width = NULL,
                  title = "Caractéristiques centrales et de dispersion", status = "warning", solidHeader = TRUE,
                  collapsible = TRUE,
                  tableOutput(outputId = "centreDisp")
                )
              )
            ),
            fluidRow(
              column(
                width = 12,
                box(
                  width = NULL,
                  title = "Courbe cumulative", status = "danger", solidHeader = TRUE,
                  collapsible = TRUE,
                  div(class = "plot-container", plotOutput(outputId = "effectifsCumCurve"))
                )
              )
            )
          ),
          conditionalPanel(
            condition = "input.tabset1 == 'tab_qualitative'",
            fluidRow(
              column(
                width = 6,
                box(
                  width = NULL,
                  title = "Diagramme en barres", status = "primary", solidHeader = TRUE,
                  collapsible = TRUE,
                  div(class = "plot-container", plotOutput(outputId = "barPlot"))
                )
              ),
              column(
                width = 6,
                box(
                  width = NULL,
                  title = "Diagramme circulaire", status = "info", solidHeader = TRUE,
                  collapsible = TRUE,
                  div(class = "plot-container", plotOutput(outputId = "pieChart"))
                )
              )
            ),
            fluidRow(
              column(
                width = 6,
                box(
                  width = NULL,
                  title = "Graphique en mosaïque", status = "success", solidHeader = TRUE,
                  collapsible = TRUE,
                  div(class = "plot-container", plotOutput(outputId = "mosaicPlot"))
                )
              ),
              column(
                width = 6,
                box(
                  width = NULL,
                  title = "Diagramme en barres empilées", status = "warning", solidHeader = TRUE,
                  collapsible = TRUE,
                  div(class = "plot-container", plotOutput(outputId = "stackedBarPlot"))
                )
              )
            ),
            fluidRow(
              column(
                width = 12,
                box(
                  width = NULL,
                  title = "Diagramme de Pareto", status = "danger", solidHeader = TRUE,
                  collapsible = TRUE,
                  div(class = "plot-container", plotOutput(outputId = "paretoChart"))
                )
              )
            )
          ),
          conditionalPanel(
            condition = "input.tabset1 == 'tab_churn'",
            fluidRow(
              column(
                width = 12,
                box(
                  title = "Proportion des individus qui ont churné", status = "primary", solidHeader = TRUE,
                  collapsible = TRUE, width = NULL,
                  div(class = "plot-container", plotOutput(outputId = "churnProportion"))
                )
              )
            ),
            fluidRow(
              column(
                width = 6,
                box(
                  title = "Histogramme pour Churn vs. Non Churn", status = "info", solidHeader = TRUE,
                  collapsible = TRUE, width = NULL,
                  div(class = "plot-container", plotOutput(outputId = "churnHistogram"))
                )
              ),
              column(
                width = 6,
                box(
                  title = "Proportion de Churn vs. Non Churn (Qualitative)", status = "success", solidHeader = TRUE,
                  collapsible = TRUE, width = NULL,
                  div(class = "plot-container", plotOutput(outputId = "churnProportionQual"))
                )
              )
            )
          ),
          uiOutput("myConditionalPanel")
        )
      ),
      tabItem(tabName = "Bv",
              fluidPage(
                fluidRow(  
                  column(
                    width = 6,
                    uiOutput('quantlistbi1')
                  ),
                  column(
                    width = 6,
                    uiOutput('quantlistbi2')
                  ),
                  
                  column( 
                    width = 12,
                    tabBox(
                      width = 12,
                      
                      title = "",
                      # The id lets us use input$tabset1 on the server to find the current tab
                      id = "tabset1", 
                      tabPanel("Caractéristiques", value = "caractéristiques",
                               tableOutput("caract")),
                      tabPanel("Nuage de point avec la regression linéaire", value = "caractéristiques",
                               plotOutput("nuagePoints"),
                               
                               textOutput("correlation")
                               
                               
                      ),
                      tabPanel("Nuage de points et Histogrammes", value = "caractéristiques",
                               plotOutput("nuagePointshist")
                               
                               
                      ),
                      tabPanel("Histogrammes dos à dos", value = "caractéristiques",
                               plotOutput("histbackback")
                               
                               
                      )

                    )
                  )
                  
                )
              )
      ),
      tabItem(tabName = "vs",
              fluidPage(
                fluidRow( 
                  column(6,uiOutput('quantlistvs')
                         
                         
                  ),
                  column(6,uiOutput('qualistvs')
                         
                  ),
                  uiOutput("myConditionalPanel2")
                  
                  
                )
              )
      ),
      tabItem(tabName = "vs2",
              fluidPage(
                fluidRow( 
                  column(6,uiOutput('qualistvs1')
                         
                         
                  ),
                  column(6,uiOutput('qualistvs2')
                         
                         
                  ),
                  uiOutput("myConditionalPanel3")
                  
                  
                )
              )
      ),
      tabItem(tabName = "Rg",
              fluidPage(
                fluidRow(
                  column(12,
                         tabBox(
                           width = 4,
                           tabPanel("Choisir le type de regression",
                                    radioButtons(
                                      "radio1",
                                      label = h3("Choisir le type de la regression"),
                                      choices = list(
                                        "Linear" = 1,
                                        "Logistic" = 2
                                        
                                      ),
                                      selected = F
                                    ),
                                    uiOutput("select2"),
                                    uiOutput("select3"),
                                    actionButton("analysis", "Analyser!"))
                         ),
                         conditionalPanel(
                           "input.radio1 == 1",
                           tabBox(
                             width=8,
                             tabPanel("Model summary",htmlOutput("testq")),
                             tabPanel("Coefficients plot",plotOutput("lcoef"))
                             
                           ),
                           tabBox(
                             width=12,
                             tabPanel("Diagnostics",plotOutput("diag")
                                      
                             )
                             
                             
                           )
                         ),
                         conditionalPanel(
                           "input.radio1 == 2",
                           tabBox(
                             width=8,
                             tabPanel("Model summary",htmlOutput("testq2")),
                             tabPanel("Coefficients plot",plotOutput("logiOdds"))
                             
                           ),
                           tabBox(
                             width=12,
                             tabPanel("GOF metrics",verbatimTextOutput("logi_gof")
                                      
                             )
                             
                             
                           )
                         )
                  )
                )
              )
      ),
      tabItem(tabName = "Rd",
              fluidPage(
                fluidRow(
                  column(12,
                         tabBox(
                           width = 4,
                           tabPanel("Principal Component Analysis",
                                    uiOutput("pdd"),
                                    
                                    actionButton("analysisPCA", "Analyze!"),
                                    #useShinyalert(),
                                    uiOutput("dimA"),
                                    uiOutput("dimB"),
                                    uiOutput("clumPCA"),
                                    uiOutput("Coll"),
                                    uiOutput("Colls"))
                         ),
                         tabBox(
                           width=8,
                           tabPanel(
                             
                             plotlyOutput("plotlyPCA", width = "65%"),
                             
                             
                             conditionalPanel(
                               condition = "output.plotlyPCA"
                               ,
                               h4("Summary")
                               ,
                               h5("Importance")
                               ,
                               verbatimTextOutput("PC.info")
                               ,
                               h5("Rotations")
                               ,
                               verbatimTextOutput("PC.info2")
                               
                             )
                           )
                           
                           
                         )
                  )
                )
              )
      ),
      tabItem(tabName = "ml",
        fluidPage(
          selectInput("target_column", "Select Target Column:", 
            choices = NULL,  # We'll update this dynamically
            selected = NULL),
          uiOutput("feature_selection_ui"),  # Dynamic UI for feature selection
          actionButton("select_all_features", "Select All Features"),
          actionButton("deselect_all_features", "Deselect All Features"),
          sliderInput("trainsplit", "Training portion:",
                      min = 0.05, max = 0.95,
                      value = 0.7, step = 0.05
          ),
          h3("Choose training parameters"),
          radioButtons("radio",
                      label = "Select",
                      choices = list("Over Sampling" = 1, "Under Sampling" = 2, "No Sampling" = 3),
                      selected = 3,
                      inline = TRUE,
                      width = "100%"),
          
          radioButtons("hyperparameter_tuning",
                      label = "Hyperparameter tuning",
                      choices = list("Default parameters" = "default", "Grid search" = "grid"),
                      selected = "default",
                      inline = TRUE,
                      width = "100%"),
          
          tabPanel("Classification",
                  tabsetPanel(
                    tabPanel("Logistic Regression", 
                      h1("Logistic Regression"),
                      conditionalPanel(
                        condition = "input.hyperparameter_tuning == 'grid'",
                        sliderInput("lr_alpha_range", "Alpha range:", min = 0, max = 1, value = c(0, 1), step = 0.1),
                        sliderInput("lr_lambda_range", "Lambda range (log scale):", min = -10, max = 2, value = c(-6, 0), step = 0.5),
                        numericInput("lr_num_lambda", "Number of lambda values:", value = 100, min = 10, max = 500)
                      ),
                      actionButton("train_lr", "Train Logistic Regression Model"),
                      fluidRow(
                        column(3, h4("Precision: "), textOutput("Acc_LR")),
                        column(3, h4("Recall: "), textOutput("Rec_LR")),
                        column(3, h4("F1_score:"), textOutput("f_score_LR")),
                        # column(3, h4("Best Alpha:"), textOutput("best_alpha_LR"))
                      ),
                      plotOutput("classification_lr"),
                      verbatimTextOutput("lr_summary")
                    ),
                    tabPanel("Decision Tree", 
                      h1("Decision Tree"),
                      conditionalPanel(
                        condition = "input.hyperparameter_tuning == 'grid'",
                        sliderInput("dt_cp", "Complexity Parameter range:", min = 0.001, max = 0.1, value = c(0.001, 0.1), step = 0.001),
                        sliderInput("dt_minsplit", "Min Split range:", min = 5, max = 50, value = c(5, 50), step = 5)
                      ),
                      actionButton("train_dt", "Train Decision Tree Model"),
                      fluidRow(
                        column(4, h4("Precision: "), textOutput("Acc_DT")),
                        column(4, h4("Recall: "), textOutput("Rec_DT")),
                        column(4, h4("F1_score:"), textOutput("f_score_DT"))
                      ),
                      fluidRow(column(12, h4("Decision tree:"), plotOutput("DT_tree"))),
                      h4("ROC Curve:"),
                      plotOutput("Classification_DT")
                    ),
                    tabPanel("SVM", 
                      fluidRow(
                        column(4,
                          selectInput("svm_kernel",
                            label = "Choose SVM kernel",
                            choices = list(
                              "Linear" = "linear",
                              "Polynomial" = "polynomial",
                              "Radial Basis Function (RBF)" = "radial",
                              "Sigmoid" = "sigmoid"
                            ),
                            selected = "linear"
                          ),
                          conditionalPanel(
                            condition = "input.hyperparameter_tuning == 'grid'",
                            sliderInput("cost_range", "Cost range (log scale):", min = -5, max = 5, value = c(-1, 3), step = 1),
                            conditionalPanel(
                              condition = "input.svm_kernel == 'polynomial'",
                              sliderInput("degree_range", "Degree range:", min = 2, max = 5, value = c(2, 3), step = 1)
                            ),
                            conditionalPanel(
                              condition = "input.svm_kernel == 'radial' || input.svm_kernel == 'sigmoid'",
                              sliderInput("gamma_range", "Gamma range (log scale):", min = -5, max = 5, value = c(-3, 1), step = 1)
                            )
                          ),
                          actionButton("train_svm", "Train SVM Model")
                        ),
                        column(8,
                          fluidRow(
                            column(4, h4("Precision: "), textOutput("Acc_SVM")),
                            column(4, h4("Recall: "), textOutput("Rec_SVM")),
                            column(4, h4("F1_score:"), textOutput("f_score_SVM"))
                          ),
                          plotOutput("classification_svm"),
                          verbatimTextOutput("svm_summary")
                        )
                      )
                    )
                  )
          ),
        )
      ),
      tabItem(tabName = "report1",
              h2("Credit fraud"),
              includeHTML("creditcard.html")
      ),
      tabItem(tabName = "report2",
              h2("Bank marketing"),
              includeHTML("Bank_marketing.html")
      ),
      tabItem(tabName = "report3",
              h2("Employee attrition"),
              includeHTML("employee_attrition.html")
      )

      
              )
                                  )
    
    
    ),
  server = function(input, output,session) {
    options(shiny.maxRequestSize=150*1024^2)
    initial_data<-reactiveVal(NULL)
    data <- reactiveVal(NULL)
    df_outliers<-reactiveVal(NULL)
    Variable_outliers=reactiveVal(NULL)
    
    # tÃ©lÃ©charger le dataset
    output$Download <- downloadHandler(
      filename = function() {
        paste(input$file,"_MAJ", ".csv", sep = "")
      },
      content = function(file) {
        write.csv(data(), file, row.names = FALSE)
      }
    )
    
    
    # initialiser dataset #
    observeEvent(input$button,{
      data(initial_data())
      dataset_server(input,output,data())
      exploration_server(input,output,data())
      univaree_server(input,output, data())
      Bivaree_server(input,output, data())
      Qnt_Qlt_server(input,output, data())
      Qlt_Qlt_server(input,output, data())
      Modele_server(input, output, data())
      Missing_server(input, output, data())
    })
    
    # chargement du fichier 
    observeEvent(input$file,{
      infile<-input$file
      if(is.null(infile)) return (NULL)
      delim<-get.delim(infile$datapath, n = 10, comment = "#", skip = 0, delims = c("\t", "\t| +", " ", ";", ",","."), large = 10, one.byte = TRUE)
      #print("HERE !!!!!")
      #print(get.ext(infile$datapath))
      if(get.ext(infile$datapath)=="xls"){
       
        d=read_xls(infile$datapath,header=input$Header, sep =input$sep)
        #print(tmpData)
        data(d)
        initial_data(d)
        
      }else{
        #print("I am here csv")
        d=read.csv(infile$datapath, header=input$Header, sep =delim)
        data(d)
        initial_data(d)
        
      }
      dataset_server(input,output,data())
      exploration_server(input,output,data())
      univaree_server(input,output, data())
      Bivaree_server(input,output, data())
      Qnt_Qlt_server(input,output, data())
      Qlt_Qlt_server(input,output, data())
      Modele_server(input, output, data())
      Missing_server(input, output, data())
    })
    # sÃ©parateur
    observeEvent(input$sep,{
      
      infile<-input$file
      if(is.null(infile)) return (NULL)
      #delim<-get.delim(infile$datapath, n = 10, comment = "#", skip = 0, delims = c("\t", "\t| +", " ", ";", ",","."), large = 10, one.byte = TRUE)
      #print(delim)
      #print("HERE !!!!!")
      #print(get.ext(infile$datapath))
      if(get.ext(infile$datapath)=="xls"){
        #print("I am here xls")
        d=read_xls(infile$datapath,header=input$Header, sep =input$sep)
        #print(tmpData)
        data(d)
        
      }else{
        #print("I am here csv")
        d=read.csv(infile$datapath, header=input$Header, sep =input$sep)
        data(d)
        
        
      }
      dataset_server(input,output,data())
      exploration_server(input,output,data())
      univaree_server(input,output, data())
      Bivaree_server(input,output, data())
      Qnt_Qlt_server(input,output, data())
      Qlt_Qlt_server(input,output, data())
      Modele_server(input, output, data())
      
      
    })
    # affichage des noms de colonnes
    observeEvent(input$Header,{
      
      infile<-input$file
      if(is.null(infile)) return (NULL)
      delim<-get.delim(infile$datapath, n = 10, comment = "#", skip = 0, delims = c("\t", "\t| +", " ", ";", ",","."), large = 10, one.byte = TRUE)
      #print(delim)
      #print("HERE !!!!!")
      #print(get.ext(infile$datapath))
      if(get.ext(infile$datapath)=="xls"){
        #print("I am here xls")
        d=read_xls(infile$datapath)
        #print(tmpData)
        data(d)
        
      }else{
        #print("I am here csv")
        d=read.csv(infile$datapath, header=input$Header, sep =input$sep)
        data(d)
        
        
      }
      dataset_server(input,output,data())
      exploration_server(input,output,data())
      univaree_server(input,output, data())
      Bivaree_server(input,output, data())
      Qnt_Qlt_server(input,output, data())
      Qlt_Qlt_server(input,output, data())
      Modele_server(input, output, data())
    })
    
    
    
    
    
    # DataTable
    
    dataset_server(input,output,data())
    
    
    
    
    univaree_server(input,output, data())
    # Analyse bivariÃ©e
    #Quantitative vs Quantitative
    Bivaree_server(input,output, data())
    
    # quantitative vs qualitative 
    Qnt_Qlt_server(input,output, data())
    
    # qualitative vs qualitative
    Qlt_Qlt_server(input,output, data())
    
    # exploration de donnÃ©es
    exploration_server(input,output,data())
    
    # Gestion des valeurs manquantes
    #Missing_server(input,output)
    
    Modele_server(input, output, data())
    
    #######################
    
    # summury dÃ©taillÃ©e
    output$sumariz1 <- renderUI({
      # print(summarytools::dfSummary(data(), graph.magnif = .7, headings = FALSE, Missing=FALSE),
      #       method = 'render')
      print(
        summarytools::dfSummary(data(), 
                                varnumbers   = FALSE,
                                na.col       = FALSE,
                                style        = "multiline",
                                plain.ascii  = FALSE,
                                headings     = FALSE,
                                graph.magnif = .8),
        method = "render"
      )
      
    })
    
    # imputation des valeurs manquantes
    # quantitative
    quantitative<- reactive({
      names(data())[!grepl('factor|logical|character',sapply(data(),class))]
    })
    
    output$quantlistM = renderUI({
      selectInput('quantlistM', 'Le choix de la variable Manquante',colnames(data())[colSums(is.na(data())) > 0])
    })
    
    observeEvent(input$imputation,{
      # imputation par la moyenne
      
      if(input$imputation=="Im"){
        if(length(quantitative())>0){
          var.names <-c(input$quantlistM)
          # Initialisation de la table
          caract.df <- data.frame()
          
          # Pour chaque colonne, calcul de min, max, mean et ecart-type
          df=data()
          df2=df
          for(strCol in var.names){
            
            if(sum(is.na(data()[,strCol]))!=0){
              
              df[is.na(df[,strCol]), strCol]<-mean(data()[,strCol], na.rm = TRUE)
              
              output$resultat_Im <- renderPlot({
                plot(df[,strCol], type='l', pch=16, col='red', xlab=strCol, ylab='')
                lines(df2[,strCol],col="green")
                legend(x = "topright",          # Position
                       legend = c(paste("avec imputation par la moyenne de la variable ",strCol,""), "avec valeurs manquantes"),  # Legend texts
                       lty = c(1, 2),           # Line types
                       col = c(2, 3),           # Line colors
                       lwd = 2)                 
              } )

            }
            
            
            
          }
          data(df)
          dataset_server(input,output,data())
          exploration_server(input,output,data())
          univaree_server(input,output, data())
          Bivaree_server(input,output, data())
          Qnt_Qlt_server(input,output, data())
          Qlt_Qlt_server(input,output, data())
          Modele_server(input, output, data())
          #print("les valeurs manquantes sont imputÃ©es")
          
        }else{
          print("ya pas de variables quantitative manquantes")
        }
        
      }
      
      
      
      # PrÃ©sence des outliers
      quantitative<- reactive({
        names(data())[!grepl('factor|logical|character',sapply(data(),class))]
      })
      

    
      
      # imputation par la mediane
      
      if(input$imputation=="Id"){
        if(length(quantitative())>0){
          var.names <-c(input$quantlistM)
          # Initialisation de la table
          caract.df <- data.frame()
          
          # Pour chaque colonne, calcul de min, max, mean et ecart-type
          df=data()
          df2=df
          for(strCol in var.names){
            
            if(sum(is.na(data()[,strCol]))!=0){
              
              df[is.na(df[,strCol]), strCol]<-median(data()[,strCol], na.rm = TRUE)
              
              output$resultat_Id <- renderPlot({
                plot(df[,strCol], type='l', pch=16, col='red', xlab=' x', ylab='')
                lines(df2[,strCol],col="green")
                legend(x = "topright",          # Position
                       legend = c(paste("avec imputation par la mediane de la variable ",strCol,""), "avec valeurs manquantes"),  # Legend texts
                       lty = c(1, 2),           # Line types
                       col = c(2, 3),           # Line colors
                       lwd = 2) 
                
                
                
              } )
              
              
              
              
            }
            
            
            
          }
          data(df)
          dataset_server(input,output,data())
          exploration_server(input,output,data())
          univaree_server(input,output, data())
          Bivaree_server(input,output, data())
          Qnt_Qlt_server(input,output, data())
          Qlt_Qlt_server(input,output, data())
          Modele_server(input, output, data())
          #print("les valeurs manquantes sont imputÃ©es avec la valeur de la mÃ©diane")
          
        }else{
          print("ya pas de variables Quantitative")
        }
        
      }
      
      # imputation par l'interpolation
      if(input$imputation=="Ip"){
        
        
        if(length(quantitative())>0){
          var.names <-c(input$quantlistM)
          # Initialisation de la table
          caract.df <- data.frame()
          
          # Pour chaque colonne, calcul de min, max, mean et ecart-type
          df=data()
          df2=df
          for(strCol in var.names){
            
            if(sum(is.na(data()[,strCol]))!=0){
              #print(df[,strCol])
              #print("-----------------------------")
              
              # df <- df %>%
              #   mutate(strCol = na.approx(strCol))
              #print(median(data()[,strCol], na.rm = TRUE))
              df[,strCol]=na.approx(df[,strCol])
              #print(df[,strCol])
              
              output$resultat_Ip <- renderPlot({
                plot(df[,strCol], type='l', pch=16, col='red', xlab=' x', ylab='')
                lines(df2[,strCol],col="green")
                legend(x = "topright",          # Position
                       legend = c(paste("avec imputation par l'interpolation e de la variable ",strCol,""), "avec valeurs manquantes"),  # Legend texts
                       lty = c(1, 2),           # Line types
                       col = c(2, 3),           # Line colors
                       lwd = 2) 
                
                
                
              } )
              
              
              
              
            }
            
            
            
          }
          data(df)
          dataset_server(input,output,data())
          exploration_server(input,output,data())
          univaree_server(input,output, data())
          Bivaree_server(input,output, data())
          Qnt_Qlt_server(input,output, data())
          Qlt_Qlt_server(input,output, data())
          Modele_server(input, output, data())
          #print("les valeurs manquantes sont imputÃ©es par l'interpolation")
          
        }else{
          print("ya pas de variables Quantitative")
        }
        
      }
      # suppression de la valeur manquantes (la colonne)
      if(input$imputation=="Sc"){
        #df=data()
        var.names <-c(input$quantlistM)
        #print(" i am here")
        #var.names<-c("Major2")
        #print(var.names)
        df =data()[,!(names(data()) %in%  var.names)]
        
        
        data(df)
        dataset_server(input,output,data())
        exploration_server(input,output,data())
        univaree_server(input,output, data())
        Bivaree_server(input,output, data())
        Qnt_Qlt_server(input,output, data())
        Qlt_Qlt_server(input,output, data())
        Modele_server(input, output, data())
        output$resultat_Sc <-renderText( print("la colonne a bien Ã©tÃ© supprimÃ©e"))
        
      }
      
      if(input$imputation=="vsl"){
        
        
        output$visualisation_manquantes <- renderPlot({
          #dfa <- data.frame()
          dfa <- data()
          
          missing.values <- dfa %>%
            gather(key = "key", value = "val") %>%
            mutate(isna = is.na(val)) %>%
            group_by(key) %>%
            mutate(total = n()) %>%
            group_by(key, total, isna) %>%
            summarise(num.isna = n()) %>%
            mutate(pct = num.isna / total * 100)
          
          
          levels <-
            (missing.values  %>% filter(isna == T) %>% arrange(desc(pct)))$key
          
          
          dfa %>%
            mutate(id = row_number()) %>%
            gather(-id, key = "key", value = "val") %>%
            mutate(isna = factor(is.na(val))) %>%
            ggplot(aes(key, id, fill = isna)) +
            geom_raster(alpha = 0.8) +
            scale_fill_manual(
              name = "",
              values = c("blue4", "grey50"),
              labels = c("Present", "Missing")
            ) +
            scale_x_discrete(limits = levels) +
            labs(x = "Variable manquantes",
                 y = "nombre de lignes", title = "valeurs manquantes par ligne") +
            coord_flip() + theme_light() + theme(text = element_text(size = 17))
          
        }, height = 350, width = 600)
        
        
      }
      
      
      
      
    })
    
    
    
    
    
    
    
    
    
    
    
    # qualitative
    
    qualitative<- reactive({
      names(data())[grepl('factor|logical|character',sapply(data(),class))]
    })
    output$qualistM = renderUI({
      selectInput('qualistM', 'Le choix de la variable Manquante',colnames(data())[colSums(data()=="" | data()=="?")>0])
    })
    output$resultat_mode2 <- renderPlot({
      if(length(qualitative())>0){
        var.names <-c(input$qualistM)
        # Initialisation de la table
        caract.df <- data.frame()
        df=data()
        # Pour chaque colonne, calcul de min, max, mean et ecart-type
        for(strCol in var.names){
          
          if(sum(as.character(data()[,strCol])=="")>0){
            # print("Iam here ya younes")
            # print(df[,strCol])
            # List the distinct / unique values
            distinct_values <- unique(data()[,strCol])
            d=distinct_values[distinct_values!=""]
            
            # Count the occurrence of each distinct value
            distinct_tabulate <- tabulate(match(data()[,strCol],d))
            
            # Return the value with the highest occurrence
            mode=distinct_values[which.max(distinct_tabulate)]
            
            #print(mode)
            df[,strCol]<-mode
            # print("-----------------------")
            # print(df[,strCol])
            
            var1<-"Major2"
            var2<-"Major3"
            ggplot(data(), aes(x = !!var1, fill = !!var2)) + geom_bar()
            data(df)
            
            
          }
          
          
          
        }
        #print("les valeurs manquantes sont imputÃ©es")
        
      }else{
        #print("ya pas de variables Quantitative")
      }
      
      
    } )
    
    
    
    observeEvent(input$imputationQ,{
      if(input$imputationQ=="sup"){
        
        
        #df=data()
        var.names <-c(input$qualistM)
        #print(" i am here")
        #var.names<-c("Major2")
        #print(var.names)
        df =data()[,!(names(data()) %in%  var.names)]
        
        
        data(df)
        dataset_server(input,output,data())
        exploration_server(input,output,data())
        univaree_server(input,output, data())
        Bivaree_server(input,output, data())
        Qnt_Qlt_server(input,output, data())
        Qlt_Qlt_server(input,output, data())
        Modele_server(input, output, data())
        output$resultat_sup <-renderText( print("la colonne a bien été supprimée"))
        
      }
      if(input$imputationQ=="mode"){
        df=data()
        var.names <-c(input$qualistM)
        for(strCol in var.names){
          #print("---- mode ----")
          #print(strCol)
          #print(df[,var.names[0]])
          #print("------------------------")
          distinct_values <- unique(data()[,strCol])
          d=distinct_values[distinct_values!=""]
          
          # Count the occurrence of each distinct value
          distinct_tabulate <- tabulate(match(data()[,strCol],d))
          
          # Return the value with the highest occurrence
          mode=distinct_values[which.max(distinct_tabulate)]
          #print(mode)
          #print(mode)
          df[,strCol]<-mode
          #print(df[,strCol])
          data(df)
          dataset_server(input,output,data())
          exploration_server(input,output,data())
          univaree_server(input,output, data())
          Bivaree_server(input,output, data())
          Qnt_Qlt_server(input,output, data())
          Qlt_Qlt_server(input,output, data())
          Modele_server(input, output, data())
          output$resultat_mode <- renderPlot({
            
            
            var1<-data()[,strCol]
            var2<-df[,strCol]
            ggplot(df, aes(x = var1,fill = var2)) + geom_bar(position = "dodge")
            
            
          })
          
        }
      }
      
      # visualisation des valeurs manquantes qualitatives
      
      if(input$imputationQ=="vslQ"){
        output$visualisation_manquantesQ <- renderPlot({
          #dfa <- data.frame()
          dfa <- data()
          
          missing.values <- dfa %>%
            gather(key = "key", value = "val") %>%
            mutate(isna = (val=="") | (val=="?")) %>%
            group_by(key) %>%
            mutate(total = n()) %>%
            group_by(key, total, isna) %>%
            summarise(num.isna = n()) %>%
            mutate(pct = num.isna / total * 100)
          
          
          levels <-
            (missing.values  %>% filter(isna == T) %>% arrange(desc(pct)))$key
          
          
          dfa %>%
            mutate(id = row_number()) %>%
            gather(-id, key = "key", value = "val") %>%
            mutate(isna = factor((val=="" | val=="?"))) %>%
            ggplot(aes(key, id, fill = isna)) +
            geom_raster(alpha = 0.8) +
            scale_fill_manual(
              name = "",
              values = c("blue4", "grey50"),
              labels = c("Present", "Missing")
            ) +
            scale_x_discrete(limits = levels) +
            labs(x = "Variable manquante",
                 y = "nombre de lignes", title = "valeurs manquantes par ligne") +
            coord_flip() + theme_light() + theme(text = element_text(size = 17))
          
        }, height = 350, width = 600)
        
      }
      
      
    })
    
    
    
    #Traitement des outliers (la vrai version)
    Outliers <- function() {
      df=data()
      list=list()
      l2=list("")
      list=colnames(df)[colSums(is.na(df)) > 0]
      #print(list)
      if(length(list)>0){
        l2=c(" il faut d'abord traiter les valeurs manquantes")
      }
      else{
        qt_list=list()
        qt_list=names(data())[!grepl('factor|logical|character',sapply(data(),class))]
        var.names<-qt_list
        #print("liste quantitative")
        #print(var.names)
        for(strCol in var.names){
          # get IQR
          iqr=IQR(df[,strCol],na.rm = TRUE)
          first_q=quantile((df[,strCol]),na.rm = TRUE)[2]
          third_q=quantile((df[,strCol]),na.rm = TRUE)[4]
          # # get threshold values for outliers
          Tmin = first_q-(1.5*iqr) 
          Tmax = third_q+(1.5*iqr) 
          # 
          value =boxplot(df[,strCol],plot=FALSE)$out
          #nb_out=length(var(df[,strCol])[which(var(df[,strCol]) < Tmin | var(df[,strCol]) > Tmax)])
          if(length(value)>0){
            #print(strCol)
            l2=append(l2,strCol)
            
          }
          
          
        }
        
      }
      print(l2)
    }
    # liste de variables quantitative contennat les outliers
    output$OutList = renderUI({
      l2=Outliers()
      selectInput('OutList', 'Variables contenant des outliers',l2)
    })
    
    
    
    observeEvent(input$Out,{
      if(length(quantitative()) >0 && input$Out=="Af"){
        df=data()
        l=list()
        var.names <-c(input$OutList)
        #     # Initialisation de la table
        #     caract.df <- data.frame()
        
        # Pour chaque colonne, calcul de min, max, mean et ecart-type
        for(strCol in var.names){
          
          
          
          
          # get IQR
          iqr=IQR(df[,strCol],na.rm = TRUE)
          first_q=quantile((df[,strCol]))[2]
          third_q=quantile((df[,strCol]))[4]
          # get threshold values for outliers
          Tmin = first_q-(1.5*iqr) 
          Tmax = third_q+(1.5*iqr) 
          
          value =boxplot(df[,strCol],plot=FALSE)$out
          # nb_out=length(var(df[,strCol])[which(var(df[,strCol]) < Tmin | var(df[,strCol]) > Tmax)])
          if(length(value)>0){
            
            df_outliers(df[df[,strCol] %in%  c(value),])
            
            #output$outlier=renderText(value)
            
            #print(df$strCol %in%  c(value))
            output$tab_outliers=DT::renderDataTable(
              df_outliers(),
              options = list(scrollY = 650,
                             scrollX = 500,
                             
                             #deferRender = TRUE,
                             scroller = TRUE)
              
              
            )
            
            
          }
          
          
        }
        
      }
      # supprimer la valeur outlier
      
      
      if(length(quantitative()) >0 && input$Out=="Oui"){
        df=df_outliers()
        df2=data()

        # index de la ligne a supprimer
        c=as.numeric(rownames(df[input$tab_outliers_rows_selected, ]))
        # #print(input$tab_outliers_rows_selected)
        if (!is.null(input$tab_outliers_rows_selected)) {
          if(length(c)>0){
            df <- df[-input$tab_outliers_rows_selected, ]
          
            for(a in c){
              
              tab=data()[-as.numeric(c), ]
              rownames(tab) <- NULL
 
              
            }
            data(tab)
            dataset_server(input,output,data())
            exploration_server(input,output,data())
            univaree_server(input,output, data())
            Bivaree_server(input,output, data())
            Qnt_Qlt_server(input,output, data())
            Qlt_Qlt_server(input,output, data())
            df_outliers(df)
            
          }
          
        }
        
        
        
      }
      
      
    })
    
    # Normalisation des variables quantitative
    
    output$NList<- renderUI({
      selectInput('NList', 'Le choix de la variable ',colnames(data())[!grepl('factor|logical|character',sapply(data(),class))])
      
    })
    observeEvent(input$normalisation,{
      
      # Visulalisation avant et apres la normalisation
      
      if(input$normalisation=="Vsd"){
        
        if(length(quantitative())>0){
          var.names <-c(input$NList)
          # Initialisation de la table
          caract.df <- data.frame()
          
          # Pour chaque colonne, calcul de min, max, mean et ecart-type
          df=data()
          df2=df
          for(strCol in var.names){
            
            
            
            
            output$visualisation_N <- renderPlot({
              plot(df[,strCol], type='l', pch=16, col='green', xlab='nombre de lignes', ylab='valeurs')
              
              legend(x = "topright",          # Position
                     legend = c(paste("les données de la variable ",strCol,""),"") # Legend texts
                     
              ) 
              
              
              
            } )

          }
          
          
        }else{
          print("ya pas de variables Quantitative")
        }
        
      }
      
      # Normalisation Min-Max
      if(input$normalisation=="Nm"){
        
        if(length(quantitative())>0){
          var.names <-c(input$NList)
          # Initialisation de la table
          caract.df <- data.frame()
          
          # Pour chaque colonne, calcul de min, max, mean et ecart-type
          df=data()
          df2=df
          for(strCol in var.names){
            
            
            #process <- preProcess(df[,strCol], method=c("range"))
            df[,strCol]<- scales::rescale(df[,strCol],to =c(0,1))
            data(df)
            dataset_server(input,output,data())
            exploration_server(input,output,data())
            univaree_server(input,output, data())
            Bivaree_server(input,output, data())
            Qnt_Qlt_server(input,output, data())
            Qlt_Qlt_server(input,output, data())
            #output$resultat_Nm <-renderText( print("la donnÃ©es ont bien Ã©tÃ© normalisÃ©es"))
            output$visualisation_Nm <- renderPlot({
              plot(df[,strCol], type='l', pch=16, col='green', xlab='nombre de lignes', ylab='valeurs')
              
              legend(x = "topright",          # Position
                     legend = c(paste("les données de la variable ",strCol,""),"") # Legend texts
                     
              ) 
              
              
              
            } )
            
            #print(df[,strCol])
            
            
            
            
            
            
            
            
          }
          
          
        }else{
          print("ya pas de variables Quantitative")
        }
        
        
        
      }
      
      
      # Normalisation Z-score
      
      # Normalisation Min-Max
      if(input$normalisation=="Nz"){
        
        if(length(quantitative())>0){
          var.names <-c(input$NList)
          # Initialisation de la table
          caract.df <- data.frame()
          
          # Pour chaque colonne, calcul de min, max, mean et ecart-type
          df=data()
          df2=df
          for(strCol in var.names){
            #Mean
            m<-mean(df[,strCol])
            
            # Finding Standard Deviation
            s<-sd(df[,strCol])
            
            #standardized vector
            df[,strCol]<-(df[,strCol]-m)/s
            
            
            data(df)
            dataset_server(input,output,data())
            exploration_server(input,output,data())
            univaree_server(input,output, data())
            Bivaree_server(input,output, data())
            Qnt_Qlt_server(input,output, data())
            Qlt_Qlt_server(input,output, data())
            #output$resultat_Nz <-renderText( print("la donnÃ©es ont bien Ã©tÃ© normalisÃ©es par la mÃ©thode Z-score"))
            output$visualisation_Nz <- renderPlot({
              plot(df[,strCol], type='l', pch=16, col='green', xlab='nombre de lignes', ylab='valeurs')
              
              legend(x = "topright",          # Position
                     legend = c(paste("les données de la variable ",strCol,""),"") # Legend texts
                     
              ) 
              
              
              
            } )
            
            #print(df[,strCol])
            
            
            
            
            
            
            
            
          }
          
          
        }else{
          print("ya pas de variables Quantitative")
        }
        
        
        
      }
    })
    
    # la dummification
    
    output$dmList<- renderUI({
      selectInput('dmList', 'Le choix de la variable ',colnames(data())[grepl('factor|logical|character',sapply(data(),class))])
      
    })
    observeEvent(input$dm,{
      
      if(input$dm=="Oui"){
        
        
        if(length(qualitative())>0){
          
          var.names <-c(input$dmList)
          for(strCol in var.names){
            missing=sum(as.character(data()[,strCol])=="")+sum(as.character(data()[,strCol])=="?")
            
            if(missing>0){
              
              output$resultat_dm2<-renderText(
                paste0("la variable ",strCol," contient des valeurs manquantes, il faut les nettoyer !!")
                
                
              ) 
              output$resultat_dm<-renderText(
                "") 
              
              
            }else{
              print(strCol)
              df=dummy_cols(data(),select_columns=strCol)
              # supprimer la variable categorielle
              df =df[,!(names(df) %in%  strCol)]
              data(df)
              dataset_server(input,output,data())
              exploration_server(input,output,data())
              univaree_server(input,output, data())
              Bivaree_server(input,output, data())
              Qnt_Qlt_server(input,output, data())
              Qlt_Qlt_server(input,output, data())
              output$resultat_dm<-renderText(
                paste0("la variable ",strCol," a bien été dummfiée")
                
              )
              output$resultat_dm2<-renderText(
                "") 
              
            }
            
            
          }
          
        }
      }
    })
    
    
    
    
    # le rÃ©Ã©quilibrage de classe
    
    output$reList<- renderUI({
      selectInput('reList', 'Le choix de la classe ',colnames(data()))
      
    })
    
    observeEvent(input$re,{
      if(input$re=="Vc"){
        var.names <-c(input$reList)
        for(strCol in var.names){
          output$Vc<-renderPlot({
            df=data()
            #col<-sym(strCol)
            #column <-ensym(strCol)
            
            
            df2=table(df[,strCol])
            
            
            # ggplot(df2, aes(x=column, y=counts, fill=column)) +
            #   geom_bar(stat = "identity") +
            #   geom_text(aes(label = counts), vjust = -0.3) +
            #   theme_pubclean()
            coul <- brewer.pal(5, "Set2")
            barplot(df2, main = "Répartition de classe",
                    xlab=sym(strCol),
                    ylab="Counts", las = 2,
                    col = coul,
                    names.arg = substr(names(df2), 1, 9))
          })
        }
        
        
      }
      
      # MÃ©thode oversampling
      if(input$re=="Over"){
        var.names <-c(input$reList)
        for(strCol in var.names){
          df=data()
          if(length(table(df[,strCol]))!=2){
            output$resultat_Over2<-renderText(
              paste0("l'attribut n'est pas binaire,, veuillez choisir la classe(target)")
              
            )
            output$resultat_Over<-renderText(
              paste0("")
              
            )
            
            
            
          }
          else{
            datnrow <- nrow(df)
            if (nrow(na.omit(df)) < datnrow) {
              output$resultat_Over2<-renderText(
                paste0("ce dataset contient des valeurs manquantes")
                
              )
              output$resultat_Over<-renderText(
                paste0("")
                
              )
            }
            else{
              df[,strCol]=factor(df[,strCol])
              
              #df2 <- ovun.sample(Outcome~., data =train, method = "over", N=1960)$data
              df2<-ROS2(df,strCol)
              #a=any(is.na(data()))
              #print(a)
              data(df2)
              dataset_server(input,output,data())
              exploration_server(input,output,data())
              univaree_server(input,output, data())
              Bivaree_server(input,output, data())
              Qnt_Qlt_server(input,output, data())
              Qlt_Qlt_server(input,output, data())
              output$resultat_Over<-renderText(
                paste0("l'over-sampling a bien été fait")
                
              )
              output$resultat_Over2<-renderText(
                paste0("")
                
              )
              
              output$Vc_Over<-renderPlot({
                df=data()
                
                
                
                df2=table(df[,strCol])
                
                coul <- brewer.pal(5, "Set2")
                barplot(df2, main = "Répartition de classe aprés Oversampling ",
                        xlab=sym(strCol),
                        ylab="Counts", las = 2,
                        col = coul,
                        names.arg = substr(names(df2), 1, 9))
              })
              
            }
          }
          
          
          
        }
        
      }
      
      # MÃ©thode under-sampling
      
      if(input$re=="Under"){
        var.names <-c(input$reList)
        for(strCol in var.names){
          df=data()
          if(length(table(df[,strCol]))!=2){
            output$resultat_Under2<-renderText(
              paste0("l'attribut n'est pas binaire, veuillez choisir la classe(target) binaire")
              
            )
            output$resultat_Under<-renderText(
              paste0("")
              
            )
            
            
            
          }
          else{
            datnrow <- nrow(df)
            if (nrow(na.omit(df)) < datnrow) {
              output$resultat_Under2<-renderText(
                paste0("ce dataset contient des valeurs manquantes")
                
              )
              output$resultat_Under<-renderText(
                paste0("")
                
              )
            }
            else{
              df[,strCol]=factor(df[,strCol])
              
              #df2 <- ovun.sample(Outcome~., data =train, method = "over", N=1960)$data
              df2<-RUS(df,strCol)
              #a=any(is.na(data()))
              #print(a)
              data(df2)
              dataset_server(input,output,data())
              exploration_server(input,output,data())
              univaree_server(input,output, data())
              Bivaree_server(input,output, data())
              Qnt_Qlt_server(input,output, data())
              Qlt_Qlt_server(input,output, data())
              output$resultat_Under<-renderText(
                paste0("l'under-sampling a bien été fait")
                
              )
              output$resultat_Under2<-renderText(
                paste0("")
                
              )
              
              output$Vc_Under<-renderPlot({
                df=data()
                
                
                
                df2=table(df[,strCol])
                
                coul <- brewer.pal(5, "Set2")
                barplot(df2, main = "Répartition de classe aprés Undersampling ",
                        xlab=sym(strCol),
                        ylab="Counts", las = 2,
                        col = coul,
                        names.arg = substr(names(df2), 1, 9))
              })
              
            }
          }
          
          
          
        }
        
      }
      
      # Both of them
      if(input$re=="Both"){
        var.names <-c(input$reList)
        for(strCol in var.names){
          df=data()
          if(length(table(df[,strCol]))!=2){
            output$resultat_Both2<-renderText(
              paste0("l'attribut n'est pas binaire, veuillez choisir la classe(target)")
              
            )
            output$resultat_Both<-renderText(
              paste0("")
              
            )
            
            
            
          }
          else{
            df[,strCol]=factor(df[,strCol])
            
            df2 <- ovun.sample(Outcome~., data =df, method = "both", N=nrow(df))$data
            #df2<-RUS(df,strCol)
            #a=any(is.na(data()))
            #print(a)
            data(df2)
            dataset_server(input,output,data())
            exploration_server(input,output,data())
            univaree_server(input,output, data())
            Bivaree_server(input,output, data())
            Qnt_Qlt_server(input,output, data())
            Qlt_Qlt_server(input,output, data())
            output$resultat_Both<-renderText(
              paste0("l'under-sampling a bien été fait")
              
            )
            output$resultat_Both2<-renderText(
              paste0("")
              
            )
            
          }
          
          
          
        }
        
      }
      
      
    })
    
    
    ## renommer la colonne ###
    output$renameList<- renderUI({
      selectInput('renameList', 'Le choix de la variable ',colnames(data()))
      
    })
    
    observeEvent(input$Save,{
      var.names <-c(input$Id)
      df=data()
      for(strCol in var.names){
        
        colnames(df)[names(df) == input$renameList] =strCol
        
        
        data(df)
        dataset_server(input,output,data())
        exploration_server(input,output,data())
        univaree_server(input,output, data())
        Bivaree_server(input,output, data())
        Qnt_Qlt_server(input,output, data())
        Qlt_Qlt_server(input,output, data())
        output$rename<-renderText(
          "l'attribut a bien été renommée"
        )
      }
      
    })
    
    
    
    # MAG les valeurs d'un att
    
    output$attList<- renderUI({
      selectInput('attList', 'Le choix de la variable ',colnames(data()))
      
    })
    output$typeList<- renderUI({
      selectInput('type', 'Type de la nouvelle valeur',c("Character","Integer","Double"))
      
    })
    observeEvent(input$attList,{
      output$valList<- renderUI({
        selectInput('valList', 'Le choix de sa valeur ',(data()[,input$attList]))
        
      })
      
      
    })
    
    observeEvent(input$MAJ,{
      
      df=data()
      if(input$valList==""){
        
      }
      else{
        if(input$Id2==""){
          
        }else{
          
          var.names <-c(input$attList)
          for(strCol in var.names){
            if(input$type=="Character"){
              df[df[,strCol]==input$valList, strCol]<-input$Id2
              
            }
            if(input$type=="Integer"){
              x=as.integer(input$Id2)
              df[df[,strCol]==input$valList, strCol]<-as.numeric(x)
              #df=transform.data.frame(df,strCol=as.numeric(strCol))
              
            }
            if(input$type=="Double"){
              x=as.double(input$Id2)
              df[df[,strCol]==input$valList, strCol]<-as.numeric(x)
              #df=transform.data.frame(df,as.numeric(df[,strCol]))
              
            }
            
            
          }
          data(df)
          dataset_server(input,output,data())
          exploration_server(input,output,data())
          univaree_server(input,output, data())
          Bivaree_server(input,output, data())
          Qnt_Qlt_server(input,output, data())
          Qlt_Qlt_server(input,output, data())
          output$MAJtxt<-renderText("la valeur a bien été mise a jour")
          
        }
        
      }
    })
    
    
########### models part ####################################
    
    # This is to select the target column

    observe({
      req(data())
      updateSelectInput(session, "target_column",
                        choices = names(data()),
                        selected = names(data())[1])
    })

    # This is to create the dynamic UI for feature selection
    output$feature_selection_ui <- renderUI({
      req(data())
      req(input$target_column)
      all_columns <- names(data())
      feature_columns <- all_columns[all_columns != input$target_column]
      
      selectInput("feature_columns", "Select Feature Columns:",
                  choices = feature_columns,
                  multiple = TRUE,
                  selected = feature_columns)
    })

    # Handle "Select All Features" button
    observeEvent(input$select_all_features, {
      req(data())
      all_columns <- names(data())
      feature_columns <- all_columns[all_columns != input$target_column]
      updateSelectInput(session, "feature_columns",
                        selected = feature_columns)
    })

    # Handle "Deselect All Features" button
    observeEvent(input$deselect_all_features, {
      updateSelectInput(session, "feature_columns",
                        selected = character(0))
    })

    # Update feature selection when target column changes
    observe({
      req(input$target_column)
      all_columns <- names(data())
      feature_columns <- all_columns[all_columns != input$target_column]
      updateSelectInput(session, "feature_columns",
                        choices = feature_columns,
                        selected = intersect(input$feature_columns, feature_columns))
    })
    
    
    # Linear Regression
  

    # Train Logistic Regression model
    lr_params <- reactiveVal(list())

    observe({
      lr_params(list(
        hyperparameter_tuning = input$hyperparameter_tuning,
        alpha_range = if(input$hyperparameter_tuning == "grid") input$lr_alpha_range else c(1, 1),
        lambda_range = if(input$hyperparameter_tuning == "grid") input$lr_lambda_range else c(-6, 0),
        num_lambda = if(input$hyperparameter_tuning == "grid") input$lr_num_lambda else 100
      ))
    })

    lr_model <- reactive({
      req(input$train_lr)
      req(lr_params())
      req(input$feature_columns)  # Ensure feature columns are selected
      
      # Get the data, target column, and selected features
      df <- data()
      target_col <- input$target_column
      feature_cols <- input$feature_columns
      
      # Ensure the target column is a factor
      df[[target_col]] <- as.factor(df[[target_col]])
      
      set.seed(2001)
      train_index <- createDataPartition(df[[target_col]], p = input$trainsplit, list = FALSE)
      train_data <- df[train_index, c(target_col, feature_cols)]
      test_data <- df[-train_index, c(target_col, feature_cols)]
      
      # Handle class imbalance
      if (input$radio == 1) {  # Over Sampling
        train_data <- upSample(x = train_data[, feature_cols],
                              y = train_data[[target_col]],
                              yname = target_col)
      } else if (input$radio == 2) {  # Under Sampling
        train_data <- downSample(x = train_data[, feature_cols],
                                y = train_data[[target_col]],
                                yname = target_col)
      }
      
      # Prepare the data matrix
      x <- as.matrix(train_data[, feature_cols])
      y <- train_data[[target_col]]
      
      params <- lr_params()
      
      if (params$hyperparameter_tuning == "grid") {
        # Perform grid search
        alpha_range <- seq(params$alpha_range[1], params$alpha_range[2], length.out = 5)
        lambda_range <- 10^seq(params$lambda_range[1], params$lambda_range[2], length.out = params$num_lambda)
        
        best_model <- NULL
        best_cvm <- Inf
        best_alpha <- NULL
        
        for (alpha in alpha_range) {
          model <- glmnet::cv.glmnet(x = x,
                                    y = y,
                                    alpha = alpha,
                                    lambda = lambda_range,
                                    family = "binomial",
                                    type.measure = "class")
          
          if (min(model$cvm) < best_cvm) {
            best_model <- model
            best_cvm <- min(model$cvm)
            best_alpha <- alpha
          }
        }
        
        model <- best_model
        attr(model, "best_alpha") <- best_alpha
        
      } else {
        # Use default parameters
        model <- glmnet::cv.glmnet(x = x,
                                  y = y,
                                  alpha = 1,
                                  family = "binomial",
                                  type.measure = "class")
        attr(model, "best_alpha") <- 1
      }
      
      return(model)
    })

    # Reactive expression for model evaluation
    lr_eval <- reactive({
      req(lr_model())
      req(input$feature_columns)  # Ensure feature columns are selected
      model <- lr_model()
      
      df <- data()
      target_col <- input$target_column
      feature_cols <- input$feature_columns
      
      set.seed(2001)
      train_index <- createDataPartition(df[[target_col]], p = input$trainsplit, list = FALSE)
      test_data <- df[-train_index, c(target_col, feature_cols)]
      
      # Make predictions on test data
      predictions <- predict(model, newx = as.matrix(test_data[, feature_cols]),
                            s = "lambda.min", type = "class")
      prob_predictions <- predict(model, newx = as.matrix(test_data[, feature_cols]),
                                  s = "lambda.min", type = "response")
      
      # Calculate metrics
      cm <- confusionMatrix(as.factor(predictions), as.factor(test_data[[target_col]]))
      precision <- cm$byClass["Precision"]
      recall <- cm$byClass["Recall"]
      f1_score <- cm$byClass["F1"]
      
      # ROC curve data
      roc_obj <- roc(as.numeric(as.factor(test_data[[target_col]])) - 1, as.numeric(prob_predictions))
      
      list(
        predictions = predictions, 
        test_data = test_data, 
        precision = precision, 
        recall = recall, 
        f1_score = f1_score,
        roc_obj = roc_obj
      )
    })



      # Outputs
    output$Acc_LR <- renderText({ 
      req(lr_eval())
      sprintf("%.2f", lr_eval()$precision) 
    })

    output$Rec_LR <- renderText({ 
      req(lr_eval())
      sprintf("%.2f", lr_eval()$recall) 
    })

    output$f_score_LR <- renderText({ 
      req(lr_eval())
      sprintf("%.2f", lr_eval()$f1_score) 
    })

    output$AUC_LR <- renderText({ 
      req(lr_eval())
      sprintf("%.3f", lr_eval()$roc_obj$auc) 
    })

    # output$best_alpha_LR <- renderText({ 
    #   req(lr_model())
    #   sprintf("%.3f", attr(lr_model(), "best_alpha"))
    # })

    output$classification_lr <- renderPlot({
      req(lr_eval()$roc_obj)
      plot(lr_eval()$roc_obj, main = paste("ROC Curve for Logistic Regression (AUC =", round(lr_eval()$roc_obj$auc, 3), ")"))
      abline(a = 0, b = 1, lty = 2, col = "gray")
    })

    output$lr_summary <- renderPrint({
      req(lr_model())
      summary(lr_model())
    })

    # SVM
    svm_params <- reactiveVal(list())

    observe({
      params <- list(
        kernel = input$svm_kernel,
        hyperparameter_tuning = input$hyperparameter_tuning,
        cost_range = input$cost_range
      )
      
      if (input$svm_kernel == "polynomial") {
        params$degree_range <- input$degree_range
      }
      
      if (input$svm_kernel %in% c("radial", "sigmoid")) {
        params$gamma_range <- input$gamma_range
      }
      
      svm_params(params)
    })

    svm_model <- reactive({
      req(input$train_svm)
      params <- svm_params()
      req(params$kernel, params$hyperparameter_tuning)
      
      # Get the data and target column
      df <- data()
      target_col <- input$target_column
      
      # Apply feature selection
      selected_features <- input$feature_columns
      df <- df[, c(target_col, selected_features)]
      
      # Ensure the target column is a factor
      df[[target_col]] <- as.factor(df[[target_col]])
      
      set.seed(2001)
      train_index <- createDataPartition(df[[target_col]], p = input$trainsplit, list = FALSE)
      train_data <- df[train_index, ]
      test_data <- df[-train_index, ]
      
      # Handle class imbalance
      if (input$radio == 1) {  # Over Sampling
        train_data <- upSample(x = train_data[, -which(names(train_data) == target_col)],
                              y = train_data[[target_col]],
                              yname = target_col)
      } else if (input$radio == 2) {  # Under Sampling
        train_data <- downSample(x = train_data[, -which(names(train_data) == target_col)],
                                y = train_data[[target_col]],
                                yname = target_col)
      }
      
      # Create the formula using only selected features
      formula <- as.formula(paste(target_col, "~", paste(selected_features, collapse = " + ")))
      
      params <- svm_params()
      
      if (params$hyperparameter_tuning == "grid") {
        # Perform grid search
        cost_range <- 10^seq(params$cost_range[1], params$cost_range[2], length.out = 5)
        
        if (params$kernel == "polynomial") {
          degree_range <- seq(params$degree_range[1], params$degree_range[2], by = 1)
          param_grid <- expand.grid(cost = cost_range, degree = degree_range)
        } else if (params$kernel %in% c("radial", "sigmoid")) {
          gamma_range <- 10^seq(params$gamma_range[1], params$gamma_range[2], length.out = 5)
          param_grid <- expand.grid(cost = cost_range, gamma = gamma_range)
        } else {
          param_grid <- data.frame(cost = cost_range)
        }
        
        # Function to train SVM and return accuracy
        train_and_evaluate <- function(cost, degree = NULL, gamma = NULL) {
          svm_params <- list(formula = formula, data = train_data, kernel = params$kernel, cost = cost)
          if (!is.null(degree)) svm_params$degree <- degree
          if (!is.null(gamma)) svm_params$gamma <- gamma
          
          model <- do.call(e1071::svm, svm_params)
          predictions <- predict(model, newdata = train_data)
          conf_matrix <- table(train_data[[target_col]], predictions)
          accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
          return(accuracy)
        }
        
        # Perform grid search
        grid_results <- apply(param_grid, 1, function(params) {
          do.call(train_and_evaluate, as.list(params))
        })
        
        best_params <- param_grid[which.max(grid_results), ]
        
        # Train final model with best parameters
        svm_params <- c(list(formula = formula, data = train_data, kernel = params$kernel), best_params)
        model <- do.call(e1071::svm, svm_params)
        
      } else {
        # Use default parameters
        model <- e1071::svm(formula, data = train_data, kernel = params$kernel)
      }
      
      return(model)
    })

    # Reactive expression for model evaluation
    model_eval <- reactive({
      req(svm_model())
      model <- svm_model()
      
      df <- data()
      target_col <- input$target_column
      
      # Apply feature selection to test data
      selected_features <- input$feature_columns
      df <- df[, c(target_col, selected_features)]
      
      set.seed(2001)
      train_index <- createDataPartition(df[[target_col]], p = input$trainsplit, list = FALSE)
      test_data <- df[-train_index, ]
      
      # Make predictions on test data
      predictions <- predict(model, newdata = test_data)
      
      # Calculate metrics
      cm <- confusionMatrix(predictions, as.factor(test_data[[target_col]]))
      precision <- cm$byClass["Precision"]
      recall <- cm$byClass["Recall"]
      f1_score <- cm$byClass["F1"]
      
      # ROC curve data
      roc_obj <- roc(as.numeric(as.factor(test_data[[target_col]])) - 1, as.numeric(as.factor(predictions)) - 1)
      
      list(
        predictions = predictions, 
        test_data = test_data, 
        precision = precision, 
        recall = recall, 
        f1_score = f1_score,
        roc_obj = roc_obj
      )
    })

    # Outputs
    output$Acc_SVM <- renderText({ 
      req(model_eval())
      sprintf("%.2f", model_eval()$precision) 
    })

    output$Rec_SVM <- renderText({ 
      req(model_eval())
      sprintf("%.2f", model_eval()$recall) 
    })

    output$f_score_SVM <- renderText({ 
      req(model_eval())
      sprintf("%.2f", model_eval()$f1_score) 
    })

    output$classification_svm <- renderPlot({
      req(model_eval()$roc_obj)
      plot(model_eval()$roc_obj, main = paste("ROC Curve for SVM (AUC =", round(model_eval()$roc_obj$auc, 3), ")"))
      abline(a = 0, b = 1, lty = 2, col = "gray")
    })

    output$svm_summary <- renderPrint({
      req(svm_model())
      summary(svm_model())
    })





    # Decision Tree
    dt_params <- reactiveVal(list())

    observe({
      params <- list(
        hyperparameter_tuning = input$hyperparameter_tuning,
        cp = input$dt_cp,
        minsplit = input$dt_minsplit
      )
      
      dt_params(params)
    })

    dt_model <- reactive({
      req(input$train_dt)
      params <- dt_params()
      req(params$hyperparameter_tuning)
      
      # Get the data and target column
      df <- data()
      target_col <- input$target_column
      
      # Apply feature selection
      selected_features <- input$feature_columns
      df <- df[, c(target_col, selected_features)]
      
      # Ensure the target column is a factor
      df[[target_col]] <- as.factor(df[[target_col]])
      
      set.seed(123)
      train_index <- createDataPartition(df[[target_col]], p = input$trainsplit, list = FALSE)
      train_data <- df[train_index, ]
      test_data <- df[-train_index, ]
      
      # Handle class imbalance
      if (input$radio == 1) {  # Over Sampling
        train_data <- upSample(x = train_data[, -which(names(train_data) == target_col)],
                              y = train_data[[target_col]],
                              yname = target_col)
      } else if (input$radio == 2) {  # Under Sampling
        train_data <- downSample(x = train_data[, -which(names(train_data) == target_col)],
                                y = train_data[[target_col]],
                                yname = target_col)
      }
      
      # Create the formula using only selected features
      formula <- as.formula(paste(target_col, "~", paste(selected_features, collapse = " + ")))
      
      if (params$hyperparameter_tuning == "grid") {
        # Perform grid search
        cp_range <- seq(params$cp[1], params$cp[2], length.out = 10)
        minsplit_range <- seq(params$minsplit[1], params$minsplit[2], by = 5)
        
        param_grid <- expand.grid(cp = cp_range, minsplit = minsplit_range)
        
        # Function to train Decision Tree and return accuracy
        train_and_evaluate <- function(cp, minsplit) {
          model <- rpart(formula, data = train_data, method = "class",
                        control = rpart.control(cp = cp, minsplit = minsplit))
          predictions <- predict(model, newdata = train_data, type = "class")
          conf_matrix <- table(train_data[[target_col]], predictions)
          accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
          return(accuracy)
        }
        
        # Perform grid search
        grid_results <- apply(param_grid, 1, function(params) {
          do.call(train_and_evaluate, as.list(params))
        })
        
        best_params <- param_grid[which.max(grid_results), ]
        
        # Train final model with best parameters
        model <- rpart(formula, data = train_data, method = "class",
                      control = rpart.control(cp = best_params$cp, minsplit = best_params$minsplit))
        
      } else {
        # Use default parameters
        model <- rpart(formula, data = train_data, method = "class")
      }
      
      return(model)
    })

    # Reactive expression for model evaluation
    dt_eval <- reactive({
      req(dt_model())
      model <- dt_model()
      
      df <- data()
      target_col <- input$target_column
      
      # Apply feature selection to test data
      selected_features <- input$feature_columns
      df <- df[, c(target_col, selected_features)]
      
      set.seed(123)
      train_index <- createDataPartition(df[[target_col]], p = input$trainsplit, list = FALSE)
      test_data <- df[-train_index, ]
      
      # Make predictions on test data
      predictions <- predict(model, newdata = test_data, type = "class")
      
      # Calculate metrics
      cm <- confusionMatrix(predictions, as.factor(test_data[[target_col]]))
      precision <- cm$byClass["Precision"]
      recall <- cm$byClass["Recall"]
      f1_score <- cm$byClass["F1"]
      
      # ROC curve data
      roc_obj <- roc(as.numeric(as.factor(test_data[[target_col]])) - 1, 
                    as.numeric(predict(model, newdata = test_data, type = "prob")[,2]))
      
      list(
        predictions = predictions, 
        test_data = test_data, 
        precision = precision, 
        recall = recall, 
        f1_score = f1_score,
        roc_obj = roc_obj,
        model = model
      )
    })
    # Outputs
    output$Acc_DT <- renderText({ 
      req(dt_eval())
      sprintf("%.2f", dt_eval()$precision) 
    })

    output$Rec_DT <- renderText({ 
      req(dt_eval())
      sprintf("%.2f", dt_eval()$recall) 
    })

    output$f_score_DT <- renderText({ 
      req(dt_eval())
      sprintf("%.2f", dt_eval()$f1_score) 
    })

    output$Classification_DT <- renderPlot({
      req(dt_eval()$roc_obj)
      plot(dt_eval()$roc_obj, main = paste("ROC Curve for Decision Tree (AUC =", round(dt_eval()$roc_obj$auc, 3), ")"))
      abline(a = 0, b = 1, lty = 2, col = "gray")
    })

    output$DT_tree <- renderPlot({
      req(dt_eval()$model)
      rpart.plot(dt_eval()$model, 
                box.palette = "auto", 
                main = "Decision Tree")
    })
    ##############################

    ###############################
    ############################### FONCTIONS GENERALES
    get_quali <- reactive({
      dataset = pretraitement()
      is.fact <- sapply(dataset, is.factor)
      quali = dataset[,is.fact]
      as.data.frame(quali)
    })
    
    get_quanti <- reactive({
      dataset = pretraitement()
      is.fact <- sapply(dataset, is.factor)
      quanti = dataset[, !is.fact]
      quanti$Attrition = dataset$Attrition
      as.data.frame(quanti)
      
    })
    
    
    
    # Statistical Tests
    
    output$cols7<- renderUI({
      selectInput('cols7', 'Choisir une variable ',names(data())[!grepl('factor|logical|character',sapply(data(),class))])
      
    })
    output$cols8<- renderUI({
      selectInput('cols8', 'Choisir une variable ',names(data())[!grepl('factor|logical|character',sapply(data(),class))])
      
    })
    
    
    output$qqp <- renderPlot({
      df <- data()
      qqnorm(df[, input$cols7]);qqline(df[, input$cols7])
    })
    
    adt <- reactive({
      df <- data()
      var <- df[, input$cols7]
      ad <- ad.test(var)
      return(ad)
    })
    
    sht <- reactive({
      df <- data()
      var <- df[, input$cols7]
      sh <- shapiro.test(var)
      return(sh)
    })
    
    kst <- reactive({
      df <- data()
      var1 <- df[, input$cols7]
      var2 <- df[, input$cols8]
      ks <- ks.test(var1, var2)
      return(ks)
    })
    
    mvst <- reactive({
      df <- data()
      var1 <- df[, input$cols7]
      var2 <- df[, input$cols8]
      return(mshapiro.test(t(as.data.frame(var1, var2))))
    })
    
    output$normtest <- renderPrint({
      
      if(input$normaltest == "A-D-Test"){
        print(adt())
      } else if(input$normaltest == "Shapiro"){
        print(sht())
      } else if(input$normaltest == "KS-Test"){
        print(kst())
      } else if(input$normaltest == "MV-Shapiro"){
        print(mvst())
      }
      
    }
    
    )
    # correlation & regression 
    
    output$cols9<- renderUI({
      selectInput('cols9', 'Choisir une variable ',names(data())[!grepl('factor|logical|character',sapply(data(),class))])
      
    })
    output$cols10<- renderUI({
      selectInput('cols10', 'Choisir une variable ',names(data())[!grepl('factor|logical|character',sapply(data(),class))])
      
    })
    
    
    cortest <- reactive({
      var1 <- data()[,input$cols9]
      var2 <- data()[,input$cols10]
      
      if (input$cormethod == "Covariance"){
        return(cov(var1, var2))
      } else if (input$cormethod == "KarlPearson"){
        return(cor.test(var1, var2, method = "pearson"))
      } else if(input$cormethod == "Spearman"){
        return(cor.test(var1, var2, method="spearman"))
      } else {
        return(cor.test(var1, var2, method="kendall"))
      }
    }
    )
    
    output$cor_t <- renderPrint({
      
      cortest()
    })
    
    output$rapport <- renderUI({
      HTML(
        paste(
          "<div style='font-family: Arial, sans-serif; color: #2c3e50;'>",
          
          # Title with animation and color
          "<h2 style='color: #2e86c1; text-align: center; font-weight: bold; animation: fadeIn 1s ease-in;'>",
          "🧠 Dashboard d'Analyse de Données Interactif", "</h2>",
          "<p style='text-align: center; font-size: 1.2em;'>",
          "Développé avec R et Shiny pour l'exploration, le prétraitement et la visualisation de données, ",
          "ainsi que pour la création de modèles d'apprentissage automatique.", "</p>",
          
          # Add decorative separator with icon
          "<hr style='border: none; border-top: 3px double #2e86c1; text-align: center; width: 80%;'>",
          "<p style='text-align: center;'><i class='fas fa-database' style='font-size: 2em; color: #add8e6;'></i></p>",
          
          # Section 1 - Importation de fichiers
          "<h3 style='color: #2980b9;'><i class='fas fa-upload'></i> 1. Importation de Fichiers</h3>",
          "<p>Importez facilement des fichiers aux formats variés (csv, txt, xlsx, data...) avec plusieurs options de séparateurs : virgule, espace, tabulation, point, point-virgule.</p>",
          
          # Section 2 - Dataset
          "<h3 style='color: #2980b9;'><i class='fas fa-table'></i> 2. Aperçu du Dataset</h3>",
          "<p>Affichez les données sous forme de table, incluant le nombre de lignes et de colonnes.</p>",
          
          # Add decorative separator with icon
          "<hr style='border: none; border-top: 2px dashed #2e86c1;'>",
          "<p style='text-align: center;'><i class='fas fa-wrench' style='font-size: 2em; color: #add8e6;'></i></p>",
          
          # Section 3 - Prétraitement des données
          "<h3 style='color: #2980b9;'><i class='fas fa-cogs'></i> 3. Prétraitement des Données</h3>",
          "<p>Appliquez des méthodes de traitement pour rendre les données plus fiables et prêtes à être utilisées dans des modèles d'apprentissage automatique.</p>",
          "<ul style='line-height: 1.5;'>",
          "<li><b>Valeurs aberrantes :</b> Identifiez et décidez de les conserver ou de les supprimer.</li>",
          "<li><b>Valeurs manquantes :</b> Remplacez les valeurs manquantes par la moyenne, la médiane, ou le mode, selon le type de données (quantitatives/qualitatives).</li>",
          "<li><b>Normalisation :</b> Appliquez des méthodes comme Min-Max ou Z-Score.</li>",
          "<li><b>Dummification :</b> Convertissez les variables catégorielles en variables numériques.</li>",
          "<li><b>Rééquilibrage des classes :</b> Over-sampling ou Under-sampling.</li>",
          "</ul>",
          
          # Section 4 - Exploration des données
          "<h3 style='color: #2980b9;'><i class='fas fa-chart-line'></i> 4. Exploration des Données</h3>",
          "<p>Obtenez un résumé détaillé des données et une heatmap des corrélations entre variables.</p>",
          
          # Section 5 - Tests Statistiques
          "<h3 style='color: #2980b9;'><i class='fas fa-calculator'></i> 5. Tests Statistiques</h3>",
          "<p>Effectuez des tests statistiques tels que Shapiro-Wilk, KS-Test, et calculez des corrélations (Pearson, Spearman, etc.).</p>",
          
          # Add decorative separator with icon
          "<hr style='border: none; border-top: 2px dashed #2e86c1;'>",
          "<p style='text-align: center;'><i class='fas fa-chart-pie' style='font-size: 2em; color: #add8e6;'></i></p>",
          
          # Section 6 - Analyse Univariée et Bivariée
          "<h3 style='color: #2980b9;'><i class='fas fa-chart-bar'></i> 6. Analyse Univariée & Bivariée</h3>",
          "<p>Analysez les variables quantitatives et qualitatives avec des outils visuels comme les histogrammes, boîtes à moustaches, diagrammes en secteurs, etc.</p>",
          
          # Section 7 - Machine Learning
          "<h3 style='color: #2980b9;'><i class='fas fa-robot'></i> 7. Apprentissage Automatique</h3>",
          "<p>Appliquez des modèles de Machine Learning (Random Forest, SVM, KNN, etc.) sur des datasets tels que Employee Attrition. Comparez les performances avec des métriques comme la précision, le recall et le F-score.</p>",
          
          # Section 8 - Sélection de Caractéristiques
          "<h3 style='color: #2980b9;'><i class='fas fa-filter'></i> 8. Sélection de Caractéristiques</h3>",
          "<p>Identifiez les variables les plus importantes dans vos modèles de classification pour améliorer les performances.</p>",
          
          # Add decorative separator with icon
          "<hr style='border: none; border-top: 3px double #2e86c1; text-align: center; width: 80%;'>",
          
          # Closing remarks
          "<h4 style='text-align: center;'>Merci d'utiliser cet outil interactif pour vos analyses de données !</h4>",
          
          "</div>"
        )
      )
    })
    
    
    
  })
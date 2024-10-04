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
library(PRROC)
library(rpart.plot)
#library("ElemStatLearn")
library(dplyr)
library(ggfortify)
library(survminer)
library(psych)
library(factoextra)
library(FactoMineR)
library(summarytools)

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
                 menuSubItem("Informaions données", tabName = "f", icon = icon("list-alt")),
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
                 menuSubItem("Qualitative VS Qualitative", tabName = "vs2", icon=icon("chart-pie")),
                 menuSubItem("Tests statistiques", tabName = "stest", icon=icon("calculator"))
        ),
        menuItem("Analyse Multivariée", tabName = "exp", icon = icon("search"),
                 menuSubItem("Matrice de Corrélation", tabName = "cr", icon=icon("project-diagram")),
                 menuSubItem("Cercle de Corrélation", tabName = "crce", icon=icon("circle"))
        ),
        menuItem("Modeles", tabName="ml", icon=icon("tasks"))
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
        tabItem(tabName = "stest",
                fluidPage(
                  tabPanel("Statistical Tests", 
                           sidebarLayout(
                             sidebarPanel(
                               uiOutput('cols7'),
                               uiOutput('cols8'),
                               radioButtons("normaltest", "Select Method:", choices = c("A-D-Test", "Shapiro", "KS-Test", "MV-Shapiro")),
                               hr(),
                               helpText("For more details visit:"),
                               a(href="https://en.wikipedia.org/wiki/Anderson%E2%80%93Darling_test", "Anderson-Darling test"), br(),
                               a(href="https://en.wikipedia.org/wiki/Shapiro%E2%80%93Wilk_test", "Shapiro-Wilk test"), br(),
                               a(href="https://en.wikipedia.org/wiki/Kolmogorov%E2%80%93Smirnov_test", "Kolmogorov-Smirnov test"), br(),
                               
                               hr()
                             ), 
                             mainPanel(
                               h3("Statistical Tests"),
                               fluidRow(
                                 div(
                                   plotOutput("qqp")
                                 ),
                                 div(
                                   verbatimTextOutput("normtest")
                                 )
                               )
                             )
                             
                           )
                  ),
                  
                  tabPanel("Correlation", 
                           sidebarLayout(
                             sidebarPanel(
                               uiOutput('cols9'),
                               uiOutput('cols10'),
                               radioButtons("cormethod", "Select Method:", choices = c("Covariance", "KarlPearson", "Spearman", "Kendals")),
                               hr(),
                               helpText("For Details Visit:"),
                               a(href="https://en.wikipedia.org/wiki/Spearman%27s_rank_correlation_coefficient", "Karl Pearson Correlation Test"),
                               hr()
                             ), 
                             mainPanel(
                               h3("Covariance & Correlation"),
                               verbatimTextOutput("cor_t")
                             )
                             
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
                             tableOutput("caract_quantitative"),
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
                                       tags$head(tags$style("#Theatmap{color:  red;
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
          fluidRow(  
            column( 
              width = 12,
              tabBox(
                width = 12,
                title = "Variables",
                id = "tabset1", 
                tabPanel("Quantitative", value = "tab_quantitative",
                        uiOutput('quantlist'),
                        verbatimTextOutput(outputId = "summary")
                ),
                tabPanel("Qualitative", value = "tab_qualitative",
                        uiOutput('qualist'))
              )
            ),
            conditionalPanel(
              condition = "input.tabset1 == 'tab_quantitative'",
              column(
                width = 12,
                box(
                  title = "Diagramme en bâtons des effectifs", status = "primary", solidHeader = TRUE,
                  collapsible = TRUE,
                  plotOutput(outputId = "effectifsDiag")
                ),
                box(
                  title = "Boîte à moustaches", status = "primary", solidHeader = TRUE,
                  collapsible = TRUE,
                  plotOutput(outputId = "boiteMoustaches")
                )
              ),
              column(
                width = 12,
                box(
                  title = "Histogramme d'effectifs", status = "info", solidHeader = TRUE,
                  collapsible = TRUE,
                  plotOutput(outputId = "effectifsHist")
                ),
                box(
                  title = "Caractéristiques de tendance centrale et de dispersion", status = "info", solidHeader = TRUE,
                  collapsible = TRUE,
                  tableOutput(outputId = "centreDisp")
                )
              ),
              column(
                width = 12,
                box(
                  width = 12,
                  title = "Courbe cumulative", status = "info", solidHeader = TRUE,
                  collapsible = TRUE,
                  plotOutput(outputId = "effectifsCumCurve")
                )
              ),
              # New plots for quantitative variables
              column(
                width = 12,
                box(
                  title = "Diagramme Quantile-Quantile", status = "success", solidHeader = TRUE,
                  collapsible = TRUE,
                  plotOutput(outputId = "qqPlot")
                ),
                box(
                  title = "Graphique de densité", status = "success", solidHeader = TRUE,
                  collapsible = TRUE,
                  plotOutput(outputId = "densityPlot")
                )
              ),
              column(
                width = 12,
                box(
                  title = "Diagramme en violon", status = "warning", solidHeader = TRUE,
                  collapsible = TRUE,
                  plotOutput(outputId = "violinPlot")
                ),
                box(
                  title = "Nuage de points", status = "warning", solidHeader = TRUE,
                  collapsible = TRUE,
                  plotOutput(outputId = "dotPlot")
                )
              ),
              column(
                width = 12,
                box(
                  title = "Diagramme tige-et-feuille", status = "danger", solidHeader = TRUE,
                  collapsible = TRUE,
                  verbatimTextOutput(outputId = "stemLeafPlot")
                ),
                box(
                  title = "Graphique ECDF", status = "danger", solidHeader = TRUE,
                  collapsible = TRUE,
                  plotOutput(outputId = "ecdfPlot")
                )
              ),
              column(
                width = 12,
                box(
                  title = "Graphique en peigne", status = "primary", solidHeader = TRUE,
                  collapsible = TRUE,
                  plotOutput(outputId = "rugPlot")
                ),
                box(
                  title = "Graphique en bandes", status = "primary", solidHeader = TRUE,
                  collapsible = TRUE,
                  plotOutput(outputId = "stripPlot")
                )
              ),
              column(
                width = 12,
                box(
                  width = 12,
                  title = "Nuage de points (Scatter Plot)", status = "info", solidHeader = TRUE,
                  collapsible = TRUE,
                  plotOutput(outputId = "scatterPlot")
                )
              )
            ),
            conditionalPanel(
              condition = "input.tabset1 == 'tab_qualitative'",
              column(
                width = 12,
                box(
                  title = "Diagramme en barres", status = "success", solidHeader = TRUE,
                  collapsible = TRUE,
                  plotOutput(outputId = "barPlot")
                ),
                box(
                  title = "Diagramme circulaire", status = "success", solidHeader = TRUE,
                  collapsible = TRUE,
                  plotOutput(outputId = "pieChart")
                )
              ),
              column(
                width = 12,
                box(
                  title = "Graphique en mosaïque", status = "warning", solidHeader = TRUE,
                  collapsible = TRUE,
                  plotOutput(outputId = "mosaicPlot")
                ),
                box(
                  title = "Diagramme en barres empilées", status = "warning", solidHeader = TRUE,
                  collapsible = TRUE,
                  plotOutput(outputId = "stackedBarPlot")
                )
              ),
              column(
                width = 12,
                box(
                  width = 12,
                  title = "Diagramme de Pareto", status = "danger", solidHeader = TRUE,
                  collapsible = TRUE,
                  plotOutput(outputId = "paretoChart")
                )
              )
            ),
            uiOutput("myConditionalPanel")
          )
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
                sliderInput("trainsplit", "Portion training:",
                            min = 0.05, max = 0.95,
                            value = 0.7, step = 0.05
                ),
                h3("Choisir les parametres d'entrainement"),
                radioButtons("radio",
                             label="Selectionnez",
                             choices = list("Over Sampling " = 1, "Under Sampling " = 2,"Sans Sampling" = 3),
                             selected = 1,
                             inline = T,
                             width = "100%"),
                tabPanel("Classification",
                         tabsetPanel(
                           tabPanel("Random Forest", h1("Random Forest"),fluidRow(column(2, h4("Precison: ")), column(2, textOutput("Acc_RF"))), fluidRow(column(2, h4("Recall: ")), column(2, textOutput("Rec_RF"))),fluidRow(column(2, h4("F1_score:")), column(2, textOutput("f_score_RF"))),plotOutput("Classification_rf")),
                           tabPanel("Logistic Regression", h1("Logistic Regression"), fluidRow(column(2, h4("Precison: ")), column(2, textOutput("Acc_LR"))), fluidRow(column(2, h4("Recall: ")), column(2, textOutput("Rec_LR"))),fluidRow(column(2, h4("F1_score:")), column(2, textOutput("f_score_LR"))),plotOutput("classification_lr")),
                           tabPanel("Decision Tree", h1("Decision Tree"),fluidRow(column(2, h4("Precison: ")), column(2, textOutput("Acc_DT"))), fluidRow(column(2, h4("Recall: ")), column(2, textOutput("Rec_DT"))),fluidRow(column(2, h4("F1_score:")), column(2, textOutput("f_score_DT"))),fluidRow(column(4, h4("Arbre de décision :")), column(12, plotOutput("DT_tree"))), h4("Courbe PR:"),plotOutput("Classification_DT")),
                           tabPanel("SVM", h1("SVM"), fluidRow(column(2, h4("Precison: ")), column(2, textOutput("Acc_SVM"))), fluidRow(column(2, h4("Recall: ")), column(2, textOutput("Rec_SVM"))),fluidRow(column(2, h4("F1_score:")), column(2, textOutput("f_score_SVM"))),plotOutput("classification_svm")),
                           tabPanel("KNN", h1("KNN"), fluidRow(column(2, h4("Precison: ")), column(2, textOutput("Acc_KNN"))), fluidRow(column(2, h4("Recall: ")), column(2, textOutput("Rec_KNN"))),fluidRow(column(2, h4("F1_score:")), column(2, textOutput("f_score_KNN"))),plotOutput("classification_knn")),
                           tabPanel("LDA", h1("LDA"), fluidRow(column(2, h4("Precison: ")), column(2, textOutput("Acc_LDA"))), fluidRow(column(2, h4("Recall: ")), column(2, textOutput("Rec_LDA"))),fluidRow(column(2, h4("F1_score:")), column(2, textOutput("f_score_LDA"))),plotOutput("classification_lda")),
                           tabPanel("Naive Bayes", h1("NB"), fluidRow(column(2, h4("Precison: ")), column(2, textOutput("Acc_NB"))), fluidRow(column(2, h4("Recall: ")), column(2, textOutput("Rec_NB"))),fluidRow(column(2, h4("F1_score:")), column(2, textOutput("f_score_NB"))),plotOutput("classification_nb"))
                           
                           
                         )
                         
                ))     
              
      ),
      
      tabItem(
        tabName = "classification",
        fluidRow(
          box(background = "black", textOutput(("ModName")))),
        fluidRow(
          
          selectInput(inputId = "choice_model", label ="Choose a Model", choices = list("Logistic Regression",
                                                                                        "LDA", "Naive Bayes", "KNN", "Support Vector Machine", "Decision Tree"), selected = "Logistic
                      Regression", multiple = FALSE),
          
          conditionalPanel(condition = "input.choice_model == 'Support Vector Machine'",
                           selectInput(inputId = "choice_type", label ="Choose a Machine Type", choices = list("C-classification",
                                                                                                               "nu-classification"), selected = "C-classification", multiple = FALSE),
                           selectInput(inputId = "choice_kernel", label ="Choose a Kernel Type", choices = list("linear",
                                                                                                                "polynomial", "radial", "sigmoid"), selected = "polynomial", multiple = FALSE)),
          conditionalPanel(condition = "input.choice_model == 'Decision Tree'",
                           sliderInput(inputId = "choice_threshold", label ="Choose Test Statistic Threshold", min = 0.90, max =
                                         0.99, value = 0.95, step = 0.01),
                           sliderInput(inputId = "choice_depth", label ="Choose Max Tree Depth", min = 1, max = 30, value = 6,
                                       step = 1),
                           sliderInput(inputId = "choice_split", label ="Choose Min Threshold for Splitting", min = 10, max = 200,
                                       value = 20, step = 1)),
          
          
          conditionalPanel(condition = "input.choice_model == 'Logistic Regression'",
                           box(title = "Summary Output", solidHeader = TRUE, background = "aqua", status = "primary",
                               verbatimTextOutput("ModSummaryLR"), width = 8),
                           box(title = "Feature Selection Parameters", solidHeader = TRUE, background = "olive", status =
                                 "primary", checkboxGroupInput(inputId = "choice_indvar", label ="Choose Independant Variables", 
                                                               choices = list( "Distance From Home" = "DistanceFromHome",  "Monthly Income" = "MonthlyIncome", "Number of Companies Worked" =
                                                                                 "NumCompaniesWorked",  "Training Times Last Year" = "TrainingTimesLastYear", "Years Since Last Promotion" = "YearsSinceLastPromotion", "Years With Current
                                                                               Manager" = "YearsWithCurrManager", 
                                                                               "Department.Sales" = " Department.Sales",
                                                                               "Education.College" = "Education.College", "Education.Bachelor" = "Education.Bachelor",
                                                                               "Education.Master" = "Education.Master", "Education.Doctor" = "Education.Doctor",
                                                                               "EducationField.Marketing" =
                                                                                 "EducationField.Marketing",
                                                                               "EducationField.Other" = "EducationField.Other", "Gender.Male" = "Gender.Male", "MaritalStatus.Married" =
                                                                                 "MaritalStatus.Married", "MaritalStatus.Single" = "MaritalStatus.Single"), selected = list( "Distance From Home" = "DistanceFromHome",
                                                                                                                                                                             "Monthly Income" = "MonthlyIncome", "Number of Companies
                                                                                                                                                                             Worked" = "NumCompaniesWorked", "Percent Salary Hike" = "PercentSalaryHike",  "Total Working
                                                                                                                                                                             Years" = "TotalWorkingYears", "Training Times Last Year" = "TrainingTimesLastYear",  "Years At Company" = "YearsAtCompany", "Years In Current Role" =
                                                                                                                                                                               "YearsInCurrentRole", "Years Since Last Promotion" = "YearsSinceLastPromotion", "Years With Current
                                                                                                                                                                             Manager" = "YearsWithCurrManager",
                                                                                                                                                                             "Department.Sales" = " Department.Sales",
                                                                                                                                                                             "Education.College" = "Education.College", "Education.Bachelor" = "Education.Bachelor",
                                                                                                                                                                             "Education.Master" = "Education.Master", "Education.Doctor" = "Education.Doctor",
                                                                                                                                                                             "EducationField.Marketing" =
                                                                                                                                                                               "EducationField.Marketing", "EducationField.Medical" = "EducationField.Medical",
                                                                                                                                                                             "EducationField.Other" = "EducationField.Other", "Gender.Male" = "Gender.Male", "MaritalStatus.Married" =
                                                                                                                                                                               "MaritalStatus.Married", "MaritalStatus.Single" = "MaritalStatus.Single")), width = 4)),
          conditionalPanel(condition = "input.choice_model == 'LDA'",
                           box(title = "Summary Output", solidHeader = TRUE, background = "aqua", status = "primary",
                               verbatimTextOutput("ModSummaryLDA"), width = 12)),
          conditionalPanel(condition = "input.choice_model == 'Naive Bayes'",
                           box(title = "Summary Output", solidHeader = TRUE, background = "aqua", status = "primary",
                               verbatimTextOutput("ModSummaryNB"), width = 12)),
          conditionalPanel(condition = "input.choice_model == 'KNN'",
                           box(title = "Summary Output", solidHeader = TRUE, background = "aqua", status = "primary",
                               verbatimTextOutput("ModSummaryKNN"), width = 12)),
          conditionalPanel(condition = "input.choice_model == 'Support Vector Machine'",
                           box(title = "Summary Output", solidHeader = TRUE, background = "aqua", status = "primary",
                               verbatimTextOutput("ModSummarySVM"), width = 12)),
          conditionalPanel(condition = "input.choice_model == 'Decision Tree'",
                           box(title = "Decision Tree Chart", solidHeader = TRUE, background = "aqua", status = "primary",
                               plotOutput("DTREE"), width = 12))
          ))
      
      
      
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
        #print("I am here")
        #data(df2)
        
        # #print(input$tab_outliers_rows_selected)
        # print("-----------------------")
        # print(df[input$tab_outliers_rows_selected, ])
        
        #  
        # # print("--------------")
        # # c=as.numeric(rownames(df))
        # # for( a in c){
        # #   print(a)
        # # }
        # index de la ligne a supprimer
        c=as.numeric(rownames(df[input$tab_outliers_rows_selected, ]))
        # #print(input$tab_outliers_rows_selected)
        if (!is.null(input$tab_outliers_rows_selected)) {
          if(length(c)>0){
            df <- df[-input$tab_outliers_rows_selected, ]
            
            
            
            #print("I am here")
            #print(c)
            for(a in c){
              #print("-----start-----")
              #print(a)
              #print('----then---')
              #print(data()[a,])
              #print('----data---')
              #print(data())
              #print(c)
              #tab=data()[-as.numeric(c), ]
              #print('------- after remove---------')
              
              tab=data()[-as.numeric(c), ]
              rownames(tab) <- NULL
              #print(tab)
              
              
              
              
              
              
              
              
              
              
              #data(df2)
              
              
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
          
          # dataset_server(input,output,data())
          # exploration_server(input,output,data())
          # univaree_server(input,output, data())
          # Bivaree_server(input,output, data())
          # Qnt_Qlt_server(input,output, data())
          # Qlt_Qlt_server(input,output, data())
          #Modele_server(input, output, data())
          #df_outliers(df[-as.numeric(input$tab_outliers_rows_selected),])
        }
        
        
        
      }
      
      
      
      # if(length(quantitative()) >0 && input$Out=="Oui"){
      #   df=data()
      #   l=list()
      #   var.names <-c(input$OutList)
      #   #     # Initialisation de la table
      #   #     caract.df <- data.frame()
      #   
      #   # Pour chaque colonne, calcul de min, max, mean et ecart-type
      #   for(strCol in var.names){
      #     
      #     
      #     
      #     
      #     # get IQR
      #     iqr=IQR(df[,strCol],na.rm = TRUE)
      #     first_q=quantile((df[,strCol]),.25)
      #     third_q=quantile((df[,strCol]),.75)
      #     # get threshold values for outliers
      #     Tmin = first_q-(1.5*iqr) 
      #     Tmax = third_q+(1.5*iqr) 
      #     
      #     # print("Tmax")
      #     # print(Tmax)
      #     # print("Tmin")
      #     # print(Tmin)
      #     # print(max(df[,strCol]))
      #     # print(var(df[,strCol]))
      #     value = df[,strCol][df[,strCol] %in% boxplot.stats(df[,strCol])$out]
      #     #nb_out=length(df[,strCol][which(df[,strCol]) < Tmin | (df[,strCol] > Tmax)])
      #     if(length(value)>0){
      #       
      #       #output$outlier=renderText(print(var(df[,strCol])))
      #       #print((var(df[,strCol]))[which(var(df[,strCol]) < Tmin | var(df[,strCol]) > Tmax)])
      #       #print("------")
      #       #print(df[,strCol])
      #       #df[,strCol]=na.approx(df[,strCol])
      #       
      #       #df[,strCol][df[,strCol] %in% value] = median(df[,strCol])
      #       df[,strCol][df[,strCol] %in% value] = NA
      #       df = drop_na(df)
      #       #df[,strCol][which(var(df[,strCol]) < Tmin | var(df[,strCol]) > Tmax)]=na.approx(df[,strCol])
      #       #df2=subset(df,df$strCol >Tmax &  df$strCol < Tmin)
      #        data(df)
      #        dataset_server(input,output,data())
      #        exploration_server(input,output,data())
      #        univaree_server(input,output, data())
      #        Bivaree_server(input,output, data())
      #        Qnt_Qlt_server(input,output, data())
      #        Qlt_Qlt_server(input,output, data())
      #        Modele_server(input, output, data())
      #       #print(var(df[,strCol]))
      #       
      #       
      #     }
      #     else{
      #       print("I am here")
      #     }
      #        
      #     
      #     
      #   }
      #   
      #   
      # }
      
    })
    # output$resultat_Oui<-renderText({
    #   df=data()
    #   var.names <-c(input$OutList)
    #   #caract.df <- data.frame()
    #   #strCol=var.names
    #   #print(strCol)
    #   
    #     
    #     # get IQR
    #     iqr=IQR(df[,input$OutList],na.rm = TRUE)
    #     
    #     first_q=quantile((df[,input$OutList]),nar.rm=TRUE)[2]
    #    
    #     third_q=quantile((df[,input$OutList]),nar.rm=TRUE)[4]
    #     
    #     
    #      # get threshold values for outliers
    #       Tmin = first_q-(1.5*iqr) 
    #       Tmax = third_q+(1.5*iqr) 
    #       print(third_q)
    #     #  nb_out=length(var(df[,strCol])[which(var(df[,strCol]) < Tmin | var(df[,strCol]) > Tmax)])
    #     #  if(nb_out>0){
    #     #    df2<-subset(df,var(df[,strCol]))
    #     #    output$outlier=renderText(print(var(df[,strCol])))
    #     #    print(df2)
    #     #     
    #     #   }
    #   
    #   #print(var.names)
    # })
    # observeEvent(input$Out,{
    #   if (input$Out=="Oui"){
    #        
    #         print("je suis la, rania")
    #         # df=data()
    #         # l=list()
    #         # var.names <-c(input$OutList)
    #         # # Initialisation de la table
    #         # caract.df <- data.frame()
    #         # print(var.names)
    #         
    #         # Pour chaque colonne, calcul de min, max, mean et ecart-type
    #         # for(strCol in var.names){
    #         #   
    #         #   
    #         #   
    #         #   
    #         #   # get IQR
    #         #   iqr=IQR(df[,strCol],na.rm = TRUE)
    #         #   first_q=quantile((df[,strCol]))[2]
    #         #   third_q=quantile((df[,strCol]))[4]
    #         #   # get threshold values for outliers
    #         #   Tmin = first_q-(1.5*iqr) 
    #         #   Tmax = third_q+(1.5*iqr) 
    #         #   
    #         #   
    #         #   nb_out=length(var(df[,strCol])[which(var(df[,strCol]) < Tmin | var(df[,strCol]) > Tmax)])
    #         #   if(nb_out>0){
    #         #     #df2<-subset(df,var(df[,strCol]))
    #         #     #output$outlier=renderText(print(var(df[,strCol])))
    #         #     #print(df2)
    #         #     
    #         #   }
    #         #   
    #         #   
    #         # }
    #         
    #       
    #     
    #   }
    # })
    
    
    
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
    
    
    #RÃ©cupÃ©ration du Dataset selectionnÃ©
    datasetInput <- reactive({
      read_csv("Employe.csv")
    })
    
    
    
    col <- reactive({
      input$variables
    })
    
    #Affichage du dataset spiral
    output$view_spr = output$view <- DT::renderDataTable({
      datasetInput()
    })
    
    pretraitement <- reactive({
      empattr = datasetInput()
      
      empattr$Education = factor(empattr$Education)
      empattr$Education = revalue(empattr$Education, c("1"="Below College", "2"="College", "3"="Bachelor", "4"="Master", "5"="Doctor"))
      
      empattr$EnvironmentSatisfaction = factor(empattr$EnvironmentSatisfaction)
      empattr$EnvironmentSatisfaction = revalue(empattr$EnvironmentSatisfaction, c("1"="Low", "2"="Medium", "3"="High", "4"="Very High"))
      
      empattr$JobInvolvement = factor(empattr$JobInvolvement)
      empattr$JobInvolvement = revalue(empattr$JobInvolvement, c("1"="Low", "2"="Medium", "3"="High", "4"="Very High"))
      
      empattr$JobSatisfaction = factor(empattr$JobSatisfaction)
      empattr$JobSatisfaction = revalue(empattr$JobSatisfaction, c("1"="Low", "2"="Medium", "3"="High", "4"="Very High"))
      
      empattr$PerformanceRating = factor(empattr$PerformanceRating)
      
      empattr$JobLevel = factor(empattr$EnvironmentSatisfaction)
      
      empattr$WorkLifeBalance = factor(empattr$WorkLifeBalance)
      empattr$WorkLifeBalance = revalue(empattr$WorkLifeBalance, c("1"="Bad", "2"="Good", "3"="Better", "4"="Best"))
      
      empattr$Attrition = factor(empattr$Attrition)
      empattr$Attrition = revalue(empattr$Attrition, c("No"="No Leave", "Yes"="Leave"))
      
      empattr$BusinessTravel = factor(empattr$BusinessTravel)
      
      empattr$Department = factor(empattr$Department)
      
      empattr$EducationField = factor(empattr$EducationField)
      
      empattr$Gender = factor(empattr$Gender)
      
      empattr$JobRole = factor(empattr$JobRole)
      
      empattr$JobLevel = factor(empattr$JobLevel)
      
      empattr$MaritalStatus = factor(empattr$MaritalStatus)
      
      empattr$Over18 = factor(empattr$Over18)
      
      empattr = empattr[,which(!(names(empattr) %in% c("EmployeeID", "EmployeeCount", "StandardHours", "Over18")))]
      empattr = na.omit(empattr)
      
      empattr
    })
    
    
    one_hot <- reactive({
      dataset<-pretraitement()
      
      dataset$EmployeeID=NULL
      dmy <- dummyVars(~., data = dataset[-7])
      data_one_cod <- data.frame(predict(dmy, newdata = dataset))
      data_one_cod$Attrition=dataset$Attrition
      
      data_one_cod
      
      
    })
   
    
    
    #The following lines of code generate a texbox for model name
    output$ModName = renderText({
      paste("Features selection for ", input$choice_model, "Algorithm")
    })
    
    indexes = reactive({
      set.seed(1234)
      sample(n,n*(input$choice_validate/100))
    })
    
    trainset = reactive({HR_trandata [indexes(),]})
    
    testset = reactive({HR_trandata [-indexes(),]})
    
    balanced_trainset = reactive({
      
      dat<-one_hot()
      res<-SMOTE(Attrition~., data = dat, perc.over=100)
      res
      #one_hot()
      
      
    })
    svm_type = reactive({
      if(input$choice_type == "nu-classification"){
        paste("nu-classification")
      } else {
        paste("C-classification")
      }
    })
    
    svm_kernel = reactive({
      if(input$choice_kernel == "linear"){
        paste("linear")
      } else if(input$choice_kernel == "radial"){
        paste("radial")
      } else if(input$choice_kernel == "sigmoid"){
        paste("sigmoid")
      } else {
        paste("polynomial")
      }
    })
    
    trainmodel = reactive({
      if(input$choice_model == "Naive Bayes"){
        naiveBayes(as.formula(paste("Attrition~ ", paste0(input$choice_indvar, collapse = "+"))), data =
                     one_hot())
      } else if(input$choice_model == "Support Vector Machine"){
        svm(as.formula(paste("Attrition~ ", paste0(input$choice_indvar, collapse = "+"))), data =
              one_hot(), type= svm_type(), kernel= svm_kernel())
      } else if(input$choice_model == "Decision Tree"){
        ctree(as.formula(paste("Attrition~ ", paste0(input$choice_indvar, collapse = "+"))), data =
                one_hot(), control = ctree_control(mincriterion = input$choice_threshold, maxdepth =
                                                               input$choice_depth, minsplit = input$choice_split))
      } else if(input$choice_model == "LDA"){
        train(as.formula(paste("Attrition~ ", paste0(input$choice_indvar, collapse = "+"))), method = "lda",
              data = one_hot())
      } else if(input$choice_model == "KNN"){
        train(as.formula(paste("Attrition~ ", paste0(input$choice_indvar, collapse = "+"))), method = "knn",
              data = one_hot())
      } else {
        glm(as.formula(paste("Attrition~ ",paste0(input$choice_indvar, collapse = "+"))), data =
              one_hot(), family = "binomial")
      }
    })
    
    #The following lines of code generate the summary output for the model(s)
    output$ModSummaryLR = renderPrint({
      summary(trainmodel())
    })
    output$ModSummaryLDA = renderPrint({
      trainmodel()$finalModel
    })
    
    output$ModSummaryNB = renderPrint({
      trainmodel()
    })
    
    output$ModSummaryKNN = renderPrint({
      trainmodel()
    })
    
    output$ModSummarySVM = renderPrint({
      trainmodel()
    })
    #The following lines of code generate the decision tree plot
    output$DTREE = renderPlot({
      if(input$choice_model == "Decision Tree"){
        plot(trainmodel(), type = "simple")}
    })
    
    
    
    
    
    
    
    
    
    train_Lr <- reactive({
      set.seed(3000)
      dataset =balanced_trainset()
      split=sample.split(dataset$Attrition,SplitRatio = input$trainsplit)
      train=subset(dataset,split==T)
      test=subset(dataset,split==F)
      
      
      
      if (input$radio == 2){
        downSample(train, train$Attrition)
      }
      if (input$radio == 1){
        
        upSample(train, train$Attrition)
        
      }
      
      attLog=glm(Attrition~.,data=train,family = binomial())
      
      
    })
    
    output$Acc_LR <- renderText({
      set.seed(3000)
      dataset = balanced_trainset()
      split=sample.split(dataset$Attrition,SplitRatio = input$trainsplit)
      train=subset(dataset,split==T)
      test=subset(dataset,split==F)
      attLog=train_Lr()
      predGlm=predict(attLog,type="response",newdata=test)
      pred <- ifelse(predGlm < 0.5, 0, 1)
      xtab = table(pred, test$Attrition)
      precision = xtab[1,1]/sum(xtab[,1])
      precision*100
      
      
    })
    output$Rec_LR <- renderText({
      set.seed(3000)
      dataset =balanced_trainset()
      split=sample.split(dataset$Attrition,SplitRatio = input$trainsplit)
      train=subset(dataset,split==T)
      test=subset(dataset,split==F)
      attLog=train_Lr()
      predGlm=predict(attLog,type="response",newdata=test)
      xtab = table(predGlm, test$Attrition)
      (xtab[1,1]/sum(xtab[1,]))*100
      
      
    })
    output$f_score_LR <- renderText({
      set.seed(3000)
      dataset =balanced_trainset()
      dataset=na.omit(dataset)
      split=sample.split(dataset$Attrition,SplitRatio = input$trainsplit)
      train=subset(dataset,split==T)
      test=subset(dataset,split==F)
      attLog=train_Lr()
      predGlm=predict(attLog,type="response",newdata=test)
      pred <- ifelse(predGlm < 0.5, 1, 2)
      
      F1_Score(as.numeric(as.factor(test$Attrition)), pred)*100
    })
    output$classification_lr <- renderPlot({
      set.seed(3000)
      dataset = balanced_trainset()
      split=sample.split(dataset$Attrition,SplitRatio = input$trainsplit)
      train=subset(dataset,split==T)
      test=subset(dataset,split==F)
      attLog=train_Lr()
      predGlm=predict(attLog,type="response",newdata=test)
      
      scores <- data.frame(predGlm,test$Attrition)
      pr <- pr.curve(scores.class0=scores[scores$test.Attrition=="Leave",]$predGlm,
                     scores.class1=scores[scores$test.Attrition=="No Leave",]$predGlm,
                     curve=T)
      
      plot(pr)
    })
    
    
    
    train_KNN <- reactive({
      set.seed(3000)
      dataset = balanced_trainset()
      dataset=na.omit(dataset)
      split=sample.split(dataset$Attrition,SplitRatio = input$trainsplit)
      train=subset(dataset,split==T)
      test=subset(dataset,split==F)
      
      if (input$radio == 2){
        downSample(train, train$Attrition)
      }
      if (input$radio == 1){
        
        upSample(train, train$Attrition)
        
      }
      knn_auto=train(Attrition~., method = "knn", data =train)
      
    })
    
    output$Acc_KNN <- renderText({
      set.seed(3000)
      dataset = balanced_trainset()
      dataset=na.omit(dataset)
      split=sample.split(dataset$Attrition,SplitRatio = input$trainsplit)
      train=subset(dataset,split==T)
      test=subset(dataset,split==F)
      auto_knn=train_KNN()
      predict_autoknn = predict(auto_knn,test)
      xtab = table(predict_autoknn, test$Attrition)
      precision = xtab[1,1]/sum(xtab[,1])
      precision*100
      
      
    })
    output$Rec_KNN<- renderText({
      set.seed(3000)
      dataset =balanced_trainset()
      dataset=na.omit(dataset)
      split=sample.split(dataset$Attrition,SplitRatio = input$trainsplit)
      train=subset(dataset,split==T)
      test=subset(dataset,split==F)
      auto_knn=train_KNN()
      predict_autoknn = predict(auto_knn,test)
      xtab = table(predict_autoknn, test$Attrition)
      (xtab[1,1]/sum(xtab[1,]))*100
      
    })
    output$f_score_KNN <- renderText({
      set.seed(3000)
      dataset =balanced_trainset()
      dataset=na.omit(dataset)
      split=sample.split(dataset$Attrition,SplitRatio = input$trainsplit)
      train=subset(dataset,split==T)
      test=subset(dataset,split==F)
      auto_knn=train_KNN()
      predict_autoknn = predict(auto_knn,test)
      xtab = table(predict_autoknn, test$Attrition)
      precision = xtab[1,1]/sum(xtab[,1])
      recall = xtab[1,1]/sum(xtab[1,])
      (2 * (precision * recall) / (precision + recall))*100
      
      
    })
    output$classification_knn <- renderPlot({
      dataset=balanced_trainset()
      
      split=sample.split(dataset$Attrition,SplitRatio = input$trainsplit)
      train=subset(dataset,split==T)
      test=subset(dataset,split==F)
      auto_knn=train_KNN()
      predict_autoknn = predict(auto_knn,test)
      scores <- data.frame(predict_autoknn,test$Attrition)
      print(scores)
      pr <- pr.curve(scores.class0=scores[scores$test.Attrition=="Leave",]$predict_autoknn,
                     scores.class1=scores[scores$test.Attrition=="No Leave",]$predict_autoknn,
                     curve=T)
      
      plot(pr)
    })
    
    
    
    train_SVM <- reactive({
      set.seed(3000)
      dataset =one_hot()
      split=sample.split(dataset$Attrition,SplitRatio = input$trainsplit)
      train=subset(dataset,split==T)
      test=subset(dataset,split==F)
      
      if (input$radio == 2){
        downSample(train, train$Attrition)
      }
      if (input$radio == 1){
        
        upSample(train, train$Attrition)
        
      }
      svm_model=svm(Attrition~., data =train, type='C-classification',
                    kernel='poly')
      
    })
    
    output$Acc_SVM <- renderText({
      dataset=one_hot()
      split=sample.split(dataset$Attrition,SplitRatio = input$trainsplit)
      train=subset(dataset,split==T)
      test=subset(dataset,split==F)
      auto_svm=train_SVM()
      predict_autosvm = predict(auto_svm, test)
      table(test$Attrition,predict_autosvm)
      xtab = table(predict_autosvm, test$Attrition)
      precision = xtab[1,1]/sum(xtab[,1])
      precision*100
      
    })
    output$f_score_SVM <- renderText({
      dataset=one_hot()
      split=sample.split(dataset$Attrition,SplitRatio = input$trainsplit)
      train=subset(dataset,split==T)
      test=subset(dataset,split==F)
      auto_svm=train_SVM()
      predict_autosvm = predict(auto_svm, test)
      F1_Score(as.numeric(as.factor(test$Attrition)), as.numeric(predict_autosvm))*100
      
      
      
    })
    output$Rec_SVM<- renderText({
      dataset=one_hot()
      split=sample.split(dataset$Attrition,SplitRatio = input$trainsplit)
      train=subset(dataset,split==T)
      test=subset(dataset,split==F)
      auto_svm=train_SVM()
      predict_autosvm = predict(auto_svm, test)
      xtab = table(predict_autosvm, test$Attrition)
      (xtab[1,1]/sum(xtab[1,]))*100
      
      
    })
    output$classification_svm <- renderPlot({
      dataset=one_hot()
      split=sample.split(dataset$Attrition,SplitRatio = input$trainsplit)
      train=subset(dataset,split==T)
      test=subset(dataset,split==F)
      auto_svm=train_SVM()
      predict_autosvm = predict(auto_svm, test)
      scores <- data.frame(predict_autosvm,test$Attrition)
      pr <- pr.curve(scores.class0=scores[scores$test.Attrition=="Leave",]$predict_autosvm,
                     scores.class1=scores[scores$test.Attrition=="No Leave",]$predict_autosvm,
                     curve=T)
      
      plot(pr)
    })
    
    
    
    
    train_LDA <- reactive({
      set.seed(3000)
      dataset = balanced_trainset()
      dataset=dataset[,-c(19,20)]
      dataset=na.omit(dataset)
      split=sample.split(dataset$Attrition,SplitRatio = input$trainsplit)
      train=subset(dataset,split==T)
      test=subset(dataset,split==F)
      
      if (input$radio == 2){
        downSample(train, train$Attrition)
      }
      if (input$radio == 1){
        
        upSample(train, train$Attrition)
        
      }
      auto_lda = train(Attrition~., method = "lda", data =train)
      
    })
    
    output$Acc_LDA <- renderText({
      dataset=balanced_trainset()
      split=sample.split(dataset$Attrition,SplitRatio = input$trainsplit)
      train=subset(dataset,split==T)
      test=subset(dataset,split==F)
      auto_lda=train_LDA()
      predict_autolda =predict(auto_lda, test)
      predicted_autolda = as.factor(predict_autolda)
      xtab = table(predict_autolda, test$Attrition)
      precision = xtab[1,1]/sum(xtab[,1])
      precision*100
      
    })
    output$f_score_LDA <- renderText({
      dataset=balanced_trainset()
      split=sample.split(dataset$Attrition,SplitRatio = input$trainsplit)
      train=subset(dataset,split==T)
      test=subset(dataset,split==F)
      auto_lda=train_LDA()
      predict_autolda = predict(auto_lda, test)
      F1_Score(as.numeric(as.factor(test$Attrition)), as.numeric(predict_autolda))*100
      
      
      
    })
    output$Rec_LDA <- renderText({
      dataset=balanced_trainset()
      split=sample.split(dataset$Attrition,SplitRatio = input$trainsplit)
      train=subset(dataset,split==T)
      test=subset(dataset,split==F)
      auto_lda=train_LDA()
      predict_autolda = predict(auto_lda, test)
      xtab = table(predict_autolda, test$Attrition)
      (xtab[1,1]/sum(xtab[1,]))*100
      
      
      
      
    })
    output$classification_lda <- renderPlot({
      dataset=balanced_trainset()
      split=sample.split(dataset$Attrition,SplitRatio = input$trainsplit)
      train=subset(dataset,split==T)
      test=subset(dataset,split==F)
      auto_lda=train_LDA()
      predict_autolda= predict(auto_lda, test)
      scores <- data.frame(predict_autolda,test$Attrition)
      print(scores)
      pr <- pr.curve(scores.class0=scores[scores$test.Attrition=="Leave",]$predict_autolda,
                     scores.class1=scores[scores$test.Attrition=="No Leave",]$predict_autolda,
                     curve=T)
      
      plot(pr)
    })
    
    
    
    train_NB <- reactive({
      set.seed(3000)
      dataset = balanced_trainset()
      dataset=na.omit(dataset)
      split=sample.split(dataset$Attrition,SplitRatio = input$trainsplit)
      train=subset(dataset,split==T)
      test=subset(dataset,split==F)
      
      if (input$radio == 2){
        downSample(train, train$Attrition)
      }
      if (input$radio == 1){
        
        upSample(train, train$Attrition)
        
      }
      auto_bayes= naiveBayes(Attrition~., data =train)
      
    })
    
    output$Acc_NB <- renderText({
      dataset=balanced_trainset()
      split=sample.split(dataset$Attrition,SplitRatio = input$trainsplit)
      train=subset(dataset,split==T)
      test=subset(dataset,split==F)
      auto_nb=train_NB()
      predict_autonb =predict(auto_nb, test)
      predicted_autonb = as.factor(predict_autonb)
      xtab = table(predict_autonb, test$Attrition)
      precision = xtab[1,1]/sum(xtab[,1])
      precision*100
      
    })
    output$f_score_NB <- renderText({
      dataset=balanced_trainset()
      split=sample.split(dataset$Attrition,SplitRatio = input$trainsplit)
      train=subset(dataset,split==T)
      test=subset(dataset,split==F)
      auto_nb=train_NB()
      predict_autonb = predict(auto_nb, test)
      F1_Score(as.numeric(as.factor(test$Attrition)), as.numeric(predict_autonb))*100
      
      
      
    })
    output$Rec_NB <- renderText({
      dataset=balanced_trainset()
      split=sample.split(dataset$Attrition,SplitRatio = input$trainsplit)
      train=subset(dataset,split==T)
      test=subset(dataset,split==F)
      auto_nb=train_NB()
      predict_autonb = predict(auto_nb, test)
      xtab = table(predict_autonb, test$Attrition)
      (xtab[1,1]/sum(xtab[1,]))*100
      
      
    })
    output$classification_nb <- renderPlot({
      dataset=balanced_trainset()
      split=sample.split(dataset$Attrition,SplitRatio = input$trainsplit)
      train=subset(dataset,split==T)
      test=subset(dataset,split==F)
      auto_nb=train_NB()
      predict_autonb= predict(auto_nb, test,type='class')
      predicted_autonb = as.factor(predict_autonb)
      scores <- data.frame(predict_autonb,test$Attrition)
      pr <- pr.curve(scores.class0=scores[scores$test.Attrition=="Leave",]$predict_autonb,
                     scores.class1=scores[scores$test.Attrition=="No Leave",]$predict_autonb,
                     curve=T)
      
      plot(pr)
    })
    
    
    train_rf<- reactive({
      set.seed(3000)
      dataset =one_hot()
      dataset=na.omit(dataset)
      split=sample.split(dataset$Attrition,SplitRatio = input$trainsplit)
      train=subset(dataset,split==T)
      test=subset(dataset,split==F)
      if (input$radio == 2){
        downSample(train, train$Attrition)
      }
      if (input$radio == 1){
        
        upSample(train,train$Attrition)
        
      }
      randomForestModel=randomForest(as.factor(Attrition)~.,data=train,ntree=100,nodesize=12)
      
    })  
    output$Acc_RF <- renderText({
      dataset=one_hot()
      split=sample.split(dataset$Attrition,SplitRatio = input$trainsplit)
      train=subset(dataset,split==T)
      test=subset(dataset,split==F)
      randomForestModel=train_rf()
      predictRF=predict(randomForestModel,newdata=test)
      xtab = table(predictRF, test$Attrition)
      precision = xtab[1,1]/sum(xtab[,1])
      precision*100
      
    })
    output$Rec_RF <- renderText({
      dataset=one_hot()
      split=sample.split(dataset$Attrition,SplitRatio = input$trainsplit)
      train=subset(dataset,split==T)
      test=subset(dataset,split==F)
      randomForestModel=train_rf()
      predictRF=predict(randomForestModel,newdata=test)
      xtab = table(predictRF, test$Attrition)
      (xtab[1,1]/sum(xtab[1,]))*100
      
      
    })
    output$f_score_RF <- renderText({
      dataset=one_hot()
      split=sample.split(dataset$Attrition,SplitRatio = input$trainsplit)
      train=subset(dataset,split==T)
      test=subset(dataset,split==F)
      randomForestModel=train_rf()
      predictRF=predict(randomForestModel,newdata=test)
      F1_Score(as.numeric(as.factor(test$Attrition)), as.numeric(predictRF))*100
    })
    output$Classification_rf <- renderPlot({
      dataset=one_hot()
      split=sample.split(dataset$Attrition,SplitRatio = input$trainsplit)
      train=subset(dataset,split==T)
      test=subset(dataset,split==F)
      
      #randomForestModel=train_rf()  
      randomForestModel=train_rf()
      predictRF=predict(randomForestModel,newdata=test,type="response")
      
      scores <- data.frame(predictRF,test$Attrition)
      pr <- pr.curve(scores.class0=scores[scores$test.Attrition=="Leave",]$predictRF,
                     scores.class1=scores[scores$test.Attrition=="No Leave",]$predictRF,
                     curve=T)
      
      
      plot(pr)
      
      
    })
    
    train_DT<- reactive({
      set.seed(3000)
      dataset = balanced_trainset()
      dataset=na.omit(dataset)
      split=sample.split(dataset$Attrition,SplitRatio = input$trainsplit)
      train=subset(dataset,split==T)
      test=subset(dataset,split==F)
      if (input$radio == 2){
        downSample(train, train$Attrition)
      }
      if (input$radio == 1){
        
        upSample(train,train$Attrition)
        
      }
      decisionTreeModel= rpart(Attrition~.,data=train,method="class")
    })  
    output$Acc_DT <- renderText({
      dataset=balanced_trainset()
      split=sample.split(dataset$Attrition,SplitRatio = input$trainsplit)
      train=subset(dataset,split==T)
      test=subset(dataset,split==F)
      decisionTreeModel=train_DT()
      predDT=predict(decisionTreeModel,newdata = test,type = "class")
      xtab = table(predDT, test$Attrition)
      precision = xtab[1,1]/sum(xtab[,1])
      precision*100
      
    })
    output$Rec_DT <- renderText({
      dataset=balanced_trainset()
      split=sample.split(dataset$Attrition,SplitRatio = input$trainsplit)
      train=subset(dataset,split==T)
      test=subset(dataset,split==F)
      decisionTreeModel=train_DT()
      predDT=predict(decisionTreeModel,newdata = test,type = "class")
      xtab = table(predDT, test$Attrition)
      (xtab[1,1]/sum(xtab[1,]))*100
      
      
    })
    output$f_score_DT <- renderText({
      dataset=balanced_trainset()
      split=sample.split(dataset$Attrition,SplitRatio = input$trainsplit)
      train=subset(dataset,split==T)
      test=subset(dataset,split==F)
      
      decisionTreeModel=train_DT()
      predDT=predict(decisionTreeModel,newdata = test,type = "class")
      F1_Score(as.numeric(as.factor(test$Attrition)), as.numeric(predDT))*100
    })
    output$Classification_DT <- renderPlot({
      dataset=balanced_trainset()
      split=sample.split(dataset$Attrition,SplitRatio = input$trainsplit)
      train=subset(dataset,split==T)
      test=subset(dataset,split==F)
      
      
      decisionTreeModel=train_DT()
      predDT=predict(decisionTreeModel,newdata = test,type = "class")
      
      scores <- data.frame(predDT,test$Attrition)
      pr <- pr.curve(scores.class0=scores[scores$test.Attrition=="Leave",]$predDT,
                     scores.class1=scores[scores$test.Attrition=="No Leave",]$predDT,
                     curve=T)
      
      
      plot(pr)
      
      
    })
    
    
    
    option <- reactive({
      dataset<-pretraitement()
      dmy <- dummyVars(~., data = dataset[-7])
      data_one_cod <- data.frame(predict(dmy, newdata = dataset))
      data_one_cod$Attrition=dataset$Attrition
      
      data_one_cod
    })
    
    output$ DT_tree <- renderPlot({
      dataset=one_hot()
      split=sample.split(dataset$Attrition,SplitRatio = input$trainsplit)
      train=subset(dataset,split==T)
      test=subset(dataset,split==F)
      
      
      decisionTreeModel=train_DT()
      
      rpart.plot(decisionTreeModel)
      
    })
    
    output$Application  = output$Application <- DT::renderDataTable({
      
      old_dataset=pretraitement()
      split=sample.split(old_dataset$Attrition,SplitRatio = 0.5)
      train2=subset(old_dataset,split==T)
      test1=subset(old_dataset,split==F)
      x=test1$EmployeeID
      
      print(x)
      dataset=option()
      split=sample.split(dataset$Attrition,SplitRatio = 0.5)
      train=subset(dataset,split==T)
      test=subset(dataset,split==F)
      
      
      
      train$EmployeeID=NULL
      test$EmployeeID=NULL
      
      #randomForestModel=train_rf()  
      attLog=train_Lr()
      predictLR=predict(attLog,newdata=test,type="response")
      
      
      dataset[,1:65]=NULL
      dataset$EmployeeID=x
      dataset$Department=test1$Department
      dataset$Gender=test1$Gender
      dataset$JobRole=test1$JobRole
      dataset$WorkLifeBalance=test1$WorkLifeBalance
      dataset$Probability_Leave=round(predictLR*100, digits = 0)
      
      dataset
      
    })
    
    
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

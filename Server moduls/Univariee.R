univaree_server <- function(input,output, df) {
  # data=reactive({
  #   infile<-input$file
  #   if(is.null(infile)) return (NULL)
  #   delim<-get.delim(infile$datapath, n = 10, comment = "#", skip = 0, delims = c("\t", "\t| +", " ", ";", ","), large = 10, one.byte = TRUE)
  #   #print(delim)
  #   #read.csv(infile$datapath, header=input$Header, sep =delim)
  #   if(get.ext(infile$datapath)=="xls"){
  #     #print("I am here xls")
  #     read_excel(infile$datapath)
  #     
  #   }else{
  #     read.csv(infile$datapath, header=input$Header, sep =delim)
  #     
  #   }
  #   
  #   
  #   
  #   
  #   
  #   
  #   
  # })
  # liste de variables quantitatives
  output$quantlist = renderUI({
    selectInput('qnt', 'Le choix de la variable', names(df)[!grepl('factor|logical|character',sapply(df,class))])
  })
  # summary variable quantitative
  output$summary <- renderPrint({
    #print(df)
    dt = df
    dt2 =dt[,input$qnt]
    #print(dt2)
    t(summary.default(as.numeric(as.character(dt2))))
    t(summary(dt2))})
  
  
  # Commande pour l'affichage du plot des effectifs
  output$effectifsDiag <- renderPlot({ 
    dt = df
    plot(table(data.frame(dt[,input$qnt])), col ="blue", xlab =sym(input$qnt), ylab ="Effectifs", 
         main ="Distribution des effectifs")
  })
  # boite aux moustaches
  output$boiteMoustaches <- renderPlot({
    # BoÃÂ®te ÃÂ  moustaches
    dt = df
    boxplot( data.frame(as.numeric(as.character(dt[,input$qnt]))), col = grey(0.8), 
             main = " ",
             ylab = "", las = 1)
    # Affichage complÃÂ©mentaires en Y des diffÃÂ©rents ÃÂ¢ges
    rug(df[,input$qnt], side = 2)
  })
  
  # Histogramm des effectifs
  
  output$effectifsHist <- renderPlot({
    dt = df
    
    # Histogramme des effectifs
    hist(as.numeric(as.character(dt[,input$qnt])) , freq = TRUE, cex.axis = 1.5, cex.main = 1.5,
         main = "Histogramme", col = "blue",
         xlab = sym(input$qnt), ylab = "Effectifs", las = 1,
         right = FALSE, cex.lab = 1.5)
  })
  
  
  
  tabCentreDisp <- reactive({
    # Noms des caractéristiques
    dt =df[,input$qnt]
    names.tmp <- c("Maximum", "Minimum", "Moyenne", "Médiane",
                   "1e quartile", "3e quartile", "Variance", "Ecart-type")
    # Calcul des caractéristiques
    
    summary.tmp <- c(max(df[,1]), min(df[,1]), mean(df[,1]), median(df[,1]),
                     quantile((df[,1]))[2], quantile((df[,1]))[4],
                     var(df[,1]), sqrt(var(df[,1])))
    # Ajout des nomes au vecteur de valeurs
    summary.tmp <- cbind.data.frame(names.tmp, summary.tmp)
    # Ajout des noms de colonnes
    colnames(summary.tmp) <- c("Caractéristique", "Valeur")
    
    summary.tmp
  })
  output$centreDisp <- renderTable({tabCentreDisp()})
  
  # Histogramme de fréquence de densité
  output$effectifsHistFreqDens <- renderPlot({
    dt = df
    # Histogramme des densitÃ©s de frÃ©quences
    hist( as.numeric(as.character(dt[,input$qnt])), freq = FALSE, cex.axis = 1.5, cex.main = 1.5,
          main = "Histogramme de la variable", col = "green",
          xlab = dt[1,input$qnt] , ylab = "Densité de fréquences", las = 1,
          right = FALSE, cex.lab = 1.5)
  })
  
  
  output$effectifsCumCurve <- renderPlot({
    dt = df
    # RÃ©cupÃ©ration des infos Ã  partir de l'histogramme
    tmp.hist <- hist(as.numeric(as.character(dt[,input$qnt])) , plot = FALSE,
                     
                     right = FALSE)
    # Courbe cumulative (effectifs)
    plot(x = tmp.hist$breaks[-1], y = cumsum(tmp.hist$counts),
         xlab =sym(input$qnt),
         ylab = "Effectifs cumulés", cex.axis = 1.5, cex.lab = 1.5,
         main = "Courbe cumulative ",
         type = "o", col = "green", lwd = 2, cex.main = 1.5)
    
  })
  
  
  
  # la liste des variables qualitatives
  
  output$qualist = renderUI({
    selectInput('choixx', 'Le choix de la variable', names(df)[grepl('factor|logical|character',sapply(df,class))])
  })
  
  # conditionnal Panel
  output$myConditionalPanel = renderUI({
    if(length(names(df)[grepl('factor|logical|character',sapply(df,class))])>0) {
      ## some ui definitions here. for example
      conditionalPanel(
        condition = "input.tabset1 == 'tab_qualitative'",
        column(
          width = 12,
          box(
            width = 12,
            title = "Tableau statistique", status = "primary", solidHeader = TRUE,
            collapsible = TRUE,
            tableOutput(outputId = "statqq")
          )
          
          
        ),
        column(
          width = 12,
          box(
            width = 6,
            title = "Courbe Cumulative", status = "primary", solidHeader = TRUE,
            collapsible = TRUE,
            plotOutput(outputId = "effectifsDiagq")
          ),
          box(
            width = 6,
            title = "Diagramme en secteurs", status = "primary", solidHeader = TRUE,
            collapsible = TRUE,
            plotOutput("secteurs")
          )
          
         
          
        ),
      column(
        width = 12,
        box(
          width = 12,
          title = "Diagramme en colonnes", status = "danger", solidHeader = TRUE,
          collapsible = TRUE,
          plotOutput("colonnes")
        )
      )
        
      )
    }  
  })
  # tableau des effectifs et de fréquences
  tabStat <- reactive({
    dt = df
    dt2 =dt[,input$choixx]
    # Calculer les effectifs et les effectifs cumulÃÂ©s
    table.tmp <- as.data.frame(table(dt2))
    table.tmp <- cbind(table.tmp, cumsum(table.tmp[[2]]))
    # Calculer les frÃÂ©quences et les frÃÂ©quences cumulÃÂ©s
    table.tmp <- cbind(table.tmp, 
                       table.tmp[[2]]/nrow(df)*100,
                       table.tmp[[3]]/nrow(df)*100)
    # Ajouter des noms de colonnes
    colnames(table.tmp) <- c(input$choixx, "Effectifs", "Effectifs Cum.",
                             "Frequences", "Frequences Cum.")
    # Renvoyer le tableau statistique
    #print(dim(table.tmp[,1]))
    return(table.tmp)
  })
  
  output$statqq <- renderTable({ 
    tabStat() })
  
  # courbe cummulative
  output$effectifsDiagq <- renderPlot({ 
    dt = df
    plot(table(data.frame(dt[,input$choixx])), col ="blue", xlab =sym(input$choixx), ylab ="Effectifs", 
         main ="Distribution des effectifs")
  })
  
  # Diagramme en secteurs
  effectifs <- reactive({
    
    dt = df
    return(table(dt[,input$choixx]))
  })
  
  output$secteurs <- renderPlot({
    pie(effectifs(), labels = substr(names(effectifs()), 1, 7), 
        main = " Diagramme en secteurs ", col=c())
  })
  
  # Diagramme en colonnes
  output$colonnes <- renderPlot({
    barplot(effectifs(), main = " ", 
            xlab=sym(input$qnt),
            ylab="Effectifs", las = 2,
            names.arg = substr(names(effectifs()), 1, 9))
    
  })
  
  
  
  
  
  
  
}
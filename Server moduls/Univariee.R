univaree_server <- function(input,output, df) {

  # Color palette
  colors <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf")
  

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
  


  output$qqPlot <- renderPlot({
    dt <- df
    qqnorm(dt[,input$qnt], main = paste("Diagramme Quantile-Quantile pour", input$qnt),
           col = colors[1], pch = 19)
    qqline(dt[,input$qnt], col = "red", lwd = 2)
    grid()
  })
  
  output$densityPlot <- renderPlot({
    dt <- df
    dens <- density(as.numeric(as.character(dt[,input$qnt])))
    plot(dens, main = paste("Graphique de densité pour", input$qnt),
         xlab = input$qnt, ylab = "Densité",
         col = colors[2], lwd = 2)
    polygon(dens, col = adjustcolor(colors[2], alpha.f = 0.5), border = NA)
    grid()
  })
  
  output$violinPlot <- renderPlot({
    dt <- df
    vioplot::vioplot(as.numeric(as.character(dt[,input$qnt])),
                     main = paste("Diagramme en violon pour", input$qnt),
                     xlab = "", ylab = "Valeurs",
                     col = adjustcolor(colors[3], alpha.f = 0.7),
                     border = colors[3])
    grid()
  })
  
  output$dotPlot <- renderPlot({
    dt <- df
    stripchart(as.numeric(as.character(dt[,input$qnt])),
               method = "jitter",
               main = paste("Nuage de points pour", input$qnt),
               xlab = input$qnt, ylab = "",
               col = adjustcolor(colors[4], alpha.f = 0.5),
               pch = 19)
    grid()
  })
  
  output$stemLeafPlot <- renderPrint({
    dt <- df
    cat(paste("Diagramme tige-et-feuille pour", input$qnt, "\n\n"))
    stem(as.numeric(as.character(dt[,input$qnt])))
  })
  
  output$ecdfPlot <- renderPlot({
    dt <- df
    plot(ecdf(as.numeric(as.character(dt[,input$qnt]))),
         main = paste("Graphique ECDF pour", input$qnt),
         xlab = input$qnt, ylab = "ECDF",
         col = colors[5], lwd = 2)
    grid()
  })
  
  output$rugPlot <- renderPlot({
    dt <- df
    plot(as.numeric(as.character(dt[,input$qnt])), 
         main = paste("Graphique en peigne pour", input$qnt), 
         xlab = input$qnt, ylab = "", type = "n")
    rug(as.numeric(as.character(dt[,input$qnt])), col = colors[6], lwd = 2)
    grid()
  })
  
  output$stripPlot <- renderPlot({
    dt <- df
    stripchart(as.numeric(as.character(dt[,input$qnt])),
               method = "stack",
               main = paste("Graphique en bandes pour", input$qnt),
               xlab = input$qnt, ylab = "",
               col = adjustcolor(colors[7], alpha.f = 0.5),
               pch = 19)
    grid()
  })
  
  output$scatterPlot <- renderPlot({
    dt <- df
    plot(as.numeric(as.character(dt[,input$qnt])), 
         main = paste("Nuage de points pour", input$qnt),
         xlab = "Index", ylab = input$qnt,
         col = adjustcolor(colors[8], alpha.f = 0.5),
         pch = 19)
    grid()
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
  
  
  

  # Enhanced plots for qualitative variables
  output$barPlot <- renderPlot({
    dt <- df
    barplot(table(dt[,input$choixx]), 
            main = paste("Diagramme en barres pour", input$choixx),
            xlab = input$choixx, ylab = "Fréquence",
            col = colors,
            border = "white",
            las = 2)
    grid()
  })
  
  output$pieChart <- renderPlot({
    dt <- df
    pie(table(dt[,input$choixx]), 
        main = paste("Diagramme circulaire pour", input$choixx),
        labels = paste(names(table(dt[,input$choixx])), "\n", 
                       round(prop.table(table(dt[,input$choixx])) * 100, 1), "%"),
        col = colors,
        border = "white")
  })
  
  output$mosaicPlot <- renderPlot({
    dt <- df
    mosaicplot(table(dt[,input$choixx]), 
               main = paste("Graphique en mosaïque pour", input$choixx),
               color = colors,
               border = "white",
               las = 2)
  })
  
  output$stackedBarPlot <- renderPlot({
    dt <- df
    # This example assumes you have another categorical variable to stack by
    # You may need to adjust this based on your actual data
    other_var <- names(df)[grepl('factor|logical|character', sapply(df, class)) & 
                           names(df) != input$choixx][1]
    if (!is.na(other_var)) {
      barplot(table(dt[,input$choixx], dt[,other_var]), 
              main = paste("Diagramme en barres empilées pour", input$choixx, "et", other_var),
              xlab = input$choixx, ylab = "Fréquence",
              col = colors,
              border = "white",
              legend = TRUE,
              args.legend = list(x = "topright", bty = "n"),
              las = 2)
      grid()
    } else {
      plot.new()
      text(0.5, 0.5, "Pas assez de variables catégorielles pour un graphique empilé")
    }
  })
  
  output$paretoChart <- renderPlot({
    dt <- df
    freq_table <- sort(table(dt[,input$choixx]), decreasing = TRUE)
    cumulative_percent <- cumsum(freq_table) / sum(freq_table) * 100
    
    par(mar = c(10, 5, 4, 5))
    bp <- barplot(freq_table, 
            main = paste("Diagramme de Pareto pour", input$choixx),
            xlab = "", ylab = "Fréquence",
            names.arg = "",
            col = colors,
            border = "white",
            las = 2)
    text(bp, par("usr")[3], labels = names(freq_table), srt = 45, adj = c(1,1), xpd = TRUE)
    
    par(new = TRUE)
    plot(bp, cumulative_percent, type = "b", axes = FALSE, 
         xlab = "", ylab = "", col = "red", ylim = c(0,100), lwd = 2)
    axis(side = 4)
    mtext("Pourcentage cumulé", side = 4, line = 3)
    grid()
  })
  
  
  
}
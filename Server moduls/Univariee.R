univaree_server <- function(input,output, df) {

  # Color palette
  colors <- c("#439ad7", "#a86200", "#2ca02c", "#d62728", "#9467bd", "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf")
  

  output$quantlist <- renderUI({
    selectInput('qnt', 'Le choix de la variable', names(df)[!grepl('factor|logical|character', sapply(df, class))])
  })
  # summary variable quantitative
  output$summary <- renderPrint({
    dt <- df
    dt2 <- dt[, input$qnt]
    t(summary.default(as.numeric(as.character(dt2))))
    t(summary(dt2))
  })
  
  
  # Commande pour l'affichage du plot des effectifs
  output$effectifsDiag <- renderPlot({
    dt <- df
    plot(table(data.frame(dt[, input$qnt])), 
        col = "darkred", # Changed color to dark red
        xlab = input$qnt, 
        ylab = "Effectifs", 
        main = "Distribution des Effectifs", # Capitalized title for consistency
        las = 1, 
        cex.axis = 0.9, # Slightly increased axis text size
        cex.lab = 0.9,  # Slightly increased label text size
        cex.main = 1.2) # Increased main title text size
    grid()
  })
  # boite aux moustaches
  output$boiteMoustaches <- renderPlot({
    dt <- df
    boxplot(data.frame(as.numeric(as.character(dt[, input$qnt]))), 
            col = "lightblue", # Changed color to light blue
            main = "Boîte à Moustaches des Effectifs", # Added more descriptive title
            ylab = input$qnt, 
            las = 1, 
            cex.axis = 0.9, # Slightly increased axis text size
            cex.lab = 0.9,  # Slightly increased label text size
            cex.main = 1.2) # Increased main title text size
    rug(as.numeric(as.character(dt[, input$qnt])), side = 2, col = "darkblue") # Changed rug color to dark blue
    grid()
  })
  
  # Histogramm des effectifs
    output$effectifsHist <- renderPlot({
      dt <- df
      hist(as.numeric(as.character(dt[, input$qnt])), 
          freq = TRUE, 
          col = "darkorange", # Changed color to dark orange
          main = "Histogramme des Effectifs", # Added more descriptive title
          xlab = input$qnt, 
          ylab = "Effectifs", 
          las = 1, 
          cex.axis = 0.9, # Slightly increased axis text size
          cex.lab = 0.9,  # Slightly increased label text size
          cex.main = 1.2) # Increased main title text size
      grid()
    })


  tabCentreDisp <- reactive({
    dt <- df[, input$qnt]
    names.tmp <- c("Maximum", "Minimum", "Moyenne", "Médiane", "1er quartile", "3e quartile", "Variance", "Écart-type")
    summary.tmp <- c(max(dt), min(dt), mean(dt), median(dt), quantile(dt)[2], quantile(dt)[4], var(dt), sd(dt))
    summary.tmp <- data.frame(Caractéristique = names.tmp, Valeur = summary.tmp)
    summary.tmp
  })
  output$centreDisp <- renderTable({tabCentreDisp()})
  
  # # Histogramme de fréquence de densité
  # output$effectifsHistFreqDens <- renderPlot({
  #   dt <- df
  #   hist(as.numeric(as.character(dt[, input$qnt])), freq = FALSE, col = "green", 
  #       main = "Histogramme de la variable", xlab = input$qnt, ylab = "Densité de fréquences", 
  #       las = 1, cex.axis = 0.8, cex.lab = 0.8, cex.main = 1)
  # })
  
  
  output$effectifsCumCurve <- renderPlot({
    dt <- df
    tmp.hist <- hist(as.numeric(as.character(dt[, input$qnt])), plot = FALSE, right = FALSE)
    plot(x = tmp.hist$breaks[-1], y = cumsum(tmp.hist$counts), type = "o", col = "green", lwd = 2, 
        main = "Courbe cumulative", xlab = input$qnt, ylab = "Effectifs cumulés", 
        cex.axis = 0.8, cex.lab = 0.8, cex.main = 1)
  })
  

  
  # la liste des variables qualitatives
  
  output$qualist <- renderUI({
    selectInput('choixx', 'Le choix de la variable', names(df)[grepl('factor|logical|character', sapply(df, class))])
  })
  
  # conditionnal Panel
  output$myConditionalPanel <- renderUI({
    if (length(names(df)[grepl('factor|logical|character', sapply(df, class))]) > 0) {
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
        )
      )
    }
  })
  
  # tableau des effectifs et de fréquences
  tabStat <- reactive({
    dt <- df
    dt2 <- dt[, input$choixx]
    table.tmp <- as.data.frame(table(dt2))
    table.tmp <- cbind(table.tmp, cumsum(table.tmp[[2]]))
    table.tmp <- cbind(table.tmp, table.tmp[[2]] / nrow(df) * 100, table.tmp[[3]] / nrow(df) * 100)
    colnames(table.tmp) <- c(input$choixx, "Effectifs", "Effectifs Cum.", "Fréquences", "Fréquences Cum.")
    table.tmp
  })
  output$statqq <- renderTable({ tabStat() })

  
  output$statqq <- renderTable({ 
    tabStat() })
  
  # courbe cummulative
  output$effectifsDiagq <- renderPlot({
    dt <- df
    plot(table(data.frame(dt[, input$choixx])), 
        col = "darkgreen", 
        xlab = input$choixx, 
        ylab = "Effectifs", 
        main = "Distribution des effectifs", 
        las = 1, 
        cex.axis = 0.9, # Slightly increased axis text size
        cex.lab = 0.9,  # Slightly increased label text size
        cex.main = 1.2) # Increased main title text size
    grid()
  })

  

  
  

  # qualitative variables
    output$barPlot <- renderPlot({
      dt <- df
      barplot(table(dt[,input$choixx]), 
              main = paste("Diagramme en barres pour", input$choixx),
              xlab = input$choixx, ylab = "Fréquence",
              col = colors,
              border = "white",
              las = 2,
              cex.names = 0.8)  # Adjust label size for better readability
      grid()
    })
    output$pieChart <- renderPlot({
      dt <- df
      pie(table(dt[,input$choixx]), 
          main = paste("Diagramme circulaire pour", input$choixx),
          labels = paste(names(table(dt[,input$choixx])), "\n", 
                        round(prop.table(table(dt[,input$choixx])) * 100, 1), "%"),
          col = colors,
          border = "white",
          cex = 0.8)  # Adjust label size for better readability
    })
      
    output$mosaicPlot <- renderPlot({
      dt <- df
      mosaicplot(table(dt[,input$choixx]), 
                main = paste("Graphique en mosaïque pour", input$choixx),
                color = colors,
                border = "white",
                las = 2,
                cex.axis = 0.8)  # Adjust axis label size for better readability
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
                args.legend = list(x = "topright", bty = "n", inset = c(-0.1, 0)),  # Adjust legend position
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
          xlab = "", ylab = "", col = "red", ylim = c(0,100), lwd = 2, pch = 16)  # Add points to the line
      axis(side = 4, col.axis = "red", col = "red")  # Color the right axis red
      mtext("Pourcentage cumulé", side = 4, line = 3, col = "red")  # Color the right axis label red
      grid()
    })
  
  





      # Update choices for churn variable
    observe({
      updateSelectInput(inputId = "churn_var", 
                        choices = names(df)[sapply(df, function(x) is.factor(x) || is.character(x))])
    })

    # Update choices for comparison variable
    observe({
      updateSelectInput(inputId = "churn_compare_var", 
                        choices = names(df)[!names(df) %in% input$churn_var])
    })

    # Churn Proportion Plot
    output$churnProportion <- renderPlot({
      req(input$churn_var)
      churn_data <- table(df[[input$churn_var]])
      barplot(prop.table(churn_data),
              main = paste("Proportion des individus qui ont churné -", input$churn_var),
              ylab = "Proportion",
              col = colors[1:length(churn_data)],
              ylim = c(0, 1),
              border = "white",  # Add white borders for better visibility
              las = 1)  # Rotate y-axis labels for better readability
    })

    # Churn Histogram
    output$churnHistogram <- renderPlot({
      req(input$churn_var, input$churn_compare_var)
      if (is.numeric(df[[input$churn_compare_var]])) {
        ggplot(df, aes_string(x = input$churn_compare_var, fill = input$churn_var)) +
          geom_histogram(position = "dodge", bins = 30, color = "black") +  # Add black borders to bars
          labs(title = paste("Histogramme pour", input$churn_var, "vs", input$churn_compare_var),
              x = input$churn_compare_var,
              y = "Compte") +
          scale_fill_manual(values = colors) +
          theme_minimal() +
          theme(plot.title = element_text(hjust = 0.5))  # Center the title
      } else {
        ggplot(df, aes_string(x = input$churn_compare_var, fill = input$churn_var)) +
          geom_bar(position = "dodge", color = "black") +  # Add black borders to bars
          labs(title = paste("Diagramme en barres pour", input$churn_var, "vs", input$churn_compare_var),
              x = input$churn_compare_var,
              y = "Compte") +
          scale_fill_manual(values = colors) +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1),
                plot.title = element_text(hjust = 0.5))  # Center the title
      }
    })

    # Churn Proportion (Qualitative)
    output$churnProportionQual <- renderPlot({
      req(input$churn_var, input$churn_compare_var)
      if (!is.numeric(df[[input$churn_compare_var]])) {
        ggplot(df, aes_string(x = input$churn_compare_var, fill = input$churn_var)) +
          geom_bar(position = "fill", color = "black") +  # Add black borders to bars
          labs(title = paste("Proportion de", input$churn_var, "par", input$churn_compare_var),
              x = input$churn_compare_var,
              y = "Proportion") +
          scale_fill_manual(values = colors) +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1),
                plot.title = element_text(hjust = 0.5))  # Center the title
      } else {
        plot.new()
        text(0.5, 0.5, "Cette visualisation n'est disponible que pour les variables qualitatives")
      }
    })

  
}


#######################################################################

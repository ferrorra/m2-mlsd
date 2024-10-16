exploration_server <- function(input, output, df) {
  
  quantitative <- reactive({
    names(df)[sapply(df, is.numeric)]
  })
  
  # Correlation Heatmap for quantitative variables
  output$heatmap <- renderPlotly({
    if (length(quantitative()) > 1) {
      Vm <- colnames(df)[colSums(is.na(df)) > 0]
      if (length(Vm) == 0) {
        cor_matrix <- cor(df[, quantitative()], use = "pairwise.complete.obs")
        
        heatmaply(
          cor_matrix,
          margins = c(50, 50),
          dendrogram = "none",
          xlab = "Variables",
          ylab = "Variables",
          main = "Correlation Heatmap",
          colors = colorRampPalette(c("#1f77b4", "#ffffff", "#ff7f0e"))(100),
          limits = c(-1, 1),
          showticklabels = c(TRUE, TRUE),
          plot_method = "plotly",
          colorbar = list(title = "Correlation"),
          node_type = "scatter",
          node_size = 8
        )
      }
    }
  })
  
  output$soustxt <- renderText({
    Vm <- colnames(df)[colSums(is.na(df)) > 0]
    if (length(Vm) > 0) {
      output$Theatmap <- renderText({
        "Il existe des valeurs quantitatives manquantes, il faut d'abord les imputer."
      })
    } else if (length(Vm) == 0) {
      output$Theatmap <- renderText({ " " })
    }
    if (length(quantitative()) == 0) {
      output$Theatmap <- renderText({
        "Ce dataset ne contient pas de valeurs quantitatives."
      })
    }
  })

  # Cercle de Corrélation (PCA) entre variables quantitatives
  output$correlationCirclePlot <- renderPlotly({
    if (length(quantitative()) > 1) {
      
      # Retirer les colonnes constantes ou entièrement remplies de NA avant le PCA
      df_quant <- df[, quantitative()]
      
      # Ne garder que les colonnes avec des valeurs non constantes et avec variance non nulle
      df_quant <- df_quant[, apply(df_quant, 2, function(x) length(unique(x[!is.na(x)])) > 1)]
      
      # Si après filtrage il reste des colonnes valides
      if (ncol(df_quant) > 1) {
        # Perform PCA
        pca_res <- prcomp(df_quant, scale. = TRUE)
        
        # Visualiser les résultats du cercle de corrélation
        library(factoextra)
        library(ggplot2)
        
        corr_circle <- fviz_pca_var(pca_res, 
                                    col.var = "contrib",  # Couleur selon les contributions
                                    gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                                    repel = TRUE,        # Éviter les chevauchements de texte
                                    label = "var",       # Afficher les noms des variables
                                    labelsize = 5,       # Taille des labels
                                    pointsize = 3,       # Taille des points
                                    title = "Cercle de corrélation",
                                    subtitle = "Basé sur l'ACP des variables quantitatives"
        ) +
          theme_minimal() +
          theme(
            plot.title = element_text(size = 16, face = "bold"),
            plot.subtitle = element_text(size = 12),
            axis.title = element_text(size = 12),
            axis.text = element_text(size = 10),
            legend.title = element_text(size = 12),
            legend.text = element_text(size = 10)
          )
        
        # Convertir en plotly pour l'interactivité
        p <- ggplotly(corr_circle, tooltip = "text") %>%
          layout(
            hoverlabel = list(bgcolor = "white", font = list(size = 12)),
            legend = list(orientation = "h", y = -0.15)
          )
        
        # Assurez-vous que les noms des variables sont visibles
        for (i in seq_along(p$x$data)) {
          if (!is.null(p$x$data[[i]]$text)) {
            p$x$data[[i]]$text <- gsub("label: ", "", p$x$data[[i]]$text)
            p$x$data[[i]]$hoverinfo <- "text"
          }
        }
        
        # Retourner le plot
        return(p)
      } else {
        # Message si aucune variable quantitative valide n'est disponible
        output$Tcercle <- renderText({
          "Il n'y a pas assez de variables quantitatives valides pour créer un cercle de corrélation."
        })
      }
      
    } else {
      output$Tcercle <- renderText({
        "Ce dataset ne contient pas assez de valeurs quantitatives pour créer un cercle de corrélation."
      })
    }
  })
  
  output$circleText <- renderText({
    Vm <- colnames(df)[colSums(is.na(df)) > 0]
    if (length(Vm) > 0) {
      output$Tcercle <- renderText({
        "Il existe des valeurs quantitatives manquantes, il faut d'abord les imputer avant de visualiser le cercle de corrélation."
      })
    } else if (length(Vm) == 0) {
      output$Tcercle <- renderText({ " " })
    }
    if (length(quantitative()) <= 1) {
      output$Tcercle <- renderText({
        "Ce dataset ne contient pas assez de valeurs quantitatives pour créer un cercle de corrélation."
      })
    }
  })
 
  # la méthode SD choisie pour determniner les outliers
  output$caract_quantitative_SD <- renderTable({
    # Définition des colonnes choisies 
    #print("I am here",input$quantlistbi1)
    if(length(quantitative())>0){
      var.names <-quantitative()
      # Initialisation de la table
      caract.df <- data.frame()
      
      # Pour chaque colonne, calcul de min, max, mean et ecart-type
      for(strCol in var.names){
        
        # la méthode choisie pour determniner les outliers
        
        
        # get threshold values for outliers
        Tmin=mean(var(df[,strCol]))-(3*sqrt(var(df[,strCol])))
        
        Tmax=mean(var(df[,strCol]))+(3*sqrt(var(df[,strCol])))
        
        
        
        
        
        
        
        nb_out=length(var(df[,strCol])[which(var(df[,strCol]) < Tmin | var(df[,strCol]) > Tmax)])
        
        prg=(sum(is.na(df[,strCol])))/(nrow(df))
        
        
        caract.vect <- c("Quantitative", nb_out, 
                         sum(is.na(df[,strCol])),prg)
        caract.df <- rbind.data.frame(caract.df, caract.vect)
      }
      # exploration de données
      
      
      
      # Définition des row/colnames
      rownames(caract.df) <- var.names
      colnames(caract.df) <- c("Type de variable", "Nombre de outliers", "Nombre de valeurs manquntes", "Pourcentage %")
      # Renvoyer la table
      caract.df
    }
  }, rownames = TRUE, digits = 0)
  
  # la méthode MAD choisie pour determniner les outliers
  output$caract_quantitative_MAD <- renderTable({
    # Définition des colonnes choisies 
    #print("I am here",input$quantlistbi1)
    if(length(quantitative())>0){
      var.names <-quantitative()
      # Initialisation de la table
      caract.df <- data.frame()
      
      # Pour chaque colonne, calcul de min, max, mean et ecart-type
      for(strCol in var.names){
        
        
        
        
        med=median(df[,strCol])
        abs_dev=abs(df[,strCol]-med)
        # get MAD
        mad=1.4826 * median(abs_dev)
        # get threshold values for outliers
        Tmin = med-(3*mad) 
        Tmax = med+(3*mad)
        
        
        nb_out=length(var(df[,strCol])[which(var(df[,strCol]) < Tmin | var(df[,strCol]) > Tmax)])
        
        
        
        prg=(sum(is.na(df[,strCol])))/(nrow(df))
        caract.vect <- c("Quantitative", nb_out, 
                         sum(is.na(df[,strCol])),prg)
        caract.df <- rbind.data.frame(caract.df, caract.vect)
      }
      
      # Définition des row/colnames
      rownames(caract.df) <- var.names
      colnames(caract.df) <- c("Type de variable", "Nombre de outliers", " Nombre de valeurs manquntes", "Pourcentage %")
      # Renvoyer la table
      caract.df
    }
  }, rownames = TRUE, digits = 0)
  
  

  
  
  # output$caract_quantitative <- renderTable({
    
  #   # Définition des colonnes choisies 
  #   #print("I am here",input$quantlistbi1)
  #   if(length(quantitative())>0){
      
  #     l=list()
  #     var.names <-quantitative()
  #     # Initialisation de la table
  #     caract.df <- data.frame()
      
  #     # Pour chaque colonne, calcul de min, max, mean et ecart-type
  #     for(strCol in var.names){
        
        
        
  #       prg=(sum(is.na(df[,strCol])))/(nrow(df))
  #       caract.vect <- c("Quantitative", 
  #                        sum(is.na(df[,strCol])),prg)
  #       caract.df <- rbind.data.frame(caract.df, caract.vect)
  #     }
  #     #print(l)
  #     #Variable_outliers(l)
      
  #     # Définition des row/colnames
  #     rownames(caract.df) <- var.names
  #     colnames(caract.df) <- c("Type de variable","Nombre de valeurs manquantes","Pourcentage %")
  #     # Renvoyer la table
  #     caract.df
  #   }
  #   else{
  #     print("ce Dataset ne contient pas de variables quantitatives")
  #   }
    
  # }, rownames = TRUE, digits = 0)
  
  # # qualitative
  
  
  # qualitative<- reactive({
  #   names(df)[grepl('factor|logical|character',sapply(df,class))]
  # })
  # output$caract_qualitative <- renderTable({
  #   # Définition des colonnes choisies 
  #   #print("I am here",input$quantlistbi1)
  #   if(length(qualitative())>0){
  #     var.names <-qualitative()
  #     # Initialisation de la table
  #     caract.df <- data.frame()
      
  #     # Pour chaque colonne, calcul de min, max, mean et ecart-type
  #     for(strCol in var.names){
  #       #print(df[,strCol])
  #       #nb_out=length(x[which(x < Tmin | x > Tmax)])
  #       att_value=unique(df[,strCol])
         
  #       prg=(sum(as.character(df[,strCol])=="")+sum(as.character(df[,strCol])=="?"))/(nrow(df))
  #       caract.vect <- c("Qualitative", 
  #                        sum(as.character(df[,strCol])=="")+sum(as.character(df[,strCol])=="?"),prg,length(att_value))
  #       caract.df <- rbind.data.frame(caract.df, caract.vect)
  #     }
      
  #     # Définition des row/colnames
  #     rownames(caract.df) <- var.names
  #     colnames(caract.df) <- c("Type de variable", "Nombre de valeurs manquntes","Pourcentage %","Nombre de catégories")
  #     # Renvoyer la table
  #     caract.df
  #   }
  #   else{
  #     print("ce dataset ne contient pas de variables qualitatives")
  #   }
  # }, rownames = TRUE, digits = 0)






output$variable_characteristics <- renderTable({
  # Combine quantitative and qualitative variables
  all_vars <- c(quantitative(), qualitative())
  
  if (length(all_vars) > 0) {
    # Initialize the table
    caract.df <- data.frame()
    
    for (strCol in all_vars) {
      # Determine variable type
      var_type <- if (strCol %in% quantitative()) "Quantitative" else "Qualitative"
      
      # Calculate missing values
      missing_count <- sum(is.na(df[, strCol])) + sum(df[, strCol] == "") + sum(df[, strCol] == "?")
      missing_percentage <- (missing_count / nrow(df)) * 100
      
      # Check if variable is constant
      is_constant <- if (length(unique(na.omit(df[, strCol]))) == 1) "Oui" else "Non"
      
      # Determine variable level
      if (var_type == "Quantitative") {
        level <- if (all(df[, strCol] %% 1 == 0, na.rm = TRUE)) "Discrète" else "Continue"
      } else {
        level <- if (is.ordered(factor(df[, strCol]))) "Ordinal" else "Nominal"
      }
      
      # Create vector for this variable
      caract.vect <- c(var_type, 
                       missing_count,
                       missing_percentage,
                       is_constant,
                       level)
      
      # Add to data frame
      caract.df <- rbind(caract.df, caract.vect)
    }
    
    # Set row and column names
    rownames(caract.df) <- all_vars
    colnames(caract.df) <- c("Type de variable", 
                              "Nombre de valeurs manquantes", 
                              "Pourcentage %", 
                              "Variable constante (oui/non)", 
                              "Niveau de variable")
    
    # Return the table
    return(caract.df)
  } else {
    return(data.frame(Message = "Ce dataset ne contient pas de variables à analyser"))
  }
}, rownames = TRUE, digits = 2)




















}

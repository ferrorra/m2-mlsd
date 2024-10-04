Bivaree_server <- function(input,output,df) {
  # Analyse bivariée
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
  # })
  #Quantitative vs Quantitative
  output$quantlistbi1 = renderUI({
    selectInput('quantlistbi1', 'Le choix de la variable X', names(df)[!grepl('factor|logical|character',sapply(df,class))])
  })
  output$quantlistbi2 = renderUI({
    df2 <- df[,!names(df) %in% c(input$quantlistbi1)]
    selectInput('quantlistbi2', 'Le choix de la variable Y', names(df2)[!grepl('factor|logical|character',sapply(df2,class))])
  })
  
  
  # table caractéristiques
  quant<- reactive({
    names(df)[!grepl('factor|logical|character',sapply(df,class))]
  })
  output$caract <- renderTable({
    # Définition des colonnes choisies 
    #print("I am here",input$quantlistbi1)
    var.names <-c(input$quantlistbi1,input$quantlistbi2)
    # Initialisation de la table
    caract.df <- data.frame()
    
    # Pour chaque colonne, calcul de min, max, mean et ecart-type
    for(strCol in var.names){
      caract.vect <- c(min(df[, strCol]), max(df[,strCol]), 
                       mean(var(df[,strCol])), sqrt(var(df[,strCol])))
      caract.df <- rbind.data.frame(caract.df, caract.vect)
    }
    
    # Définition des row/colnames
    rownames(caract.df) <- var.names
    colnames(caract.df) <- c("Minimum", "Maximum", "Moyenne", "Ecart-type")
    # Renvoyer la table
    caract.df
  }, rownames = TRUE, digits = 0)
  
  
  # Nuage de points et histogrammes
  
  output$nuagePointshist <- renderPlot({
    
    options(digits=1, col="blue")
    dt = df
    dt2 =dt[,input$quantlistbi1]
    dt2 = as.numeric(dt2)
    dt = df
    dt3 =dt[,input$quantlistbi2]
    dt3 = as.numeric(dt3)
    X = dt2; 
    Y = dt3;
    
    scatter.with.hist(X,Y, col="blue")
  })
  
  # histogramme dos a dos
  output$histbackback <- renderPlot({
    options(digits=1)
    x.var = input$quantlistbi1 ; y.var = input$quantlistbi2;
    dt = df
    dt2 =dt[,input$quantlistbi1]
    dt2 = as.numeric(dt2)
    dt = df
    dt3 =dt[,input$quantlistbi2]
    dt3 = as.numeric(dt3)
    histbackback(x = dt2, y = dt3,
                 xlab = c(x.var, y.var), main = paste(x.var, "and", y.var), 
                 las = 2)
  })
  
  
  
  # nuage de Points
  output$nuagePoints <- renderPlot({
    # Simple nuage de point 
    options(scipen=999)
    x.var = input$quantlistbi1 ; y.var = input$quantlistbi2;
    plot(x = df[, x.var], y = df[, y.var], col = "blue",
         las = 2, cex.axis = 0.7,
         main = paste(y.var, "en fonction de", x.var),
         xlab = x.var, ylab = y.var, cex.lab = 1.2
    )
    # Droite de régression linéaire (y~x) 
    abline(lm(df[, y.var]~df[, x.var]), col="red", lwd = 2)
    options(scipen=0)
    
    
  })
  #corrélation
  output$correlation <- renderText({
    dt = df
    dt2 =dt[,input$quantlistbi1]
    dt2 = as.numeric(dt2)
    dt = df
    dt3 =dt[,input$quantlistbi2]
    dt3 = as.numeric(dt3)
    #x.var = input$choix ; y.var = input$choixx;
    coeff.tmp <- cov(dt2, dt3)/(sqrt(var(dt2)*var(dt3)))
    paste("Coefficient de corrélation linéaire =", round(coeff.tmp,digits = 2))
  })
  
  
  
  
}

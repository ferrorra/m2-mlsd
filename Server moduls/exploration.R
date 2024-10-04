exploration_server <- function(input,output,df) {
  
  quantitative<- reactive({
    names(df)[!grepl('factor|logical|character',sapply(df,class))]
  })
  
   # Correlation Heatmap entre les variable quantitative
   
    
     output$heatmap <- renderPlotly({
       if(length(quantitative())>1){
         Vm<-colnames(df)[colSums(is.na(df) > 0)]
         if(length(Vm)==0){
         
           heatmaply(cor(df[,quantitative()]), margins = c(40, 40),
                     k_col = 2, k_row = 2,
                     limits = c(-1,1))
         }
           
         
         
       }
     })
   
   
     output$soustxt<-renderText({
       
     Vm<-colnames(df)[colSums(is.na(df) > 0)]
     if(length(Vm)>0){
       output$Theatmap<-renderText({
         " il existe des valeurs quantitatives manquantes, il faut dabord les imputer"
       })
     }
     if(length(Vm)==0){
       output$Theatmap<-renderText({
             " "       })
       
     }
     if(length(quantitative())==0){
       output$Theatmap<-renderText({
         "Ce dataset ne contient pas de valeurs quantitatives"       })
      
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
  
  
  
  # # la méthode IQR choisie pour determniner les outliers
  # output$caract_quantitative_IQR <- renderTable({
  #   # Définition des colonnes choisies 
  #   #print("I am here",input$quantlistbi1)
  #   if(length(quantitative())>0){
  #     var.names <-quantitative()
  #     # Initialisation de la table
  #     caract.df <- data.frame()
  #     
  #     # Pour chaque colonne, calcul de min, max, mean et ecart-type
  #     for(strCol in var.names){
  #       
  #       
  #       
  #       
  #       # get IQR
  #       iqr=IQR(df[,strCol],na.rm = TRUE)
  #       first_q=quantile((df[,strCol]))[2]
  #       third_q=quantile((df[,strCol]))[4]
  #       # get threshold values for outliers
  #       Tmin = first_q-(1.5*iqr) 
  #       Tmax = third_q+(1.5*iqr) 
  #       
  #       
  #       nb_out=length(var(df[,strCol])[which(var(df[,strCol]) < Tmin | var(df[,strCol]) > Tmax)])
  #       
  #       
  #       
  #       prg=(sum(is.na(df[,strCol])))/(nrow(df))
  #       caract.vect <- c("Quantitative", nb_out, 
  #                        sum(is.na(df[,strCol])),prg)
  #       caract.df <- rbind.data.frame(caract.df, caract.vect)
  #     }
  #     
  #     # Définition des row/colnames
  #     rownames(caract.df) <- var.names
  #     colnames(caract.df) <- c("Type de variable", "Présence de outliers","Présence de valeurs manquantes","Pourcentage %")
  #     # Renvoyer la table
  #     caract.df
  #   }
  # }, rownames = TRUE, digits = 0)
  
   
  
  
  
  # output$caract_quantitative <- renderTable({
  #   # Définition des colonnes choisies 
  #   #print("I am here",input$quantlistbi1)
  #   if(length(quantitative())>0){
  #   var.names <-quantitative()
  #   # Initialisation de la table
  #   caract.df <- data.frame()
  #   
  #   # Pour chaque colonne, calcul de min, max, mean et ecart-type
  #   for(strCol in var.names){
  #    
  #     # la méthode choisie pour determniner les outliers
  #     print(input$methode)
  #     if(input$methode=="SD"){
  #       print("I am here amir")
  #       # get threshold values for outliers
  #       Tmin=mean(var(df[,strCol]))-(3*sqrt(var(df[,strCol])))
  #       
  #       Tmax=mean(var(df[,strCol]))+(3*sqrt(var(df[,strCol])))
  #       
  #      
  #       
  #     }
  #     if(input$methode=="MAD"){
  #       med=median(df[,strCol])
  #       abs_dev=abs(df[,strCol]-med)
  #       # get MAD
  #       mad=1.4826 * median(abs_dev)
  #       # get threshold values for outliers
  #       Tmin = med-(3*mad) 
  #       Tmax = med+(3*mad)
  #       
  #     }
  #     if(input$methode=="IQR"){
  #       # get IQR
  #       iqr=IQR(df[,strcol])
  #       first_q=quantile((df[,strcol]))[2]
  #       third_q=quantile((df[,strcol]))[4]
  #       # get threshold values for outliers
  #       Tmin = first_q-(1.5*iqr) 
  #       Tmax = third_q+(1.5*iqr) 
  #       
  #       
  #     }
  #     
  #     nb_out=length(var(df[,strCol])[which(var(df[,strCol]) < Tmin | var(df[,strCol]) > Tmax)])
  #     
  #     
  #     
  #     
  #     caract.vect <- c("Quantitative", nb_out, 
  #                      sum(is.na(df[,strCol])))
  #     caract.df <- rbind.data.frame(caract.df, caract.vect)
  #   }
  #   
  #   # Définition des row/colnames
  #   rownames(caract.df) <- var.names
  #   colnames(caract.df) <- c("Type de variable", "Présence de outliers", "Présence de valeurs manquntes")
  #   # Renvoyer la table
  #   caract.df
  # }
  # }, rownames = TRUE, digits = 0)
  
  
  output$caract_quantitative <- renderTable({
    
    # Définition des colonnes choisies 
    #print("I am here",input$quantlistbi1)
    if(length(quantitative())>0){
      
      l=list()
      var.names <-quantitative()
      # Initialisation de la table
      caract.df <- data.frame()
      
      # Pour chaque colonne, calcul de min, max, mean et ecart-type
      for(strCol in var.names){
        
        
        
        prg=(sum(is.na(df[,strCol])))/(nrow(df))
        caract.vect <- c("Quantitative", 
                         sum(is.na(df[,strCol])),prg)
        caract.df <- rbind.data.frame(caract.df, caract.vect)
      }
      #print(l)
      #Variable_outliers(l)
      
      # Définition des row/colnames
      rownames(caract.df) <- var.names
      colnames(caract.df) <- c("Type de variable","Nombre de valeurs manquantes","Pourcentage %")
      # Renvoyer la table
      caract.df
    }
    else{
      print("ce Dataset ne contient pas de variables quantitatives")
    }
    
  }, rownames = TRUE, digits = 0)
  
  # qualitative
  
  
  qualitative<- reactive({
    names(df)[grepl('factor|logical|character',sapply(df,class))]
  })
  output$caract_qualitative <- renderTable({
    # Définition des colonnes choisies 
    #print("I am here",input$quantlistbi1)
    if(length(qualitative())>0){
      var.names <-qualitative()
      # Initialisation de la table
      caract.df <- data.frame()
      
      # Pour chaque colonne, calcul de min, max, mean et ecart-type
      for(strCol in var.names){
        #print(df[,strCol])
        #nb_out=length(x[which(x < Tmin | x > Tmax)])
        att_value=unique(df[,strCol])
         
        prg=(sum(as.character(df[,strCol])=="")+sum(as.character(df[,strCol])=="?"))/(nrow(df))
        caract.vect <- c("Qualitative", 
                         sum(as.character(df[,strCol])=="")+sum(as.character(df[,strCol])=="?"),prg,length(att_value))
        caract.df <- rbind.data.frame(caract.df, caract.vect)
      }
      
      # Définition des row/colnames
      rownames(caract.df) <- var.names
      colnames(caract.df) <- c("Type de variable", "Nombre de valeurs manquntes","Pourcentage %","Nombre de catégories")
      # Renvoyer la table
      caract.df
    }
    else{
      print("ce dataset ne contient pas de variables qualitatives")
    }
  }, rownames = TRUE, digits = 0)
}

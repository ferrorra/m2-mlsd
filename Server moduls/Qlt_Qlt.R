Qlt_Qlt_server <- function(input,output,df) {
  
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
  
  
  output$qualistvs1 = renderUI({
    selectInput('qualistvs1', 'Le choix de la variable', names(df)[grepl('factor|logical|character',sapply(df,class))])
  })
  output$qualistvs2 = renderUI({
    selectInput('qualistvs2', 'Le choix de la variable', names(df)[grepl('factor|logical|character',sapply(df,class))])
  })
  # Diagramme Profils
  output$barplotProfils <- renderPlot({
    # Diagramme de profils entre les variables 'V2' et 'V6'
    var1<-sym(input$qualistvs1)
    var2<-sym(input$qualistvs2)
    ggplot(df, aes(x = !!var2, fill = !!var1)) + geom_bar(position = "fill")
  })
  
  
  # Force de la liaison entre 'especes' et 'couleur'
  
  output$force <- renderTable({
    force.df <- as.data.frame(matrix(NA, nrow = 3, ncol = 1))
    rownames(force.df) = c("X2", "Phi2", "Cramer")
    
    # La table de contingence des profils observés
    #tab = with(df, table(input$quantlistvs, input$qualistvs))
    var1<-input$qualistvs1
    var2<-input$qualistvs2
    data<-cbind(df[var1],df[var2])
    data<-as.data.frame(data)
    colnames(data)<-c("var1","var2")
    
    tab =with(data,table(var1,var2))
    #print(tab)
    # La table de contigence s'il y a indépendence
    tab.indep = tab
    n = sum(tab)
    tab.rowSum = apply(tab, 2, sum)
    tab.colSum = apply(tab, 1, sum)
    #print(tab.colSum)
    #print(tab.rowSum)
    #print(c)
    for(i in c(1:length(tab.colSum))){
      for(j in c(1:length(tab.rowSum))){
        tab.indep[i,j] = tab.colSum[i]*tab.rowSum[j]/n
      }
    }
    
    # Calcul du X²
    force.df[1,1] = sum((tab-tab.indep)^2/tab.indep)
    # Calcul du Phi²
    force.df[2,1] = force.df[1,1]/n
    # Calcul du Cramer
    force.df[3,1] = sqrt(force.df[2,1]/(min(nrow(tab), ncol(tab))-1))
    
    force.df
    
  }, rownames=TRUE, colnames=FALSE)
  
  # conditionnalPanel3
  output$myConditionalPanel3 = renderUI({
    if(length(names(df)[grepl('factor|logical|character',sapply(df,class))])>0) {
  column(12,
         tabBox(
           width = 12,
           
           
           tabPanel("Diagramme en barres des profils-colonnes", value = "caractéristiques",
                    plotOutput("barplotProfils"),
                    tableOutput("contingency")
           ),
           tabPanel("Indices", value = "caractéristiques",
                    tableOutput("force")
                    
           ),
           
           
           
         )
  )
    }
    else{
      column(12,
             tabBox(
               width = 12,
               tabPanel("Absence de variables qualitatives" )
             ))
      
    }
  })
  
}


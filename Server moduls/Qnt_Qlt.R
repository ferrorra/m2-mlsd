Qnt_Qlt_server <- function(input,output,df) {
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
  # })
  output$quantlistvs = renderUI({
    selectInput('quantlistvs', 'Le choix de la variable quantitative', names(df)[!grepl('factor|logical|character',sapply(df,class))])
  })
  output$qualistvs = renderUI({
    selectInput('qualistvs', 'Le choix de la variable qualitative', names(df)[grepl('factor|logical|character',sapply(df,class))])
  })
  
  # Bidimensionnel
  output$barplotBi <- renderPlot({
    # Diagramme en barres entre les variables 'V2' et 'V6'
    var1<-sym(input$quantlistvs)
    var2<-sym(input$qualistvs)
    ggplot(df, aes(x = !!var1, fill = !!var2)) + geom_bar()
  })
  
  
  # Boîtes parallèles
  
  # table caractéristiques
  quant<- reactive({
    names(df)[!grepl('factor|logical|character',sapply(df,class))]
  })
  
  output$boxplotBasic <- renderPlot({
    
    d <- df
    d.stack <- melt(d, measure.vars = quant())
    # Boxplot basique
    d.stack$value <- as.numeric(d.stack$value)
    boxplot(d.stack$value ~ d.stack$variable , col="grey",
            xlab = "Modalités", ylab = "Mesures")
  })
  
  output$boxplotGgplot <- renderPlot({
    d <- df
    d.stack <- melt(d, measure.vars = quant())
    d.stack$value <- as.numeric(d.stack$value)
    # Boxplot élaborée
    qplot(x = d.stack[,2], y = d.stack[,1], 
          xlab = "Modalités", ylab = "Mesures",
          geom=c("boxplot", "jitter"), fill=d.stack[,2]) +
      theme(legend.title=element_blank())
  })
  
  
  
  # myconditionnal Panel2
  output$myConditionalPanel2 = renderUI({
  if(length(names(df)[grepl('factor|logical|character',sapply(df,class))])>0 && length(names(df)[!grepl('factor|logical|character',sapply(df,class))])>0 ) {
  column( 
    width = 12,
    tabBox(
      width = 12,
      
      title = "",
      # The id lets us use input$tabset1 on the server to find the current tab
      id = "tabset1", 
      tabPanel("Diagramme en barre", value = "caractéristiques",
               plotOutput("barplotBi")
      ),
      tabPanel("Boites parallèles", value = "caractéristiques",
               plotOutput("boxplotBasic"),
               plotOutput("boxplotGgplot")
      ),
      
      
      
    )
  )
  }
  else{
    tabBox(
      width = 12,
    tabPanel("Absence de variables quantitatives ou qualitatives" )
    )
             
      
    }
  })
}
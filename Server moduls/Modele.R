Modele_server <- function(input,output,df) {
  
  data=reactive({
    infile<-input$file
    if(is.null(infile)) return (NULL)
    delim<-get.delim(infile$datapath, n = 10, comment = "#", skip = 0, delims = c("\t", "\t| +", " ", ";", ",","."), large = 10, one.byte = TRUE)
    #print(delim)
    print("HERE !!!!!")
    print(get.ext(infile$datapath))
    if(get.ext(infile$datapath)=="xls"){
      #print("I am here xls")
      read_xls(infile$datapath)
      #print(tmpData)
      
    }else{
      #print("I am here csv")
      read.csv(infile$datapath, header=input$Header, sep =delim)
      
    }
    
    
  })
  # Modeles de classifoication
  # LINEAR REGRESSION ----
  
  # FORMULA for REGRESSION ----
  
  myformula <- reactive({
    expln <- paste(req(input$IndVar), collapse = "+")
    as.formula(paste(req(input$DepVar), " ~ ", expln))
  })
  
  mod <- eventReactive(req(input$analysis), {
    lm(myformula(), data = df)
  })
  
  output$select2 <- renderUI({
    varSelectInput(
      inputId = "DepVar",
      label = "Dependent Variable:",
      df,
      selected = F,
      multiple = FALSE
    )
  })
  output$select3 <- renderUI({
    varSelectInput(
      inputId = "IndVar",
      label = "Independent Variables:",
      df,
      selected = F,
      multiple = TRUE
    )
  })
  
  output$testq <- renderUI({
    t.lm <- tab_model(mod())
    
    HTML(t.lm$knitr)
    
  })
  
  
  output$lcoef <- renderPlot({
    plot(parameters(mod()))
  })
  
  
  
  output$diag <- renderPlot({
    performance::check_model(mod())
  }, height = 400, width = 500)
  
  
  
  
  # LOGISTIC REGRESSION ----
  
  # binary
  
  mod2 <- eventReactive(req(input$analysis), {
    glm(myformula(),
        data = df,
        family = binomial(link = "logit"))
  })
  output$testq2 <- renderUI({
    t.lm2 <- tab_model(mod2())
    
    HTML(t.lm2$knitr)
    
  })
  
  
  
  output$logiOdds <- renderPlot({
    plot(parameters(mod2()))
  })
  
  output$logi_gof <- renderPrint({
    anova(mod2(),
          update(mod2(), ~ 1),    # update here produces null model for comparison
          test = "Chisq")
    
    
  })
  
  
  # PCA
  
  output$diag2 <-  renderPlot({
    performance::check_model(mod2())
  }, height = 650, width = 750)
  
  output$pdd <- renderUI({
    ddaa <- data.frame(df)
    
    selectizeInput(
      inputId = "pc.vars",
      label = "Select variables to include in PCA",
      choices = names(ddaa[, sapply(ddaa, is.numeric)]),
      selected = T,
      multiple = T
    )
    
  })
  
  res.pca <- eventReactive(req(input$analysisPCA), {
    df <- data.frame(na.exclude(data.frame(df)))
    
    p.x <- input$pc.vars
    p.i <- NULL
    
    for (i in seq_along(p.x)) {
      p.i[i] <- which(names(df) == p.x[i])
    }
    
    
    data_num <- na.exclude(df[, c(p.i)])
    
    pca <- prcomp(req(data_num), scale. = T, center = F)
    
    return(pca)
  })
  
  
  observeEvent(!is.null(res.pca()) , {
    output$PC.info <- renderPrint({
      (summary(res.pca()))
      
    })
    
    output$PC.info2 <- renderPrint({
      (res.pca()$rotation)
      
      
    })
    
  })
  
  
  
  output$PCA <- renderPrint({
    PC <- res.pca()
    summary(PC)
    
  })
  
  
  output$dimA <- renderUI({
    pc <- data.frame(res.pca()$x)
    selectizeInput(
      inputId = "dimA",
      label = "Dimension X-axis",
      choices = names(pc),
      multiple = F
    )
    
  })
  
  output$dimB <- renderUI({
    pc <- data.frame(res.pca()$x)
    selectizeInput(
      inputId = "dimB",
      label = "Dimension Y-axis",
      choices = names(pc),
      multiple = F
    )
    
  })
  
  
  output$clumPCA <- renderUI({
    selectizeInput(
      inputId = "clu_mPCA",
      label = "Choose clustering method",
      choices = c(
        "ward.D",
        "ward.D2",
        "single",
        "complete",
        "average",
        "mcquitty",
        "median",
        "centroid",
        "kmeans"
      ) ,
      selected = "kmeans",
      multiple = F
    )
    
  })
  
  
  output$Colls <- renderUI({
    selectizeInput(
      inputId = "Colls",
      label = "Group colors",
      choices = c("Set1", "Set2", "Paired", "Dark2"),
      multiple = F
    )
    
  })
  
  
  
  output$plotlyPCA <- renderPlotly({
    pcc <- data.frame(res.pca()$x)
    
    
    p.xA <- input$dimA
    #p.xB <- input$dimB
    
    p.ixP <- NULL
    
    for (i in seq_along(p.xA)) {
      p.ixP[i] <- which(names(pcc) == p.xA[i])
      #p.ixP[i] <- which(names(pcc) == p.xB[i])
    }
    
    
    data_PCs <- na.exclude(pcc[, c(p.xA)])
    
    
    #daaPCA <- data.frame(pcc[[c(as.name(input$dimA),as.name(input$dimB))]])
    #da1PCA <- data.frame(na.exclude(daaPCA))
    
    bnPCA <-
      NbClust(data_PCs,
              method = input$clu_mPCA,
              distance = "euclidean")
    Cluster <- factor(bnPCA$Best.partition)
    
    
    
    
    theme <-
      theme(
        panel.background = element_blank(),
        panel.border = element_rect(fill = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"),
        axis.ticks = element_line(colour = "black"),
        plot.margin = unit(c(1, 1, 1, 1), "line")
      )
    
    ggplotly(
      ggplot(data = pcc, aes(
        x = .data[[as.name(input$dimA)]], y = .data[[as.name(input$dimB)]], color = Cluster
      )) +
        geom_point() +
        ggtitle("Biplot") + theme + xlab(noquote(input$dimA)) +
        ylab(noquote(input$dimB)) + scale_color_brewer(palette = input$Colls)
    )
    
    
  })
  
  
  
  outputOptions(output, "plotlyPCA", suspendWhenHidden = FALSE)
  
  
  
  
  
  
}
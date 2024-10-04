Missing_server <- function(input,output, df) {
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
  
  # gestion des valeurs manquantes 
  # imputation
  
  output$mivsx <- renderUI({
    numericInput(
      inputId = "mivx",
      label = "Max iterations",
      min = 1 ,
      max = 100,
      value = 50
    )
    
  })
  
  output$missmap <- renderPlot({
    #dfa <- data.frame()
    dfa <- df
    
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
      labs(x = "Variable",
           y = "nombre de lignes", title = "valeurs manquantes par ligne") +
      coord_flip() + theme_light() + theme(text = element_text(size = 17))
    
  }, height = 350, width = 400)
  
  
  dat_impos <- eventReactive(req(input$impute), {
    withProgress(message = "Computing...(this may take several minutes)", value =
                   0, {
                     dat_miss <- data.frame()
                     
                     dat_impo <-
                       mice(dat_miss, printFlag = F, maxit = input$mivx)
                     
                     
                     return(dat_impo)
                     
                   })
  })
  
  output$mivs <- renderUI({
    selectizeInput(
      inputId = "miv",
      label = "Choose variable",
      choices =  names( df%>% select_if(is.numeric)),
      multiple = F
    )
    
  })
  
  
  output$mi.s <- renderPlot({
    prod <- data.frame()
    impL <- complete(dat_impos())
    
    Imp <- NULL
    for (i in 1:nrow(prod)) {
      if (sum(is.na(prod[i, ])) > 0) {
        Imp[i] <- "Imputed"
      } else {
        Imp[i] <- "observed"
      }
      
    }
    
    
    impL$Imputation <- factor(Imp)
    
    g <- NULL
    for (i in 1:ncol(impL)) {
      g[i] <- list(
        ggplot(
          data = impL,
          aes_string(
            x = colnames(impL)[i],
            group = "Imputation",
            colour = "Imputation"
          )
        ) +
          geom_density(
            adjust = 1.5,
            alpha = .4,
            size = 1
          ) +
          theme_ipsum() +
          scale_colour_manual(values = c("cyan3", "blue4")) + ggtitle(paste(colnames(impL)[i])) +
          theme(legend.title = element_blank())
      )
      
    }
    
    g[which(names(prod) == input$miv)]
    
    
  }, height = 350, width = 350)
}
dataset_server <- function(input, output, df) {
  
  # Nombre de lignes
  output$nbrlignes <- renderInfoBox({
    infoBox(
      "Nombre de lignes", nrow(df), icon = icon("list", lib = "font-awesome"),
      color = "blue", fill = TRUE
    )
  })
  
  # Nombre de colonnes
  output$nbrcolonnes <- renderInfoBox({
    infoBox(
      "Nombre de colonnes", ncol(df), icon = icon("columns", lib = "font-awesome"),
      color = "blue", fill = TRUE
    )
  })
  
  # Résumé Statistique (data.desc equivalent)
  output$data_desc <- DT::renderDataTable({
    # Generate summary statistics similar to data.describe()
    summary_stats <- data.frame(
      Variable = names(df),
      Mean = sapply(df[sapply(df, is.numeric)], mean, na.rm = TRUE),
      Median = sapply(df[sapply(df, is.numeric)], median, na.rm = TRUE),
      SD = sapply(df[sapply(df, is.numeric)], sd, na.rm = TRUE),
      Min = sapply(df[sapply(df, is.numeric)], min, na.rm = TRUE),
      Max = sapply(df[sapply(df, is.numeric)], max, na.rm = TRUE)
    )
    
    DT::datatable(
      summary_stats,
      options = list(pageLength = 10, searching = FALSE),
      rownames = FALSE
    )
  })
  
  # Data Table
  output$table1 <- DT::renderDataTable({
    DT::datatable(
      df,
      options = list(
        pageLength = 10, # Adjust visible rows to 10
        scrollX = TRUE,
        searching = FALSE
      )
    )
  })
}

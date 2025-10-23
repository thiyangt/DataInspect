# Shiny app: Data Quality Analyzer
# Features:
# - upload CSV or specify package::dataset
# - Panel 1: Overview (dimensions, head, structure)
# - Panel 2: Summary using dlookr::diagnose + visdat::vis_dat
# - Panel 3: Missing value visualisations (gg_miss_upset, gg_miss_var, gg_miss_span, vis_miss)
# - Panel 4: Outlier detection (IQR rule) with boxplots and outlier table

library(shiny)
library(dplyr)
library(ggplot2)
library(DT)
library(here)

# Optional packages used for more advanced diagnostics
pkg_check <- function(pkgs){
  for(p in pkgs){
    if(!requireNamespace(p, quietly = TRUE)){
      message(sprintf("Please install '%s' for full functionality.", p))
    }
  }
}

pkg_check(c("dlookr","visdat","naniar","janitor"))

ui <- fluidPage(
  titlePanel("DataInspect: Data Quality Analyzer"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV", accept = c(".csv",".txt")),
      textInput("pkgdata", "Or specify package::dataset (e.g. datasets::iris)", value = ""),
      actionButton("load", "Load dataset"),
      hr(),
      uiOutput("choose_var_ui"),
      hr(),
      helpText("If no file or package dataset provided, sample datasets (mtcars, iris, airquality) are available."),
      selectInput("sample", "Or choose a sample dataset:", choices = c("--none--","mtcars","iris","airquality"), selected = "--none--")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Overview", value = "overview",
                 verbatimTextOutput("dims"),
                 DTOutput("head"),
                 verbatimTextOutput("str")
        ),
        tabPanel("Summary", value = "summary",
                 uiOutput("dlookr_ui"),
                 plotOutput("visdat_plot", height = "600px")
        ),
        tabPanel("Missing", value = "missing",
                 fluidRow(
                   column(6, plotOutput("plot_miss_upset", height = "350px")),
                   column(6, plotOutput("plot_miss_var", height = "350px"))
                 ),
                 fluidRow(
                   column(6, plotOutput("plot_miss_span", height = "350px")),
                   column(6, plotOutput("plot_vis_miss", height = "350px"))
                 )
        ),
        tabPanel("Outliers", value = "outliers",
                 uiOutput("outlier_ui"),
                 plotOutput("boxplots", height = "600px"),
                 DTOutput("outlier_table")
        )
      )
    )
  )
)

server <- function(input, output, session){
  # Reactive for dataset
  dataset <- eventReactive(input$load, {
    # Priority: uploaded file -> package::dataset -> sample -> NULL
    if(!is.null(input$file)){
      tryCatch(
        {
          df <- read.csv(input$file$datapath, stringsAsFactors = FALSE)
          df <- janitor::clean_names(df)
          return(df)
        }, error = function(e){
          showNotification(paste("Error reading file:", e$message), type = "error")
          return(NULL)
        }
      )
    }
    
    txt <- trimws(input$pkgdata)
    if(nzchar(txt)){
      # try to parse package::dataset or dataset
      tryCatch({
        if(grepl("::", txt)){
          parts <- strsplit(txt, "::", fixed = TRUE)[[1]]
          pkg <- parts[1]; dat <- parts[2]
          data(list = dat, package = pkg, envir = environment())
          df <- get(dat, envir = environment())
          df <- as.data.frame(df)
          df <- janitor::clean_names(df)
          return(df)
        } else {
          # try available datasets
          if(exists(txt, where = "package:datasets")){
            data(list = txt, package = "datasets", envir = environment())
            df <- get(txt, envir = environment())
            df <- as.data.frame(df)
            df <- janitor::clean_names(df)
            return(df)
          } else {
            showNotification("Could not find dataset. Try package::dataset", type = "error")
            return(NULL)
          }
        }
      }, error = function(e){
        showNotification(paste("Error loading dataset:", e$message), type = "error")
        return(NULL)
      })
    }
    
    # fallback to sample selection
    if(input$sample != "--none--"){
      df <- get(input$sample, envir = asNamespace("datasets"))
      df <- as.data.frame(df)
      df <- janitor::clean_names(df)
      return(df)
    }
    
    return(NULL)
  }, ignoreNULL = FALSE)
  
  # Update variable chooser when dataset loads
  output$choose_var_ui <- renderUI({
    df <- dataset()
    if(is.null(df)) return(NULL)
    vars <- names(df)
    selectInput("vars", "Variables (select numeric vars for outlier tab)", choices = vars, multiple = TRUE)
  })
  
  output$dims <- renderPrint({
    df <- dataset()
    if(is.null(df)) return(cat("No dataset loaded yet. Click 'Load dataset'."))
    cat("Rows:", nrow(df), "\n")
    cat("Columns:", ncol(df), "\n")
    cat("Column names:\n")
    print(names(df))
  })
  
  output$head <- renderDT({
    df <- dataset()
    if(is.null(df)) return(NULL)
    dat <- head(df, 20)
    datatable(dat, options = list(scrollX = TRUE))
  })
  
  output$str <- renderPrint({
    df <- dataset()
    if(is.null(df)) return(NULL)
    str(df)
  })
  
  # Summary: dlookr diagnose (if available)
  output$dlookr_ui <- renderUI({
    df <- dataset()
    if(is.null(df)) return(NULL)
    if(requireNamespace("dlookr", quietly = TRUE)){
      tagList(
        h4("dlookr: diagnose output"),
        DTOutput("dlookr_table")
      )
    } else {
      helpText("Install 'dlookr' package to see variable diagnostics (install.packages('dlookr'))")
    }
  })
  
  output$dlookr_table <- renderDT({
    df <- dataset()
    req(df)
    if(!requireNamespace("dlookr", quietly = TRUE)) return(NULL)
    diag <- dlookr::diagnose(df)
    datatable(diag, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  output$visdat_plot <- renderPlot({
    df <- dataset()
    req(df)
    if(requireNamespace("visdat", quietly = TRUE)){
      visdat::vis_dat(df)
    } else {
      plot.new(); text(0.5,0.5, "Install 'visdat' to see this plot")
    }
  })
  
  # Missing value plots using naniar functions
  output$plot_miss_upset <- renderPlot({
    df <- dataset()
    req(df)
    if(requireNamespace("naniar", quietly = TRUE)){
      try({
        naniar::gg_miss_upset(df)
      })
    } else {
      plot.new(); text(0.5,0.5, "Install 'naniar' to see this plot")
    }
  })
  
  output$plot_miss_var <- renderPlot({
    df <- dataset()
    req(df)
    if(requireNamespace("naniar", quietly = TRUE)){
      try({
        naniar::gg_miss_var(df)
      })
    } else {
      plot.new(); text(0.5,0.5, "Install 'naniar' to see this plot")
    }
  })
  
  output$plot_miss_span <- renderPlot({
    df <- dataset()
    req(df)
    # gg_miss_span requires a var and span_every - we attempt to pick a good numeric/time var
    if(requireNamespace("naniar", quietly = TRUE)){
      # choose first numeric column
      numcols <- names(df %>% select(where(is.numeric)))
      if(length(numcols) >= 1){
        try({
          naniar::gg_miss_span(df, var = numcols[1], span_every = 100)
        })
      } else {
        plot.new(); text(0.5,0.5, "No numeric column available for gg_miss_span")
      }
    } else {
      plot.new(); text(0.5,0.5, "Install 'naniar' to see this plot")
    }
  })
  
  output$plot_vis_miss <- renderPlot({
    df <- dataset()
    req(df)
    if(requireNamespace("naniar", quietly = TRUE)){
      naniar::vis_miss(df)
    } else {
      plot.new(); text(0.5,0.5, "Install 'naniar' to see this plot")
    }
  })
  
  # Outlier detection: IQR method for numeric variables
  outlier_results <- reactive({
    df <- dataset()
    req(df)
    nums <- df %>% select(where(is.numeric))
    if(ncol(nums) == 0) return(NULL)
    out_list <- list()
    for(v in names(nums)){
      x <- nums[[v]]
      # remove NA for calculation
      q1 <- quantile(x, probs = 0.25, na.rm = TRUE)
      q3 <- quantile(x, probs = 0.75, na.rm = TRUE)
      iqr <- q3 - q1
      lower <- q1 - 1.5 * iqr
      upper <- q3 + 1.5 * iqr
      out_idx <- which(!is.na(x) & (x < lower | x > upper))
      out_rows <- data.frame(variable = v, row = out_idx, value = x[out_idx])
      out_list[[v]] <- out_rows
    }
    out_df <- bind_rows(out_list)
    return(out_df)
  })
  
  output$outlier_ui <- renderUI({
    df <- dataset()
    req(df)
    nums <- names(df %>% select(where(is.numeric)))
    if(length(nums)==0) return(helpText("No numeric variables available."))
    tagList(
      h4("Numeric variables detected:"),
      p(paste(nums, collapse = ", "))
    )
  })
  
  output$boxplots <- renderPlot({
    df <- dataset()
    req(df)
    nums <- df %>% select(where(is.numeric))
    if(ncol(nums)==0){
      plot.new(); text(0.5,0.5, "No numeric variables to plot")
      return()
    }
    # melt for ggplot
    long <- tidyr::pivot_longer(nums, cols = everything(), names_to = "variable", values_to = "value")
    ggplot(long, aes(x = variable, y = value)) +
      geom_boxplot() +
      facet_wrap(~variable, scales = "free", ncol = 2) +
      theme(axis.text.x = element_blank())
  })
  
  output$outlier_table <- renderDT({
    out_df <- outlier_results()
    if(is.null(out_df) || nrow(out_df)==0) return(datatable(data.frame(message = "No outliers detected under IQR rule."), options = list(dom = 't')))
    datatable(out_df, options = list(pageLength = 10, scrollX = TRUE))
  })
  
}

shinyApp(ui, server)

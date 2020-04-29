server <- function(input, output) {
  
  ########################### INPUT ############################
  ########################### ********** Datatable #############
  dt <- eventReactive(input$go_csv, {
    
    str.csv <- input$port_csv
    
    str.vec <- str.csv %>% strsplit('\n') %>% unlist() %>% strsplit(',') %>% unlist()
    n_plus <- length(str.vec)
    
    w <- str.vec[seq(1,(n_plus - 1), by = 2)]
    s <- str.vec[seq(2,n_plus, by = 2)]
    
    
    res <- tibble(Weights = w[-1],
                  Stocks = s[-1]) %>% 
      mutate(Stocks = as.double(Stocks))
    
    return(res)
  })
  
  
  ########################### OUTPUT ###########################
  ########################### ********** Datatable #############
  output$dt.port <- renderDT({
    
    dt <- dt()
    
    dt %>% datatable()
    
  })
  
}
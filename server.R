server <- function(input, output) {
  
  ########################### INPUT ############################
  ########################### ********** Portfolio #############
  dt_csv <- eventReactive(input$go_csv, {
    
    str.csv <- input$port_csv
    
    str.vec <- str.csv %>% strsplit('\n') %>% unlist() %>% strsplit(',') %>% unlist()
    n_plus <- length(str.vec)
    
    s <- str.vec[seq(1,(n_plus - 1), by = 2)]
    w <- str.vec[seq(2,n_plus, by = 2)]
    
    
    res <- tibble(Stocks = s[-1],
                  Weights = w[-1]) %>% 
      mutate(Weights = as.double(Weights))
    
    return(res)
  })
  
  ########################### ********** Percentual Returns and Cumulative Returns from the Portfolio #############
  port.pret.cr <- eventReactive(input$go_csv, {
    
    # Stocks names and weights
    dt.csv <- dt_csv()
    symbols <- dt.csv$Stocks
    weights <- dt.csv$Weights
    # Start date to analyze
    aux_date <- input$date
    
    prices <- stock_prices(symbols, aux_date)
    res <- portfolio_returns_cr(prices, symbols, weights)
    
    return(res)
    
  })
  
  ########################### OUTPUT ###########################
  ########################### ********** Datatable Portfolio #############
  output$dt.port <- renderDT({
    
    dt_csv() %>% datatable()
    
  })
  
  # ########################### ********** Pie Chart #############
  # output$dt.port <- renderDT({
  #   
  #   
  # })
  
}
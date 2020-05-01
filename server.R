server <- function(input, output) {
  
  ########################### / INPUT / ############################
  ########################### Portfolio Overview #############
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
  
  ########################### ********** Prices from the stocks in the portfolio #############
  port.prices <- eventReactive(input$go_csv, {
    
    # Stocks names and weights
    dt.csv <- dt_csv()
    symbols <- dt.csv$Stocks
    weights <- dt.csv$Weights
    # Start date to analyze
    aux_date <- input$date
    
    prices <- stock_prices(symbols, aux_date)
    
    return(prices)
  })
  
  
  ########################### ********** Percentual Returns and Cumulative Returns from the Portfolio #############
  port.pret.cr <- eventReactive(input$go_csv, {
    
    # Stocks names and weights
    dt.csv <- dt_csv()
    symbols <- dt.csv$Stocks
    weights <- dt.csv$Weights
    
    prices <- port.prices()
    res <- portfolio_returns_cr(prices, symbols, weights)
    
    return(res)
    
  })
  ########################### Modified Sharpe Ratio and CAPM ######################
  ########################### ********** Market Index Returns ####################
  market_index_return <- eventReactive(input$go_sharpe, {
    
    symbols <- input$market_index
    # Start date to analyze
    aux_date <- input$date
    
    aux <- stock_prices(symbols, aux_date)
    
   res <- aux %>% 
     rename(Market = any_of(symbols)) %>% 
     mutate(PRet = if_else(Market != 0, 
                           ( Market - lag(Market) ) / lag(Market),
                           NA_real_))
    
    return(res)
    
  })
  
  ########################### ********** Proability of the ES ####################
  prob_ES <- eventReactive(input$go_sharpe, {
    
    aux <- input$p_ES_sh
    
    return(aux)
  })
  
  ########################### / OUTPUT / ###########################
  ########################### Portfolio Overview #############
  output$port_weights <- renderPlotly({
    
    data <- dt_csv()
    
    fig <- plot_ly(data, labels = ~Stocks, values = ~Weights, type = 'pie')
    fig <- fig %>% layout(title = '',
                          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
    return(fig)
  })
  
  ########################### ********** Total Growth #############
  output$last_cr <- renderPlotly({
    
    aux <- port.pret.cr() %>% 
      group_by(Stocks) %>% 
      summarise(Growth = scales::percent(last(CR))) %>% 
      dplyr::arrange(Growth)
    
    
    aux %>% 
      mutate(Stocks = factor(Stocks, levels = aux$Stocks)) %>% 
      rename(Rank = Growth) %>% 
      ggplot() +
      geom_bar(aes(x = Stocks, y = Rank),
               stat="identity", width=.5, fill="tomato3") +
      labs(title="", 
           y = 'Growth',
           caption="source: Yahoo Finance") + 
      coord_flip() +
      theme(axis.text.x = element_text(angle=0, vjust=0.6)) 
    
  })
  
  ########################### ********** Historical Prices #############
  output$price_plot <- renderPlotly({
    
    symbols <- dt_csv() %>% pull(Stocks)
    
    aux <- port.prices() %>% 
      pivot_longer(cols = any_of(symbols), names_to = 'Stocks', values_to = 'Price')
    
    
    aux %>% 
      ggplot(aes(x = Data, y = Price)) +
      geom_path(aes(colour = Stocks))
    
    
  })
  
  ########################### ********** Historical Cumulative Return #############
  output$CR_plot <- renderPlotly({
    
    aux <- port.pret.cr() %>% 
      ggplot(aes(x = Data, y = CR)) +
      geom_path(aes(colour = Stocks))
    
  })
  
  ########################### Sharpe Ratio and CAPM ######################
  ########################### ********** Portfolio versus Market Sharpe Ratio #############
  output$market_portfolio_sr <- renderPlotly({
    
    market <- market_index_return() 
    p <- prob_ES()
    
    # Using GARCH to calculate the Expected Shortfall (ES)
    g = fGarch::garchFit(~garch(1,1), market$PRet,
                         cond.dist = "norm", include.mean = FALSE, trace = FALSE)
    
    # Computing the white noise from the schock from the GARCH(1,1)
    # If our model is right it is supposed to
    # follow the distribution from the 'cond.dist' in garchFit
    #epsilon.t <- (g@residuals/g@sigma.t)
    
    # # Compute the fitted standard deviation series
    Vol_vec <- g %>% fBasics::volatility()
    VaR_vec <- Vol_vec * qnorm(p)
    ES_vec <- - (Vol_vec^2) * dnorm(-VaR_vec, sd = Vol_vec) / p
    
    res <- cbind(Vol_vec, VaR_vec, ES_vec) %>% as_tibble() %>% mutate(Index = 1:n())
    
    res %>% ggplot(aes(x = Index)) + geom_path(aes(y = Vol_vec)) +
      geom_path(aes(y = VaR_vec)) + geom_path(aes(y = ES_vec))
    
  })
 
  # End of the server function 
}


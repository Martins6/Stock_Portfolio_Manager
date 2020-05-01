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
  market_index_return <- eventReactive(input$go_sharpe_capm, {
    
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
  ########################### ********** Sharpe Ratio Portfolio versus Market #############
  output$market_portfolio_sr <- renderPlotly({
    
    # Choosing the timeframe to analyze
    if(input$period_to_analyze_sr == '1 Week Ago'){aux_data <- 7}
    if(input$period_to_analyze_sr == '2 Week Ago'){aux_data <- 15}
    if(input$period_to_analyze_sr == '1 Month Ago'){aux_data <- 31}
    if(input$period_to_analyze_sr == 'Whole Period'){aux_data <- NULL}
    
    
    # P-Value from the risk-measure: Expected Shortfall
    p <- 0.05
    # Market M. Sharpe Ratio
    market <- market_index_return() %>% drop_na()
    vec <- market$PRet - mean(market$PRet, na.rm = TRUE)
    # Using GARCH to calculate the Expected Shortfall (ES)
    g = fGarch::garchFit(~garch(1,1), vec,
                         cond.dist = "norm",
                         include.mean = FALSE, trace = FALSE)
    # # Compute the fitted standard deviation series
    Vol_vec <- g %>% fBasics::volatility()
    VaR_vec <- Vol_vec * qnorm(p)
    ES_vec <- -(Vol_vec^2) * dnorm(-VaR_vec, sd = Vol_vec) / p
    
    market <- market %>% 
      mutate(ES = abs(ES_vec),
             MarketSharpeRatio = PRet/ES)
    
    # Portfolio M. Sharpe Ratio
    port <- port.pret.cr() %>% filter(Stocks == 'Portfolio')
    vec <- port$PRet - mean(port$PRet, na.rm = TRUE)
    # Using GARCH to calculate the Expected Shortfall (ES)
    g = fGarch::garchFit(~garch(1,1), vec,
                         cond.dist = "norm",
                         include.mean = FALSE, trace = FALSE)
    # # Compute the fitted standard deviation series
    Vol_vec <- g %>% fBasics::volatility()
    VaR_vec <- Vol_vec * qnorm(p)
    ES_vec <- -(Vol_vec^2) * dnorm(-VaR_vec, sd = Vol_vec) / p
    
    port <- port %>% 
      mutate(ES = abs(ES_vec),
             PortSharpeRatio = PRet/ES) %>% 
      slice()
    
    # Plotting both data
    res <- ggplot() +
      geom_path(data = port,
                aes(x = Data, y = PortSharpeRatio), colour = 'red') +
      geom_path(data = market,
                aes(x = Data, y = MarketSharpeRatio), colour = 'blue') +
      labs(title = 'Market vs. Portfolio')
    res %>% ggplotly()
    
  })
  
  ########################### ********** CAPM Plot ################################
  output$market_portfolio_CAPM <- renderPlotly({
    
    port <- port.pret.cr() %>% drop_na() %>% filter(Stocks == 'Portfolio')
    market <- market_index_return() %>% drop_na()

    aux <- port %>%
      # Adding the market returns to the portfolio data
      mutate(M.PRet = market$PRet) %>%
      # Changing the name of the portfolio returns
      rename(P.PRet = PRet) %>% 
      # Choosing just what we want
      select(P.PRet, M.PRet)
    
    # Adjusting the linear regression
    fit <- lm(aux, formula = P.PRet ~ M.PRet)
    # Extracting coefficients
    fit.coef <- coef(fit)
    
    # Plotting
    aux %>% 
      ggplot(aes(x = M.PRet, y = P.PRet)) +
      geom_point() +
      geom_abline(slope = fit.coef[2], intercept = fit.coef[1], colour = 'purple')
    
  })
  
  ########################### ********** CAPM Beta ################################
  output$capm_beta <- renderValueBox({
    
    port <- port.pret.cr() %>% drop_na() %>% filter(Stocks == 'Portfolio')
    market <- market_index_return() %>% drop_na()
    
    aux <- port %>%
      # Adding the market returns to the portfolio data
      mutate(M.PRet = market$PRet) %>%
      # Changing the name of the portfolio returns
      rename(P.PRet = PRet) %>% 
      # Choosing just what we want
      select(P.PRet, M.PRet)
    
    # Adjusting the linear regression
    fit <- lm(aux, formula = P.PRet ~ M.PRet)
    # Extracting coefficients
    fit.coef <- coef(fit)
    
    valueBox(round(fit.coef[2], 2),
             subtitle = 'CAPM Beta',icon = icon('chart-line') )
    
  })
  
  ########################### ********** CAPM Alpha ################################
  output$capm_alpha <- renderValueBox({
    
    port <- port.pret.cr() %>% drop_na() %>% filter(Stocks == 'Portfolio')
    market <- market_index_return() %>% drop_na()
    
    aux <- port %>%
      # Adding the market returns to the portfolio data
      mutate(M.PRet = market$PRet) %>%
      # Changing the name of the portfolio returns
      rename(P.PRet = PRet) %>% 
      # Choosing just what we want
      select(P.PRet, M.PRet)
    
    # Adjusting the linear regression
    fit <- lm(aux, formula = P.PRet ~ M.PRet)
    # Extracting coefficients
    fit.coef <- coef(fit)
    
    valueBox(round(fit.coef[1],4),
             subtitle = 'CAPM Alpha',
             color = 'maroon',
             icon = icon('not-equal'))
    
  })
  
 
  # End of the server function 
}


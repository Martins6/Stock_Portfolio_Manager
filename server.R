server <- function(input, output) {
  
  ########################### / INPUT / ############################
  ########################### > Description < #####################
  ########################### Portfolio Overview #################
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
  
  ########################### ********** Date Selected Function #############
  date_selected <- eventReactive(input$go_csv, {
    
    # If the user wants a quick update
    if(input$quick_update == TRUE){
      # Choosing the timeframe to analyze
      if(input$quick_update_selection == '1 Week Ago'){aux_date <- today() - 8}
      if(input$quick_update_selection == '2 Weeks Ago'){aux_date <- today() - 15}
      if(input$quick_update_selection == '1 Month Ago'){aux_date <- today() - 31}
    }else{
      aux_date <- input$date
    }
    
    return(aux_date)
  })
  
  ########################### ********** Prices from the stocks in the portfolio #############
  port.prices <- eventReactive(input$go_csv, {
    
    # Stocks names and weights
    dt.csv <- dt_csv()
    symbols <- dt.csv$Stocks
    weights <- dt.csv$Weights
    # Start date to analyze
    aux_date <- date_selected()
    
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
  ########################### Classical Sharpe Ratio and CAPM ######################
  ########################### ********** Market Index Returns #####################
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
  ########################### > Modelling < #######################################
  ########################### Risk Modelling ######################
  ########################### ********** p-value #####################
  p.val.risk <- eventReactive(input$go_garch, {
    
    res <- input$p_risk/100 %>% as.double()
    
    return(res)
  })
  ########################### Modified Sharpe Ratio and CAPM ######################
  ########################### ********** Market Index Returns #####################
  market_index_return_model <- eventReactive(input$go_sharpe_model, {
    
    symbols <- input$market_index_model
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
  ########################### > Description < #####################
  ########################### Portfolio Overview #############
  ########################### ********** UI: In case of quick updates #############
  output$quick_update_conf <- renderUI({
    
    if(input$quick_update == TRUE){
      
    p('In this case, you should refrain from continue the in-depth study.')
    selectInput('quick_update_selection', 'What time period?', c('1 Week Ago', '2 Weeks Ago', '1 Month Ago'),
                selected = '1 Week Ago')
    }
    
  })
  ########################### ********** Portfolio Weights Pie-Chart #############
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
      summarise(Growth = (last(CR)) - 1) %>% 
      dplyr::arrange( desc(Growth) ) 
    
    
    aux %>% 
      mutate(Stocks = factor(Stocks, levels = aux$Stocks)) %>% 
      mutate(Growth = round(Growth, 4)) %>% 
      ggplot() +
      geom_bar(aes(x = Stocks, y = Growth),
               stat="identity", width=.5, fill="tomato3") +
      scale_y_continuous(labels = scales::percent) +
      labs(title="", 
           y = 'Growth',
           x = '',
           caption="source: Yahoo Finance") + 
      coord_flip() +
      theme_minimal()
    
  })
  
  ########################### ********** Historical Prices #############
  output$price_plot <- renderPlotly({
    
    symbols <- dt_csv() %>% pull(Stocks)
    
    aux <- port.prices() %>% 
      pivot_longer(cols = any_of(symbols), names_to = 'Stocks', values_to = 'Price')
    
    
    aux %>% 
      ggplot(aes(x = Data, y = Price)) +
      geom_path(aes(colour = Stocks)) +
      theme_bw()
    
    
  })
  
  ########################### ********** Historical Cumulative Return #############
  output$CR_plot <- renderPlotly({
    
    aux <- port.pret.cr() %>% 
      ggplot(aes(x = Data, y = CR)) +
      geom_path(aes(colour = Stocks)) +
      theme_bw()
    
  })
  ########################### Classical Sharpe Ratio and CAPM ######################
  ########################### ********** Sharpe Ratio Portfolio versus Market #############
  output$market_portfolio_sr <- renderPlotly({
    
    # Choosing the timeframe to analyze
    if(input$period_to_analyze_sr == '1 Week Ago'){aux_data <- 7}
    if(input$period_to_analyze_sr == '2 Weeks Ago'){aux_data <- 15}
    if(input$period_to_analyze_sr == '1 Month Ago'){aux_data <- 31}
    if(input$period_to_analyze_sr == 'Whole Period'){aux_data <- NULL}
    
    # Market M. Sharpe Ratio
    market <- market_index_return() %>% drop_na()
    # Portfolio M. Sharpe Ratio
    port <- port.pret.cr() %>% filter(Stocks == 'Portfolio')
    
    # Adjusting the dataframe to time window selected
    if(!is.null(aux_data)){
      port <- port %>% 
        mutate(PortSharpeRatio = round(PRet/sd(PRet), 4)) %>% 
        slice(( n()-aux_data ):n() )
      
      market <- market %>% 
        mutate(MarketSharpeRatio = round(PRet/sd(PRet), 4)) %>% 
        slice(( n()-aux_data ):n() )
      
    }else{
      port <- port %>% 
        mutate(PortSharpeRatio = round(PRet/sd(PRet), 4))
      
      market <- market %>% 
        mutate(MarketSharpeRatio = round(PRet/sd(PRet), 4)) 
    }
    
    # Plotting both data
    res <- ggplot() +
      geom_path(data = port,
                aes(x = Data, y = PortSharpeRatio), colour = 'red') + 
      geom_point(data = port,
                 aes(x = Data, y = PortSharpeRatio), colour = 'red') +
      geom_path(data = market,
                aes(x = Data, y = MarketSharpeRatio), colour = 'blue') +
      geom_point(data = market,
                 aes(x = Data, y = MarketSharpeRatio), colour = 'blue') +
      labs(title = 'Market vs. Portfolio',
           y = 'Sharpe Ratio',
           x = 'Date') +
      theme_bw()
      
    res %>% ggplotly()
    
  })
  
  ########################### ********** CAPM Plot ################################
  output$market_portfolio_CAPM <- renderPlot({
    
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
      geom_abline(slope = fit.coef[2], intercept = fit.coef[1], colour = 'purple') +
      labs(x = 'Market Percentual Returns',
           y = 'Portfolio Percentual Returns') +
      theme_minimal()
    
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
  ########################### > Modelling < ##############################
  ########################### Percentual Return Distribution ######################
  ########################### ********** Distribution Plot #############
  output$dist_plot <- renderPlot({
    
    port <- port.pret.cr()
    
    # Choosing the optimal bindwidth with Freedman-Diaconis
    fd <- 2 * IQR(port$PRet) / length(port$PRet)^(1/3)
    
    port %>% 
      ggplot() +
      geom_histogram(aes(x = PRet), binwidth = fd,
                     colour = 'darkblue', fill = 'lightblue') +
      labs(x = 'Returns (%)',
           y = '')
    
    
  })
  
  ########################### ********** Key-Statistics Datatable #############
  output$dist_key_stat <- renderDT({
    
    port <- port.pret.cr() %>% filter(Stocks == 'Portfolio')
    
    # Calculating the actual Volatility, VaR and ES through Historical Simulation.
    # Quantile that we wish to consider.
    p <- 0.05
    ## Our vector of Percentual Returns
    ys = sort(port$PRet)
    ## Our index of the VaR
    op = length(ys)*p
    ## The actual VaR
    VaR = -ys[op]
    ## The ES
    ES <- mean(ys[1:op])
    ## Volatility
    Vol <- sd(port$PRet) %>% round(4)
    
    aux <- c(Vol, VaR, ES) %>% round(4) %>% scales::percent()
    
    # Other interesting statistics
    skew <- PerformanceAnalytics::skewness(port$PRet) %>% round(4) 
    kurt <- PerformanceAnalytics::kurtosis(port$PRet) %>% round(4) 
    Mu <- mean(port$PRet) %>% round(4)
    
    res <- tibble(Statistic = c('Mean', 'Volatility', 'Historical Value-at-Risk (5%)',
                                'Historical Expected Shortfall (5%)', 'Skewness Coef.', 'Kurtosis Coef.'),
                  Value = c(Mu, aux, skew, kurt))
    
    res %>% datatable()
    
  })
  ########################### Risk Modelling ############################################
  ########################### ********** UI: Which Stock to Analyze #####################
  output$stock_selection_risk <- renderUI({
    
    aux <- port.pret.cr() %>% pull(Stocks) %>% unique()
    
    selectInput('stock_selection_risk_ui', 'Which Stock do you wish to study?', aux, 'Portfolio')
    
  })
  ########################### ********** Plot #############
  output$risk_plot <- renderPlotly({
    
    # Quantile that we wish
    p <- p.val.risk()
    # What kind of GARCH model
    garch.config <- input$garch_selection
    # Stock to study
    stock_selected <- input$stock_selection_risk_ui
    # Our portfolio and Stocks data
    port <- port.pret.cr() %>% filter(Stocks == stock_selected)
    
    if(stock_selected == 'Portfolio'){
      
      if(garch.config == 'N-GARCH'){
        
        g = fGarch::garchFit(~garch(1,1), port$PRet,
                             cond.dist = "norm", trace = FALSE)
        
        # compute sigma for t + 1
        sigma. = fGarch::predict(g)[1,3]
        
        # Computing the white noise from the schock from the GARCH(1,1)
        # If our model is right it is supposed to
        # follow the distribution from the 'cond.dist' in garchFit
        #epsilon.t <- (g@residuals/g@sigma.t)
        
        # Compute VAR forecast
        `VaR Forecast` = -sigma. * qnorm(p)
        # Compute ES forecast under normality
        `ES Forecast` = - (sigma.^2) * dnorm(-`VaR Forecast`, sd = sigma.) / p
        
        # # Compute the fitted standard deviation series
        Vol_vec <- g %>% fBasics::volatility()
        VaR_vec <- Vol_vec * qnorm(p)
        ES_vec <- - (Vol_vec^2) * dnorm(VaR_vec, sd = Vol_vec) / p
        
        # Computing the next step forecast
        ## Data
        `Date Forecast` <- port$Data %>% 
          last(.) + 1
        
        # Vizualising
        ## VaR
        ply <- port %>% 
          # Adding our model to the data matrix
          mutate(Vol = Vol_vec,
                 VaR = VaR_vec,
                 ES = ES_vec) %>% 
          ggplot(aes(x = Data)) +
          geom_path(aes(y = PRet)) +
          geom_path(aes(y = VaR, colour = 'Value-at-Risk')) +
          geom_path(aes(y = ES, colour = 'Expected-Shortfall')) +
          geom_point(aes(x = `Date Forecast`, y = -`VaR Forecast`, colour = 'VaR Forecast')) +
          geom_point(aes(x = `Date Forecast`, y = `ES Forecast`, colour = 'ES Forecast')) +
          scale_colour_manual(name = '',
                              values = c(`Value-at-Risk` = 'red',
                                         `Expected-Shortfall` = 'blue',
                                         `VaR Forecast` = 'darkred',
                                         `ES Forecast` =  'darkblue')) +
          labs(title = 'Model Results',
               caption = 'source: Yahoo Finance') +
          theme_bw()
        
        res <- ggplotly(ply)
      }
      
      if(garch.config == 't-GARCH'){
        
        g = fGarch::garchFit(~garch(1,1), port$PRet,
                     cond.dist = "std", include.mean = FALSE, trace = FALSE)
        
        # compute sigma for t + 1
        sigma. = fGarch::predict(g)[1,3] 
        
        # Computing the white noise from the schock from the GARCH(1,1)
        # If our model is right it is supposed to
        # follow the distribution from the 'cond.dist' in garchFit
        epsilon.t <- (g@residuals/g@sigma.t)
        
        # Estimating the distribution of the shocks.
        tstud.fit <- MASS::fitdistr(epsilon.t, 't')
        mu.tstud <- tstud.fit$estimate[1]
        sigma.tstud <- tstud.fit$estimate[2]
        df.tstud <- tstud.fit$estimate[3]
        # Which should follow a t-student with 'df.tstud'.
        standard.epsilon.t <- (epsilon.t - mu.tstud ) / sigma.tstud
        # Finding the quantile of the t-stud
        q <- fGarch::qstd(p, nu = df.tstud)
        
        # Compute VAR forecast
        `VaR Forecast` = -sigma. * q
        
        # # Compute the fitted standard deviation series
        Vol_vec <- g %>% fBasics::volatility()
        VaR_vec <- Vol_vec * q
        
        # Computing the next step forecast
        ## Data
        `Date Forecast` <- port$Data %>% 
          last(.) + 1
        
        # Vizualising
        ## VaR
        ply <- port %>% 
          # Adding our model to the data matrix
          mutate(VaR = VaR_vec) %>% 
          ggplot(aes(x = Data)) +
          geom_path(aes(y = PRet)) +
          geom_path(aes(y = VaR, colour = 'Value-at-Risk')) +
          geom_point(aes(x = `Date Forecast`, y = -`VaR Forecast`, colour = 'VaR Forecast')) +
          scale_colour_manual(name = '',
                              values = c(`Value-at-Risk` = 'red',
                                         `VaR Forecast` = 'darkred')) +
          labs(title = 'Model Results',
               caption = 'source: Yahoo Finance') +
          theme_bw()
        
        res <- ggplotly(ply)
        
      }
    }
    
    
    ############# If our Stock is not the Portfolio...
    
    if(stock_selected != 'Portfolio'){
      
    stock_price <- port.prices()[stock_selected] %>% pull()
    
    if(garch.config == 'N-GARCH'){
      
      g = fGarch::garchFit(~garch(1,1), port$PRet,
                           cond.dist = "norm", trace = FALSE)
      
      # compute sigma for t + 1
      sigma. = fGarch::predict(g)[1,3]
      
      # Computing the white noise from the schock from the GARCH(1,1)
      # If our model is right it is supposed to
      # follow the distribution from the 'cond.dist' in garchFit
      #epsilon.t <- (g@residuals/g@sigma.t)
      
      # Adjusting the prices
      price.vec <- stock_price[-1]
      lag.price.vec <- lag(stock_price) %>% na.omit()
      
      # Compute VAR forecast
      `VaR Forecast` = -sigma. * qnorm(p)
      VaR_forecast = `VaR Forecast` * last(price.vec)
      VaR_forecast_price = last(price.vec) - VaR_forecast
      # Compute ES forecast under normality
      `ES Forecast` = - (sigma.^2) * dnorm(-`VaR Forecast`, sd = sigma.) / p
      ES_forecast = `ES Forecast` * last(price.vec)
      ES_forecast_price = last(price.vec) + ES_forecast
      # # Compute the fitted standard deviation series
      Vol_vec <- g %>% fBasics::volatility()
      VaR_vec <- Vol_vec * qnorm(p)
      ES_vec <- - (Vol_vec^2) * dnorm(VaR_vec, sd = Vol_vec) / p
      
      # Computing the next step forecast
      ## Data
      `Date Forecast` <- port$Data %>% 
        last(.) + 1
      
      # Vizualising
      ply <- port %>% 
        # Adding our model to the data matrix
        mutate(price = price.vec,
               LaggedPrice = lag.price.vec,
               VaR = VaR_vec * LaggedPrice,
               ES = ES_vec * LaggedPrice,
               VaR_price = price + VaR,
               ES_price = price + ES) %>% 
        drop_na() %>% 
        ggplot(aes(x = Data)) +
        geom_path(aes(y = price)) +
        geom_path(aes(y = VaR_price, colour = 'Value-at-Risk')) +
        geom_ribbon(aes(ymin = VaR_price,
                        ymax = price + (price - VaR_price),
                        alpha = 0.3), fill = 'purple') +
        geom_path(aes(y = ES_price, colour = 'Expected-Shortfall')) +
        geom_point(aes(x = `Date Forecast`, y = VaR_forecast_price, colour = 'VaR Forecast')) +
        geom_point(aes(x = `Date Forecast`, y = ES_forecast_price, colour = 'ES Forecast')) +
        scale_colour_manual(name = '',
                            values = c(`Value-at-Risk` = 'red',
                                       `Expected-Shortfall` = 'blue',
                                       `VaR Forecast` = 'darkred',
                                       `ES Forecast` =  'darkblue',
                                       Volatility = 'purple')) +
        labs(title = 'Model Results',
             caption = 'source: Yahoo Finance') +
        theme_bw()
      
      res <- ggplotly(ply)
    }
    
    if(garch.config == 't-GARCH'){
      
      g = fGarch::garchFit(~garch(1,1), port$PRet,
                           cond.dist = "std", trace = FALSE)
      
      # compute sigma for t + 1
      sigma. = fGarch::predict(g)[1,3] 
      
      # Computing the white noise from the schock from the GARCH(1,1)
      # If our model is right it is supposed to
      # follow the distribution from the 'cond.dist' in garchFit
      epsilon.t <- (g@residuals/g@sigma.t)
      
      # Estimating the distribution of the shocks.
      tstud.fit <- MASS::fitdistr(epsilon.t, 't')
      mu.tstud <- tstud.fit$estimate[1]
      sigma.tstud <- tstud.fit$estimate[2]
      df.tstud <- tstud.fit$estimate[3]
      # Which should follow a t-student with 'df.tstud'.
      standard.epsilon.t <- (epsilon.t - mu.tstud ) / sigma.tstud
      # Finding the quantile of the t-stud
      q <- fGarch::qstd(p, nu = df.tstud)
      
      # Adjusting the prices
      price.vec <- stock_price[-1]
      lag.price.vec <- lag(stock_price) %>% na.omit()
      
      # Compute VAR forecast
      `VaR Forecast` = -sigma. * q
      VaR_forecast = `VaR Forecast` * last(price.vec)
      VaR_forecast_price = last(price.vec) - VaR_forecast
      # # Compute the fitted standard deviation series
      Vol_vec <- g %>% fBasics::volatility()
      VaR_vec <- Vol_vec * q
      
      # Computing the next step forecast
      ## Data
      `Date Forecast` <- port$Data %>% 
        last(.) + 1
      
      # Vizualising
      ply <- port %>% 
        # Adding our model to the data matrix
        mutate(price = price.vec,
               LaggedPrice = lag.price.vec,
               VaR = VaR_vec * LaggedPrice,
               VaR_price = price + VaR) %>% 
        drop_na() %>% 
        ggplot(aes(x = Data)) +
        geom_path(aes(y = price)) +
        geom_path(aes(y = VaR_price, colour = 'Value-at-Risk')) +
        geom_ribbon(aes(ymin = VaR_price,
                        ymax = price + (price - VaR_price),
                        alpha = 0.3), fill = 'purple') +
        geom_point(aes(x = `Date Forecast`, y = VaR_forecast_price, colour = 'VaR Forecast')) +
        scale_colour_manual(name = '',
                            values = c(`Value-at-Risk` = 'red',
                                       `VaR Forecast` = 'darkred')) +
        labs(title = 'Model Results',
             caption = 'source: Yahoo Finance') +
        theme_bw()
      
      res <- ggplotly(ply)
      
    }
    }
    
    return(res)
  })
  ########################### ********** Model Diagnostics Distribution Assumptions ################
  output$model_diag_hist <- renderPlot({
    
    # Quantile that we wish
    p <- p.val.risk()
    # What kind of GARCH model
    garch.config <- input$garch_selection
    # Stock to study
    stock_selected <- input$stock_selection_risk_ui
    # Our portfolio and Stocks data
    port <- port.pret.cr() %>% filter(Stocks == stock_selected)
    
    if(stock_selected == 'Portfolio'){
      
      if(garch.config == 'N-GARCH'){
        
        g = fGarch::garchFit(~garch(1,1), port$PRet,
                             cond.dist = "norm", trace = FALSE)
        
        # Computing the white noise from the schock from the GARCH(1,1)
        # If our model is right it is supposed to
        # follow the distribution from the 'cond.dist' in garchFit
        epsilon.t <- (g@residuals/g@sigma.t)
      }
      
      if(garch.config == 't-GARCH'){
        
        g = fGarch::garchFit(~garch(1,1), port$PRet,
                             cond.dist = "std", include.mean = FALSE, trace = FALSE)
        
        # Computing the white noise from the schock from the GARCH(1,1)
        # If our model is right it is supposed to
        # follow the distribution from the 'cond.dist' in garchFit
        epsilon.t <- (g@residuals/g@sigma.t)
        
      }
    }
    ############# If our Stock is not the Portfolio...
    if(stock_selected != 'Portfolio'){
      
      stock_price <- port.prices()[stock_selected] %>% pull()
      
      if(garch.config == 'N-GARCH'){
        
        g = fGarch::garchFit(~garch(1,1), port$PRet,
                             cond.dist = "norm", trace = FALSE)
        
        # Computing the white noise from the schock from the GARCH(1,1)
        # If our model is right it is supposed to
        # follow the distribution from the 'cond.dist' in garchFit
        epsilon.t <- (g@residuals/g@sigma.t)

      }
      
      if(garch.config == 't-GARCH'){
        
        g = fGarch::garchFit(~garch(1,1), port$PRet,
                             cond.dist = "std", trace = FALSE)
        
        # Computing the white noise from the schock from the GARCH(1,1)
        # If our model is right it is supposed to
        # follow the distribution from the 'cond.dist' in garchFit
        epsilon.t <- (g@residuals/g@sigma.t)
        
      }
    }
      
      # Choosing the optimal bindwidth with Freedman-Diaconis
      fd <- 2 * IQR(epsilon.t) / length(epsilon.t)^(1/3)
      
      if(garch.config == 'N-GARCH'){
        
        dist_fit <- MASS::fitdistr(epsilon.t, densfun = 'normal')
        param <- dist_fit$estimate
        
        line_dist2 <- dnorm(seq(-4,4, l = 100), mean = 0, sd = 1)
        l <- length(seq(-4,4, l = 100))
        line_dt2 <- line_dist2 %>% 
          enframe() %>% 
          mutate(name = seq(min(epsilon.t), max(epsilon.t), l = l))
        
        res <- epsilon.t %>% 
          enframe() %>% 
          ggplot() +
          geom_density(aes(x = value),
                       colour = 'darkblue', fill = 'lightblue') +
          geom_path(data = line_dt2,
                    mapping = aes(y = value,
                                  x = (name - mean(name)) )) + 
          labs(x = 'Returns (%)',
               y = '')
        
      }
      
      if(garch.config == 't-GARCH'){
        
        dist_fit <- MASS::fitdistr(epsilon.t, densfun = 't')
        param <- dist_fit$estimate
        
        l <- length(seq(-4,4, l = 100))
        line_dist2 <- fGarch::dstd(seq(-4,4, l = 100), mean = 0, sd = 1, nu = param[3])
        line_dt2 <- line_dist2 %>% 
          enframe() %>% 
          mutate(name = seq(min(epsilon.t), max(epsilon.t), l = l))
        
        
        res <- epsilon.t %>% 
          enframe() %>% 
          ggplot() +
          geom_density(aes(x = value),
                         colour = 'darkblue', fill = 'lightblue') +
          geom_path(data = line_dt2,
                    mapping = aes(y = value,
                                  x = (name - mean(name))  )) + 
          labs(x = 'Returns (%)',
               y = '')
        
      }

    return(res)
  })
  ########################### ********** Model Diagnostics LJung-Box Test ################
  output$`ljung-box` <- renderDT({
    
    # Quantile that we wish
    p <- p.val.risk()
    # What kind of GARCH model
    garch.config <- input$garch_selection
    # Stock to study
    stock_selected <- input$stock_selection_risk_ui
    # Our portfolio and Stocks data
    port <- port.pret.cr() %>% filter(Stocks == stock_selected)
    
    if(stock_selected == 'Portfolio'){
      
      if(garch.config == 'N-GARCH'){
        
        g = fGarch::garchFit(~garch(1,1), port$PRet,
                             cond.dist = "norm", trace = FALSE)
        
        # Computing the white noise from the schock from the GARCH(1,1)
        # If our model is right it is supposed to
        # follow the distribution from the 'cond.dist' in garchFit
        epsilon.t <- (g@residuals/g@sigma.t)
      }
      
      if(garch.config == 't-GARCH'){
        
        g = fGarch::garchFit(~garch(1,1), port$PRet,
                             cond.dist = "std", include.mean = FALSE, trace = FALSE)
        
        # Computing the white noise from the schock from the GARCH(1,1)
        # If our model is right it is supposed to
        # follow the distribution from the 'cond.dist' in garchFit
        epsilon.t <- (g@residuals/g@sigma.t)
        
      }
    }
    ############# If our Stock is not the Portfolio...
    if(stock_selected != 'Portfolio'){
      
      stock_price <- port.prices()[stock_selected] %>% pull()
      
      if(garch.config == 'N-GARCH'){
        
        g = fGarch::garchFit(~garch(1,1), port$PRet,
                             cond.dist = "norm", trace = FALSE)
        
        # Computing the white noise from the schock from the GARCH(1,1)
        # If our model is right it is supposed to
        # follow the distribution from the 'cond.dist' in garchFit
        epsilon.t <- (g@residuals/g@sigma.t)
        
      }
      
      if(garch.config == 't-GARCH'){
        
        g = fGarch::garchFit(~garch(1,1), port$PRet,
                             cond.dist = "std", trace = FALSE)
        
        # Computing the white noise from the schock from the GARCH(1,1)
        # If our model is right it is supposed to
        # follow the distribution from the 'cond.dist' in garchFit
        epsilon.t <- (g@residuals/g@sigma.t)
        
      }
    }
    
    btest <- epsilon.t %>% Box.test(type = 'Ljung-Box') %>% 
      broom::tidy() %>% 
      select(-parameter) %>% 
      mutate(Interpretation = if_else(p.value > p, 'Not Serially Correlated', 'Serially Correlated'),
             statistic = round(statistic, 4),
             p.value = round(p.value, 4)) %>% 
      select(method, statistic, p.value, Interpretation) %>% 
      datatable()
    
    return(btest)
  })
  
  
  ########################### Modified Sharpe Ratio ############################################
  output$market_portfolio_model_sr <- renderPlotly({
    
    # Choosing the timeframe to analyze
    if(input$period_to_analyze_model_sr == '1 Week Ago'){aux_data <- 7}
    if(input$period_to_analyze_model_sr == '2 Weeks Ago'){aux_data <- 15}
    if(input$period_to_analyze_model_sr == '1 Month Ago'){aux_data <- 31}
    if(input$period_to_analyze_model_sr == 'Whole Period'){aux_data <- NULL}
    
    # Market M. Sharpe Ratio
    market <- market_index_return_model() %>% drop_na()
    # Portfolio M. Sharpe Ratio
    port <- port.pret.cr() %>% filter(Stocks == 'Portfolio')
    
    # Quantile that we wish
    p <- p.val.risk()
    # What kind of GARCH model
    garch.config <- input$garch_selection
    
    if(garch.config == 'N-GARCH'){
      g = fGarch::garchFit(~garch(1,1), port$PRet,
                           cond.dist = "norm", trace = FALSE)
      
      Vol_vec <- g %>% fBasics::volatility()
      VaR_vec <- -Vol_vec * qnorm(p)
    }
    if(garch.config == 't-GARCH'){
      
      g = fGarch::garchFit(~garch(1,1), port$PRet,
                           cond.dist = "std", trace = FALSE)
      
      # Computing the white noise from the schock from the GARCH(1,1)
      # If our model is right it is supposed to
      # follow the distribution from the 'cond.dist' in garchFit
      epsilon.t <- (g@residuals/g@sigma.t)
      
      # Estimating the distribution of the shocks.
      tstud.fit <- MASS::fitdistr(epsilon.t, 't')
      mu.tstud <- tstud.fit$estimate[1]
      sigma.tstud <- tstud.fit$estimate[2]
      df.tstud <- tstud.fit$estimate[3]
      # Which should follow a t-student with 'df.tstud'.
      standard.epsilon.t <- (epsilon.t - mu.tstud ) / sigma.tstud
      # Finding the quantile of the t-stud
      q <- fGarch::qstd(p, nu = df.tstud)
      
      # # Compute the fitted standard deviation series
      Vol_vec <- g %>% fBasics::volatility()
      VaR_vec <- -Vol_vec * q
    }
    
    # Adjusting the dataframe to time window selected
    if(!is.null(aux_data)){
      port <- port %>% 
        mutate(PortSharpeRatio = round(PRet/VaR_vec, 4)) %>% 
        slice(( n()-aux_data ):n() )
      
      market <- market %>% 
        mutate(MarketSharpeRatio = round(PRet/VaR_vec, 4)) %>% 
        slice(( n()-aux_data ):n() )
      
    }else{
      port <- port %>% 
        mutate(PortSharpeRatio = round(PRet/VaR_vec, 4))
      
      market <- market %>% 
        mutate(MarketSharpeRatio = round(PRet/VaR_vec, 4)) 
    }
    
    # Plotting both data
    res <- ggplot() +
      # Portfolio
      geom_path(data = port,
                aes(x = Data, y = PortSharpeRatio), colour = 'red') +
      geom_point(data = port,
                 aes(x = Data, y = PortSharpeRatio), colour = 'red') +
      # Market
      geom_path(data = market,
                aes(x = Data, y = MarketSharpeRatio), colour = 'blue') +
      geom_point(data = market,
                 aes(x = Data, y = MarketSharpeRatio), colour = 'blue') +
      labs(title = 'Market vs. Portfolio') +
      theme_bw()
    
    res <- res %>% ggplotly()
    return(res)
  })
  
  
  # End of the server function 
}


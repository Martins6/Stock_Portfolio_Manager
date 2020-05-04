######################## ************ Portfolio Calculations ############################
stock_prices <- function(stocks, start.date){
  
  symbols <- stocks
  
  # In the very specific case of the brazillian market index ^BVSP on the Yahoo Finance.
  if(length(symbols) == 1 & symbols == '^BVSP'){
    # Get the data from Yahoo Finance, we will be working the names of the objects
    quantmod::getSymbols(symbols, src = 'yahoo', from = start.date,
                         auto.assign = TRUE, warnings = FALSE) 
    
    prices <- BVSP %>% quantmod::Ad()
    
    # If it has missing values, use inputation of values
    if(sum(is.na(prices)) != 0){
      prices <- prices %>%
        zoo::na.approx()
    }
    
    # Collecting the vector of dates from the xts object.
    Data <- prices %>% zoo::index() %>% lubridate::date()
    # Trasnforming from xts to tibble.
    prices <- prices %>% 
      as_tibble(.name_repair = 'unique') %>%
      `colnames<-`('Market') %>%
      mutate_all(~as.vector(.)) %>% 
      mutate(Data = Data) %>%
      drop_na()
    
  }else{
  # Constructing our Adjusted.Close dataframe with each Stock.
  prices <- 
    # Get the data from Yahoo Finance, we will be working the names of the objects
    quantmod::getSymbols(symbols, src = 'yahoo', from = start.date,
               auto.assign = TRUE, warnings = FALSE) %>% 
    # Acessing the xts objects throught the vector of names (get) and getting
    # the the Adjusted.Close (Ad) of each object
    map(~quantmod::Ad(get(.)))
  
  # If it has missing values, use inputation of values
  if(sum(is.na(prices)) != 0){
    prices <- prices %>%
      zoo::na.approx()
  }
  
  # Collecting the vector of dates from the xts object.
  Data <- prices[[1]] %>% zoo::index()
  # Trasnforming from xts to tibble.
  prices <- prices %>% 
    as_tibble(.name_repair = 'unique') %>%
    `colnames<-`(symbols) %>%
    mutate_all(~as.vector(.)) %>% 
    mutate(Data = Data) %>%
    drop_na()
  }
  
  
  return(prices)
  
}

portfolio_returns_cr <- function(prices, stocks, weights){
  
  # Inputs that we need
  symbols <- stocks
  weights <- weights
  
  # Adjusting tibble
  prices1 <- prices %>% 
    drop_na() %>%
    pivot_longer(cols = any_of(symbols)) %>% 
    rename(VQ = value)
  
  # Calculating what we want from each stock and storing
  stocks_calculations <- prices1 %>% 
    group_by(name) %>% 
    # Prices Return
    mutate(Ret = VQ - lag(VQ),
           PRet = if_else(VQ != 0, 
                          ( VQ - lag(VQ) ) / lag(VQ),
                          NA_real_)) %>% 
    # Basically, taking out the first observation
    drop_na() %>% 
    # Calculating Cumulative Return
    mutate(CR = cumprod(1 + PRet)) %>% 
    ungroup()
  
  # Calculating the Portfolio Percentual Returns (PRet) and Cumulative Return (CR)
  return_matrix <- stocks_calculations %>% 
    select(PRet, Data, name) %>% 
    pivot_wider(names_from = name, values_from = PRet) %>% 
    select(-Data) %>% 
    as.matrix()
  
  cr_matrix <- stocks_calculations %>% 
    select(CR, Data, name) %>% 
    pivot_wider(names_from = name, values_from = CR) %>% 
    select(-Data) %>% 
    as.matrix()
  
  port_pret <- return_matrix %*% weights %>% as.vector() 
  port_cr <- cr_matrix %*% weights %>% as.vector() 
  
  # Creating a dataset with everything that we want
  pret.stocks <- stocks_calculations %>% 
    select(PRet, Data, name) %>% 
    pivot_wider(names_from = name, values_from = PRet) %>% 
    mutate(Portfolio = port_pret) %>% 
    pivot_longer(cols = any_of(c(symbols, 'Portfolio')),
                 names_to = 'Stocks', values_to = 'PRet')
  
  cr.stocks <- stocks_calculations %>% 
    select(CR, Data, name) %>% 
    pivot_wider(names_from = name, values_from = CR) %>% 
    mutate(Portfolio = port_cr) %>% 
    pivot_longer(cols = any_of(c(symbols, 'Portfolio')),
                 names_to = 'Stocks', values_to = 'CR')
  
  dt_complete <- right_join(cr.stocks, pret.stocks)
  
  return(dt_complete)
}
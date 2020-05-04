########################### / HEADER / #################################
header <- dashboardHeader(title = "Portfolio Management")

########################## / SIDEBAR / #################################
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("About", tabName = "about"),
    menuItem("Descriptive", tabName = "stats"),
    menuItem("Modelling", tabName = "model")
  )
)

################################### / BODY / ##########################
body <- dashboardBody(
  tabItems(
    #####################  About Section #####################
    tabItem(tabName = "about",
            
            fluidRow(
              box(h1('About Section')),
              withMathJax()
            ),
            
            fluidRow(
              box(width = 12,
                  
                  p('"Shiny Portfolio Management" was created to help with the more quantitative understanding of stocks'),
                  p('and also apply the most used and modern techniques to stock portfolio. Hope you enjoy!'),
                  p('The app was created by Adriel Martins.'),
                  p('In this tab I will try to explain some of the key statistics of the app.'),

                  )
            ),
            
            fluidRow(
              box(h3('Cumulative Returns'))
            ),
            
            fluidRow(
              box(width = 12,
                  
                  p('It basically means "How much did one unit of currency became over this period of time?".'),
                  p('Did it became more than one unit? Or less? If we were trading in America: "How much worth is the one dollar that I spent in the beginning now?"')
                  
              )
            ),
            
            fluidRow(
              box(h3('Classical Sharpe Ratio'))
            ),
            
            fluidRow(
              box(width = 12,
                  
                  p('We want to understand our daily portfolio\'s return over volatility that means we want to understand if the return we had really compensated for the risk we took.'),
                  p('All the returns in the app were calculated using $$\\frac{(PR_t  - PR_{t-1})}{PR_{t-1}}$$'),
                  p('The single volatility was caculated using the standard-deviation of returns, which is the square root of the variance of returns.'),
                  p('We compare both the Market and Portfolio\'s Sharpe\'s Ratio, so that we can study if our Portfolio is doing better than the market.'),
                  p('If our Sharpe\'s Ratios were greater than the Market\'s that would mean we have perfomed better over the risk we took.'),
                  p('This tool is especially good to compare punctual returns, in the sense that: "Was today our return better than the market\'s?"')
                  
              )
            ),
            
            fluidRow(
              box(h3('Capital Asset Pricing Model [ CAPM ]'))
            ),
            
            fluidRow(
              box(width = 12,
                  
                  p('What if we wanted to understand the expected value of our Portfolio\'s Returns explained by the Market\'s?'),
                  p('We wish to adjust a simple model, simple linear regression using the least-square method, trying to find two coeficients: alpha and beta.'),
                  p('Our model is of such a form:'),
                  strong('$$PR_t = \\alpha + \\beta * MR_t $$'),
                  p('The meaning of such coefficients are:'),
                  em('alpha'),
                  p('If the alpha is greater than zero, we would say that this coefficient is the true greatest advantage over the market.
                  It is something that the combination of the portfolio\'s return have that the market\'s does not have. And, in a more philosophical way, even if the market had zero return (which is very unlikely),
                  we would still have an "alpha" return.'),
                  p('If the alpha is lower than zero, that would mean that you\'d have an intrisic loss in your portfolio, that is generally not a good sign.'),
                  em('beta'),
                  p('the meaning of the beta coefficient is this: for every one return of the market you would have "beta" return on your portfolio. So, if the beta coefficient is greater than one, that would mean that are proffiting more every time the market profits. If the beta coefficient is lower than one, that would mean that you are not proffiting as much as the market.
                    '),
                  
              )
            ),
            
            fluidRow(
              box(h3('Return Distribution Properties'))
            ),
            
            fluidRow(
              box(width = 12,
                  
                  p('Keep in mind that the current return distribution is just a snap photo, it is a sample distribution. The return distribution could change a lot in few days or through the time period you wish to observe.'),
                  p('When looking at our Return\'s Distribution we wish to ascertain some properties. "Is it more probable that I win more or lose more?", etc.
                    The best way to answer that questions numerically and not only graphically is through some key statistics.'),
                  strong('Mean'),
                  p('Just the average of all our returns. In the stock market, that mean generally is around zero.'),
                  strong('Volatility'),
                  p('Volatility was caculated using the standard-deviation of returns, which is the square root of the variance of returns. Intuitively, is just how far the observations are from the mean.'),
                  strong('Historical Value-at-Risk (p) [ VaR(p) ]'),
                  p('It is no more than the p-quantile of our distribution. If p was 5%, we would take the 5%-percentile of our distribution.'),
                  p('We will explore more the meaning and how to better calculate the VaR(p) in the following section'),
                  strong('Historical Expected Shortfall (p) [ ES(p) ]'),
                  p('It is the mean of returns that are equal or lower than the VaR(p). We will, just like the VaR(p), explore more of this statistic in the next section.'),
                  strong('Skewness Coefficient'),
                  p('Measures how is the symmetry of our sample distribution. If the Skew. Coefficient is 0,
                    than the distribution is perfectly symmetric. If it is significantly greater than 0, means that our Portfolio
                    tends to have more positive returns than negative. It is more likely or probable to have positive returns.
                    Likewise, if the Skew. Coefficient is less significantly than 0, our portfolio tends to have more negative returns.'),
                  strong('Kurtosis Coefficient'),
                  p('Essentialy, this statistic measures how much of the returns of our distribution are on the "tails", that means how much likely we are to have both huge gains or huge losses. We are measuring the concentration of the returns around the mean.
                    The formal understanding of this statistic is a bit tricky for those that don\'t have familiarity with the standardized Normal/Bell/Gaussian Distribution. But, just understand that we have a sort of benchmark, and we are measuring how far our sample distribution is from such benchmark.
                    If the Kurtosis Coefficient is approximate to zero, means that our distribution, have an "okay" return os the tails, we are not likekly to see huge variatons from great wins and big losses.
                    If the Coefficient is greater than 0, we have a more concentrated distribution around the mean,
                    whic makes it more predictable in a sense.
                    If it the Coefficient is less than 0, we have a more disperse distribution around the mean,
                    whic makes it less predictable, more volatile.')
              )
            ),
            
            fluidRow(
              box(h3('GARCH Model'))
            ),
            
            fluidRow(
              box(width = 12,
                  
                  p('GARCH stands for Generalized Auto Regressive Conditional Heteroscedasticity.
                    It means that we are trying to model the actual volatility (standard-deviation)
                    through past squared returns and past volatilities.'), 
                  p('We will use GARCH(1,1), which means that we will only use the one-step back volatility and squared return.
                    Our model has such a form:'),
                  strong('$$ \\sigma_t = \\omega + \\phi * \\sigma_{t-1} + \\theta * Y^2_t $$'),
                  p('Being Y the series we are using.'),
                  strong('GARCH Model Diagnostics'),
                  p('To to do so, to model in such a way, we have to assume one thing only, basically:'),
                  p('$$ \\epsilon_t = \\frac{Y_t}{\\sigma_t} $$'),
                  p('conditioned to past observations follows independently known distribution with expected value of zero and variaton of one.
                  There are only two that are most used distributions: Normal or t-Student.
                  The major differences between them is that one is more concentrated around the mean than the other. The t-Student\'s is more concentrated.
                  Or, to get more statistical, the t-Student\'s distribution has greater Kurtosis than the Normal distribution.
                  So, if you suspect that your returns are less volatile, use t-Student. Otherwise, use the Normal.
                    Those two distributions makes the two GARCH models: N-GARCH and t-GARCH.'),
                  p('As we don\'t have the theoretical Epsilons, because we don\'t have the daily volatilities, we will use our predictions of volatilities. We will call it, Hat Epsillon.'),
                  p('$$ \\hat{\\epsilon_t} = \\frac{Y_t}{\\hat{\\sigma_t}} $$'),
                  p('If our model is correct Hat Epsilon will follow the distribution we have assumed, and will also be serially independent.'),
                  p('That is why we plot the Hat Epsilons with their theoretical models,
                    and in a separate table give the result of the LJung-Box test, which test serial independence.')
                  
              )
            ),
            
    ),
    ##################### Descriptive Section #####################
    tabItem(tabName = "stats",
            
            ################## ********************  Portfolio Overview #################
            # Intro
            fluidRow(
              box(h3('Portfolio Overview'),
                  p('For a weekly update or the start of a in-depth study, both for descriptive or modelling.'),
                  )
            ),
            
            fluidRow(
              # Portfolio Input
              box(title = "Your Portfolio", width = 4, height = 300,
                  solidHeader = TRUE, status = 'primary',
                  textAreaInput('port_csv',
                                height = '150px',
                                'Enter in the CSV format here',
                                value = 'Stock,Weight,\nAAPL,0.25,\nAMZN,0.25,\nTSLA,0.25,\nSQ,0.25,'),
                  #helpText('Or, upload your own CSV below:'),
                  #fileInput('port_csv_upload'),
              ),
              
              # Date
              box(title = "", width = 3, solidHeader = TRUE, #height = '300px',
                  dateInput('date', 'Since when do you want to analyze your Portfolio?',
                            value = start_date),
                  awesomeCheckbox('quick_update', 'Want some quick update?',
                                  value = F),
                  uiOutput('quick_update_conf'),
                  actionButton('go_csv', 'Submit')
              ),
              
              # Display Portfolio on a Datatable
              box(title = "Portfolio Weights", width = 5, solidHeader = TRUE,
                  plotlyOutput('port_weights')
              )
            ),
            
            fluidRow(
              # Portfolio's Cumulative Return by Stock
              box(title = "Stock's Growth", width = 6, solidHeader = TRUE,
                  plotlyOutput('last_cr')
              ),
              
              # Prices and Historical Cumulative Return
              tabBox(
                width = 6,
                title = "Historical Prices and Cumulative Returns",
                id = "tabset_return",
                tabPanel("Prices", plotlyOutput('price_plot')),
                tabPanel("Cumulative Returns", plotlyOutput('CR_plot'))
              )
              
            ),
            
            ################## ********************  Classic Sharpe Ratio and CAPM #################
            # Intro
            fluidRow(
              box(h3('Classical Sharpe Ratio and CAPM'),
                  p('In this section, we wish to compare our Portfolio with the Market, in order to understand how well we are perfoming against the market.'),
                  p('For better understanding of the statistics, please checkout the About tab.'),
                  p('Keep in mind that in this section, the more data we have the better our analysis is.')
                  ),
              
              # Inputs
              box(title = '', width = 4, solidHeader = TRUE,
                  helpText('For more precise results, please use at least one year worth of data.'),
                  textInput('market_index',
                            'Which Market Index (or ETF) do you wish to compare your Portfolio?',
                            value = 'SPY'),
                  actionButton('go_sharpe_capm', 'Submit')
              )
            ),
            
            fluidRow(
              # Sharpe Ratio comparison
              box(title = 'Comparing Classical Sharpe Ratio (Return/Volatiltiy)', width = 10,
                  plotlyOutput('market_portfolio_sr')
                  ),
              box(title = '', width = 2,
                  selectInput('period_to_analyze_sr',
                              label = 'Since when do you wish to analyze?', 
                              choices = c('1 Week Ago', '2 Weeks Ago', '1 Month Ago', 'Whole Period'),
                              multiple = FALSE,
                              selected = '1 Week Ago'
                              )
                  )
            ),
            
            fluidRow(
              # CAPM 
              box(title = 'Capital Asset Pricing Model (CAPM)', width = 12,
                  plotOutput('market_portfolio_CAPM')
              )
            ),
            
            fluidRow(
              # CAPM Beta 
              valueBoxOutput('capm_beta', width = 6),
              # CAPM Alpha
              valueBoxOutput('capm_alpha', width = 6)
            )
            
     # End of the Descriptive Section       
    ),
    #################################### Modelling Section #####################
    tabItem(tabName = "model",
            
            ################## ********************  Percentual Return Distribution #################
            # Intro
            fluidRow(
              box(h3('Percentual Return Distribution'),
                  p('Understanding the probabilistic properties of the return in our Portfolio through some key statistics.'),
                  p('For better understanding of the statistics, please checkout the About tab.'),
                  p('Keep in mind that in this section, the more data we have the better our analysis is.')
              )
            ),
            
            fluidRow(
              # Distribution Plot
              box(title = 'Distribution Plot', width = 6,
                  plotOutput('dist_plot')
              ),
              # Distribution Key-Statistics
              box(title = 'Key Statistics', width = 6,
                  DTOutput('dist_key_stat')
              )
              
            ),
            
            
            ################## ******************** Risk Modelling #######################
            fluidRow(
              box(h3('Risk Modelling'),
                  p('In this section, we wish to create confidence intervals and forecast the VaR and ES of the fowarding day.'),
                  p('For better understanding of the statistics, please checkout the About tab.'),
                  p('Keep in mind that in this section, the more data we have the better our analysis is.')
              ),
            ),
            
            fluidRow(
              # GARCH Plot
              box(title = '', width = 10,
                    plotlyOutput('risk_plot')
                    ),
              # Choosing GARCH Model  
              box(title = '', width = 2,
                  uiOutput('stock_selection_risk'),
                  selectInput('garch_selection', 'Which GARCH Model do you wish to use?',
                                choices = c('N-GARCH', 't-GARCH'),
                                selected = 'N-GARCH', multiple = FALSE),
                  numericInput('p_risk',label = 'What quantile do you wish to consider? Or, how much risk? (%)',
                               value = 5, min = 0.1, max = 15),
                  actionButton('go_garch', 'Submit')
                    )
            ),
            
            fluidRow(
              # Diagnostics of the model
              box(title = 'Model Diagnostics: Theoretical vs. Empirical Distribution', width = 6,
                  plotOutput('model_diag_hist')
              ),
              
              box(title = 'Is it serially independent?', width = 6,
                  DTOutput('ljung-box'))
              
            ),
            ################## ******************** Modified Sharpe Ratio #################
            # Intro
            fluidRow(
              box(h3('Modified Sharpe Ratio'),
                  p('In this section, we wish to compare our Portfolio Returns with the Market Returns, in order to understand how well we are perfoming against the market.'),
                  p('For better understanding of the statistics, please checkout the About tab.'),
                  p('Keep in mind that in this section, the more data we have the better our analysis is.')
              ),
              
              # Inputs
              box(title = '', width = 4, solidHeader = TRUE,
                  helpText('For more precise results, please use at least one year worth of data.'),
                  helpText('Keep in mind that the VaR is calculated by the model above.'),
                  textInput('market_index_model',
                            'Which Market Index (or ETF) do you wish to compare your Portfolio?',
                            value = 'SPY'),
                  actionButton('go_sharpe_model', 'Submit')
              )
            ),
            
            fluidRow(
              # Sharpe Ratio comparison
              box(title = 'Comparing Modified Sharpe Ratio (Return/VaR)', width = 10,
                  plotlyOutput('market_portfolio_model_sr')
              ),
              box(title = '', width = 2,
                  selectInput('period_to_analyze_model_sr',
                              label = 'Since when do you wish to analyze?', 
                              choices = c('1 Week Ago', '2 Weeks Ago', '1 Month Ago', 'Whole Period'),
                              multiple = FALSE,
                              selected = '1 Week Ago'
                  )
              )
            )  
            
      # End of the Modelling Section
    )
    # End of the tabitems
  )
  # End of the body
)

############################## Putting it all together in the UI #################################
ui <- dashboardPage(
  skin = 'blue',
  header, sidebar, body)

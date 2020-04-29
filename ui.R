########################### / HEADER / #################################
header <- dashboardHeader(title = "Gestão Econométrica")

########################## / SIDEBAR / #################################
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("About", tabName = "about"),
    menuItem("App", tabName = "app")
  )
)

################################### / BODY / ##########################
body <- dashboardBody(
  tabItems(
    ##################### ******************** About Section #####################
    tabItem(tabName = "about",
            
            fluidRow(
              box(h1('About Section'))
            ),
            
            fluidRow(
              box(width = 12,
                  
                  p('"Shiny Portfolio Management" was created to help with the more quantitative understanding of stocks'),
                  p('and also apply the most used and modern techniques to stock portfolio. Hope you enjoy!'),
                  p('The app was created by Adriel Martins.')
                  
                  )
            ),
    ),
    ##################### ******************** App Section #####################
    tabItem(tabName = "app",
            
            fluidRow(
              # Portfolio Input
              box(title = "Your Portfolio", width = 6, solidHeader = TRUE, status = 'primary',
                  textAreaInput('port_csv', 'Enter in the CSV format here', value = 'Stock,Weight'),
                  #helpText('Or, upload your own CSV below:'),
                  #fileInput('port_csv_upload'),
                  actionButton('go_csv', 'Submit')
              ),
              # Display Portfolio on a Datatable
              box(title = "Datatable", width = 6, solidHeader = TRUE,
                  DTOutput('dt.port')
              )
              
            )
    )
    # End of the tabitems
  )
  # End of the body
)

############################## Putting it all together in the UI #################################
ui <- dashboardPage(
  skin = 'blue',
  header, sidebar, body)

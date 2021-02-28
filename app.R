library(shiny)
library(readxl)
library(memoise)
library(DT)


cleantech_cat = c('Agriculture & Waste',
                  'Chemicals & Advanced Materials',
                  'Energy Distribution & Storage',
                  'Energy Efficiency',
                  'Energy Generation',
                  'Green Buildings',
                  'Information & Communication Tech',
                  'Transportation',
                  'Water')
expertise = c('Agriculture and Farming',
              'Architecture and Engineering',
              'Arts and Entertainment',
              'Automotive',
              'Business Services',
              'Business Strategy & Planning',
              'Construction',
              'Education',
              'Finance and Insurance',
              'Finances & Accounting',
              'Forestry, Fishing and Hunting',
              'Fundraising & Business Development',
              'Government',
              'Health Care',
              'Home Improvement',
              'Human Resources & Team Building',
              'Information Technology and Web Services',
              'Legal & IP',
              'Manufacturing',
              'Marketing & Communications',
              'Media and Information',
              'Mining, Oil and Gas',
              'Operations',
              'Product Development - Hardware',
              'Product Development - Services',
              'Product Development - Software',
              'Real Estate',
              'Recreation and Travel',
              'Restaurants and Hospitality',
              'Retail and Wholesale Trade',
              'Sales',
              'Supply Chain & Distribution',
              'Sustainability',
              'Technology & IT Services',
              'Transportation and Warehousing',
              'User Experience & Design',
              'Utilities',
              'Waste Management',
              'Other')

password = 'cleantech'



get_show = memoise(function(data, index, column){

  if (length(index)==0) return(NULL)
  data[index, column]

})

read_data = memoise(function(){
  mentors <- read_excel("MASTER MENTOR LIST - Demo.xlsx", sheet = 1, col_names = TRUE)
  names(mentors) <- c("Name", "Email", "Mentor tagline", "Linkedin", "Twitter", "Mobile Number", "Mentor Region", "Company", "Title", "Company Website", "Company Stages", "Most Convenient City to Meet in Person", "Mentee Desired Characteristics", "Mentoring Goals", "Concise Bio", "Generalist or Specialist", "Entrepreneurial Experience", "Past Mentoring Experience", "Cleantech Categories", "Product Expertise", "Functional Expertise", "Industry Expertise", "Func_Ind Expertise", "Expertise Keywords")
  mentors_copy <- mentors
  mentors_copy
})


ui <- fluidPage(
  
  # Title
  titlePanel("Mentor Matching Engine"),
  
  #Intro
  fluidRow(
    column(10,
           h4("This tool is meant for seeking mentors with certain expertise or background.")),
    column(12,
           h6("Copyright: Zihang Pan, Global Key Advisors. Contact: ggpanzihang@gmail.com"))
  ),
  
  
  fluidRow(
    column(7,
           h4("Step 1: Please enter the password to see results")
           )
  ),
  
  fluidRow(
    column(2,
           passwordInput("password", "password:")
    )
  ),
  
  fluidRow(
    column(12,
   h4("Step 2: Search among expertises, categories, companies and other keywords")) 
  ),
  
  # input and output
  fluidPage(
    br(),
    fluidRow(
      #Input: search 'column' for 'keyword'
      column(5,
      selectizeInput(
        'expertise_input',
        'What area(s) of industry or functional expertise are you seeking in a mentor?',
        choices = expertise, multiple = TRUE
      )),
      
      column(5,
      selectizeInput(
        'category_input',
        'What area(s) of cleantech categories are you seeking in a mentor?',
        choices = cleantech_cat, multiple = TRUE
      ))
    
    ),
    
    fluidRow(
      column(5,
      textInput(
        'company',
        'What company/organizational affiliations ideally would you like this mentor to have?'
      )),
      column(5,
             br(),
             h5("For other keywords, use 'Search:' below:"))
      
      
    ),
    

    sidebarPanel(
        checkboxGroupInput("show_vars",
                           "Columns to show:",
                           c("Name", "Email", "Mentor tagline", "Linkedin", "Twitter", "Mobile Number", "Mentor Region", "Company", "Title", "Company Website", "Company Stages", "Most Convenient City to Meet in Person", "Mentee Desired Characteristics", "Mentoring Goals", "Concise Bio", "Generalist or Specialist", "Entrepreneurial Experience", "Past Mentoring Experience", "Cleantech Categories", "Product Expertise", "Functional Expertise", "Industry Expertise", "Expertise Keywords"),
                           selected = c("Name",
                                        "Mentor tagline",
                                        "Company",
                                        "Cleantech Categories",
                                        "Functional Expertise",
                                        "Industry Expertise"))
    ),

    mainPanel(
      h4(textOutput("text")),
      br(),
      DT::dataTableOutput("result")
    )
  )
)



server <- function(input, output, session) {
  
  
  ################################################
  
  get_data <- reactive({

      withProgress({
        setProgress(message = "Searching...")
        
        mentors_copy <- read_data()

        if (is.null(input$category_input))
        {cat_index = seq(1,nrow(mentors_copy))}
        else
        {cat_index = grep(paste(sort(input$category_input), collapse = ".*"),mentors_copy$'Cleantech Categories')}
        if (is.null(input$expertise_input))
        {exp_index = seq(1,nrow(mentors_copy))}
        else
        {exp_index = grep(paste(sort(input$expertise_input), collapse = ".*"),mentors_copy$'Func_Ind Expertise')}
        if (input$company=='')
        {company_index = seq(1,nrow(mentors_copy))}
        else
        {company_index_1 = grep(input$company, mentors_copy$'Company', ignore.case = TRUE)
         company_index_2 = grep(input$company, mentors_copy$'Concise Bio', ignore.case = TRUE)
         company_index_3 = grep(input$company, mentors_copy$'Past Mentoring Experience', ignore.case = TRUE)
         company_index = union(union(company_index_1, company_index_2), company_index_3)
        }

        if ((length(cat_index)+length(exp_index)+length(company_index))==0)
        {final_index = (seq(1,nrow(mentors)))}
        else
        {final_index = intersect(intersect(cat_index, exp_index), company_index)}

        column = input$show_vars
        
        output$text <- renderText(paste('Number of results: ', length(final_index)))
        get_show(mentors_copy, final_index, column)
        
      })

  })
 
  
  output$result <- DT::renderDataTable({

      if (input$password == password)
      {mentors_copy <- get_data()}
      else {mentors_copy <- data.frame(password = c('PASSWORD'), incorrect = c('INCORRECT'))}

  })

  

}

shinyApp(ui, server)
## app.R ##
library(shinydashboard)
library(shinyjs)

#contact form
fieldsMandatory <- c("name", "email")

labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

appCSS <-
  ".mandatory_star { color: red; }"

ui <- dashboardPage(skin = "blue",
  dashboardHeader(title = "Data Analysis",
                  dropdownMenu(type = "messages",
                               messageItem(
                                 from = "Support",
                                 message = HTML("<div id='messsageSupport1'>Welcome to Data Analytics</div>
                                                 <div id='messsageSupport2'>for Policy Formulation!</div>"),
                                 icon = icon("hand-peace-o")
                               )
                  )),
  
  ## Sidebar content
  dashboardSidebar(
    # Custom CSS to hide the default logout panel
    tags$head(tags$style(HTML('.shiny-server-account { display: none; }'))),
    
    # The dynamically-generated user panel
    uiOutput("userpanel"),
    
    sidebarMenu(
      #sidebarSearchForm(textId = "searchText", buttonId = "searchButton",label = "Search..."),
      #For this search form, the corresponding values in the server-side code would be input$searchText and input$searchButton.
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Help", tabName = "help", icon = icon("info-circle")),
      menuItem("Data", tabName = "data", icon = icon("database")),
      menuItem("Parameters", tabName = "parameters", icon = icon("anchor")),
      menuItem("Summary", tabName = "summary", icon = icon("glyphicon glyphicon-book",lib = "glyphicon")),
      menuItem("Correlations", tabName = "correlation", icon = icon("table")),
      menuItem("Histograms", tabName = "histogram", icon = icon("bar-chart")),
      menuItem("Scatter Plots", tabName = "scatterPlot", icon = icon("line-chart")),
      menuItem("Regression Output", tabName = "regressionOutput", icon = icon("flash")),
      menuItem("Residual Plots", tabName = "residualPlot", icon = icon("line-chart")),
      menuItem("Residual Histograms", tabName = "residualhistogram", icon = icon("bar-chart")),
      menuItem("Residual Scatter Plots", tabName = "residualscatterPlot", icon = icon("line-chart")),
      menuItem("Source code", icon = icon("file-code-o"), 
               href = "https://github.com/chaudharyparth/Data-Analytics.git"),
      #menuItem("Ping us!", tabName = "message", icon = icon("paper-plane"))
    )
  ),
  ## Body content
  dashboardBody(
    #link an external css file
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    tabItems(
      # First tab content
      tabItem(tabName = "home",
              h1("Data Analytics", span("for Policy Formulation", style = "font-weight: 300"), 
                 style = "font-family: 'Source Sans Pro';
                 color: #fff; text-align: center;
                 background-image: url('texturebg.png');
                 padding: 10px"),
              fluidRow(
                box(
                  title = "About this project", status = "success", solidHeader = TRUE,
                  h3("Welcome to Data Analytics for Policy Formulation!"),
                  h4("This is a ",
                     a(href = 'http://shiny.rstudio.com', 'Shiny'),
                     "web application to analyze factors responsible for the student dropout in our country, powered by",
                     a(href = 'https://github.com/rstudio/shinydashboard/', 'shinydashboard'),
                     'and',
                     a(href = 'https://github.com/rstudio/shinyjs/', 'shinyjs.')),
                  
                  h4("Click", em("Data"), " in the sidepanel to get started"),
                  
                  h4('Copyright 2016 By PC. ',
                     a(href = 'http://www.apache.org/licenses/LICENSE-2.0', 'Terms of Use.'))
                )
              )
              ),
      
      #Help tab content
      tabItem(tabName = "help",
              HTML("<h4>This section guides you about tips to use the application.</h4>"),
              img(src="help.jpg",height = 600, width = 1050)
              ),
      
      # Data tab content
      tabItem(tabName = "data",
              fluidRow(
                box(
                  title = "Primary Dataset", status = "primary", solidHeader = TRUE,
                  "Choose the data to use",
                  selectInput('datafile_name_coded', '', c("FinalVariables"),multiple = FALSE)
                )
              ),
              
              fluidRow(
                box(
                  title = "Choose the dependent variable", status = "danger", solidHeader = TRUE,
                  "",
                  selectInput("dependent_variable","Dependent variable",  choices=c("dependent Variable"),selected=NULL, multiple=FALSE)
                ),
                box(
                  title = "Choose the independent variable", status = "warning", solidHeader = TRUE,
                  "",
                  selectInput("independent_variables","Independent variables",  choices = c("independent Variables"), selected=NULL, multiple=TRUE)
                )
              ),
              
              fluidRow(
                box(
                  title = "Glossary", status = "success", solidHeader = TRUE,
                  HTML("<table class='table table-bordered'>"),
                  HTML("
                   <tr>
                   <td>SD</td>
                   <td>Student Dropout (%)</td>
                   </tr>
                   <tr>
                   <td>POP</td>
                   <td>Population (crore)</td>
                   </tr>
                    <tr>
                   <td>SR</td>
                   <td>Sex Ratio (Females/1000 Males)</td>
                   </tr>
                    <tr>
                   <td>TL</td>
                   <td>Total Literacy (%)</td>
                   </tr>
                  <tr>
                   <td>GG</td>
                   <td>Gender Gap (%)</td>
                   </tr>
                    <tr>
                   <td>MMR</td>
                   <td>Maternal Mortality Rate (per 100000 live births)</td>
                   </tr>
                    <tr>
                   <td>BR</td>
                   <td>Birth Rate (%)</td>
                   </tr>
                    <td>TR</td>
                   <td>Transport</td>
                   </tr>
                   "),
                  HTML("</table>")
                )
              )
      ),
      
      # Paramter tab content
      tabItem(tabName = "parameters",
              actionButton("action_parameters", "Show/Update Results"),
              tags$hr(),
              fluidRow(
                  box(
                    title = "Parameters", status = "success", solidHeader = TRUE,
                    "Summary of key parameters",
                    HTML("<br>"),
                    tableOutput('parameters')
                  ),
                  
                  column(width = 4,
                         box(
                           title = "Dependent Variable", status = "danger", width = NULL,solidHeader = TRUE,
                           tableOutput('dependent_variable')
                         ),
                         box(
                           title = "Independent Variables", status = "warning", width = NULL,solidHeader = TRUE,
                           tableOutput('independent_variables')
                         )
                  )

              )
              
              
      ),
      
      # Summary tab content
      tabItem(tabName = "summary",
              actionButton("action_parameters", "Show/Update Results"),
              tags$hr(),
              fluidRow(
                box(
                  title = "Summary", width = 12,status = "info", solidHeader = TRUE,
                  "",
                  HTML("<br>"),
                  tableOutput('summary')
                )
              )
      ),
      
      # Correlation tab content
      tabItem(tabName = "correlation",
              fluidRow(
                box(
                  title = "Correlation",status = "primary", collapsible = TRUE, background = "light-blue",
                  "
A correlation matrix is used to investigate the dependence between multiple variables at the same
time. The result is a table containing the correlation coefficients between each variable and the
others.There are different methods for correlation analysis : Pearson parametric correlation test,
Spearman and Kendall rank-based correlation analysis.",
                  HTML("<br/>"),
                  "
The Pearson correlation coefficient indicates the strength of a linear relationship between two
variables, but its value generally does not completely characterize their relationship. In
particular, if the conditional mean of Y given X, denoted E(Y|X), is not linear in X, the
correlation coefficient will not fully determine the form of E(Y|X).",
                  HTML("<br/>"),
                  "
Correlation is a technique for investigating the relationship between two quantitative, continuous
variables, for example, age and blood pressure. Pearson's correlation coefficient (r) is a measure
of the strength of the association between the two variables.
                  "
                )
              ),
              actionButton("action_parameters", "Show/Update Results"),
              tags$hr(),
              fluidRow(
                box(
                  title = "Correlation Table",status = "info", solidHeader = TRUE,
                  "",
                  tableOutput('correlation')
                )
              )
      ),
      
      # Histogram tab content
      tabItem(tabName = "histogram",
              fluidRow(
                box(
                  title = "Histogram",status = "primary", collapsible = TRUE, background = "light-blue",
                  "A histogram is a graphical representation of the distribution of numerical data. It 
                  is an estimate of the probability distribution of a continuous variable (quantitative variable) 
                  and was first introduced by Karl Pearson. To construct a histogram, the first step is to 'bin'
                  the range of values-that is, divide the entire range of values into a series of intervals-and then
                  count how many values fall into each interval. The bins are usually specified as consecutive, 
                  non-overlapping intervals of a variable. The bins (intervals) must be adjacent, and are usually equal 
                  size.",
                  HTML("<br/>"),
                  "Histograms give a rough sense of the density of the underlying distribution of the data, and often for 
                  density estimation: estimating the probability density function of the underlying variable. The total 
                  area of a histogram used for probability density is always normalized to 1. If the length of the intervals
                  on the x-axis are all 1, then a histogram is identical to a relative frequency plot."
                )
              ),
              fluidRow(
                box(
                  title = "Select the name of variable to see(must be a variable in dataset)",status = "danger", solidHeader = TRUE,
                  selectInput("hist_var","Select Variable",  choices=c("attributes used"),selected=NULL, multiple=FALSE),
                  tags$hr(),
                  actionButton("action_Histograms", "Show/Update Results"),
                  HTML("<br>")
                  ),
                
                box(
                  title = "Histogram",status = "info", solidHeader = TRUE,
                  "",
                  div(class="span6",plotOutput('histogram'))
                )
              )
      ),
      # ScatterPlot tab content
      tabItem(tabName = "scatterPlot",
              fluidRow(
                box(
                  title = "Scatter Plot",status = "primary", collapsible = TRUE, background = "light-blue",
                  "A scatterplot is a useful summary of a set of bivariate data (two variables), usually drawn before
working out a linear correlation coefficient or fitting a regression line. It gives a good visual
picture of the relationship between the two variables, and aids the interpretation of the
correlation coefficient or regression model. ",
                HTML("<br/>"),  
                "A scatter plot is a type of plot or mathematical
diagram using Cartesian coordinates to display values for typically two variables for a set of
data. If the points are color-coded you can increase the number of displayed variables to three.
The data is displayed as a collection of points, each having the value of one variable determining
the position on the horizontal axis and the value of the other variable determining the position on
the vertical axis."
                )
              ),
              fluidRow(
                box(
                  title = "Select the name of variable to see(must be a variable in dataset)",status = "danger", solidHeader = TRUE,
                  selectInput("scatter1","x-axis",  choices=c("attributes used"),selected=NULL, multiple=FALSE)
                   ),
                
                box(
                  title = "Select the name of variable to see(must be a variable in dataset)",status = "danger", solidHeader = TRUE,
                  selectInput("scatter2","y-axis",  choices=c("attributes used"),selected=NULL, multiple=FALSE)
                ),
                column(width = 6,
                actionButton("action_scatterplots", "Show/Update Results"),
                HTML("<br>"),
                tags$br()
                )
              ),
              
              fluidRow(
                box(
                  title = "The Scatter Plot",status = "info", solidHeader = TRUE,
                  "",
                  div(class="span6",plotOutput('scatter'))
                )
              )
      ),
      
      # Regression Output tab content
      tabItem(tabName = "regressionOutput",
              fluidRow(
                box(
                  title = "Regression Analysis", status = "primary", collapsible = TRUE, background = "light-blue",
                  "In statistical modeling, regression analysis is a statistical process for estimating the relationships
among variables. It includes many techniques for modeling and analyzing several variables,
when the focus is on the relationship between a dependent variable and one or more independent
variables (or 'predictors').",
                  HTML("<br/>"),
"More specifically, regression analysis helps one understand how the
typical value of the dependent variable (or 'criterion variable') changes when any one of the
independent variables is varied, while the other independent variables are held fixed. Most
commonly, regression analysis estimates the conditional expectation of the dependent variable
given the independent variables â that is, the average value of the dependent variable when the
independent variables are fixed.",
                  HTML("<br/>"),
"Less commonly, the focus is on a quantile, or other location
parameter of the conditional distribution of the dependent variable given the independent
variables. In all cases, the estimation target is a function of the independent variables called the
regression function. In regression analysis, it is also of interest to characterize the variation of the
dependent variable around the regression function which can be described by a probability
distribution."
                )
              ),
              fluidRow(
                box(
                  title = "Regression Table",status = "info", solidHeader = TRUE,
                  "",width = 5,
                  tableOutput("regression_output")
                )
              ),
              fluidRow(
                box(
                  title = "R square", status = "danger", collapsed = TRUE, collapsible = TRUE, solidHeader = TRUE,
                  "The coefficient of determination (denoted by R 2 ) is a key output of regression analysis. It is
interpreted as the proportion of the variance in the dependent variable that is predictable from the
independent variable.",
                  HTML("<br/>"),
                  HTML("<ol>"),
                  "The coefficient of determination is the square of the correlation (r) between predicted y
scores and actual y scores; thus, it ranges from 0 to 1.",
                  HTML("</ol><ol>"),
                  "With linear regression, the coefficient of determination is also equal to the square of the
correlation between x and y scores.",
                  HTML("</ol><ol>"),
                  "An R 2 of 0 means that the dependent variable cannot be predicted from the independent
variable.",
                  HTML("</ol><ol>"),
                  "An R 2 of 1 means the dependent variable can be predicted without error from the
independent variable.",
                  HTML("</ol><ol>"),
                  "An R 2 between 0 and 1 indicates the extent to which the dependent variable is
predictable. An R 2 of 0.10 means that 10 percent of the variance in Y is predictable
from X; an R 2 of 0.20 means that 20 percent is predictable; and so on.",
                  HTML("</ol>"),
                  img(src="r2.png",height = 200, width = 200),
                  tableOutput("rSquare")
                ),
                box(
                  title = "Adjusted R square", status = "success",collapsed = TRUE, collapsible = TRUE, solidHeader = TRUE,
                  "The use of an adjusted R 2 (pronounced R bar squared) is an attempt to
take account of the phenomenon of the R 2 automatically and spuriously increasing when extra
explanatory variables are added to the model. It is a modification due to Theil of R 2 that adjusts
for the number of explanatory terms in a model relative to the number of data points.",
                  HTML("<br/>"),
                  "The adjusted R 2 can be negative, and its value will always be less than or equal to that of R 2 .
Unlike R 2 , the adjusted R 2 increases only when the increase in R 2 (due to the inclusion of a new
explanatory variable) is more than one would expect to see by chance. If a set of explanatory
variables with a predetermined hierarchy of importance are introduced into a regression one at a
time, with the adjusted R 2 computed each time, the level at which adjusted R 2 reaches a
maximum, and decreases afterward, would be the regression with the ideal combination of
having the best fit without excess/unnecessary terms.",
                  HTML("<br/>"),
                  img(src="adjr2.png",height = 170, width = 170),
                  tableOutput("adjustedrSquare")
                ),
                box(
                  title = "F statistic", status = "warning",collapsed = TRUE, collapsible = TRUE, solidHeader = TRUE,
                  "The F statistic is a ratio of 2 different measure of variance for the data.",
                  HTML("<br/>"),
                  "If the null hypothesis is true then these are both estimates of the same thing and the ratio will be around 1.",
                  HTML("<br/>"),
                  "If the null hypothesis is false and the means are not all equal, then this measure of variance will be larger.",
                  HTML("<br/>"),
                  tags$hr(),
                  tableOutput("fStatistic")
                ),
                box(
                  title = "P value", status = "info",collapsed = TRUE, collapsible = TRUE, solidHeader = TRUE,
                  "p-value is defined as the measure of strength of evidence against hypothesis.For typical analysis, a widely used interpretation is :",
                  HTML("<br/><ol>"),
                  "A small p-value (â¤ Î±) indicates strong evidence for the null hypothesis, so it is rejected.",
                  HTML("</ol><ol>"),
                  "A large p-value (> Î±) indicates weak evidence for the null hypothesis (fail to reject).",
                  HTML("</ol><ol>"),
                  "p-values very close to the cutoff (~ Î±) are considered to be marginal (need attention).",
                  HTML("</ol>"),
                  "Î± =  level of significance , generally 0.05",
                  tags$hr(),
                  tableOutput("pValue")
                ),
                box(
                  title = "Residual Standard Error", status = "success",collapsed = TRUE, collapsible = TRUE, solidHeader = TRUE,
                  "What is Residual Standard Error ?",
                  tags$hr(),
                  tableOutput("residualSE")
                ),
                box(
                  title = "Degree of Freedom", status = "primary",collapsed = TRUE, collapsible = TRUE, solidHeader = TRUE,
                  "the number of degrees of freedom is the number of values in the final calculation of a statistic that are free to vary.",
                  HTML("<br/>"),
                  "The number of independent ways by which a dynamic system can move, without violating any constraint imposed on it, is called number of degrees of freedom. In other words, the number of degrees of freedom can be defined as the minimum number of independent coordinates that can specify the position of the system completely.",
                  tags$hr(),
                  tableOutput("degreeOF")
                )
              )
              ),
      
      # Residuals tab content
      tabItem(tabName = "residualPlot",
              fluidRow(
                box(
                  title = "Residual Plot",status = "primary", collapsible = TRUE, background = "light-blue",
                  "A residual plot is a graph that shows the residuals on the vertical axis and the independent variable on the horizontal axis. If the points in a residual plot are randomly dispersed around the horizontal axis, a linear regression model is appropriate for the data; otherwise, a non-linear model is more appropriate."
                )
              ),
              actionButton("action_parameters", "Show/Update Results"),
              tags$hr(),
              fluidRow(
                box(
                  title = "Residual Plot",status = "info", solidHeader = TRUE,
                  "",
                  plotOutput("residuals_plot")
                )
              )
      ),
      
      # Residuals histogram tab content
      tabItem(tabName = "residualhistogram",
              fluidRow(
                box(
                  title = "Residual Histogram",status = "primary", collapsible = TRUE, background = "light-blue",
                  "The residual plot of histogram is called residual histogram. These residual plots can be used to assess the quality of the regression. You can examine the underlying statistical assumptions about residuals such as constant variance, independence of variables and normality of the distribution."
                )
              ),
              actionButton("action_parameters", "Show/Update Results"),
              tags$hr(),
              fluidRow(
                box(
                  title = "Residual Histogram",status = "info", solidHeader = TRUE,
                  "",
                  plotOutput("residuals_hist")
                )
              )
      ),
      
      # Residuals Scatter Plot tab content
      tabItem(tabName = "residualscatterPlot",
              fluidRow(
                box(
                  title = "Residual Scatter Plot",status = "primary", collapsible = TRUE, background = "light-blue",
                  "A residual is the difference between the observed y-value (from scatter plot) and the predicted y-value (from regression equation line). It is the vertical distance from the actual plotted point to the point on the regression line."
                )
              ),
              fluidRow(
                box(
                  title = "Select the name of variable to see(must be a variable in dataset)",status = "danger", solidHeader = TRUE,
                  selectInput("residual_scatter1","Select Variable",  choices=c("attributes used"),selected=NULL, multiple=FALSE),
                  tags$hr(),
                  actionButton("action_residuals_scatter", "Show/Update Results"),
                  HTML("<br>")
                ),
                
                box(
                  title = "The Scatter Plot",status = "info", solidHeader = TRUE,
                  "",
                  div(class="span6",plotOutput('residuals_scatter'))
                )
              )
      ),
      
      # Ping Us! tab content
      # tabItem(tabName = "message",
      #         fluidRow(
      #           box(
      #             title = "Do Leave a message.....!", status = "success", solidHeader = TRUE,
      #             div(
      #               id = "contactForm",
      #               
      #               shinyjs::useShinyjs(),
      #               shinyjs::inlineCSS(appCSS),
      #               
      #               column(width = 6,
      #               textInput("name",labelMandatory("Full Name"),""),
      #               textInput("email",labelMandatory("Email Address"),"")
      #               ),
      #               column(width = 12,
      #               textInput("message","Your message",""),
      #               #actionButton("submit", "Submit", class = "btn-danger btn-large")
      #               HTML("<button class = 'btn btn-primary' id='submit'>Submit</button>")
      #               )
      #             )
      #           )
      #         )
      # )
      )
      
    )
  )


##################################----- SERVER ------#########################################################

server <- function(input, output, session) {

  #setwd("~/R/Rstudio-0/shiny-dashboard/Using Data Analytics for Policy Formulation")
  datafile_name_coded = "FinalVariables"
  csvPath = "data/FinalVariables.csv"
  ProjectData <- read.csv(csvPath)
  
  read_dataset <- reactive({
    
    #ProjectData <- read.csv(csvPath)
    
    updateSelectInput(session, "dependent_variable","Dependent variable", colnames(ProjectData), selected=NULL)
    updateSelectInput(session, "independent_variables","Independent variables", colnames(ProjectData), selected=NULL)
    #updateSelectInput(session, "independent_variables","Independent variables", colnames(ProjectData[!names(ProjectData)%in% c(input$dependent_variable) ]), selected=NULL)
    
    updateSelectInput(session, "hist_var","Select Variable",  colnames(ProjectData), selected=colnames(ProjectData)[1])
    updateSelectInput(session, "scatter1","x-axis",  colnames(ProjectData), selected=colnames(ProjectData)[1])
    updateSelectInput(session, "scatter2","y-axis",  colnames(ProjectData), selected=colnames(ProjectData)[1])
    updateSelectInput(session, "residual_scatter1","Select Variable",  colnames(ProjectData), selected=colnames(ProjectData)[1])
    
    ProjectData
    
  })
  
  user_inputs <- reactive({
    datafile_name_coded
    input$dependent_variable
    input$independent_variables
    
    list(ProjectData = read_dataset(), 
         dependent_variable = input$dependent_variable, 
         independent_variables = setdiff(input$independent_variables,input$dependent_variable))
  }) 
  
  the_parameters_tab<-reactive({
    # list the user inputs the tab depends on (easier to read the code)
    datafile_name_coded
    input$dependent_variable
    input$independent_variables
    input$action_parameters
    
    all_inputs <- user_inputs()
    ProjectData <-  all_inputs$ProjectData
    dependent_variable <- all_inputs$dependent_variable
    independent_variables <- all_inputs$independent_variables
    
    if (is.null(dependent_variable) | is.null(independent_variables)){
      res <- matrix(0,ncol=1)
      colnames(res) <- "Waiting for variable selection"
      return(res)
    }
    
    allparameters=c(nrow(ProjectData),ncol(ProjectData), colnames(ProjectData))
    allparameters <- matrix(allparameters,ncol=1)    
    rownames(allparameters)<-c("Number of Observations", "Number of Variables",
                               paste("Variable:",1:ncol(ProjectData)))
    colnames(allparameters)<-NULL
    
    allparameters
  })
  
  dependent_variable_tab<-reactive({
    # list the user inputs the tab depends on (easier to read the code)
    datafile_name_coded
    input$dependent_variable
    all_inputs <- user_inputs()
    ProjectData <-  all_inputs$ProjectData
    dependent_variable <- all_inputs$dependent_variable
    
    
    if (is.null(dependent_variable)){
      res <- matrix(0,ncol=1)
      colnames(res) <- "Waiting for variable selection"
      return(res)
    }
    
    allparameters=c(dependent_variable)
    allparameters <- matrix(allparameters,ncol=1)    
    rownames(allparameters)<-c("Dependent Variable")
    colnames(allparameters)<-NULL
    
    allparameters
  })
  
  independent_variables_tab<-reactive({
    # list the user inputs the tab depends on (easier to read the code)
    datafile_name_coded
    input$independent_variables
    input$action_parameters
    
    all_inputs <- user_inputs()
    ProjectData <-  all_inputs$ProjectData
    independent_variables <- all_inputs$independent_variables
    
    if (is.null(independent_variables)){
      res <- matrix(0,ncol=1)
      colnames(res) <- "Waiting for variable selection"
      return(res)
    }
    
    allparameters=c(independent_variables)
    allparameters <- matrix(allparameters,ncol=1)    
    rownames(allparameters)<-c(paste("Independent Variable:",1:length(independent_variables)))
    colnames(allparameters)<-NULL
    
    allparameters
  })
  
  output$parameters<-renderTable({
    the_parameters_tab()
  })
  
  output$dependent_variable<-renderTable({
    dependent_variable_tab()
  })
  
  output$independent_variables<-renderTable({
    independent_variables_tab()
  })
  
  #summary tab
  
  my_summary <- function(thedata){
    res = apply(thedata, 2, function(r) c(min(r), quantile(r, 0.25), quantile(r, 0.5), mean(r), quantile(r, 0.75), max(r), sd(r)))
    colnames(res) <- colnames(thedata)
    rownames(res) <- c("Min", "1st Quartile", "Median", "Mean", "3rd Quartile", "Max", "Std")
    res
  }
  
  the_summary_tab<-reactive({
    # list the user inputs the tab depends on (easier to read the code)
    datafile_name_coded
    input$dependent_variable
    input$independent_variables
    input$action_summary
    
    all_inputs <- user_inputs()
    ProjectData <-  all_inputs$ProjectData
    dependent_variable <- all_inputs$dependent_variable
    independent_variables <- all_inputs$independent_variables
    
    my_summary(ProjectData)
  })
  
  # Now pass to ui.R what it needs to display this tab
  output$summary <- renderTable({        
    the_summary_tab()
  })
  
  ########## The Correlations Tab
  
  # first the reactive function doing all calculations when the related inputs were modified by the user
  
  the_correlation_tab<-reactive({
    # list the user inputs the tab depends on (easier to read the code)
    datafile_name_coded
    input$dependent_variable
    input$independent_variables
    input$action_correlations
    
    all_inputs <- user_inputs()
    ProjectData <-  all_inputs$ProjectData
    dependent_variable <- all_inputs$dependent_variable
    independent_variables <- all_inputs$independent_variables
    
    if ((length(intersect(colnames(ProjectData),independent_variables)) & length(intersect(colnames(ProjectData),dependent_variable)))){
      data_reorder=cbind(ProjectData[,independent_variables,drop=F],ProjectData[,dependent_variable,drop=F])
    } else {
      data_reorder=ProjectData[,1,drop=F]
    }
    thecor=round(cor(data_reorder),2)
    colnames(thecor)<-colnames(thecor)
    rownames(thecor)<-rownames(thecor)
    thecor    
  })
  
  #Now pass to ui.R what it needs to display this tab
  source("heatmapOutput.R")
  output$correlation<-renderHeatmap({
    the_correlation_tab()
  })
  
  
  ########## The Histograms Tab
  
  # first the reactive function doing all calculations when the related inputs were modified by the user
  
  the_histogram_tab<-reactive({
    # list the user inputs the tab depends on (easier to read the code)
    datafile_name_coded
    input$dependent_variable
    input$independent_variables
    input$hist_var
    input$action_Histograms
    all_inputs <- user_inputs()
    ProjectData <-  all_inputs$ProjectData
    dependent_variable <- all_inputs$dependent_variable
    independent_variables <- all_inputs$independent_variables
    
    ProjectData <- data.matrix(read_dataset() )# call the data reading reactive FUNCTION (hence we need "()" )
    if (!length(intersect(colnames(ProjectData), input$hist_var))){ 
      res = NULL
    } else {
      res = ProjectData[,input$hist_var, drop=F]
    }
    res
  })
  
  # Now pass to ui.R what it needs to display this tab
  output$histogram<-renderPlot({ 
    data_to_plot = the_histogram_tab()
    if (!length(data_to_plot)) {
      hist(0, main = "VARIABLE DOES NOT EXIST" )      
    } else {
      hist(data_to_plot, main = paste("Histogram of", as.character(input$hist_var), sep=" "), xlab=as.character(input$hist_var),col = "skyblue", breaks = max(5,round(length(data_to_plot)/5)))      
    }
  })
  
  
  ########## The Scatter Plots Tab
  
  # first the reactive function doing all calculations when the related inputs were modified by the user
  
  the_scatter_plots_tab<-reactive({
    # list the user inputs the tab depends on (easier to read the code)
    datafile_name_coded
    input$dependent_variable
    input$independent_variables
    input$scatter1    
    input$scatter2  
    input$action_scatterplots
    
    all_inputs <- user_inputs()
    ProjectData <-  all_inputs$ProjectData
    dependent_variable <- all_inputs$dependent_variable
    independent_variables <- all_inputs$independent_variables
    
    ProjectData <- data.matrix(read_dataset() )# call the data reading reactive FUNCTION (hence we need "()" )
    
    if ((length(intersect(colnames(ProjectData),independent_variables)) & length(intersect(colnames(ProjectData),dependent_variable)))){
      res = ProjectData[, c(input$scatter1,input$scatter2)]      
    } else {
      res = 0*ProjectData[,1:2]
      colnames(res)<- c("Not Valid Variable Name", "Not Valid Variable Name")
    }
    res
  })
  
  # Now pass to ui.R what it needs to display this tab
  output$scatter<-renderPlot({   
    thedata <- the_scatter_plots_tab()
    plot(thedata[,1], thedata[,2], xlab=colnames(thedata)[1], ylab=colnames(thedata)[2], col = "red")
  })
  
  ########## The Regression Output Tab
  
  # first the reactive function doing all calculations when the related inputs were modified by the user
  
  the_regression_tab<-reactive({
    # list the user inputs the tab depends on (easier to read the code)
    datafile_name_coded
    input$dependent_variable
    input$independent_variables
    input$action_regression
    
    all_inputs <- user_inputs()
    ProjectData <-  all_inputs$ProjectData
    dependent_variable <- all_inputs$dependent_variable
    independent_variables <- all_inputs$independent_variables
    
    if ((length(intersect(colnames(ProjectData),independent_variables)) & length(intersect(colnames(ProjectData),dependent_variable)))){
      if (length(independent_variables) == 1){ 
        regression_model=paste(paste(dependent_variable, "~",sep=""), independent_variables,sep="")
      } else {
        res=independent_variables[1]
        for (iter in 2:(length(independent_variables)-1))
          res=paste(res,independent_variables[iter],sep="+")
        res=paste(res,tail(independent_variables,1),sep="+")
        regression_model = paste(dependent_variable, res, sep="~")  
      }
      the_fit<-lm(regression_model,data=ProjectData)
    } else {      
      regression_model = paste(paste(colnames(ProjectData)[1], "~",sep=""), colnames(ProjectData)[2],sep="")
      the_fit<-lm(regression_model,data=ProjectData)      
    }
    print_the_fit<-as.list(summary(the_fit))
    
    the_res= c(print_the_fit$r.squared, print_the_fit$adj.r.squared, 
               print_the_fit$fstatistic["value"], pf(print_the_fit$fstatistic[1], print_the_fit$fstatistic[2], print_the_fit$fstatistic[3],lower.tail = FALSE),
               sqrt(deviance(the_fit)/df.residual(the_fit)),the_fit$df)
    the_res = as.matrix(the_res,ncol=1)
    rownames(the_res)<-c("R square","Adjusted R Square", "F-statistic", "p-value",
                         "Residual standard error","degrees of freedom")
    colnames(the_res)<-"Values"
    the_res = as.data.frame(the_res)
    
    r_square = c(print_the_fit$r.squared)
    r_square = as.matrix(r_square,ncol=1)
    rownames(r_square)<-c("R square")
    colnames(r_square)<-"Value"
    r_square = as.data.frame(r_square)
    
    adjustedr_square = c(print_the_fit$adj.r.squared)
    adjustedr_square = as.matrix(adjustedr_square,ncol=1)
    rownames(adjustedr_square)<-c("Adjusted R square")
    colnames(adjustedr_square)<-"Value"
    adjustedr_square = as.data.frame(adjustedr_square)
    
    fStatistic = c(print_the_fit$fstatistic["value"])
    fStatistic = as.matrix(fStatistic,ncol=1)
    rownames(fStatistic)<-c("F Statistic")
    colnames(fStatistic)<-"Value"
    fStatistic = as.data.frame(fStatistic)
    
    pValue= c(pf(print_the_fit$fstatistic[1], print_the_fit$fstatistic[2], print_the_fit$fstatistic[3],lower.tail = FALSE))
    pValue= as.matrix(pValue,ncol=1)
    rownames(pValue)<-c("P-value")
    colnames(pValue)<-"Value"
    pValue= as.data.frame(pValue)
    
    residualSE= c(sqrt(deviance(the_fit)/df.residual(the_fit)))
    residualSE= as.matrix(residualSE,ncol=1)
    rownames(residualSE)<-c("Residual standard error")
    colnames(residualSE)<-"Value"
    residualSE= as.data.frame(residualSE)
    
    degreeOF= c(the_fit$df)
    degreeOF= as.matrix(degreeOF,ncol=1)
    rownames(degreeOF)<-c("Degrees of freedom")
    colnames(degreeOF)<-"Value"
    degreeOF= as.data.frame(degreeOF)
    
    list(the_fit=the_fit,
         the_res = the_res,
         r_square = r_square,
         adjustedr_square = adjustedr_square,
         fStatistic = fStatistic,
         residualSE = residualSE,
         degreeOF = degreeOF)
  })
  
  # Now pass to ui.R what it needs to display this tab
  output$regression_output <- renderTable({
    the_fit_all = the_regression_tab()
    summary(the_fit_all$the_fit)
  })
  
  output$Resparameters <- renderTable({
    the_fit_all = the_regression_tab()
    the_fit_all$the_res
  })
  
  output$rSquare <- renderTable({
    the_fit_all = the_regression_tab()
    the_fit_all$r_square
  })
  
  output$adjustedrSquare <- renderTable({
    the_fit_all = the_regression_tab()
    the_fit_all$adjustedr_square
  })
  
  output$fStatistic<- renderTable({
    the_fit_all = the_regression_tab()
    the_fit_all$fStatistic
  })
  
  output$residualSE <- renderTable({
    the_fit_all = the_regression_tab()
    the_fit_all$residualSE
  })
  
  output$degreeOF <- renderTable({
    the_fit_all = the_regression_tab()
    the_fit_all$degreeOF
  })
  
  ########## The Residuals plot Tab
  
  # first the reactive function doing all calculations when the related inputs were modified by the user
  
  the_residuals_plot_tab<-reactive({
    # list the user inputs the tab depends on (easier to read the code)
    datafile_name_coded
    input$dependent_variable
    input$independent_variables
    input$action_residuals
    input$action_residualshist
    
    all_inputs <- user_inputs()
    ProjectData <-  all_inputs$ProjectData
    dependent_variable <- all_inputs$dependent_variable
    independent_variables <- all_inputs$independent_variables
    
    the_fit = the_regression_tab()    
    residuals(the_fit$the_fit)       
  })
  
  # Now pass to ui.R what it needs to display this tab
  output$residuals_plot<-renderPlot({    
    plot(the_residuals_plot_tab(),xlab="Observations",ylab="Residuals",main="The Residuals", col = "red")
  })
  
  ########## The Residuals Histogram Tab
  
  # first the reactive function doing all calculations when the related inputs were modified by the user
  
  # this one uses the same reactive function as the previous tab...
  
  # Now pass to ui.R what it needs to display this tab
  output$residuals_hist<-renderPlot({    
    dataused = the_residuals_plot_tab()
    hist(dataused, main = "Histogram of the Residuals", breaks = max(5,round(length(dataused)/5)), col = "skyblue")
  })
  
  ########## The Residuals Scatter Plots Tab
  
  # first the reactive function doing all calculations when the related inputs were modified by the user
  
  the_residuals_scatter_tab<-reactive({
    # list the user inputs the tab depends on (easier to read the code)
    datafile_name_coded
    input$dependent_variable
    input$independent_variables
    input$residual_scatter1
    input$action_residuals_scatter
    
    all_inputs <- user_inputs()
    ProjectData <-  all_inputs$ProjectData
    dependent_variable <- all_inputs$dependent_variable
    independent_variables <- all_inputs$independent_variables
    
    ProjectData <- data.matrix(read_dataset() )# call the data reading reactive FUNCTION (hence we need "()" )    
    the_residuals <- the_residuals_plot_tab()
    
    if (length(intersect(colnames(ProjectData),input$residual_scatter1))){
      res = cbind(ProjectData[, input$residual_scatter1], the_residuals)
      colnames(res)<- c(input$residual_scatter1, "Residuals")
    } else {
      res = 0*ProjectData[,1:2]
      colnames(res)<- c("Not Valid Variable Name", "Not Valid Variable Name")
    }
    res
  })
  
  # Now pass to ui.R what it needs to display this tab
  output$residuals_scatter<-renderPlot({    
    thedata <- the_residuals_scatter_tab()
    
    plot(thedata[,1],thedata[,2],xlab=colnames(thedata)[1],ylab=colnames(thedata)[2], col = "red")
  })
  
  #contact/message form
  observe({
    mandatoryFilled <-
      vapply(fieldsMandatory,
             function(x) {
               !is.null(input[[x]]) && input[[x]] != ""
             },
             logical(1))
    mandatoryFilled <- all(mandatoryFilled)
    
    shinyjs::toggleState(id = "submit", condition = mandatoryFilled)
  })    
  
  
  #dynamically-generated-user-panel
  output$userpanel <- renderUI({
    # session$user is non-NULL only in authenticated sessions
    if (!is.null(session$user)) {
      sidebarUserPanel(
        span("Logged in as ", session$user),
        subtitle = a(icon("sign-out"), "Logout", href="__logout__"))
    }
  })
}

shinyApp(ui, server)

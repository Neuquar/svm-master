library(ggplot2)
library(e1071)
library(rbokeh)
library(shinyjs)
library(shinythemes)
library(DT)

#Generacion del data frame de 3000 registros divididos equitativamente en 3 clases
x=c(rnorm(1000,1000,100),rnorm(1000,2000,200),rnorm(1000,3000,400))
y=c(abs(rnorm(1000,50,25)),rnorm(1000,200,50),rnorm(1000,100,30))
clases=as.factor(c(rep(1,1000),rep(2,1000),rep(3,1000)))
datos=data.frame(x,y,clases)
w = nrow(datos)

server <-  function(input, output) {
  
  inFile <- reactive({input$file1}) #Archivo CSV
  
  dtPrev <- reactive({ #Si no se ha ingresado un CSV, usa los datos preestablecidos
    if (is.null(inFile())||input$chek ==FALSE){
      datos
    }else{
      read.csv(inFile()$datapath)
    }})
  
  #Parametros de la muestra y el tipo de kernel
  param <- reactiveValues() 
  observe(param$n <- if (input$chek == FALSE){input$numData}
                        else{input$numData2})
  observe(param$k <- input$kernel)
  observe(param$c <- input$C)
  observe(param$g <- input$gamma)
  observe(param$cf <- input$coef0)
  observe(param$dg <- input$degree)
  #Nombres de las columnas de la tabla de datos
  cols <- reactive(colnames(dtPrev())) 
  #se definen x, y, c que de las columnas seleccionadas de los datos 
  sc <- reactiveValues() 
  observe(sc$x <- which(cols() == input$xcol)) #variable x
  observe(sc$y <- which(cols() == input$ycol)) #variable y
  observe(sc$c <- which(cols() == input$ccol)) #variable que define las clases
  #Funcion que crea un nuevo data frame a partir del ingresado, que solo tendra las 3 columnas necesarias
  #ya seleccionadas, dichas columnas se estandarizan a ser nombradas x, y, c
  filtro <- function(x,y,c){ 
    data.frame(x,y,c)
  }
  #Funcion filtro aplicada a los datos
  dtMaster <- reactive({filtro(dtPrev()[,sc$x], dtPrev()[,sc$y], dtPrev()[,sc$c])})
  
  indices <- function(data){
    set.seed(101)
    sample(1:nrow(data),size=(param$n))
  }
  
  datax <- reactiveValues()
  observe(datax$indices <- indices(dtMaster()))
  observe(datax$entrenamiento <- dtMaster()[datax$indices,])
  observe(datax$test <- dtMaster()[-datax$indices,])
  
  svmx <- reactive({
    switch(input$kernel,
           "linear" =  svm(c~y + x, data=datax$entrenamiento, 
                           kernel=param$k, cost = param$c),
           
           "polynomial" =  svm(c~y + x, data=datax$entrenamiento, 
                               kernel=param$k, cost = param$c, gamma = param$g, coef0 = param$cf, degree = param$dg),
           
           "radial" = svm(c~y + x, data=datax$entrenamiento, 
                          kernel=param$k, cost = param$c, gamma = param$g),
           
           "sigmoid" = svm(c~y + x, data=datax$entrenamiento, 
                           kernel=param$k, cost = param$c, gamma = param$g, coef0 = param$cf))
  })
  
  output$ui3 <- renderUI({
    sliderInput("numData2", "Submuestra", value = 50, min = 10, max = nrow(dtPrev()), step = 1)
  })
  
  output$ui2 <- renderUI({
    tabPanel("Columnas",
             selectInput("xcol", "X", cols(), selected = cols()[1]),
             selectInput("ycol", "Y", cols(), selected = cols()[2]),
             selectInput("ccol", "Clases", cols(), selected = cols()[3]))
  })
  
  output$ui <- renderUI({
    if (is.null(input$kernel))
      return()
    
    switch(input$kernel,
           
           "linear" = tabPanel("Lineal", 
                               sliderInput('C', 'Training parameter C', value = 1, min = 1, max = 1000, step = 1)),
           
           "polynomial" = tabPanel("Polinominal",
                                   sliderInput('C', 'Training parameter C', value = 1, min = 1, max = 1000, step = 1),
                                   sliderInput('gamma', 'Training parameter gamma', value = 0.25, min = 0, max = 10, step = 0.05),
                                   sliderInput('coef0', 'Training parameter coef0', value = 0, min = 0, max = 10, step = 0.5),
                                   sliderInput('degree', 'Training parameter degree', value = 3, min = 0, max = 10, step = 0.5)),
           
           "radial" = tabPanel("Radial",
                               sliderInput('C', 'Training parameter C', value = 1, min = 1, max = 1000, step = 1),
                               sliderInput('gamma', 'Training parameter gamma', value = 0.25, min = 0, max = 10, step = 0.05)),
           
           "sigmoid" = tabPanel("Sigmoid",
                                sliderInput('C', 'Training parameter C', value = 1, min = 1, max = 1000, step = 1),
                                sliderInput('gamma', 'Training parameter gamma', value = 0.25, min = 0, max = 10, step = 0.05),
                                sliderInput('coef0', 'Training parameter coef0', value = 0, min = 0, max = 10, step = 0.5))
    )
  })
  
  output$newPlot <- renderRbokeh({
    
    p <- figure(width = 1000, height = 450) %>%
      ly_points(x, y, data = datax$entrenamiento,
                color = c, glyph = c(21,25)[1:param$n %in% svmx()$index + 1],
                hover = list(y, x))
    p
  })
  
  output$plot1 <- renderPlot({
    #plot(svmx(), data=datax$entrenamiento[,c(1,2,3)])
    plot(svmx(), datax$entrenamiento, y ~ x, 
         slice = list(x = 1, y = 2))
  })
  
  output$pred <- renderPrint({
    #Prediccion de los restantes
    asignado <- predict(svmx(),new=datax$test)
    #Tabla de confusion
    mc <- with(datax$test,(pred=table(asignado,c)))
    mc
  })
  
  output$sum <- renderPrint({
    summary(svmx())
  })
  
  output$state <- renderText({
    #Prediccion de los restantes
    asignado <- predict(svmx(),new=datax$test)
    #Tabla de confusion
    mc <- with(datax$test,(pred=table(asignado,c)))
    #porcentaje correctamente clasificados
    if(is.nan(correctos <- sum(diag(mc)) / nrow(datax$test) *100)){
      correctos <- 100
    }else {
      correctos <- sum(diag(mc)) / nrow(datax$test) *100
    }
  })
  
  output$nsv <- renderText({
    #total de vectores soporte
    svmx()$tot.nSV
  })
  
  output$table <- DT::renderDataTable({
    DT::datatable(datax$entrenamiento, options=list(orderClasses = TRUE))
  })
}

ui <- navbarPage(theme = shinytheme("cerulean"),"SVM shiny app",
                 tabPanel("SVM",
                          sidebarLayout(
                            sidebarPanel(
                              checkboxInput("chek", label = "Usar archivo CSV", value = FALSE),
                              conditionalPanel("input.chek == false",
                                sliderInput("numData", "Submuestra", value = 50, min = 10, max = w, step = 1)
                              ),
                              conditionalPanel("input.chek == true",
                                fileInput('file1', 'Selecciona un CSV para analizar',
                                         accept=c('text/csv', 
                                                  'text/comma-separated-values,text/plain', 
                                                  '.csv')),
                                uiOutput("ui3")
                              ),
                              uiOutput("ui2"),
                              selectInput("kernel", "kernel",
                                          c(Lineal = "linear",
                                            Polinominal = "polynomial",
                                            Radial = "radial",
                                            Sigmoid = "sigmoid")),
                              uiOutput("ui"),
                              h4("Matriz de confusion: ", align = "rigth"),
                              verbatimTextOutput('pred'),
                              h4("Porcentaje correctamente clasificados:", align = "rigth"),
                              htmlOutput('state'),
                              h4("Total de vectores soporte:", align = "rigth"),
                              htmlOutput('nsv')
                            ),
                            mainPanel(
                              tabsetPanel(
                                tabPanel("Grafica 1",
                                         rbokehOutput('newPlot')
                                ),
                                tabPanel("Grafica 2",
                                         plotOutput("plot1")
                                ),
                                tabPanel("Tabla",
                                         DT::dataTableOutput("table")
                                ),
                                tabPanel("Sumario",
                                        verbatimTextOutput("sum")        
                                )
                              )
                            )
                          )
                 ),
                 tabPanel("Acerca",
                          h4("Aplicacion de SVM v 4.0"),
                          p("Esta es una sencilla shiny app que implementa SVM sobre un conjunto de 3000 datos 
                            creados por R y clasificados en 3 clasess. Del total de datos, especifique una submuestra, 
                            seleccione el tipo de kernel y en base a eso configure los parametros necesarios. Se muestra 
                            la tabla de confusion, el porcentaje correctamente clasificado segun la prediccion, 
                            el total de vectores soporte y la grafica."),
                          p("Basada en ejemplos y la documentacion de RStudio Shiny"),
                          a(href="https://www.twitter.com/axelmora93", "Twitter"),
                          a(href="https://www.linkedin.com/in/axel-sunem-mora-olvera-338aa6108/", "LinkedIn")
                          )
                 )

shinyApp(ui = ui, server = server)
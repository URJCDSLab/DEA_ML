library(ggpubr)
library(ggplot2)
library(shiny)
library(shinydashboard)
library(stargazer)
library(shiny)
library(datasets)
library(DT)
library(shinycssloaders)
library(ggdist)
library(plotly)
library(shiny)
library(datasets)
library(shinydashboard)
library(DT)
library(stargazer)
library(shinycssloaders)
library(markdown)

function(input, output, session) {
  updateTabItems(session, "sidebar", "home")
  data = reactiveVal()

# data <- reactive({ 
    #  req(input$file1)
        #inFile <- input$file1 
     # read.csv(input$file1$datapath, header = input$header, sep = input$sep,
        #      quote = input$quote)
    
 
    
  #})  
  observe({
    req(input$file1)
    mydata= read.csv(input$file1$datapath, header = input$header, sep = input$sep,
                   quote = input$quote)
    data(mydata)
  })

  
  output$SelectX <- renderUI({
    req(data())
    print(colnames(data()))
    xa<-colnames(data())
    selectInput(inputId = 'xcol',
                label="Variable:",
                choices = names(data()),selected = names(data()))
    
  })
  
  output$SelectY1 <- renderUI({
    req(data())
    print(colnames(data()))
    xa<-colnames(data())
    selectInput(inputId = 'y1col',
                label="Variable:",
                choices = names(data()),selected = names(data()))
    
  })
  output$SelectY2 <- renderUI({
    req(data())
    print(colnames(data()))
    xa<-colnames(data())
    selectInput(inputId = 'y2col',
                label="Variable:",
                choices = names(data()),selected = names(data()))
    
  })
  output$SelectYY <- renderUI({
    req(data())
    print(colnames(data()))
    xa<-colnames(data())
    selectInput(inputId = 'SelectY',
                label="Select variable to predict:",
                choices = names(data()),selected = names(data()))
    
  })
  #updateSelectInput(session, inputId = 'y1col', label = 'Group',
  #                  choices = names(df), selected = names(df))
  #updateSelectInput(session, inputId = 'y2col', label = 'Categories',
  #                  choices = names(df), selected = names(df))
  #updateSelectInput(session, inputId = 'SelectX',
  #                  choices = names(df), selected = names(df))
  #updateSelectInput(session, inputId = 'SelectY',
  #                  choices = names(df), selected = names(df))
 # df_global1 <- reactiveVal(df)

  
  observeEvent(input$addButton, {
    print(paste("Ello"))
    x    <- data()[, input$xcol]
    z <- as.numeric(input$Lambda)
    if (z!=0)
      x=(x^z-1)/z
    if (z==0)
      x=log(x)
    newdata=data()
    newdata[paste(input$xcol,"cox",sep="_")]=x
    data(newdata)
    })
  output$table3 <- renderTable({
    head(data())
  })
  #updateSelectInput(session, inputId = 'xcol', label = 'Variable',
  #                  choices = names(data), selected = names(data))

  #table output:
  output$table1 <- renderTable({
    df=data()
    head(df)
  })
  
  output$Summ <-
    renderPrint(
      stargazer(
        data(),
        type = "text",
        title = "Descriptive statistics",
        digits = 1,
        out = NULL
      )
    )
  output$Summ_old <- renderPrint(summary(data()))
  output$structure <- renderPrint(str(data()))
  
  output$Data <- renderDT({
    data()
  })
  
  #table output:
  output$table2 <- renderTable({
    summary(data())
  })
  
  output$doc_to_display <- renderUI({
    includeMarkdown("www/home.md")
  })

  
  output$MyPlot <- renderPlot({
    x    <- data()[, input$xcol]
    z <- as.numeric(input$Lambda)
    if (z!=0)
      x=(x^z-1)/z
    if (z==0)
      x=log(x)
    x=data.frame(x)
    
    #hist(x,col = '#2171b5', border = 'white',main = "Histograma")
    ggplot(x,aes(x=x))+
      geom_histogram(aes(y=..density..), position="identity", alpha=1,fill="#2171b5")+
      geom_density(alpha=0.6)+labs(title="Histogram plot")+
      theme(
        panel.background = element_rect(fill='transparent'), #transparent panel bg
        plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
       # panel.grid.major = element_blank(), #remove major gridlines
      #  panel.grid.minor = element_blank(), #remove minor gridlines
        legend.background = element_rect(fill='transparent'), #transparent legend bg
        legend.box.background = element_rect(fill='transparent') #transparent legend panel
      )
  },bg="transparent",height = 250, width = 500 )
      #geom_histogram(fill="#2171b5")+geom_density(alpha=0.6)
  

  output$MyPlot2 <- renderPlot({
    x    <- data()[, input$xcol]
    z <- as.numeric(input$Lambda)
    if (z!=0)
      x=(x^z-1)/z
    if (z==0)
      x=log(x)
    x=data.frame(x)
    #boxplot(x,col = '#2171b5', border = 'royalblue3',main = "BoxPlot",horizontal=TRUE)
    ggplot(x,aes(x=x))+geom_boxplot(fill="#2171b5")+labs(title="Boxplot")+
      theme(
        panel.background = element_rect(fill='transparent'), #transparent panel bg
        plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
     #   panel.grid.major = element_blank(), #remove major gridlines
    #    panel.grid.minor = element_blank(), #remove minor gridlines
        legend.background = element_rect(fill='transparent'), #transparent legend bg
        legend.box.background = element_rect(fill='transparent') #transparent legend panel
      )
  },bg="transparent",height = 250, width = 500 )
  
  output$MyPlot3 <- renderPlot({
    x1   <- data()[, input$y1col]
    x2   <- data()[, input$y2col]
    df=data.frame(cbind(x1,x2))
    names(df)=c(input$y1col,input$y2col)
    #plot(x1,x2)
    #if ((typeof(x1)=="factor") & (typeof(x2)=="factor")){
    ggplot(data = df, aes(x = df[,2], fill = df[,1])) +
      geom_bar()+
      xlab(input$y2col)+
      guides(fill=guide_legend(title=input$y1col))+
      theme(
        panel.background = element_rect(fill='transparent'), #transparent panel bg
        plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
        #   panel.grid.major = element_blank(), #remove major gridlines
        #    panel.grid.minor = element_blank(), #remove minor gridlines
        legend.background = element_rect(fill='transparent'), #transparent legend bg
        legend.box.background = element_rect(fill='transparent') #transparent legend panel
        
      )
   # }
    },bg="transparent",height = 350, width = 600 )
  
  
  output$MyPlot4 <- renderPlot({
    x    <- data()[, input$xcol]
    z <- as.numeric(input$Lambda)
    if (z!=0)
      x=(x^z-1)/z
    if (z==0)
      x=log(x)
    x=data.frame(x)
    #boxplot(x,col = '#2171b5', border = 'royalblue3',main = "BoxPlot",horizontal=TRUE)
    p=ggplot(x,aes(x=x))+labs(title="Boxplot")+
      ggdist::stat_halfeye(adjust=0.5,justification=-.5,.width=0,point_colour=NA)+
      geom_boxplot(fill="#2171b5",alpha=0.5)+
      theme(
        panel.background = element_rect(fill='transparent'), #transparent panel bg
        plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
        #   panel.grid.major = element_blank(), #remove major gridlines
        #    panel.grid.minor = element_blank(), #remove minor gridlines
        legend.background = element_rect(fill='transparent'), #transparent legend bg
        legend.box.background = element_rect(fill='transparent') #transparent legend panel
      )
  },bg="transparent",height = 250, width = 500 )
  
  
  output$MyPlot5 <- renderPlotly({
    myplot2()})
  myplot2 = reactive({
    x    <- data()[, input$xcol]
    z <- as.numeric(input$Lambda)
    if (z!=0)
      x=(x^z-1)/z
    if (z==0)
      x=log(x)
    x=data.frame(x)
    #boxplot(x,col = '#2171b5', border = 'royalblue3',main = "BoxPlot",horizontal=TRUE)
    p=ggplot(x,aes(x=x))+labs(title="Boxplot")+
      ggdist::stat_halfeye(adjust=0.5,justification=-.5,.width=0,point_colour=NA)+
      geom_boxplot(fill="#2171b5",alpha=0.5)
    ggplotly(p)%>% 
      layout(legend = list(orientation = 'h', x = 0.45, y = 1.1))
  })
    

  
  
  
  
  output$SelectXX <- renderUI({
    req(data())
   #df=data()
    print(colnames(data()))
    xa<-colnames(data())
    selectInput(inputId = 'xvar',
                label = 'Selected variables used in the model:',
                choices = c(xa[1:length(xa)]), selected=xa[1],
               # options = list(`style` = "btn-info"),
                multiple = TRUE)
    
  })
  
  InputDataset_model <- reactive({
    req(data(),input$xvar,input$SelectY)
#    dt <- as.numeric(data()[[as.name(input$xvar)]])
  #  if (is.null(input$xvar)) {
  #    dt <- data()
  #  }
  #  else{
      dt <- data()[, c(input$xvar,input$SelectY)]
#    }
    
    # dt <- as.numeric(data()[[as.name(input$xvar)]])
  })
  
  
  set.seed(123)  # setting seed to reproduce results of random sampling
  trainingRowIndex <-
    reactive({
      sample(1:nrow(InputDataset_model()),
             splitSlider() * nrow(InputDataset_model()))
    })# row indices for training data
  
  trainingData <- reactive({
    tmptraindt <- InputDataset_model()
    tmptraindt[trainingRowIndex(), ]
  })
  
  testData <- reactive({
    tmptestdt <- InputDataset_model()
    tmptestdt[-trainingRowIndex(),]
  })
  
  

  output$cntTrain <-
    renderText(paste("Train Data:", NROW(trainingData()), "records"))
  output$cntTest <-
    renderText(paste("Test Data:", NROW(testData()), "records"))
  
  f <- reactive({
    as.formula(paste(input$SelectY, "~."))
  })
  
  
  Linear_Model <- reactive({
    #glm(f(), data = trainingData(),family="binomial")
    lm(f(), data = trainingData())
  })
  
  output$Model <- renderPrint(summary(Linear_Model()))
  output$Model_new <-
    renderPrint(
      stargazer(
        Linear_Model(),
        type = "text",
        title = "Model Results",
        digits = 1,
        out = "table1.txt"
      )
    )
  
  Importance <- reactive({
    varImp(Linear_Model(), scale = FALSE)
  })
  
  tmpImp <- reactive({
    
    imp <- as.data.frame(varImp(Linear_Model()))
    imp <- data.frame(overall = imp$Overall,
                      names   = rownames(imp))
    imp[order(imp$overall, decreasing = T),]
    
  })
  
  output$ImpVar <- renderPrint(tmpImp())
  splitSlider <- reactive({
    input$Slider / 100
  })
}



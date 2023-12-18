
output$MyPlot4 <- renderPlot({
  x    <- data()[, input$xcol]
  z <- as.numeric(input$Lambda)
  if (z!=0)
    x=(x^z-1)/z
  if (z==0)
    x=log(x)
  x=data.frame(x)
  #boxplot(x,col = '#2171b5', border = 'royalblue3',main = "BoxPlot",horizontal=TRUE)
  ggplot(x,aes(x=x))+labs(title="Boxplot")+
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

SimPrevMeans <- read.csv("SimPrevAverage.csv") %>%
  select(-X) %>%
  #subset(ProbGender %in% c("0-0", "0.002-0.00041")) %>%
  subset(ProbGender %in% c("0-0", "0.00071-0.00023")) %>%
  # mutate(Model = recode(ProbGender,
  #                       "0-0" = 'Mated Pairs',
  #                       #"0.002-0.00041" = 'Individual Worms',
  #                       "0.00071-0.00023"= 'Individual Worms')) %>%
  # mutate(MDADrugEfficacy = recode(MDADrugEfficacy,
  #                                 "Default" = 'Ours',
  #                                 "Irvine" = "Irvine's")) %>%
  mutate(MDADrugEfficacy = if_else(MDADrugEfficacy == 'Default',
                                   if_else(MDADrugs == 'DA2+', 'High', 'Low'),
                                   if_else(MDADrugs == 'DA2+','Low', 'High'))) %>%
  pivot_longer(cols = PrevalenceAll.average.:Ratio15AndOver.95.,
               names_pattern = "(.*)[.](.*)\\.",names_to = c(".value","Kind"))

FirstYear <- min(SimPrevMeans$Year)
LastYear <- max(SimPrevMeans$Year)

vline <- function(x = 0, color = "black") {
  list(
    type = "line",
    y0 = 0,
    y1 = 1,
    yref = "paper",
    x0 = x,
    x1 = x,
    line = list(color = color, dash="dot")
  )
}


ProbReachTargetMeasures <- c("Prob Ag Prevalence (6-7 year-olds) < 1%",
                             "Prob Mf Prevalence = 0%",
                             "Prob Mf Prevalence < 1%",
                             "Prob Mf Prevalence < 0.5%",
                             "Prob Mf Prevalence < 0.1%",
                             "Prob Mf Prevalence < 0.01%",
                             "Prob Ag Prevalence < 1%",
                             "Prob Ag Prevalence < 0.5%",
                             "Prob Ag Prevalence < 0.1%",
                             "Prob Ag Prevalence < 0.01%")

#Define code to produce one of the plots
renderLFFig <- function(input,TimeScale,Measure,YScale){
  SelectedDrugs <- input$ChooseDrugs %>%
    # recode('DA 2+ year-olds' = 'DA2+',
    #        'IDA 5+ year-olds and DA 2-4 year-olds' = 'IDA5+/DA2-4',
    #        'IDA 5+ year-olds' = 'IDA5+')
    recode('DA' = 'DA2+',
           'IDA' = 'IDA5+/DA2-4')

  Data <- SimPrevMeans %>%
    subset(MDAYearsBetweenRounds==1) %>%
    subset(MDADrugs %in% SelectedDrugs) %>%
    #subset(Model %in% input$ChooseModel) %>%
    subset(MDADrugEfficacy %in% input$ChooseDrugAssumptions) %>%
    subset(MDACoverageAttempted %in% input$ChooseCoverage) %>%
    subset(MDANumRounds %in% input$ChooseRounds)

  #Combine all the variables that are being used for comparisons
  #into a single grouping varaible with an imformative name (as
  #it is going to appear in the legend). Only include variables
  #that are being used for comparisons. i.e. If all the scenarios
  #being compared have just 3 rounds of MDA, you don't need to have
  #the number of rounds of MDA in the legend name
  Data <- Data %>%
    mutate(ColorGroup = paste0(ifelse(rep(length(input$ChooseDrugAssumptions)>1,nrow(Data)),
                                      paste0('Efficacy:',MDADrugEfficacy,'; '),
                                      rep("",nrow(Data))),
                               ifelse(rep(length(input$ChooseCoverage)>1,nrow(Data)),
                                      paste0('Coverage:',MDACoverageAttempted,'; '),
                                      rep("",nrow(Data))),
                               ifelse(rep(length(input$ChooseRounds)>1,nrow(Data)),
                                      paste0('Rounds:',MDANumRounds,'; '),
                                      rep("",nrow(Data)))#,
                               # ifelse(rep(length(input$ChooseModel)>1,nrow(Data)),
                               #        paste0('Model:',Model,'; '),
                               #        rep("",nrow(Data)))
                               )) %>%
    mutate(ColorGroup = substr(ColorGroup,1,nchar(ColorGroup)-2)) %>% #Strip off the final semicolon
    mutate(MDADrugs = ifelse(rep(length(SelectedDrugs)>1,nrow(Data)),
                             paste0('Drugs:',
                                    recode(MDADrugs,
                                           'DA2+' = 'DA',
                                           'IDA5+/DA2-4' = 'IDA')),
                             rep("",nrow(Data))))

  # if(Measure %in% ProbReachTargetMeasures){ # If displaying probability of reaching target only plot years starting at first MDA year
  #   Data <- Data %>% subset(Year >= MDAStartYear)
  # }
  if(!input$ShowBeforeMDA){
    Data <- Data %>% subset(Year >= MDAStartYear)
  }
  

  Data$Output <- switch(Measure,
                        'Mf Prevalence (All)' = Data$PrevalenceAll,
                        "Mf Prevalence (6-7 year olds)" = Data$Prevalence6_7,
                        "Mf Prevalence (6-9 year olds)" = Data$Prevalence6_9,
                        "Mf Prevalence (11-12 year olds)" = Data$Prevalence11_12,
                        "Mf Prevalence (15-16 year olds)" = Data$Prevalence15_16,
                        "Mf Prevalence (10+ year olds)" = Data$Prevalence10AndOver,
                        "Mf Prevalence (15+ year olds)" = Data$Prevalence15AndOver,
                        "Prob Ag Prevalence (6-7 year-olds) < 1%" = Data$AgPrev6_7Under1pc,
                        "Prob Mf Prevalence = 0%" = Data$Eradicated,
                        "Prob Mf Prevalence < 0.01%" = Data$PrevUnder001pc,
                        "Prob Mf Prevalence < 0.1%" = Data$PrevUnder01pc,
                        "Prob Mf Prevalence < 0.5%" = Data$PrevUnder05pc,
                        "Prob Mf Prevalence < 1%" = Data$PrevUnder1pc,
                        "Prob Ag Prevalence < 0.01%" = Data$AgPrevUnder001pc,
                        "Prob Ag Prevalence < 0.1%" = Data$AgPrevUnder01pc,
                        "Prob Ag Prevalence < 0.5%" = Data$AgPrevUnder05pc,
                        "Prob Ag Prevalence < 1%" = Data$AgPrevUnder1pc,
                        "Mf Prevalence (Fagali'i)" = Data$PrevalenceFagalii,
                        "Mf Prevalence (Ili'ili)" = Data$PrevalenceIliili,
                        "Ag Prevalence (All)" = Data$Prev_AntigenAll,
                        "Ag Prevalence (6-7 year olds)" = Data$Prev_Antigen6_7,
                        "Ag Prevalence (6-9 year olds)" = Data$Prev_Antigen6_9,
                        "Ratio Ag:Mf Prevalence (All)" = Data$RatioAll)

  MinPrec <- Data$Output[Data$Output>0] %>% min %>% log10 %>% floor #Precision at which to display the tooltips and x axes
  ZeroOutput = any(Data$Output==0, na.rm = T) #True if the output (mean or credible intervals) are exactly 0 for any of the outcomes
  
  Data$PlotYear <- switch(TimeScale,
                           'Calendar Year' = Data$Year,
                           'Years since last MDA' = with(Data, Year - (MDAStartYear + (MDANumRounds-1) * MDAYearsBetweenRounds)))


  Data[] <- lapply(Data, function(x) if(is.factor(x)) factor(x) else x)


  # print(ifelse(Measure %in% c(ProbReachTargetMeasures),
  #        '.0%',
  #        ifelse(Measure %in% c("Ratio Ag:Mf Prevalence"),
  #               ".1f",
  #               ifelse(YScale == "Log",
  #                      paste0(".",as.character(-min(0,MinPrec+2)),"%"),
  #                      "%"))))
  # print(TimeScale)
  # print(switch(TimeScale,
  #              'Calendar Year' = list(FirstYear,LastYear+0.2),
  #              'Years since last MDA' = list(min(Data$PlotYear),max(Data$PlotYear)+0.2)))
  
  PO <- plot_ly(data = Data %>% subset(Kind == 'average'),
          x = ~PlotYear,
          y = ~Output,
          linetype = ~MDADrugs,
          type = "scatter",
          mode= 'lines',
          color = ~ColorGroup,
          line = list(width = 4)) %>%
    #style(hoverinfo = "y") %>%
    layout(yaxis = list(type = switch(YScale,
                                      Linear = 'linear',
                                      Log = 'log'),#,
                                      #Automatic = ifelse(Measure %in% c(ProbReachTargetMeasures,"Ratio Ag:Mf Prevalence (All)"),
                                      #                   "linear",
                                      #                   "log")
                        title = Measure,
                        rangemode= "nonnegative",
                        tickformat = ifelse(Measure %in% c(ProbReachTargetMeasures),
                                            '.0%',
                                            ifelse(Measure %in% c("Ratio Ag:Mf Prevalence (All)"),
                                                   ".1f",
                                                   ifelse(YScale == "Log",
                                                          paste0(".",as.character(-min(0,MinPrec+2)),"%"),
                                                          ".1%"))),
                        hoverformat = ifelse(Measure %in% c(ProbReachTargetMeasures),
                                            '.1%',
                                            ifelse(Measure %in% c("Ratio Ag:Mf Prevalence"),
                                                   ".1f",
                                                   paste0(".",as.character(-min(0,MinPrec+2)),"%")))
    ),
    xaxis = list(title = TimeScale,
                 range = switch(TimeScale,
                                'Calendar Year' = list(if(input$ShowBeforeMDA){FirstYear}else{unique(Data$MDAStartYear)},
                                                       LastYear+0.2),
                                'Years since last MDA' = list(min(Data$PlotYear),max(Data$PlotYear)+0.2))),
    showlegend = !input$HideLegend & (length(unique(Data$ColorGroup)) > 1 | length(unique(Data$MDADrugs)) > 1),
    legend = list(x = 0, y = 100 ,orientation = 'h'),
    hovermode = 'compare')

  NumComparisons <- with(Data,length(unique(MDADrugs)) * length(unique(ColorGroup)))
  if(NumComparisons == 1){
    PO <- PO %>%
      style(name = 'mean') %>%
      add_trace(data = Data %>% subset(Kind == '95'),
                x = ~PlotYear,
                y = ~Output,
                type = "scatter",
                mode= 'lines',
                color = ~ColorGroup,
                line = list(width = 1),
                showlegend = F,
                name = '95%') %>%
      add_trace(data = Data %>% subset(Kind == '5'),
                x = ~PlotYear,
                y = ~Output,
                type = "scatter",
                mode= 'lines',
                color = ~ColorGroup,
                line = list(width = 1),
                fill = 'tonexty', fillcolor= ~ColorGroup,
                showlegend = F,
                name = '5%')
  }
  
  if(TimeScale == 'Calendar Year'){
    PO <- PO %>% layout(shapes = list(vline(unique(Data$MDAStartYear))))
  }
  
  return(PO)
}

#Define server
Server <- function(input, output, session) {
  output$Chart1 <- renderPlotly({renderLFFig(input,
                                            input$TimeScale1,
                                            input$Measure1,
                                            input$YScale1)})
  output$Chart2 <- renderPlotly({renderLFFig(input,
                                             input$TimeScale2,
                                             input$Measure2,
                                             input$YScale2)})
}

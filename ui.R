library(tidyverse)
library(shiny)
library(shinydashboard)
library(plotly)
library(markdown)

MeasureOptions <- c("Mf Prevalence (All)",
                    "Mf Prevalence (6-7 year olds)",
                    "Mf Prevalence (6-9 year olds)",
                    "Mf Prevalence (11-12 year olds)",
                    "Mf Prevalence (15-16 year olds)",
                    "Mf Prevalence (10+ year olds)",
                    "Mf Prevalence (15+ year olds)",
                    "Ag Prevalence (All)",
                    "Ag Prevalence (6-7 year olds)",
                    "Ag Prevalence (6-9 year olds)",
                    "Prob Mf Prevalence < 1%",
                    "Prob Mf Prevalence < 0.5%",
                    "Prob Mf Prevalence < 0.1%",
                    "Prob Mf Prevalence < 0.01%",
                    "Prob Mf Prevalence = 0%",
                    "Prob Ag Prevalence < 1%",
                    "Prob Ag Prevalence < 0.5%",
                    "Prob Ag Prevalence < 0.1%",
                    "Prob Ag Prevalence < 0.01%",
                    "Prob Ag Prevalence (6-7 year-olds) < 1%",
                    "Ratio Ag:Mf Prevalence (All)",
                    "Mf Prevalence (Fagali'i)",
                    "Mf Prevalence (Ili'ili)")
TimeFrameOptions <- c('Calendar Year','Years since last MDA')
#DrugOptions <- c("DA 2+ year-olds",'IDA 5+ year-olds','IDA 5+ year-olds and DA 2-4 year-olds')
DrugOptions <- c("DA",'IDA')
MDACoverageOptions <- c(0.55,0.65,0.75)
#DrugEfficacyOptions <- c("Irvine's","Ours")
DrugEfficacyOptions <- c("Low","High")
NumRoundsOptions <- 2:7
#WormModelOptions <- c('Mated Pairs','Individual Worms')
YScaleOptions <- c('Linear','Log')

UI <- dashboardPage(

  dashboardHeader(title = "MDA for LF in American Samoa",titleWidth = 299),
  dashboardSidebar(width = 299,
                   sidebarMenu(
                     menuItemOutput("menu"),
                     menuItem("Description", tabName = "Description", icon = icon("question")),
                     menuItem("Model Output",
                              tabName = "widgets",
                              icon = icon('chart-line'),
                              startExpanded = T,
                              menuSubItem(tabName = "widgets",
                                          selected = T,
                                          selectInput(inputId = "Measure1",
                                                      label = "Measure - Chart 1",
                                                      MeasureOptions,
                                                      selected=list("Mf Prevalence (6-7 year olds)"),
                                                      multiple = F)),
                              menuSubItem(tabName = "widgets",
                                          selectInput(inputId = "Measure2",
                                                      label = "Measure - Chart 2",
                                                      MeasureOptions,
                                                      selected=list("Prob Mf Prevalence < 0.5%"),
                                                      multiple = F))),
                     menuItem("MDA Scenarios",
                              tabName = "widgets",
                              icon = icon('capsules'),
                              menuSubItem(tabName = "widgets",
                                          selectInput(inputId = "ChooseDrugs",
                                                      label = "Drugs",
                                                      DrugOptions,
                                                      selected= DrugOptions,
                                                      multiple = T)),

                              menuSubItem(tabName = "widgets",
                                          selectInput(inputId = "ChooseCoverage",
                                                      label = "Coverage",
                                                      MDACoverageOptions,
                                                      selected=list(0.75),
                                                      multiple = T)),

                              menuSubItem(tabName = "widgets",
                                          selectInput(inputId = "ChooseRounds",
                                                      label = "Number of Rounds",
                                                      NumRoundsOptions,
                                                      selected=list(3,5),
                                                      multiple = T))),
                     menuItem(tabName = "widgets",
                              "Drug Assumptions",
                              icon = icon('question-circle'),
                              # menuSubItem(tabName = "widgets",
                              #             selectInput(inputId = "ChooseModel",
                              #                         label = "Worm Model",
                              #                         WormModelOptions,
                              #                         selected=list('Individual Worms'),
                              #                         multiple = T)),
                              menuSubItem(tabName = "widgets",
                                          selectInput(inputId = "ChooseDrugAssumptions",
                                                      label = "Drug Efficacy Assumptions",
                                                      DrugEfficacyOptions,
                                                      selected=list("High"),
                                                      multiple = T))),
                     menuItem(tabName = "widgets",
                              "Time-axis display options",
                              icon = icon('calendar'),
                              menuSubItem(selectInput(inputId = "TimeScale1",
                                                      label = "Chart 1",
                                                      TimeFrameOptions,
                                                      selected=list("Calendar Year"),
                                                      multiple = F)),

                              menuSubItem(tabName = "widgets",
                                          selectInput(inputId = "TimeScale2",
                                                      label = "Chart 2",
                                                      TimeFrameOptions,
                                                      selected=list("Calendar Year"),
                                                      multiple = F))),
                     menuItem(tabName = "widgets",
                              "y-axis display options",
                              icon  = icon("sliders-h"),
                              menuSubItem(selectInput(inputId = "YScale1",
                                                      label = "Chart 1",
                                                      YScaleOptions,
                                                      selected=list("Linear"),
                                                      multiple = F)),

                              menuSubItem(tabName = "widgets",
                                          selectInput(inputId = "YScale2",
                                                      label = "Chart 2",
                                                      YScaleOptions,
                                                      selected=list("Linear"),
                                                      multiple = F))),
                     checkboxInput(inputId = "HideLegend",
                                   label = "Hide legend",
                                   value = FALSE,
                                   width = NULL),
                     checkboxInput(inputId = "ShowBeforeMDA",
                                   label = "Show years before MDA",
                                   value = TRUE,
                                   width = NULL)

                   )
  ),

  dashboardBody(
    tabItems(
      tabItem(tabName = "Description",
              fluidRow(box(width = 12,
                           includeMarkdown("AppDescription.Rmd")))),

      tabItem(tabName = "widgets",
              fluidRow(box(plotlyOutput("Chart1",width = "100%"), width = 12)),
              fluidRow(box(plotlyOutput("Chart2",width = "100%"), width = 12))
      )
    )
  )
)


# Code for printing all simulation overlayed
#
# renderPlotly({
#   Data <- SimPrev %>%
#     subset(MDAYearsBetweenRounds==1 & MDAStartYear == 2018) %>%
#     subset(MDADrugs %in% input$ChooseDrugs) %>%
#     subset(MDADrugEfficacy %in% input$ChooseDrugAssumptions) %>%
#     subset(MDACoverageAttempted %in% input$ChooseCoverage) %>%
#     subset(MDANumRounds %in% input$ChooseRounds) %>%
#     mutate(ColorGroup = paste(MDADrugs,MDADrugEfficacy,MDACoverageAttempted,MDANumRounds,sep="-"))
#
#   Data[] <- lapply(Data, function(x) if(is.factor(x)) factor(x) else x)
#   plot_ly(data = Data,
#           x = ~Year, y = ~PrevalenceAll, split=~Seed, type = "scatter", mode='lines',alpha = 0.1, color = ~ColorGroup)
# })


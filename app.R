# This Shiny app shows the results of paper:
# Association of a child’s mental disorder with parental income and employment.
# By Komulainen, K., Niemi, R., Gutvilig, M., Böckerman, P., Momen, N., Elovainio, M., Plana-Ripoll, O., & Hakulinen, C. (2025-1-13). 
# Published as Preprint in medRxiv 2025.03.21.25324381; https://doi.org/10.1101/2025.03.21.25324381 

# Script started on 4.4.2023
# Script written by Ripsa Niemi & Mai Gutvilig

# In this Shiny app you can
# 1) Look at descriptives and matching criteria of study cohorts
# 2) Look at the marginal prediction plots of GEE models for
#   a) different outcomes: Earnings, Disposable income, Social welfare Benefits, Employment
#   b) analysis types: main analysis, child's age at diagnosis -stratification
#   c) exposure diagnoses: Child's specific diagnoses

# All results are shown separately for women and men and exposed and unexposed parents

# libraries
library(shiny)
library(shinythemes)
library(tidyverse)
library(readxl)
library(gt)


################ PREPPING ####################  

# For UI

# exposures
diags <- c("Substance use disorders (F10–F19)" = "F1", 
           "Psychotic disorders (F20–F29)" = "F2", 
           "Mood disorders (F30–F39)" = "F3",
           "Anxiety disorders (F40–F48)" = "F4", 
           "Eating disorders (F50)" = "F50",
           "Specific personality disorders (F60)" = "F60",
           "Intellectual disabilities (F70–F79)" = "F7", 
           "Pervasive developmental disorders (F84)" = "F84", 
           "Childhood onset disorders (F90–F98)" = "F9", 
           "Any mental disorder (F00–F99)" = "F10")

# gee outcomes
outcomes_gee <- c("Earnings" = "ktulo", 
                  "Disposable income" = "kturaha", 
                  "Social welfare benefits" = "saatusi", 
                  "Employment" = "work")

# types of analysis
analysis_types <- c("Main analysis" = "main", 
                    "Diagnosis age stratification" = "age") 

countries <- c("Finland" = "fin", "Denmark" = "den")

# For Server

# Key-values

# Outcomes
values_out <- c("Earnings", "Disposable income", "Social welfare Benefits", "Employment")
keys_out <- c("ktulo", "kturaha", "saatusi", "work")
outcomes_list <- as.list(values_out) %>% setNames(keys_out)

# Analysis type
values_ana <- c("Main analysis", "Diagnosis age strata")
keys_ana <- c("main", "age")
analyses_list <- as.list(values_ana) %>% setNames(keys_ana)

# Exposure diagnoses
diags_list <- c("Substance use disorders (F10–F19)", 
                "Psychotic disorders (F20–F29)", 
                "Mood disorders (F30–F39)",
                "Anxiety disorders (F40–F48)", 
                "Eating disorders (F50)",
                "Specific personality disorders (F60)",
                "Intellectual disabilities (F70–F79)",
                "Pervasive developmental disorders (F84)",
                "Childhood onset disorders (F90–F98)", 
                "Any mental disorder (F00–F99)")
keys_diags <- c("F1", "F2", "F3", "F4", 
                "F50", "F60", "F7", "F84", 
                "F9", "F10")
diags_list <- as.list(diags_list) %>% setNames(keys_diags)

# Countries
countries_list <- as.list(c("Finland", "Denmark")) %>% setNames(c("fin", "den"))

# Plot y-limits
limits_list <- list(list(0, 70), list(0, 50), list(0, 15), list(0.40, 0.95)) %>% setNames(keys_out)

# Y-lab names
labs_list <- as.list(c(rep(c("Thousand Euros (€)"),3), "Probability")) %>% setNames(keys_out)

# New facet label names
facet.labs <- c("", "1–4 years", "5–9 years", "10–14 years", "5–14 years", "15–19 years", "20–25 years")
names(facet.labs) <- c(0,4,5,6,7,8,9)

# Lines
lines <- c("dashed", "solid")

# Plot colors
colors_ci <- c("#82c2cb", "#FFBE98") # Confidence intervals
colors_line <- c("#125B9A", "#F05A7E") #lines

font <- "Helvetica"

# Read in marginal predictions
margs_finnish <- read_excel("data/margs_finnish.xlsx") # finnish results
margs_danish <- read_excel("data/margs_danish.xlsx") # danish results

# Read in study cohort tables
des_tables_finnish <- read_excel("data/descr_tables_de_finnish.xlsx") # finnish descriptives
mat_tables_finnish <- read_excel("data/descr_tables_ma_finnish.xlsx") # finnish matching criteria
des_tables_danish <- read_excel("data/descr_tables_de_danish.xlsx") # danish descriptives
mat_tables_danish <- read_excel("data/descr_tables_ma_danish.xlsx") # danish matching criteria

############### SHINY APP ###########################

# Function for side panel
sidebarPanel_funk <- function(exp1, out1, ana1, outcomes_choices) {
  sidebarPanel(
    selectInput(exp1, "Select child's diagnosis:", choices = diags, selected = diags[10]),
    selectInput(out1, "Select an outcome:", choices = outcomes_choices),
    selectInput(ana1, "Select a type of analysis:", choices = analysis_types),
    
    width = 3
  )
}

# Define user interface
ui <- fluidPage(theme = shinytheme("paper"),
                tags$head(
                  tags$style(HTML("
                      # select-input style
                      .selectize-input {
                        height: 15px;
                        width: 100%;
                        font-size: 8pt;
                        padding-top: 2px;
                      }
                      
                      body {
                        font-family: 'Helvetica';
                      }
                      
                      .gt_table {
                        font-family: 'Helvetica';
                      }
                                      
                
                    "))
                ),
                
                
                navbarPage("Child's mental disorder and parental income and employment",
                           
                           # First panel for study cohort stats
                           tabPanel("Descriptive statistics",
                                    
                                    sidebarLayout(
                                      sidebarPanel(
                                        selectInput("exp_tab", "Select child's diagnosis:", choices = diags, selected = diags[10]),
                                        selectInput("type_tab", "Select a descriptive table:", choices = c("Demographics" = "demo", "Matching criteria" = "match")),
                                        selectInput("country_tab", "Select a country:", choices = c("Finland" = "fin", "Denmark" = "den")),
                                        
                                        
                                        width = 3
                                      ),
                                      
                                      
                                      mainPanel(
                                        fluidRow(
                                          column(9, gt_output("table")),
                                          width = 9
                                        ),
                                      )
                                    )
                           ),
                           
                           # Second panel for marginal prediction plots
                           tabPanel("Main and stratified analyses",
                                    
                                    sidebarLayout(
                                      sidebarPanel_funk("exp", "out", "ana", outcomes_gee),
                                      
                                      mainPanel(
                                        fluidRow(
                                          column(8, HTML("<h5>Finland</h5>"), plotOutput("geePlot_fin",
                                                                                         width = "150%")),
                                          width = 9
                                        ),
                                        fluidRow(
                                          column(8, HTML("<h5>Denmark</h5>"), plotOutput("geePlot_den",
                                                                                         width = "150%")),
                                          width = 9
                                        )
                                      )
                                    )
                           ),
                           
                           # Third panel for information
                           tabPanel("Info",
                                    
                                    mainPanel(
                                      p("This is an online supplement to preprint paper"),
                                      strong("Association of a child’s mental disorder with parental income and employment."),
                                      p(" "),
                                      p("Information on the study cohort and methods can be read in:"),
                                      p(" "),
                                      p("Komulainen, K., Niemi, R., Gutvilig, M., Böckerman, P., Momen, N., Elovainio, M., Plana-Ripoll, O., & Hakulinen, C. (2025-1-13). 
                                        Association of a child’s mental disorder with parental income and employment. 
                                        medRxiv 2025.03.21.25324381; https://doi.org/10.1101/2025.03.21.25324381 "),
                                    )
                                    
                           )
                           
                           
                           
                           
                           
                ),
                
)

# Server
server <- function(input, output) {

  # Table function
  
  # Descriptive table
  des_table_funk <- function(data, exp, country) {
    
    tab_title <- paste(countries_list[country], diags_list[exp], sep=", ")
    
    table <- data |> 
      filter(diagnosis == exp) |>
      select(-diagnosis) |>
      gt()  |>
      tab_header(
        title = md(paste("Demographics")),
        subtitle = md(tab_title)
      ) |>
      tab_footnote(
        footnote = "* Thousand Euros (€)."
      ) |>
      tab_source_note(source_note = md(
        "Measured at time 0."
      )) |>
      sub_missing(
        missing_text = ""
      ) |>
      tab_style(
        style = cell_borders(
          sides = "all",
        ),
        locations = list(
          cells_body(), 
          cells_column_labels(),
          cells_footnotes(), 
          cells_title(),
          cells_source_notes()
        )
      ) |>
      opt_table_lines(
        extent = c("none")) |>
      opt_table_font(
        font=font) |>
      tab_options(
        table.font.size = 12,
        heading.title.font.size = 14,
        heading.subtitle.font.size = 12
      )
    
    return(table)
  }
  
  # Matching criteria table
  match_table_funk <- function(data, exp, country) {
    
    tab_title <- paste(countries_list[country], diags_list[exp], sep=", ")
    
    table <- data |> 
      filter(diagnosis == exp) |>
      select(-diagnosis) |>
      gt() |>
      tab_header(
        title = md("Matching criteria"),
        subtitle = md(tab_title)
      ) |>
      sub_missing(
        missing_text = ""
      ) |>
      tab_style(
        style = cell_borders(
          sides = "all",
        ),
        locations = list(
          cells_body(), 
          cells_column_labels(),
          cells_title()
        )
      ) |>
      opt_table_lines(
        extent = c("none")) |>
      opt_table_font(
        font=font) |>
      tab_options(
        table.font.size = 12,
        heading.title.font.size = 14,
        heading.subtitle.font.size = 12
      )
    
    return(table)
  }
  
  # Output Tables: Descriptives and Matching criteria
  
  output$table <- render_gt({
    
    # Finnish
    if(input$country_tab == "fin") {
      
      if(input$type_tab == "demo") {
        
        des_table_funk(des_tables_finnish, input$exp_tab, "fin")
        
      } else if(input$type_tab == "match"){
        
        match_table_funk(mat_tables_finnish, input$exp_tab, "fin")
        
      }
    
    # Danish
    } else if(input$country_tab == "den") {
      
      if(input$type_tab == "demo") {
        
        des_table_funk(des_tables_danish, input$exp_tab, "den")
        
      } else if(input$type_tab == "match"){
        
        match_table_funk(mat_tables_danish, input$exp_tab, "den")
        
      }
      
    }
    
  },
  
  width = "150%"
  
  )
  
  # Function for plots
  marg_plot_funk <- function(out, exp, ana, dat) {
    
    # choose y-lims and y-lab from key-values
    lims <- as.vector(c(limits_list[out][[1]][[1]], limits_list[out][[1]][[2]]))
    ylab <- labs_list[out]
    
    # filter data
    data <- dat %>% filter(outcome == out & exposure == exp & analysis == ana)
    
    cols <- n_distinct(data$group) # n of facet wrap groups
    
    # plot
    plot <- data %>% 
      ggplot(aes(y=margin, x=as.factor(time), 
                 group=interaction(as.factor(diag), as.factor(parent)))) +
      geom_ribbon(aes(ymin=lci, ymax=hci, fill=as.factor(parent)), alpha=0.5) + # ci
      geom_path(aes(color=as.factor(parent), linetype=as.factor(diag))) + # line
      geom_vline(xintercept= "0", linetype="dashed", color="black", alpha=0.5) + # dash line to zero
      labs(x = "Years since diagnosis", y=ylab, fill="", color="", linetype="") + # labs
      scale_fill_manual(labels = c("Men", "Women"), values = colors_ci) + # ci cols: men, women
      scale_color_manual(labels = c("Men", "Women"), values = colors_line) + # line cols: men, women
      scale_linetype_manual(labels = c("Unexposed", "Exposed"), values = lines) + # line types
      facet_wrap(~ group, ncol = cols, labeller = labeller(group = facet.labs)) + # sub group facets
      coord_cartesian(ylim = c(lims[1], lims[2])) + # y-limits
      theme_light() +
      theme(strip.background = element_rect(fill="white"), # general theme settings
            strip.text = element_text(colour = 'black'), 
            legend.position = "bottom", 
            panel.background = element_rect(fill = "#FFFFFF", 
                                            color = "#EDF1F1", 
                                            size = 1, 
                                            linetype = "solid"),
            legend.background = element_rect(fill = "#FFFFFF"),
            legend.key = element_rect(fill = "#FFFFFF", color = NA),
            text = element_text(family=font, size = 12), 
            axis.text.y = element_text(color="black", size = 10), 
            axis.text.x = element_text(color="black", size = 10)) +
      guides(linetype = guide_legend(override.aes = list(fill = "#FFFFFF")))
    
    return(plot)
  }
  
  # GEE Margins Plot Finnish
  output$geePlot_fin <- renderPlot({
    
    marg_plot_funk(input$out, input$exp, input$ana, margs_finnish)
    
  })
  
  # GEE Margins Danish Plot
  output$geePlot_den <- renderPlot({
    
    marg_plot_funk(input$out, input$exp, input$ana, margs_danish)
    
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

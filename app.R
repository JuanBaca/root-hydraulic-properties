library(shiny)
library(tidyverse)
library(readxl)
library(scales)
library(DT)
library(magrittr)
library(data.table)
library(shinyWidgets)
library(ggasym)

gf_levels <- c("Crop", "Graminoid", "Succulent", "Shrub", "Tree", "Other")
Krs <- read_excel("./data/Krs_database.xlsx") %>%
  mutate(Growth_form = factor(Growth_form, 
                              levels = gf_levels[-3])) %>%
  arrange(Growth_form, PFT) %>%
  mutate(Species = factor(Species, levels = unique(Species)),
         PFT = factor(PFT, levels = unique(PFT)),
         Genus = factor(Genus, levels = unique(Genus))) %>%
  mutate(radial_axial = "total") %>%
  filter(Value_type == "treatment average") %>%
  select(-Value_type)
Lpr_kx <- bind_rows(read_excel("./data/kr_database.xlsx") %>%
                      mutate(radial_axial = "radial"),
                    read_excel("./data/kx_database.xlsx") %>%
                      mutate(radial_axial = "axial")) %>%
  mutate(Growth_form = factor(Growth_form, 
                              levels = gf_levels)) %>%
  arrange(Growth_form, PFT) %>%
  mutate(Species = factor(Species, levels = unique(Species)),
         PFT = factor(PFT, levels = unique(PFT)),
         Genus = factor(Genus, levels = unique(Genus))) %>%
  filter(Value_type == "treatment average") %>%
  select(-Value_type)
conductances <- list(Krs, Lpr_kx)
names(conductances) <-c("whole root system", "individual roots; root segments")
selection <- bind_rows(Krs %>%
                            select(PFT, Genus, Species, Driving_force),
                       Lpr_kx %>%
                            select(PFT, Genus, Species, Driving_force))
ui <- fluidPage(
  h1("Root hydraulic properties explorer"),
  sidebarLayout(
    sidebarPanel(
      titlePanel("Data selection"),
      selectInput('tissue', h3('1.Select by root tissue type'), 
                  choices = names(conductances)),
      conditionalPanel(condition = "input.tissue == 'individual roots; root segments'",
                       selectInput("radax", "Select the root hydraulic property",
                                   choices = c("radial", "axial"), 
                                   multiple = F)),
      selectInput("criteria", h3("2. Select by species, genus or PFT"),
                  choices = c("Show all data", "Filter by species", 
                              "Filter by genus", "Filter by PFT")),
      conditionalPanel(condition = "input.criteria == 'Filter by PFT'",
                       selectInput("PFT", "Select the plant functional group",
                                   choices = unique(selection$PFT), 
                                   multiple = T)),
      conditionalPanel(condition = "input.criteria == 'Filter by genus'",
                       selectInput("Genus", "Select the genus",
                                   choices = unique(selection$Genus), 
                                   multiple = T)),
      conditionalPanel(condition = "input.criteria == 'Filter by species'",
                       selectInput("Species", "Select the species",
                                   choices = unique(selection$Species), 
                                   multiple = T)),
      uiOutput("picker"),
      selectInput("force", h3("4. Select by driving force used for measurement"),
                  unique(selection$Driving_force)),
      titlePanel("Data download"),
      downloadButton('download',"Download all selected data"),
      downloadButton('download_sum',"Download data summary")),
    mainPanel(
      navbarPage(
        "",
        navbarMenu(
          strong("Data"),
          tabPanel(
            title = "All selected data",
            DTOutput("selected_table")
          ),
          tabPanel(
            title = "Data summary",
            DTOutput("summary_table")
          )
        ),
        navbarMenu(
          strong("Variability plots"),
          tabPanel(
            title = "Range of variability",
            plotly::plotlyOutput("range_plot", width = "100%", height = "700px"),
            textOutput("explanation_range")
          ),
          tabPanel(
            title = "Range of variability (normalized)",
            uiOutput("range_norm")
          ),
          tabPanel(
            title = "Differences among PFT's",
            plotly::plotlyOutput("PFT_plot",
                                 width = "100%",
                                 height = "700px"),
            textOutput("explanation_PFT")
          )
        ),
        navbarMenu(
          strong("Factors for variation"),
          tabPanel(
            title = "Effect of age on crop Krs",
            uiOutput("Krs_age"),
            textOutput("explanation_age")
            ),
          tabPanel(
            title = "Krs response to drought stress",
            plotly::plotlyOutput("resp_drought"),
            textOutput("explanation_drought")
            ),
          tabPanel(
            title = "Responses to AQP inhibition",
            uiOutput("resp_AQP"),
            textOutput("explanation_AQP"))),
        navbarMenu(
          strong("About the Database"),
          tabPanel(title = "Detailled description",
                   textOutput("description")),
          tabPanel(title = "How to cite",
                   textOutput("citation")),
          tabPanel("Original data source",
                   textOutput("data_source")))))))

server <- function(input, output, session) {
  filt1 <- reactive({
    if (input$tissue == "whole root system") {
      conductances2 <- conductances %>%
        extract(input$tissue) %>%
        as.data.table(.)
      colnames(conductances2) <- gsub(".*\\.", "", colnames(conductances2))
      conductances2 %>%
        filter(Driving_force %in% input$force) %>%
        select(-Driving_force)
    }
    else {
      conductances2 <- conductances %>%
        extract(input$tissue) %>%
        as.data.table(.)
      colnames(conductances2) <- gsub(".*\\.", "", colnames(conductances2))
      conductances2 %>%
        filter(Driving_force %in% input$force,
               radial_axial %in% input$radax) %>%
        select(-Driving_force)
    }
  })
  filtered <- reactive({
    if (input$criteria == "Show all data") {
      filt1() 
    }
    else if (input$criteria == "Filter by PFT") {
      filt1() %>%
        filter(PFT %in% input$PFT)
    }
    else if (input$criteria == "Filter by genus") {
      filt1() %>%
        filter(Genus %in% input$Genus)
    }
    else {
     filt1() %>%
        filter(Species %in% input$Species)
    }
  })
  drought <- reactive({
    if (input$tissue == "whole root system") {
      filtered() %>%
        select(Reference:Treatment_level1, Treatment_level2, Treatment_level3,
               Root_type:Conductance_leafA) %>%
        filter(str_detect(Experimental_treatment, "Drought"),
               Treatment_level1 %in% c("Control", "Drought", "Drought severe")) %>%
        mutate(Treatment_level1 = ifelse(Treatment_level1 == "Control", 
                                         "Control", "Drought")) %>%
        pivot_longer(cols = Conductance:Conductance_leafA) %>%
        filter(value != is.na(value)) %>%
        group_by(Reference, Species, Genus, PFT, Growth_form,
                 Age_days, Treatment_level1, Treatment_level2, Treatment_level3) %>%
        summarise(v = mean(value)) %>%
        pivot_wider(names_from = "Treatment_level1", values_from = v) %>%
        mutate(r = Drought/Control) %>%
        group_by(Reference, Species, PFT) %>%
        summarise(r_mean = (exp(mean(log(r), na.rm = T))-1)*100)
    }
    else {
      filtered() %>%
        select(Reference:Treatment_level1, Treatment_level2, Treatment_level3,
               Root_type:Conductivity) %>%
        filter(str_detect(Experimental_treatment, "Drought"),
               Treatment_level1 %in% c("Control", "Drought", "Drought severe")) %>%
        mutate(Treatment_level1 = ifelse(Treatment_level1 == "Control", 
                                         "Control", "Drought")) %>%
        pivot_longer(cols = Conductance:Conductivity) %>%
        filter(value != is.na(value)) %>%
        group_by(Reference, Species, Genus, PFT, Growth_form, Age_days, 
                 Treatment_level1, Treatment_level2, Treatment_level3) %>%
        summarise(v = mean(value)) %>%
        pivot_wider(names_from = "Treatment_level1", values_from = v) %>%
        mutate(r = Drought/Control) %>%
        group_by(Reference, Species, PFT) %>%
        summarise(r_mean = (exp(mean(log(r), na.rm = T))-1)*100)
    }
  })
  AQP <- reactive({
    if (input$tissue == "whole root system") {
      bind_rows(filtered() %>%
                  select(Reference:Treatment_level1, Treatment_level2, Treatment_level3,
                         Root_type:Conductance_leafA) %>%
                  filter(str_detect(Experimental_treatment, "AQP"),
                         !str_detect(Experimental_treatment, "Drought")),
                filtered() %>%
                  select(Reference:Treatment_level1, Treatment_level2, Treatment_level3,
                         Root_type:Conductance_leafA) %>%
                  filter(str_detect(Experimental_treatment, "AQP"),
                         str_detect(Experimental_treatment, "Drought")) %>%
                  swap_cols(Treatment_level1, Treatment_level2))  %>%
        filter(Treatment_level1 %in% c("Control", "AQPinhibition")) %>%
        pivot_longer(cols = Conductance:Conductance_leafA) %>%
        filter(value != is.na(value)) %>%
        group_by(Reference, Species, Genus, PFT, Growth_form,
                 Age_days, Treatment_level1, Treatment_level2, Treatment_level3) %>%
        summarise(v = mean(value)) %>%
        pivot_wider(names_from = "Treatment_level1", values_from = v) %>%
        mutate(r = AQPinhibition/Control) %>%
        group_by(Reference, Species, PFT) %>%
        summarise(r_mean = (exp(mean(log(r), na.rm = T))-1)*100)
    }
    else {
      bind_rows(filtered() %>%
                  select(Reference:Treatment_level1, Treatment_level2, Treatment_level3,
                         Root_type:Conductivity) %>%
                  filter(str_detect(Experimental_treatment, "AQP"),
                         !str_detect(Experimental_treatment, "Drought")),
                filtered() %>%
                  select(Reference:Treatment_level1, Treatment_level2, Treatment_level3,
                         Root_type:Conductivity) %>%
                  filter(str_detect(Experimental_treatment, "AQP"),
                         str_detect(Experimental_treatment, "Drought")) %>%
                  swap_cols(Treatment_level1, Treatment_level2))  %>%
        filter(Treatment_level1 %in% c("Control", "AQPinhibition")) %>%
        pivot_longer(cols = Conductance:Conductivity) %>%
        filter(value != is.na(value)) %>%
        group_by(Reference, Species, Genus, PFT, Growth_form, 
                 Age_days, Treatment_level1, Treatment_level2, Treatment_level3) %>%
        summarise(v = mean(value)) %>%
        pivot_wider(names_from = "Treatment_level1", values_from = v) %>%
        mutate(r = AQPinhibition/Control) %>%
        group_by(Reference, Species, PFT) %>%
        summarise(r_mean = (exp(mean(log(r), na.rm = T))-1)*100)
    }
  })
  ref_species  <- reactive({
    if (input$tissue == "whole root system") {
      filtered() %>%
        mutate(TT = case_when(Treatment_type1 == "Other" ~ "Other",
                              Experimental_treatment == "No treatment" ~ "No treatment",
                              Treatment_type2 == "Other" ~ Treatment_level1,
                              T ~ paste(Treatment_level1,Treatment_level2))) %>%
        filter(PFT != "Other") %>%
        group_by(Reference, Species, Genus, PFT, Growth_form, TT) %>%
        summarise(Conductance = mean(Conductance),
                  Conductance_surface_normalized = mean(Conductivity)) %>%
        mutate(Treatment = ifelse(TT %in% c("Control", "Control NA", "Control Control",
                                            "Other", "No treatment"), 
                                  "Control", "Non control")) %>%
        ungroup(.) %>%
        select(-TT)
    }
    else if (input$tissue == "individual roots; root segments" &
             input$radax == "radial") {
      filtered() %>%
        mutate(TT = case_when(Treatment_type1 == "Other" ~ "Other",
                              Experimental_treatment == "No treatment" ~ "No treatment",
                              Treatment_type2 == "Other" ~ Treatment_level1,
                              T ~ paste(Treatment_level1,Treatment_level2))) %>%
        filter(PFT != "Other") %>%
        group_by(Reference, Species, Genus, PFT, Growth_form, TT, Root_section) %>%
        summarise(Conductivity = mean(Conductivity)) %>%
        mutate(Treatment = ifelse(TT %in% c("Control", "Control NA", "Control Control",
                                            "Other", "No treatment"), 
                                  "Control", "Non control")) %>%
        ungroup(.) %>%
        select(-TT)
    }
    else {
      filtered() %>%
        mutate(TT = case_when(Treatment_type1 == "Other" ~ "Other",
                              Experimental_treatment == "No treatment" ~ "No treatment",
                              Treatment_type2 == "Other" ~ Treatment_level1,
                              T ~ paste(Treatment_level1,Treatment_level2))) %>%
        filter(PFT != "Other") %>%
        group_by(Reference, Species, Genus, PFT, Growth_form, TT, Root_section) %>%
        summarise(Conductance = mean(Conductance), 
                  Conductivity = mean(Conductivity)) %>%
        mutate(Treatment = ifelse(TT %in% c("Control", "Control NA", "Control Control",
                                            "Other", "No treatment"), 
                                  "Control", "Non control")) %>%
        ungroup(.) %>%
        select(-TT)
    }
  })
  PFT_averages <- reactive({
    if (input$tissue == "whole root system") {
      ref_species() %>%
        pivot_longer(cols = Conductance:Conductance_surface_normalized) %>%
        group_by(PFT, Growth_form, name) %>% 
        summarise(count = n(),
                  minimum = min(value, na.rm = T),
                  average = mean(value, na.rm = T),
                  maximum = max(value, na.rm = T)) %>%
        filter(average != is.na(average))
    }
    else if (input$tissue == "individual roots; root segments" &
             input$radax == "radial") {
      ref_species() %>%
        group_by(PFT, Growth_form) %>% 
        summarise(count = n(),
                  minimum = min(Conductivity, na.rm = T),
                  average = mean(Conductivity, na.rm = T),
                  maximum = max(Conductivity, na.rm = T)) %>%
        filter(average != is.na(average))
    }
    else {
      ref_species() %>%
        pivot_longer(cols = Conductance:Conductivity) %>%
        group_by(PFT, Growth_form, name) %>% 
        summarise(count = n(),
                  minimum = min(value, na.rm = T),
                  average = mean(value, na.rm = T),
                  maximum = max(value, na.rm = T)) %>%
        filter(average != is.na(average))
    }
  })
  column_sel <- reactive({
    if (input$tissue == "whole root system") {
      c("Reference", "Species", "Genus", "Family", "PFT", "Conductance", 
        "Conductivity", "Conductance_DW", "Conductance_FW", "Conductance_leafA",
        "Experimental_treatment", "Measurement_Method")
    }
    else if (input$tissue == "individual roots; root segments" &
             input$radax == "radial") {
      c("Reference", "Species", "Genus", "Family", "PFT", "Conductivity", 
        "Experimental_treatment", "Measurement_Method")
    }
    else {
      c("Reference", "Species", "Genus", "Family","PFT", "Conductance", 
        "Conductivity", "Experimental_treatment", "Measurement_Method")
    }
  })
  output$picker <- renderUI({
    pickerInput(inputId = 'pick', 
                label = 'Choose',
                selected =  column_sel(),
                choices = colnames(filtered()),
                options = list(`actions-box` = TRUE), multiple = T)
  })
  #all selected data (for export)
  output$selected_table <- renderDT({filtered() %>% 
      select(input$pick)
  })
  output$download <- downloadHandler(
    filename = function() {"root_hydraulics.csv"},
    content = function(file) {
      write.csv(filtered() %>% 
                  select(input$pick), file, row.names = FALSE)
    }
  )
  #Summary tables
  data_summary <- reactive({
    if (input$tissue == "whole root system") {
      ref_species() %>%
        ungroup(.) %>%
        filter(Conductance != is.na(Conductance) |
                 Conductance_surface_normalized != is.na(Conductance_surface_normalized)) 
    }
    else if (input$tissue == "individual roots; root segments" &
             input$radax == "radial") {
      ref_species() %>%
        ungroup(.) %>%
        filter(Conductivity != is.na(Conductivity)) 
    } 
    else {
      ref_species() %>%
        ungroup(.) %>%
        filter(Conductance != is.na(Conductance) |
                 Conductivity != is.na(Conductivity)) 
    } 
  })
  output$summary_table <- 
    renderDT({data_summary()
    })
  output$download_sum <- downloadHandler(
    filename = function() {"root_hydraulics.csv"},
    content = function(file) {
      write.csv(data_summary(), 
                file, row.names = FALSE)
    }
  )
  #conductivity/conductance variability for all selected species/studies
  output$range_plot <- 
    plotly::renderPlotly({
      if (input$tissue == "whole root system") {
        filtered() %>%
          group_by(Reference, Species, PFT, Growth_form) %>%
          summarise(C = mean(Conductance, na.rm = T)) %>%
          filter(C != is.na(C)) %>%
          mutate(`Reference and species` = paste(Reference, Species, 
                                                 sep = "\n")) %>%
          ggplot(aes(PFT, C,
                     label = `Reference and species`, color = Growth_form)) +
          geom_point() +
          scale_y_log10() +
          coord_flip() +
          theme_classic()
      }
      else if (input$tissue == "individual roots; root segments" &
               input$radax == "radial") {
        filtered() %>%
          group_by(Reference, Species, PFT, Growth_form, Root_section) %>%
          summarise(C = mean(Conductivity, na.rm = T)) %>%
          filter(C != is.na(C)) %>%
          mutate(`Reference and species` = paste(Reference, Species, 
                                                 sep = "\n")) %>%
          ggplot(aes(PFT, C,
                     label = `Reference and species`, color = Growth_form,
                     shape = Root_section)) +
          geom_point() +
          scale_y_log10() +
          coord_flip() +
          theme_classic() +
          guides(color = F)
      }
      else {
        filtered() %>%
          group_by(Reference, Species, PFT, Growth_form, Root_section) %>%
          summarise(c = mean(Conductivity, na.rm = T),
                    C = mean(Conductance)) %>%
          filter(c != is.na(c) | C != is.na(C)) %>%
          pivot_longer(cols = c(c, C), 
                       names_to = "Krs_krs", values_to = "cond") %>%
          mutate(`Reference and species` = paste(Reference, Species, 
                                                 sep = "\n")) %>%
          ggplot(aes(PFT, cond,
                     label = `Reference and species`, color = Growth_form,
                     shape = Root_section)) +
          geom_point() +
          facet_wrap(~Krs_krs, scales = "free_x") +
          scale_y_log10() +
          coord_flip() +
          theme_classic() +
          guides(color = F) 
      }
    })
  output$explanation_range <-
    renderText("This graph shows all data reported in the literature")
  output$range_norm <- renderUI({
    if (input$tissue == "whole root system") {
      plotly::plotlyOutput("range_norm_plot")
    } 
    else {
      textOutput("warning_range_norm") 
    }
  })
  output$range_norm_plot <- 
    plotly::renderPlotly({
      filtered() %>%
        group_by(Reference, Species, PFT, Growth_form) %>%
        summarise(c = mean(Conductivity, na.rm = T),
                  C_DW = mean(Conductance_DW, na.rm = T),
                  C_FW = mean(Conductance_FW, na.rm = T),
                  C_leaf = mean(Conductance_leafA, na.rm = T)
                  ) %>%
        filter(c != is.na(c) | C_DW != is.na(C_DW) | 
                 C_FW != is.na(C_FW) | C_leaf != is.na(C_leaf))  %>%
        pivot_longer(cols = c:C_leaf, 
                     names_to = "Krs_krs", values_to = "cond") %>%
        mutate(`Reference and species` = paste(Reference, Species, 
                                               sep = "\n")) %>%
        ggplot(aes(PFT, cond,
                   label = `Reference and species`, color = Growth_form)) +
        geom_point() +
        facet_wrap(~Krs_krs, scales = "free_x") +
        scale_y_log10() +
        coord_flip() +
        theme_classic()
    })
  output$warning_range_norm <- renderText("This graph only applies for
                                          the whole root system data")
  #graphical comparison between PFT's
  output$PFT_plot <-
    plotly::renderPlotly({
      if (input$tissue == "individual roots; root segments" &
          input$radax == "radial") {
        PFT_averages() %>%
          ggplot(aes(PFT, average)) +
          geom_segment(aes(x = , xend = PFT,
                           y = minimum, yend = maximum, color = Growth_form), 
                       linewidth = 5, alpha = .7) +
          geom_point(color = "black", size = 3.5) +
          scale_y_log10() +
          coord_flip() +
          theme_classic() +
          theme(legend.position = "none")
      }
      else {
        PFT_averages() %>%
          ggplot(aes(PFT, average)) +
          geom_segment(aes(x = , xend = PFT,
                           y = minimum, yend = maximum, color = Growth_form), 
                       linewidth = 5, alpha = .7) +
          geom_point(color = "black", size = 3.5) +
          facet_wrap(~name, scales = "free_x") +
          scale_y_log10() +
          coord_flip() +
          theme_classic() +
          theme(legend.position = "none")
      }
    })
  #Effect of age on Krs
  output$Krs_age <- renderUI({
    if (input$dataset != "total") {
      textOutput("warning_age") # 
      } 
    else {
      plotly::plotlyOutput("age_plot") # create a plot
      }
    })
  output$age_plot <- 
    plotly::renderPlotly({
      filtered() %>%
        filter(str_detect(PFT, "Crop") | str_detect(PFT, "grass")) %>%
        group_by(Species, PFT, Age_days, Reference) %>%
        summarise(Krs = mean(Conductance, na.rm = T)) %>%
        ggplot(aes(Age_days, Krs, color = Species, label = Reference)) +
        geom_point() +
        geom_smooth(method = "lm", formula = y~exp(1/x), se = F, 
                    linetype = "longdash") +
        scale_y_log10() +
        facet_wrap(~PFT, scales = "free_x")
    })
  output$explanation_age <-
    renderText("Graph showing the relationship between root age and Krs in 
               different crop types")
  output$warning_age <- renderText("Please select the total conductance dataset")
  #Effect of drought on Krs / kr / kx
  output$resp_drought <-  
    plotly::renderPlotly({
      drought() %>%
        mutate(`Reference and species` = paste(Reference, Species, 
                                               sep = "\n")) %>%
        ggplot(aes(PFT, r_mean, color = PFT, 
                   label = `Reference and species`)) +
        geom_point() +
        geom_hline(yintercept = 0, linetype = "longdash") +
        coord_flip() +
        theme_classic() +
        theme(legend.position = "none")
      })
  output$explanation_drought <-
    renderText("Graph showing the reponse of Krs to drought treatments")
  output$warning_drought <- renderText("Please select total or radial dataset")
  #Effect of AQP inhibition on Krs
  output$resp_AQP <- renderUI({
    if (input$tissue == "individual roots; root segments" &
        input$radax == "axial") {
      textOutput("warning_AQP") # 
    } 
    else {
      plotly::plotlyOutput("AQP_plot") # create a plot
    }
  })
  output$AQP_plot <- 
    plotly::renderPlotly({
      AQP() %>%
        mutate(`Reference and species` = paste(Reference, Species, 
                                               sep = "\n")) %>%
        ggplot(aes(PFT, r_mean, color = PFT, 
                   label = `Reference and species`)) +
        geom_point() +
        geom_hline(yintercept = 0, linetype = "longdash") +
        coord_flip() +
        theme_classic() +
        theme(legend.position = "none")
    })
  output$explanation_AQP <-
    renderText("Graph showing the response of Krs to AQP inhibition")
  output$warning_AQP <- renderText("Please select a different dataset")
  }

shinyApp(ui = ui, server = server)



 

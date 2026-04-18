library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(Hmisc)
library(NeuroDataSets)

# Load Data
data <- AD_biomarkers_tbl_df

# Rename Variables for better viewing
Mapper <- c(
  "Total Tau (pg/mL)" = "tau",
  "Thyroid Stimulating Hormone (uIU/mL)" = "Thyroid_Stimulating_Hormone",
  "Transforming Growth Factor alpha (pg/mL)" = "TGF-alpha",
  "Cortisol (ng/mL)" = "Cortisol",
  "Myoglobin (ng/mL)" = "Myoglobin",
  "Vascular Endothelial Growth Factor (pg/mL)" = "VEGF",
  "Complement C3 (mg/mL)" = "Complement_3",
  "Follicle-Stimulating Hormone (ng/mL)" = "FSH_Follicle_Stimulation_Hormon",
  "Thrombopoetin (ng/mL)" = "Thrombopoetin",
  "IL-6 Receptor (ng/mL)" = "IL_6_Receptor",
  "Leptin (ng/mL)" = "Leptin",
  "Insulin (uIU/mL)" = "Insulin",
  "Cognitive Status" = "Class",
  "Gender" = "male"
  )
data <- data %>% rename(all_of(Mapper))

# Define button mapping
Outcomes = c("Total Tau (pg/mL)",
             "Thyroid Stimulating Hormone (uIU/mL)",
             "Transforming Growth Factor alpha (pg/mL)",
             "Cortisol (ng/mL)",
             "Myoglobin (ng/mL)",
             "Vascular Endothelial Growth Factor (pg/mL)",
             "Complement C3 (mg/mL)",
             "Follicle-Stimulating Hormone (ng/mL)",
             "Thrombopoetin (ng/mL)",
             "IL-6 Receptor (ng/mL)",
             "Leptin (ng/mL)",
             "Insulin (uIU/mL)"
                )

# Factor Gender Variable for mapping\

data$Gender <-  factor(data$Gender, levels = c(0,1), labels = c("Female", "Male"))
                        

# Define UI for application
ui <- fluidPage(
  layout_columns(
    card(
      radioButtons("exposure", "Exposure Variable:", choices = Exposures),
      radioButtons("outcome", "Outcome Variable", choices = Outcomes),
      input_task_button("start", "Start Fit"),
      textOutput("Citation")
      ),
    card(
      plotOutput("results")
    )
    )
  )

# Define server logic required
server <- function(input, output) {
  output$Citation <-  renderText("Citation:
                                 Rabie El Kharoua. (2024). 🧠 Alzheimer's Disease Dataset 🧠 [Data set]. 
                                 Kaggle. https://doi.org/10.34740/KAGGLE/DSV/8668279")
  simPlot <- eventReactive(
    input$start,
    {
      inputx <- input$exposure
      inputy <- input$outcome
      ggplot(data, aes(x = .data[[inputx]], y = .data[[inputy]])) + 
        stat_summary(fun = "mean", geom = "bar", colour = 'black', fill = "white") +
        stat_summary(fun.data = mean_cl_normal, geom = "errorbar", colour = "black", width = 0.2)
    }
   )
  output$results <- renderPlot(simPlot())
}

# Run the application 
shinyApp(ui = ui, server = server)

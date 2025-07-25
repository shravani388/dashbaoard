library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(DT)
library(readr)

# Load and clean data
data <- read_csv("C:/Users/CCC/Documents/internship/final_excel.csv") %>%
  select(Institute, Program, Quota, Category, Gender, `Closing Rank`) %>%
  mutate(
    `Closing Rank` = as.numeric(`Closing Rank`),
    college = Institute,
    branch = Program,
    quota = Quota,
    category = Category,
    gender = Gender,
    cutoff_percentile = 100 * (1 - (`Closing Rank` / max(`Closing Rank`, na.rm = TRUE)))
  ) %>%
  select(college, branch, quota, category, gender, cutoff_percentile)

# UI
ui <- dashboardPage(
  dashboardHeader(title = "🎓 JEE College Predictor"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("chart-bar")),
      numericInput("percentile", "Enter JEE Percentile:", min = 0, max = 100, value = 90, step = 0.1),
      selectInput("quota", "Select Quota:", choices = c("All", unique(data$quota)), selected = "All"),
      selectInput("gender", "Select Gender:", choices = c("All", unique(data$gender)), selected = "All"),
      selectInput("category", "Select Category:", choices = c("All", unique(data$category)), selected = "All")
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard",
              fluidRow(
                box(title = "Eligible Colleges and Branches", width = 12, status = "primary", solidHeader = TRUE,
                    DTOutput("results"))
              ),
              fluidRow(
                box(title = "Top Colleges by Eligible Branches", width = 12, status = "info", solidHeader = TRUE,
                    plotOutput("collegeBar", height = "400px"))
              ),
              fluidRow(
                box(title = "Top 6 Branches + Others", width = 12, status = "success", solidHeader = TRUE,
                    plotOutput("branchPie", height = "400px"))
              )
      )
    )
  )
)

# Server
server <- function(input, output) {
  
  eligibleData <- reactive({
    filtered <- data %>%
      filter(cutoff_percentile <= input$percentile)
    
    if (input$quota != "All") filtered <- filtered %>% filter(quota == input$quota)
    if (input$gender != "All") filtered <- filtered %>% filter(gender == input$gender)
    if (input$category != "All") filtered <- filtered %>% filter(category == input$category)
    
    filtered %>% arrange(desc(cutoff_percentile))
  })
  
  output$results <- renderDT({
    edata <- eligibleData()
    if (nrow(edata) == 0) {
      return(datatable(
        data.frame(Message = "❌ No eligible colleges/branches for this percentile and filter."),
        options = list(dom = 't'),
        rownames = FALSE
      ))
    }
    datatable(edata, options = list(pageLength = 10), rownames = FALSE)
  })
  
  output$collegeBar <- renderPlot({
    edata <- eligibleData()
    if (nrow(edata) == 0) return(NULL)
    
    top_colleges <- edata %>%
      count(college, sort = TRUE) %>%
      top_n(10, n) %>%
      mutate(college = reorder(college, n))
    
    ggplot(top_colleges, aes(x = college, y = n, fill = n)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = n), hjust = -0.2, size = 4.2) +
      coord_flip() +
      scale_fill_gradient(low = "#AED6F1", high = "#1B4F72") +
      labs(
        title = "Top Colleges by Number of Eligible Branches",
        x = "College",
        y = "Number of Eligible Branches",
        fill = "Branch Count"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
        axis.text.y = element_text(size = 11),
        axis.text.x = element_text(size = 11),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "right"
      ) +
      expand_limits(y = max(top_colleges$n) + 2)
  })
  
  output$branchPie <- renderPlot({
    edata <- eligibleData()
    if (nrow(edata) == 0) return(NULL)
    
    branch_count <- edata %>%
      count(branch, sort = TRUE)
    
    top_branches <- branch_count %>% top_n(6, n)
    other_count <- sum(branch_count$n) - sum(top_branches$n)
    
    plot_data <- top_branches
    if (other_count > 0) {
      plot_data <- rbind(plot_data, data.frame(branch = "Others", n = other_count))
    }
    
    plot_data <- plot_data %>%
      mutate(percent = round(100 * n / sum(n), 1),
             short_label = case_when(
               grepl("Data Science", branch, ignore.case = TRUE) ~ "CSE (DS)",
               grepl("Machine Learning", branch, ignore.case = TRUE) ~ "CSE (ML)",
               grepl("Artificial Intelligence", branch, ignore.case = TRUE) ~ "CSE (AI)",
               grepl("Internet of Things", branch, ignore.case = TRUE) ~ "ECE (IoT)",
               grepl("Communication", branch, ignore.case = TRUE) ~ "ECE",
               grepl("Human Computer", branch, ignore.case = TRUE) ~ "CSE (HCI)",
               grepl("Industrial Design", branch, ignore.case = TRUE) ~ "Design",
               grepl("Chemical", branch, ignore.case = TRUE) ~ "Chemical",
               grepl("Mechanical", branch, ignore.case = TRUE) ~ "Mechanical",
               grepl("Civil", branch, ignore.case = TRUE) ~ "Civil",
               TRUE ~ substr(branch, 1, 20)
             ),
             legend_label = paste0(short_label, " (", percent, "%)"))
    
    ggplot(plot_data, aes(x = "", y = n, fill = legend_label)) +
      geom_bar(stat = "identity", width = 1, color = "white") +
      coord_polar("y", start = 0) +
      theme_void(base_size = 13) +
      theme(legend.position = "bottom") +
      labs(title = "Top 6 Branches + Others", fill = "Branch (with %)")
  })
}

# Run the app
shinyApp(ui, server)

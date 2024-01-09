library(shiny)
library(DT) 


# Data frame sesuai dengan yang Anda berikan
CTR <- data.frame(
  ad_placement=c("Day 1", "Day 2", "Day 3", "Day 4", "Day 5", "Day 6", "Day 7", "Day 8", "Day 9", "Day 10"),
  left_sidebar=c(2.5, 2.7, 2.8, 2.6, 3.0, 2.4, 2.9, 2.5, 2.6, 2.7),
  center_page=c(3.8, 3.5, 4.0, 3.7, 3.9, 3.6, 4.1, 3.4, 3.8, 3.9),
  right_sidebar=c(3.1, 2.9, 3.0, 3.2, 3.3, 2.8, 3.4, 3.1, 3.2, 3.5)
)

# Define UI
ui <- fluidPage(
  titlePanel("Dashboard ANOVA CTR (Click-Through Rates)"),
  sidebarLayout(
    sidebarPanel(
      numericInput("day", "Day", value = 1),
      numericInput("left_sidebar", "Left Sidebar", value = 0),
      numericInput("center_page", "Center Page", value = 0),
      numericInput("right_sidebar", "Right Sidebar", value = 0),
      actionButton("add_data", "Tambahkan Data"),
      br(),
      actionButton("delete_data", "Hapus Data"),
      br(),
      actionButton("analyze_button", "Hasil ANOVA & Boxplot")
    ),
    mainPanel(
      DTOutput("data_table"),
      verbatimTextOutput("anova_summary"),
      verbatimTextOutput("keterangan_text"),
      plotOutput("boxplot_output")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  data <- reactiveVal(CTR) # Inisialisasi data dengan CTR yang telah didefinisikan sebelumnya
  
  observeEvent(input$add_data, {
    new_entry <- data.frame(ad_placement = paste("Day", input$day),
                            left_sidebar = input$left_sidebar,
                            center_page = input$center_page,
                            right_sidebar = input$right_sidebar)
    data(rbind(data(), new_entry))
  })
  
  output$data_table <- renderDT({
    datatable(data(), selection = "single", editable = TRUE)
  })
  
  observeEvent(input$delete_data, {
    selected_row <- input$data_table_rows_selected
    if (length(selected_row) > 0) {
      data(data()[-selected_row, ])
    }
  })
  
  observeEvent(input$analyze_button, {
    CTR <- data()
    CTR_vector <- c(CTR$left_sidebar, CTR$center_page, CTR$right_sidebar)
    Ad_Place <- gl(3, nrow(CTR), labels = c("Left Sidebar", "Center Page", "Right Sidebar"))
    new_data_B <- data.frame(CTR = CTR_vector, Ad_Place)
    
    # Analisis ANOVA
    CTR.aov <- aov(CTR ~ Ad_Place, data = new_data_B)
    output$anova_summary <- renderPrint({
      summary(CTR.aov)
    })
    
    output$boxplot_output <- renderPlot({
      CTR <- data()
      boxplot(CTR[, -1], col = c("skyblue", "lightgreen", "lightcoral"),
              main = "Boxplot Ad Placement", xlab = "Ad Placement", ylab = "CTR")
    })
  })
  
  output$keterangan_text <- renderText({
    "Analisis Variansi (ANOVA) adalah uji statistik yang digunakan untuk menentukan apakah terdapat perbedaan signifikan antara rata-rata dari tiga atau lebih kelompok. Hipotesis nolnya menyatakan bahwa tidak ada perbedaan yang signifikan antara rata-rata kelompok, sementara hipotesis alternatifnya menyatakan bahwa setidaknya satu pasang kelompok memiliki rata-rata yang berbeda secara signifikan. Evaluasi dilakukan dengan melihat nilai p-value yang terkait dengan statistik uji F. Jika nilai p-value kurang dari tingkat signifikansi yang dipilih (biasanya 0.05), kita menolak hipotesis nol dan menyimpulkan adanya perbedaan yang signifikan antara setidaknya satu pasang kelompok. Jika nilai p-value lebih besar dari tingkat signifikansi yang dipilih, tidak cukup bukti statistik untuk menyimpulkan adanya perbedaan yang signifikan antara kelompok yang diuji."
  })
}

# Run the application
shinyApp(ui = ui, server = server)

# Copyright (c) 2019 YA-androidapp(https://github.com/YA-androidapp) All rights reserved.

library(shiny)
library(cluster)
library(RColorBrewer)

# Define UI for application that draws a histogram
ui <- fluidPage(

# Application title
  titlePanel("アップロードしたCSVファイルに含まれるデータセットのクラスタリング"),

# Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      fileInput(
        "file",
        "CSVファイルを選択",
        accept = c("text/csv", "text/comma-separated-values,text/plain",  ".csv")
      ),
      tags$hr(),
      htmlOutput("colname0"), # ラベルを含む列
      htmlOutput("colname1"), # x軸を含む列
      htmlOutput("colname2"), # y軸を含む列
      selectInput("clustering_method", "the method of cluster analysis", c("Hierarchical" = "hclust", "k-means" = "k-means")),
      htmlOutput("number"), # クラスターの数
      actionButton("submit", "plot")
    ),

# Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        type = "tabs",
        tabPanel("Table", tableOutput('table')),
        tabPanel("Plot", plotOutput("plot"))
      )
    )
  )
)

getColumnsWhichhasUniqueItems = function(df) {
  result = c()

  cols = colnames(df)
  for (col in cols) {
    vec = as.vector(df[, col])
    if (is.character(vec) && length(unique(vec)) == length(vec)) {
      # 要素の型が文字列型
      # かつ
      # そのラベル列の要素に重複するものがない
      result = c(result, col)
    }
  }

  return(as.character(result))
}

# Define server logic required to draw a histogram
server = function(input, output, session) {

  observeEvent(input$file, {

    csv_file = reactive(read.csv(input$file$datapath))
    output$table = renderTable(csv_file())

    output$colname0 = renderUI({
      selectInput("l", "label", getColumnsWhichhasUniqueItems(csv_file()), selected = getColumnsWhichhasUniqueItems(csv_file())[1])
    })
    output$colname1 = renderUI({
      selectInput("x", "x-axis", colnames(csv_file()), selected = colnames(csv_file())[2])
    })
    output$colname2 = renderUI({
      selectInput("y", "y-axis", colnames(csv_file()), selected = colnames(csv_file())[3])
    })
    output$number = renderUI({
      numericInput("number", "the number of clusters", 3, min = 1, max = nrow(csv_file()))
    })
  })

  observeEvent(input$submit, {
    cols = colorRampPalette(c("#0068b7", "white", "#f39800"))

    csv_file = reactive(read.csv(input$file$datapath))

    l = csv_file()[input$l]
    x = csv_file()[input$x]
    y = csv_file()[input$y]

    data = cbind(x, y)

    labelvec = as.vector(csv_file()[, input$l])
    if (length(unique(labelvec)) == length(labelvec)) {
      # ラベル列に重複する要素がない場合は行名に代入
      rownames(data) = labelvec
    } else {
      rownames(data) = 1:length(labelvec)
    }

    if (input$clustering_method == "hclust") {
      hc = hclust(dist(data))
      clusters = cutree(hc, input$number)
      color = clusters
    }
    else {
      #select k-means
      clusters = kmeans(data, input$number)
      color = clusters$cluster
    }

    output$plot = renderPlot({
      # plot(data, col = color, pch = 20, cex = 3)
      clusplot(data, color, color = TRUE, shade = TRUE, labels = 2, lines = 0, col.clus = brewer.pal(4, "RdYlBu"), col.txt = "#22313F", col.p = "#22313F")
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)

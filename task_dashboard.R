library(shiny)
library(shinydashboard)
library(reactable)
library(dplyr)
library(data.table)
library(tidyr)

task_data <- readRDS("data/task_data.rds")
gene_symbol_choices <- unique(sort(paste0(task_data$gene_symbol, " - ", task_data$label)))
go_term_separated <- task_data %>% select(go_term_label) %>% separate_rows(go_term_label, sep = ",")
go_term_choices <- sort(unique(pull(go_term_separated, go_term_label)))
all_choices <- c(gene_symbol_choices, go_term_choices)

CSS <- "
h2 {
  height: 380px; width: 190px; color: #F5F5DC; border: 2px solid coral; border-radius: 10px;
  font-size: 16px; top: 400px;
}
.menu {position: fixed; display: block; bottom: 30px; left: 0px; text-align: center;}
img {margin-bottom: 20px; bottom: 10px; left: 10px;}
.skin-blue .sidebar-menu li a {color: #F5F5DC; font-family: Arial Black; font-size: 16px;}
.main-sidebar {background-color: #101F2D !important; opacity: 0.8;}
.main-header .logo {
  padding: 20px; text-align: center; height: 120px; background-color: #101F2D !important;
  color: white !important; font-weight: bold; font-size: 30px;
}
.content-wrapper {background-color: #ECF0BB;}
"

header <- dashboardHeader(
  title = span(
    tagList(
      tags$img(
        src = "dna.gif",
        height = 80,
        width = 200,
        align = "left",
        style = "bottom: 10px; left:100px;"
      ),
      "GENESTACK",
      br(),
      icon("dna"),
      "Test Task",
      icon("dna")
    )
  ),
  titleWidth = 1400
)

sidebar <- dashboardSidebar(
  width = 230,
  div(
    class = "menu",
    box(
      background = "navy",
      solidHeader = FALSE,
      collapsible = FALSE,
      width = 12,
      h2(
        tags$i(tags$b("Importance of Biotechnology")),
        br(),
        br(),
        "Biotechnology is the technologies applied to biology and genetic.
        Biotechnology utilizes cellular and biomolecular processes to create technologies
        and products that help improve our lives and the nature. Recent biotechnology develops
        breakthrough products and technologies to fight diseases, reduce our environmental harm,
        feed the hungry, use less and cleaner energy, and have safer, cleaner and more efficient
        industrial manufacturing processes."
      )
    ),
  )
)

# BODY ----
body <- dashboardBody(
  tags$style(CSS),
  selectizeInput(
    "select_gene",
    "Select a Gene Symbol / GO Term",
    choices = NULL,
    selected = 1,
    width = 1200
  ),
  reactableOutput("outputs")
)

# UI ----
ui <- dashboardPage(header, sidebar, body)

# SERVER ----
server <- function(input, output, session) {
  updateSelectizeInput(
    session = session,
    inputId = "select_gene",
    choices = all_choices,
    server = TRUE
  )
  
  filtered <- reactive({
    req(input$select_gene)
    task_data %>% 
      filter(paste0(gene_symbol," - ", label) == input$select_gene | grepl(input$select_gene, go_term_label) == TRUE) %>%
      select(
        gene_symbol,
        gene_synonyms,
        go_term_label,
        ensembl_transcript_id,
        ensembl_protein_id,
        label
      ) %>%
      setnames(
        old = c(
          "gene_symbol",
          "gene_synonyms",
          "go_term_label",
          "ensembl_transcript_id",
          "ensembl_protein_id",
          "label"
        ),
        new = c("GeneSymbol","GeneSynonyms","GOTerm", "Transcript ID", "Protein Id", "Label")
      )
  })
  
  output$outputs <- renderReactable({
    reactable(
      filtered(),
      defaultColDef = colDef(align = "left", headerStyle = list(background = "#f7f7f8")),
      bordered = TRUE,
      highlight = TRUE,
      filterable = TRUE,
      striped = FALSE,
      columns = list(
        GeneSymbol = colDef(cell = function(value) {
          if(is.na(value)) paste0("\u274c", value) else paste0("\u2714\ufe0f", value)
        }),
        GeneSynonyms = colDef(cell = function(value) {
          if(nchar(value) < 1) paste0("\u274c", value) else paste0("\u2714\ufe0f", value)
        }),
        GOTerm = colDef(cell = function(value) {
          if(nchar(value) < 1) paste0("\u274c", value) else paste0("\u2714\ufe0f", value)
        })
      ),
      theme = reactableTheme(
        borderColor = "#101F2D",
        stripedColor = "#91C6F8",
        highlightColor = "#59F876"
      )
    )
  })
}

shinyApp(ui, server)
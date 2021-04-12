####################################
# Data Professor                   #
# http://youtube.com/dataprofessor #
# http://github.com/dataprofessor  #
####################################

# Modified from Winston Chang, 
# https://shiny.rstudio.com/gallery/shiny-theme-selector.html

# Concepts about Reactive programming used by Shiny, 
# https://shiny.rstudio.com/articles/reactivity-overview.html

# Load R packages
library(shiny)
library(shinythemes)

#functions:
complement <- function(dna){
  lookup <- c("A" = "T", "T" = "A", "G" = "C", "C" = "G") # named vector, think key/value
  dna_split <- strsplit(dna, "")[[1]]
  dna_complement <- paste0(lookup[dna_split], collapse = "")
  return(dna_complement)
} 

mk_codons <- function(dna, s = 1){
  l = nchar(dna)
  codons <- substring(dna,
                      first = seq(from = s, to = l-3+1, by = 3),
                      last = seq(from = 3+s-1, to = l, by = 3))
  return(codons)
}

dna_codons_to_aa <- function(codons){
  std_code_table <- c("TTT" = "F", "TCT" = "S", "TAT" = "Y", "TGT" = "C",
                      "TTC" = "F", "TCC" = "S", "TAC" = "Y", "TGC" = "C",
                      "TTA" = "L", "TCA" = "S", "TAA" = "*", "TGA" = "*",
                      "TTG" = "L", "TCG" = "S", "TAG" = "*", "TGG" = "W",
                      "CTT" = "L", "CCT" = "P", "CAT" = "H", "CGT" = "R",
                      "CTC" = "L", "CCC" = "P", "CAC" = "H", "CGC" = "R",
                      "CTA" = "L", "CCA" = "P", "CAA" = "Q", "CGA" = "R",
                      "CTG" = "L", "CCG" = "P", "CAG" = "Q", "CGG" = "R",
                      "ATT" = "I", "ACT" = "T", "AAT" = "N", "AGT" = "S",
                      "ATC" = "I", "ACC" = "T", "AAC" = "N", "AGC" = "S",
                      "ATA" = "I", "ACA" = "T", "AAA" = "K", "AGA" = "R",
                      "ATG" = "M", "ACG" = "T", "AAG" = "K", "AGG" = "R",
                      "GTT" = "V", "GCT" = "A", "GAT" = "D", "GGT" = "G",
                      "GTC" = "V", "GCC" = "A", "GAC" = "D", "GGC" = "G",
                      "GTA" = "V", "GCA" = "A", "GAA" = "E", "GGA" = "G",
                      "GTG" = "V", "GCG" = "A", "GAG" = "E", "GGG" = "G")
  aa <- paste0(std_code_table[codons], collapse = "")
  return(aa)
}

# Define UI
ui <- fluidPage(theme = shinytheme("cerulean"),
                navbarPage(
                  # theme = "cerulean",  # <--- To use a theme, uncomment this
                  "DNA analysis tool",
                  tabPanel("Navbar 1",
                           sidebarPanel(
                             tags$h3("Input:"),
                             textInput("txt1", "DNA sequence:", ""),
              
                             #actionbar
                             actionButton("com", "Complementary DNA"),
                             actionButton("codon", "Make codons")
                           ), # sidebarPanel
                           mainPanel(
                             h1("Output"),
                             
                             h4("Result"),
                             verbatimTextOutput("txtout"),
                             
                           ) # mainPanel
                           
                  ) # Navbar 1, tabPanel
                  #tabPanel("Navbar 2", "This panel is intentionally left blank"),
                  #tabPanel("Navbar 3", "This panel is intentionally left blank")
                  
                ) # navbarPage
) # fluidPage


# Define server function  
server <- function(input, output) {
  v <- reactiveValues(data = NULL)
  
  observeEvent(input$com, {
    v$data <- complement(input$txt1)
  })
  
  observeEvent(input$codon, {
    v$data <- mk_codons(input$txt1)
  })
  
  output$txtout <- renderText({
    paste( v$data,sep = " " )
  })
} # server


# Create Shiny object
shinyApp(ui = ui, server = server)
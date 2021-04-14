# Load R packages
library(shiny)
library(shinythemes)
library(tidyverse)

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

plot_bases <- function(dna) {
  l = str_length(dna)
  #get percentages for each base
  a_percent <- (str_count(dna, "A")/l)*100
  t_percent <- (str_count(dna, "T")/l)*100
  c_percent <- (str_count(dna, "C")/l)*100
  g_percent <- (str_count(dna, "G")/l)*100
  
  #data <- tibble(A = a_percent, T = t_percent, C = c_percent, G = g_percent)
  data <- tibble(bases=c("A", "T", "C", "G"), percentages=c(a_percent, t_percent, c_percent, g_percent))
  
  base_plot <- data %>% ggplot(aes(x=bases, y=percentages, fill = bases)) +
    geom_bar(stat="identity") 
  
  return(base_plot)
}

# Define UI
ui <- fluidPage(theme = shinytheme("superhero"),
                navbarPage(
                  theme = "superhero",  # <--- To use a theme, uncomment this
                  "DNA analysis tool",
                  tabPanel("DNA analysis",
                           sidebarPanel(
                             tags$h3("Input:"),
                             textInput("txt1", "DNA sequence:", ""),
              
                             #actionbar
                             actionButton("com", "Complementary DNA", 
                                          class = "btn-danger"),
                             actionButton("codon", "Make codons",
                                          class = "btn-success"),
                             actionButton("aa", "Make aminoacid",
                                          class = "btn-info"),
                             actionButton("plot1", "Make plot",
                                          class = "btn-warning")
                           ), # sidebarPanel
                           mainPanel(
                             h1("Output"),
                             
                             h4("Result"),
                             verbatimTextOutput("txtout"),
                             
                             h4("Plot of base content"),
                             plotOutput("example_plot", width = "100%"),
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
  
  observeEvent(input$aa, {
    v$data <- dna_codons_to_aa(mk_codons(input$txt1))
  })
  
  output$txtout <- renderText({
    paste( v$data,sep = " " )
  })
  
  observeEvent(input$plot1, {
    output$example_plot <- renderPlot({
      return(plot_bases(input$txt1))
    })
  })

} # server


# Create Shiny object
shinyApp(ui = ui, server = server)
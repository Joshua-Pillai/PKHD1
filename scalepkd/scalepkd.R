# ---- Load Libraries ----
library(shiny)
library(shiny.molstar)
library(readr)
library(dplyr)
library(DT)

# ---- Ensure www Exists ----
if (!dir.exists("www")) dir.create("www")

# Copy pkhd1.pdb into www if not already there
if (!file.exists("www/pkhd1.pdb") && file.exists("pkhd1.pdb")) {
  file.copy("pkhd1.pdb", "www/pkhd1.pdb", overwrite = TRUE)
}

# ---- Ensure ScalePKD CSV is in correct directory ----
app_dir <- getwd()
expected_file <- file.path(app_dir, "ScalePKD (1).csv")
possible_sources <- c(
  "C:/Users/pjpil/OneDrive/Documents/ScalePKD (1).csv",
  "C:/Users/pjpil/OneDrive/Documents/csv_molstar_app/www/ScalePKD (1).csv"
)

if (!file.exists(expected_file)) {
  found_source <- possible_sources[file.exists(possible_sources)]
  if (length(found_source) > 0) {
    file.copy(found_source[1], expected_file, overwrite = TRUE)
    message("✅ Copied ScalePKD (1).csv into app directory: ", expected_file)
  } else {
    warning("⚠️ Could not find 'ScalePKD (1).csv' in known source locations.")
  }
} else {
  message("✅ ScalePKD (1).csv already in app directory: ", expected_file)
}

# ---- Load ScalePKD Data ----
data_path <- expected_file

if (file.exists(data_path)) {
  scalepkd <- read_csv(data_path, show_col_types = FALSE)
  names(scalepkd) <- trimws(names(scalepkd))
} else {
  warning("⚠️ Could not find 'ScalePKD (1).csv' in working directory: ", getwd())
  scalepkd <- data.frame()
}

# ---- Amino Acid Conversion Map ----
aa_map <- c(
  A = "Ala", R = "Arg", N = "Asn", D = "Asp", C = "Cys",
  E = "Glu", Q = "Gln", G = "Gly", H = "His", I = "Ile",
  L = "Leu", K = "Lys", M = "Met", F = "Phe", P = "Pro",
  S = "Ser", T = "Thr", W = "Trp", Y = "Tyr", V = "Val"
)

convert_one_to_three <- function(term) {
  # Converts something like T36M → Thr36Met
  pattern <- "^([A-Z])([0-9]+)([A-Z])$"
  if (grepl(pattern, term, ignore.case = FALSE)) {
    parts <- regmatches(term, regexec(pattern, term))[[1]]
    from <- aa_map[toupper(parts[2])]
    to <- aa_map[toupper(parts[4])]
    if (!is.na(from) && !is.na(to)) {
      return(paste0(from, parts[3], to))
    }
  }
  return(term)
}

# ---- Disclaimer Text ----
disclaimer_text <- HTML("
  <div style='margin-top:20px; padding:15px; background-color:#f7f7f7;
              border-top:1px solid #ddd; border-radius:6px;
              font-size:0.85em; color:#555; font-style:italic;'>
    <strong>Disclaimer:</strong> The ScalePKD Database and other information provided on or linked to this site 
    is for theoretical prediction only; caution should be exercised in use. It is provided \"as-is\" without any warranty 
    of any kind, whether express or implied. For clarity, no warranty is given that use of the information shall not 
    infringe the rights of any third party. The information provided is not intended to be a substitute for professional 
    medical advice, diagnosis, or treatment, and does not constitute medical or other professional advice. 
    Data contained within the ScalePKD Database is provided for non-commercial research use only under 
    <a href='https://creativecommons.org/licenses/by-nc-sa/4.0/' target='_blank'>CC BY-NC-SA 4.0 license</a>.
  </div>
")

# ---- UI ----
ui <- navbarPage(
  "ScalePKD",
  inverse = TRUE,
  collapsible = TRUE,
  header = tags$style(HTML("
      body { background-color: #fafafa; font-family: 'Segoe UI', Arial, sans-serif; }
      .navbar { background-color: #2c3e50 !important; border: none; }
      .navbar-brand, .navbar-nav li a { color: #ecf0f1 !important; font-weight: 600; }
      .navbar-nav li a:hover { background-color: #1a242f !important; }
      h2, .title-panel { color: #2c3e50; }
  ")),
  
  # --- Welcome Page ---
  tabPanel(
    "Home",
    fluidRow(
      column(
        width = 10, offset = 1,
        div(
          style = "background-color:white; padding:40px; border-radius:10px;
                   box-shadow:0 3px 10px rgba(0,0,0,0.1); margin-top:40px;",
          h2("Welcome to ScalePKD"),
          tags$p("
            ScalePKD is an integrated platform designed for the exploration and analysis 
            of fibrocystin structure, interdomain interactions, and missense variants 
            implicated in autosomal recessive polycystic kidney disease (ARPKD).
          "),
          tags$ul(
            tags$li(HTML("<strong>Structural Analyses:</strong> Explore the 3D structure of fibrocystin using the Mol* viewer in the 'Structural Analyses' tab.")),
            tags$li(HTML("<strong>ScalePKD Predictions:</strong> Search and visualize variant data across multiple prediction scores in the 'ScalePKD' tab.")),
            tags$li(HTML("<strong>Credits & Attribution:</strong> View the contributing authors and citation details in the 'Credits' tab."))
          ),
          tags$p("
            Please note that all results are provided for research purposes only and should not 
            be used as a substitute for clinical judgment or medical advice.
          "),
          disclaimer_text
        )
      )
    )
  ),
  
  # --- Protein Structure (Mol*) ---
  tabPanel(
    "Structural Analyses",
    titlePanel("Tap on Residue of Interest for Wild-Type Interactions"),
    fluidRow(
      column(
        width = 12,
        tags$style(HTML("
          #molstar-viewer iframe, #molstar-viewer {
            width: 100%;
            height: 80vh;
            min-height: 600px;
            border: none;
            border-radius: 8px;
            box-shadow: 0 2px 8px rgba(0,0,0,0.1);
          }
        ")),
        div(
          id = "molstar-viewer",
          Molstar(
            pdbId = NULL,
            url = "pkhd1.pdb",
            useInterface = TRUE,
            showControls = TRUE,
            showAxis = TRUE,
            showButtons = TRUE
          )
        ),
        disclaimer_text
      )
    )
  ),
  
  # --- Data Viewer Tab ---
  tabPanel(
    "ScalePKD",
    titlePanel("ScalePKD Predictions"),
    sidebarLayout(
      sidebarPanel(
        textInput("variant_search", "Search term:", ""),
        actionButton("search_btn", "Search", class = "btn btn-primary"),
        helpText("Search across all columns in the dataset. Supports three-letter (Thr36Met) and one-letter (T36M) notation.")
      ),
      mainPanel(
        div(
          style = "background-color: white; border-radius: 8px; 
                   box-shadow: 0 2px 8px rgba(0,0,0,0.1); padding: 10px;",
          DTOutput("variant_table")
        ),
        disclaimer_text
      )
    )
  ),
  
  # --- Credits Tab ---
  tabPanel(
    "Credits",
    titlePanel("Credits & Citation"),
    fluidRow(
      column(
        width = 10,
        offset = 1,
        div(
          style = "background-color: white; border-radius: 8px; box-shadow: 0 2px 8px rgba(0,0,0,0.1);
                   padding: 25px; margin-top: 20px; line-height: 1.6;",
          
          h3("ScalePKD"),
          HTML("
            <p>ScalePKD was conceptualized by <strong>Joshua P. Pillai</strong> and <strong>John A. Sayer</strong>.</p>
            
            <h4 style='margin-top: 25px;'>Please cite the following paper:</h4>
            
            <p style='margin-left: 15px; font-size: 0.95em; text-align: justify;'>
              <strong>
              Molecular basis of fibrocystin: A structure uncovering all motifs, interdomains, and missense variants 
              implicated in autosomal recessive polycystic kidney disease
              </strong><br><br>
              
              <em>Joshua P. Pillai<sup>1,2,3,4</sup>, Sidhant Puntambekar<sup>4,5</sup>, 
              Matthew G. Sampson<sup>4,5,6,7+</sup>, John A. Sayer<sup>8,9,10+</sup></em><br><br>
              
              <sup>1</sup>School of Biological Sciences, University of California, San Diego, La Jolla, CA, USA.<br>
              <sup>2</sup>Department of Neurosciences, University of California San Diego, School of Medicine, La Jolla, CA, USA.<br>
              <sup>3</sup>Biophotonics Technology Center, Institute of Engineering in Medicine, University of California, San Diego, La Jolla, CA, USA.<br>
              <sup>4</sup>Division of Nephrology, Boston Children’s Hospital, Boston, MA, USA.<br>
              <sup>5</sup>Kidney Disease Initiative, Broad Institute, Cambridge, MA, USA.<br>
              <sup>6</sup>Department of Pediatrics, Harvard Medical School, Boston, MA, USA.<br>
              <sup>7</sup>Division of Renal Medicine, Brigham and Women’s Hospital, Boston, MA, USA.<br>
              <sup>8</sup>Newcastle University, Newcastle Upon Tyne, United Kingdom.<br>
              <sup>9</sup>Newcastle Hospitals NHS Foundation Trust, Newcastle Upon Tyne, United Kingdom.<br>
              <sup>10</sup>NIHR Newcastle Biomedical Research Centre, Newcastle upon Tyne, United Kingdom.
            </p>
            
            <p style='margin-top: 25px; font-size: 0.9em; color: #555;'>
              © ScalePKD Project. Licensed under the 
              <a href='https://creativecommons.org/licenses/by-nc-sa/4.0/' target='_blank'>CC BY-NC-SA 4.0 License</a>.
            </p>
          ")
        ),
        disclaimer_text
      )
    )
  )
)

# ---- JavaScript fix for Mol* render issue ----
js_fix <- "
$(document).on('shown.bs.tab', 'a[data-value=\"Structural Analyses\"]', function (e) {
  setTimeout(function() { 
    window.dispatchEvent(new Event('resize')); 
  }, 500);
});
"
ui <- tagList(ui, tags$script(HTML(js_fix)))

# ---- Server ----
server <- function(input, output, session) {
  results_data <- reactiveVal(scalepkd)
  
  observeEvent(input$search_btn, {
    req(input$variant_search)
    term <- trimws(input$variant_search)
    
    # Support one-letter AA search
    expanded_term <- convert_one_to_three(term)
    
    results <- scalepkd %>%
      filter(if_any(everything(), ~ grepl(term, .x, ignore.case = TRUE) |
                      grepl(expanded_term, .x, ignore.case = TRUE)))
    
    results_data(results)
  })
  
  output$variant_table <- renderDT({
    datatable(
      results_data(),
      options = list(pageLength = 10, scrollX = TRUE),
      style = "bootstrap",
      rownames = FALSE
    )
  })
}

# ---- Run App ----
shinyApp(ui, server)

#!/usr/bin/env Rscript
# ============================================================================
# TUGAS TUTORIAL 3 - ANALISIS DAN VISUALISASI DATA
# Aplikasi Shiny untuk Visualisasi Data Cuaca Interaktif
# ============================================================================

# --- SETUP ENVIRONMENT ---
lib_path <- "~/Workspace/self/R/lib"
if (dir.exists(lib_path)) {
  .libPaths(c(lib_path, .libPaths()))
}

# --- FUNGSI UNTUK LOAD/INSTALL PACKAGES ---
safe_load <- function(pkg) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
        message("Installing package: ", pkg)
        install.packages(pkg, 
                        lib = if (dir.exists(lib_path)) lib_path else NULL, 
                        repos = "https://cloud.r-project.org")
    }
    library(pkg, character.only = TRUE)
}

# --- LOAD REQUIRED PACKAGES ---
cat("Loading required packages...\n")
required_packages <- c("shiny", "ggplot2", "DT", "readxl", "dplyr")

for (pkg in required_packages) {
    tryCatch({
        safe_load(pkg)
        cat("✓", pkg, "loaded successfully\n")
    }, error = function(e) {
        cat("✗ Error loading", pkg, ":", e$message, "\n")
        quit(save = "no", status = 1)
    })
}

# --- BACA DATA ---
dataset_folder <- "tugas3_dataset"
xlsx_file <- file.path(dataset_folder, "tugas3.xlsx")

# Validasi file
if (!file.exists(xlsx_file)) {
    cat("✗ File Excel tidak ditemukan:", xlsx_file, "\n")
    cat("  Pastikan file ada di lokasi yang benar.\n")
    quit(save = "no", status = 1)
}

cat("\nMembaca data dari:", xlsx_file, "\n")

# Baca data dengan header yang benar
tryCatch({
  # LANGKAH 1: Baca header dari baris 2
  header_row <- readxl::read_excel(
    xlsx_file,
    range = "A2:V2",  # Sesuaikan range jika ada lebih banyak kolom
    col_names = FALSE
  )
  
  # Konversi header menjadi vector dan bersihkan
  column_headers <- as.character(header_row[1, ])
  column_headers <- column_headers[!is.na(column_headers)]
  
  cat("✓ Header ditemukan:", length(column_headers), "kolom\n")
  cat("  Header:", paste(head(column_headers, 5), collapse = ", "), "...\n\n")
  
  # LANGKAH 2: Baca data mulai dari baris 3
  data <- readxl::read_excel(
    xlsx_file,
    skip = 2,  # Skip baris 1 (judul "weather") dan baris 2 (header)
    col_names = FALSE
  )
  
  # Hapus baris yang semua kolomnya NA
  data <- data[rowSums(is.na(data)) != ncol(data), ]
  
  # LANGKAH 3: Set nama kolom dengan header yang benar
  if(ncol(data) == length(column_headers)) {
    names(data) <- make.names(column_headers, unique = TRUE)
  } else {
    # Jika jumlah tidak sama, gunakan sebanyak yang ada
    n_cols <- min(ncol(data), length(column_headers))
    names(data)[1:n_cols] <- make.names(column_headers[1:n_cols], unique = TRUE)
    if(ncol(data) > length(column_headers)) {
      # Berikan nama default untuk kolom ekstra
      names(data)[(n_cols+1):ncol(data)] <- paste0("Extra_", 1:(ncol(data)-n_cols))
    }
  }
  
  cat("✓ Data berhasil dibaca:", nrow(data), "baris,", ncol(data), "kolom\n")
  cat("\nNama kolom yang digunakan:\n")
  for(i in 1:min(10, length(names(data)))) {
    cat(sprintf("  %2d. %s\n", i, names(data)[i]))
  }
  if(length(names(data)) > 10) {
    cat(sprintf("  ... dan %d kolom lainnya\n", length(names(data)) - 10))
  }
  cat("\n")
  
  # Tambahkan ID untuk time series
  data$ID_Baris <- 1:nrow(data)
  
}, error = function(e) {
  cat("✗ Error membaca data:", e$message, "\n")
  cat("  Detail:", conditionMessage(e), "\n")
  quit(save = "no", status = 1)
})

# --- IDENTIFIKASI TIPE VARIABEL ---
numeric_vars <- names(data)[sapply(data, is.numeric)]
categorical_vars <- names(data)[sapply(data, function(x) {
  is.character(x) | is.factor(x)
})]

cat("Variabel numerik:", length(numeric_vars), "\n")
cat("Variabel kategorikal:", length(categorical_vars), "\n\n")

# ============================================================================
# UI - USER INTERFACE
# ============================================================================
ui <- fluidPage(
  # Judul aplikasi
  titlePanel(
    div(
      h2("Aplikasi Visualisasi Data Cuaca Interaktif"),
      h4("Tugas Tutorial 3 - Analisis dan Visualisasi Data")
    )
  ),
  
  # Layout utama
  sidebarLayout(
    # --- SIDEBAR PANEL ---
    sidebarPanel(
      width = 3,
      h4("Pengaturan Visualisasi"),
      hr(),
      
      # Pilih variabel
      selectInput(
        "var", 
        "Pilih Variabel Y:",
        choices = names(data),
        selected = if(length(numeric_vars) > 0) numeric_vars[1] else names(data)[1]
      ),
      
      # Pilih jenis plot
      selectInput(
        "plot_type", 
        "Jenis Visualisasi:",
        choices = c(
          "Scatter Plot" = "scatter",
          "Line Plot" = "line",
          "Bar Plot" = "bar",
          "Tabel Data" = "table"
        ),
        selected = "scatter"
      ),
      
      # UI dinamis (untuk scatter plot)
      uiOutput("dynamic_controls"),
      
      hr(),
      
      # Informasi dataset
      h5("ℹInfo Dataset"),
      textOutput("data_info")
    ),
    
    # --- MAIN PANEL ---
    mainPanel(
      width = 9,
      
      # Tab panel untuk organisasi output
      tabsetPanel(
        type = "tabs",
        
        # Tab Visualisasi
        tabPanel(
          "Visualisasi",
          br(),
          plotOutput("plot", height = "600px")
        ),
        
        # Tab Tabel
        tabPanel(
          "Tabel Data",
          br(),
          DTOutput("table")
        ),
        
        # Tab Info
        tabPanel(
          "Informasi",
          br(),
          h4("Tentang Aplikasi"),
          p("Aplikasi ini dibuat untuk memvisualisasikan data cuaca secara interaktif."),
          h5("Fitur:"),
          tags$ul(
            tags$li("Scatter Plot: Menampilkan hubungan antara dua variabel numerik"),
            tags$li("Line Plot: Menampilkan tren data sepanjang waktu"),
            tags$li("Bar Plot: Menampilkan distribusi data kategorikal atau histogram"),
            tags$li("Tabel Data: Menampilkan data dalam format tabel interaktif")
          ),
          hr(),
          h5("Daftar Variabel yang Tersedia:"),
          verbatimTextOutput("available_vars"),
          hr(),
          h5("Struktur Data:"),
          verbatimTextOutput("data_structure")
        )
      )
    )
  )
)

# SERVER - LOGIC
server <- function(input, output, session) {
  
  # DYNAMIC UI CONTROLS
  output$dynamic_controls <- renderUI({
    if (input$plot_type == "scatter") {
      tagList(
        hr(),
        selectInput(
          "x_var", 
          "Pilih Variabel X:",
          choices = numeric_vars,
          selected = if(length(numeric_vars) > 1) numeric_vars[2] else numeric_vars[1]
        ),
        checkboxInput("add_smooth", "Tambahkan Trend Line", value = FALSE)
      )
    }
  })
  
  #INFO DATASET
  output$data_info <- renderText({
    paste0(
      "Jumlah Baris: ", nrow(data), "\n",
      "Jumlah Kolom: ", ncol(data), "\n",
      "Var. Numerik: ", length(numeric_vars), "\n",
      "Var. Kategorikal: ", length(categorical_vars)
    )
  })
  
  #STRUKTUR DATA
  output$data_structure <- renderPrint({
    str(data)
  })
  
  # DAFTAR VARIABEL
  output$available_vars <- renderPrint({
    cat("Variabel Numerik:\n")
    cat(paste("  -", numeric_vars), sep = "\n")
    cat("\n")
    if(length(categorical_vars) > 0) {
      cat("Variabel Kategorikal:\n")
      cat(paste("  -", categorical_vars), sep = "\n")
    }
  })
  
  # RENDER PLOT
  output$plot <- renderPlot({
    req(input$var, input$plot_type)
    
    var <- input$var
    plot_type <- input$plot_type
    
    # Validasi variabel ada di data
    if (!var %in% names(data)) {
      return(NULL)
    }
    
    # SCATTER PLOT
    if (plot_type == "scatter") {
      req(input$x_var)
      x_var <- input$x_var
      
      # Validasi kedua variabel numerik
      if (!is.numeric(data[[var]]) || !is.numeric(data[[x_var]])) {
        plot.new()
        text(0.5, 0.5, "⚠️ Kedua variabel harus numerik untuk Scatter Plot", 
             cex = 1.5, col = "red")
        return()
      }
      
      p <- ggplot(data, aes(x = .data[[x_var]], y = .data[[var]])) +
        geom_point(alpha = 0.6, size = 3, color = "steelblue") +
        labs(
          title = paste("Scatter Plot:", var, "vs", x_var),
          x = x_var,
          y = var
        ) +
        theme_minimal(base_size = 14) +
        theme(
          plot.title = element_text(face = "bold", hjust = 0.5),
          panel.grid.major = element_line(color = "gray90"),
          panel.border = element_rect(color = "gray70", fill = NA)
        )
      
      # Tambahkan trend line jika diminta
      if (!is.null(input$add_smooth) && input$add_smooth) {
        p <- p + geom_smooth(method = "lm", color = "red", se = TRUE)
      }
      
      print(p)
    }
    
    # LINE PLOT
    else if (plot_type == "line") {
      if (!is.numeric(data[[var]])) {
        plot.new()
        text(0.5, 0.5, "⚠️ Variabel harus numerik untuk Line Plot", 
             cex = 1.5, col = "red")
        return()
      }
      
      p <- ggplot(data, aes(x = ID_Baris, y = .data[[var]])) +
        geom_line(color = "darkgreen", linewidth = 1) +
        geom_point(color = "darkgreen", size = 2, alpha = 0.5) +
        labs(
          title = paste("Line Plot:", var),
          x = "Urutan Data (ID Baris)",
          y = var
        ) +
        theme_minimal(base_size = 14) +
        theme(
          plot.title = element_text(face = "bold", hjust = 0.5),
          panel.grid.major = element_line(color = "gray90"),
          panel.border = element_rect(color = "gray70", fill = NA)
        )
      
      print(p)
    }
    
    # BAR PLOT
    else if (plot_type == "bar") {
      if (var %in% categorical_vars) {
        p <- ggplot(data, aes(x = .data[[var]])) +
          geom_bar(fill = "coral", color = "black", alpha = 0.7) +
          labs(
            title = paste("Bar Plot:", var),
            x = var,
            y = "Frekuensi"
          ) +
          theme_minimal(base_size = 14) +
          theme(
            plot.title = element_text(face = "bold", hjust = 0.5),
            axis.text.x = element_text(angle = 45, hjust = 1),
            panel.grid.major = element_line(color = "gray90")
          )
        
        print(p)
      }
      # Untuk variabel numerik: histogram
      else if (var %in% numeric_vars) {
        p <- ggplot(data, aes(x = .data[[var]])) +
          geom_histogram(
            bins = 30, 
            fill = "skyblue", 
            color = "black", 
            alpha = 0.7
          ) +
          labs(
            title = paste("Histogram:", var),
            x = var,
            y = "Frekuensi"
          ) +
          theme_minimal(base_size = 14) +
          theme(
            plot.title = element_text(face = "bold", hjust = 0.5),
            panel.grid.major = element_line(color = "gray90")
          )
        
        print(p)
      }
      else {
        plot.new()
        text(0.5, 0.5, "⚠️ Variabel tidak valid untuk Bar Plot", 
             cex = 1.5, col = "red")
      }
    }
  })
  
  # RENDER TABLE
  output$table <- renderDT({
    DT::datatable(
      data,
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        scrollY = "500px",
        searching = TRUE,
        ordering = TRUE,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel')
      ),
      filter = 'top',
      rownames = FALSE,
      class = 'cell-border stripe'
    )
  })
}

cat("Menjalankan aplikasi Shiny...\n\n")
shinyApp(ui = ui, server = server)
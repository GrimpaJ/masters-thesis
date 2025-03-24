library(dplyr)
library(zoo)
library(shiny)
library(shinyWidgets)
library(plotly)
library(viridisLite)
library(colorspace)
library(randomcoloR)
library(RColorBrewer)
library(grDevices)
library(tidyr)

get_ready <- function() {
  current_wd <- getwd()
  if(endsWith(current_wd, "code")) {
    setwd("..")
  }
  
  source(file.path(getwd(), "code", "utils.R"))
  source_script("data_processing")
  source_script("all")
  source_script("lpa")
  
  if (exists("all", envir = .GlobalEnv)) {
    df_all <- get("all", envir = .GlobalEnv)
  } else {
    df_all <- create_all_df(lpa = TRUE)
  }
  if (exists("codebook", envir = .GlobalEnv)) {
    df_codebook <- get("codebook", envir = .GlobalEnv)
  } else {
      df_codebook <- process_data("codebook")
  }
  
  return(list(df_all = df_all, df_codebook = df_codebook))
}

dfs <- get_ready()
df_all <- dfs$df_all
df_codebook <- dfs$df_codebook

plot_df <- df_all %>% 
  mutate(ID = as.factor(ID))

# Create list of colors
color_list <- function(n, method) {
  supported_methods <- c("viridisLite", "rainbow_hcl", "distinct", "brewer", "grDevices", "ID")
  
  if(!(method %in% supported_methods)) {
    warning(paste0("Die Angegebene Methode ist nicht verfügbar./nUnterstütze Methoden sind: ", supported_methods))
    return(NULL)
  }
  colors <- dplyr::case_when(
    method == "viridisLite" ~ viridisLite::turbo(n, begin = 0.1, end = 0.9),
    method == "rainbow_hcl" ~ colorspace::rainbow_hcl(n),
    method == "distinct" ~ randomcoloR::distinctColorPalette(n),
    method == "brewer" ~  {
      temp <- brewer.pal.info[brewer.pal.info$category == 'qual',]
      temp <- unlist(mapply(brewer.pal, temp$maxcolors, rownames(temp)))
      sample(temp, n, replace = FALSE)
    },
    method == "grDevices" ~ {
      temp <- grDevices::colors()[grep('gr(n|e)y', grDevices::colors(), invert = T)]
      sample(temp, n, replace = FALSE)
    },
    method == "ID" ~ {
      if(n > 40) {
        warning(paste0("n ist zu groß. Es gibt nur 40 IDs."))
        return(NULL)
      } else {
        ID_colors <- c(
          "#483d8b","#000080","#0000f2","#49708a","#4169e1","#4f94cd","#03bfff",
          "#87cefa","#20b2aa","#698b69","#02ff7f","#009b75","#006400","#238b21",
          "#76ee00","#6b8e22","#ffff00","#efa605","#ff7f24","#f4a361","#cd853f",
          "#8b6915","#733a1b","#ff0000","#cd5c5b","#ff1393","#8b0a50","#b452cd",
          "#ff6899","#8b008b","#ee81ee","#551a8b","#9400d3","#8a2be2","#9470db",
          "#695acd","#8b658b","#deafee","#8b795e","#f0e68c"
        )
        sample(ID_colors, n, replace = FALSE)
      }
    },
    TRUE ~ rep(NA_character_, n)
  )
  return(colors)
}


# creates color matrix from list of colors
color_matrix <- function(colors, label = TRUE) {
  n = length(colors)
  ncol = ceiling(sqrt(n))
  nrow = ceiling(n / ncol)
  matrix = matrix(1:(nrow * ncol), nrow = nrow, byrow = TRUE)
  plot(c(0, ncol), c(0, nrow), type = "n", xlab = "", ylab = "", axes = FALSE)
  for (i in 1:nrow(matrix)) {
    for (j in 1:ncol(matrix)) {
      index <- matrix[i, j]
      if (!is.na(index)) {
        rect(j - 1, nrow - i, j, nrow - i + 1, col = colors[index], border = NA)
        if(label == TRUE) {
          text(j - 0.5, nrow - i + 0.5, labels = colors[index], cex = 0.7)
        }
      }
    }
  }
}

# function für label bei ID auswahl
create_label_for_id <- function(ID, color, icon_usable, icon_sex, icon_age) {
  HTML(sprintf(
    "<div style='display: flex; align-items: center;margin-bottom: 10px;'>
    <div style='width: 12px; height: 12px; background-color: %s; margin-right: 10px;'></div>
    <span style='width: 60px; display: inline-block;'>ID %s</span>
    <div style='display: flex; align-items: center;'>
      <i class='fa-solid %s' style='width: 22px;'></i>
      <i class='fa-solid %s' style='width: 22px;'></i>
      <i class='fa-solid %s' style='width: 22px;'></i>
    </div>
  </div>",
    color, ID, icon_sex, icon_age, icon_usable
    ))
}

debug_view <- function(obj) {
  if (is.data.frame(obj)) {
    View(obj)
  } else {
    str(obj)
  }
}

# UI mit klar getrennten Bereichen
ui <- fluidPage(
  titlePanel("PHQ-Verläufe"),
  sidebarLayout(
    sidebarPanel(
      h4("Parameter"),
      
      # Preset-Buttons
      h5("Presets"),
      actionButton("all_usable", "usable",
                   icon = icon("mobile")),
      actionButton("all_non_usable", "non-usable",
                   icon = icon("ban")),
      actionButton("all_male", "",
                   icon = icon("mars")),
      actionButton("all_female", "",
                   icon = icon("venus")),
      
      # ID-Auswahl
      div(style = "margin-top: 20px;",
      pickerInput(
        inputId = "selected_ids",
        label = "ID-Auswahl:",
        choices = NULL,
        selected = NULL,
        options = list(
          `actions-box` = TRUE,
          `live-search` = TRUE,
          `selected-text-format` = "count",
          `tick-icon` = "glyphicon-glass",
          size = 10,
          noneSelectedText = "Select an ID",
          title = NULL
        ),
        multiple = TRUE,
        choicesOpt = list(
          icon = NULL
        )
      )),
      
      
      radioButtons(
        "color_scheme",
        "Farbschema:",
        c("Nach ID" = "id", "Nach Veränderung" = "change")
      ),
      width = 3,
      
      conditionalPanel(
        "input.color_scheme == 'change'",
        div(style = "margin-top: 20px;",
            h5("PHQ Change:"),
            div(icon("square", class = "fa-solid", style = "color: #d83e3d; background: #d83e3d"), " verschlechtert"),
            div(icon("square", class = "fa-solid", style = "color: #69e963; background: #69e963"), " verbessert"),
            div(icon("square", class = "fa-solid", style = "color: #0000fd; background: #0000fd"), " unverändert")
        )
      ),
      checkboxInput("show_zones", "PHQ Interpretation anzeigen", value = FALSE),
      conditionalPanel(
        condition = "input.show_zones",
        div(style = "margin-top: 20px;",
            h5("Depressive Symptomatik:"),
            div(style = "background-color: #fefff1; padding: 5px;", "minimal"),
            div(style = "background-color: #fff7ee; padding: 5px;", "mild"),
            div(style = "background-color: #ffeadd; padding: 5px;", "leicht"),
            div(style = "background-color: #feeae7; padding: 5px;", "mittel"),
            div(style = "background-color: #fee0e0; padding: 5px;", "schwer")
        )
      ),
      checkboxInput("show_missing", "Missing", value = FALSE),
      checkboxInput("show_regression", "Regressionslinien", value = FALSE),
      checkboxInput("show_boxplots", "Verteilung", value = FALSE)
    ),
    mainPanel(
      h4("Ergebnis"),
      plotlyOutput("main_plot", height = "600px"),
      plotlyOutput("pie_chart", height = "250px")
    )
  )
)

tags$head(
  tags$style(HTML("
    .checkbox { margin-top: 0px; margin-bottom: 0px; }
    .checkbox-inline { margin-top: 0px; margin-bottom: 0px; }
    .shiny-input-container { padding-bottom: 0px; }
    .form-group { margin-bottom: 5px; }
    .js-irs-0 .irs-bar { border-top-color: #dcdcdc; border-bottom-color: #dcdcdc; }
    .checkbox { margin-top: -10px; }
    .checkbox { margin-top: -5px; margin-bottom: -5px; }
    .shiny-input-container { padding-bottom: 8px; }
  "))
)


# Server-Logik
server <- function(input, output, session) {
  
  # Farbpalette für IDs (global)
  id_colors <- reactive({
    ids <- sort(unique(plot_df$ID))
    n <- length(ids)
    colors <- color_list(n, "ID")
    setNames(colors, as.character(ids))
  })
  
  # PHQ Bereiche
  phq_zones <- data.frame(
    zone = c("minimal", "mild", "leicht", "mittel", "schwer"),
    y_start = c(0, 4.5, 9.5, 14.5, 19.5),
    y_end = c(4.5, 9.5, 14.5, 19.5, 27),
    color = c("#fefff1", "#fff7ee", "#ffeadd", "#feeae7", "#fee0e0")
  )
  
  observe({
    choices <- df_codebook %>%
      mutate(
        color = id_colors()[as.character(ID)],
        icon_usable = case_when(usable == "yes" ~ "fa-mobile",
                                usable == "no" ~ "fa-ban"),
        icon_sex = case_when(sex == "m" ~ "fa-mars",
                             sex == "f" ~ "fa-venus",
                             sex == "d" ~ "fa-mars-and-venus"),
        icon_age = case_when(age > 17 ~ "fa-person-cane",
                             age < 18 ~ "fa-child")
        ) %>% 
          rowwise() %>% 
          mutate(
            label = create_label_for_id(ID, color, icon_usable, icon_sex, icon_age)
            ) %>% 
              ungroup()
    
    updatePickerInput(
      session,
      "selected_ids",
      #choices = split(choices$ID, choices$usable),
      choices = choices$ID,
      selected = head(choices$ID),
      choicesOpt = list(
        content = choices$label
      )
    )
  })

  
    # Preset-Handler
    observeEvent(input$all_usable, {
      usable_ids <- df_codebook %>% filter(usable == "yes") %>% pull(ID)
      updatePickerInput(session, "selected_ids", selected = usable_ids)
    })
    
    observeEvent(input$all_non_usable, {
      non_usable_ids <- df_codebook %>% filter(usable == "no") %>% pull(ID)
      updatePickerInput(session, "selected_ids", selected = non_usable_ids)
    })
    
    observeEvent(input$all_male, {
      male_ids <- df_codebook %>% filter(sex == "m") %>% pull(ID)
      updatePickerInput(session, "selected_ids", selected = male_ids)
    })
    
    observeEvent(input$all_female, {
      female_ids <- df_codebook %>% filter(sex == "f") %>% pull(ID)
      updatePickerInput(session, "selected_ids", selected = female_ids)
    })
    
    ####### HAUPTDATENSATZ ######
    # Reaktive Datenfilterung
    filtered_data <- reactive({
      req(input$selected_ids)
      
      plot_df %>%
        filter(ID %in% input$selected_ids) %>%
        arrange(ID, day)
    })
    
    title_text <- reactive({
      hover <- event_data("plotly_hover", source = "A")
      req(hover)
      
      hovered_id <- hover$key[[1]]
      hovered_day <- hover$x
      
      paste0("App-Category Verteilung von ID ",
             hovered_id,
             " an Tag ",
             hovered_day)
    })
    
    hovered_pie_data <- reactive({
      hover <- event_data("plotly_hover", source = "A")  # Holt die Hover-Daten
      
      if (is.null(hover) || is.null(hover$key) || is.null(hover$x)) return(NULL)
      
      req(hover)
      
      # Extrahiere ID und Tag aus dem Hover-Event
      hovered_id <- hover$key[[1]]
      hovered_day <- hover$x
      
      filtered_row <- filtered_data() %>%
        filter(ID == hovered_id, day == hovered_day)
      
      if (nrow(filtered_row) == 0) return(NULL)
      
      # Bestimme die App-Kategorie-Spalten (ohne ID, day, PHQ, time_on, PHQ_1-9)
      exclude_cols <- c("ID", "day", "PHQ", "time_on", paste0("PHQ_", 1:9))
      # Zusätzlich alle anderen nicht-numerischen Spalten oder Spalten, die keine Screentime-Daten enthalten
      exclude_cols <- c(exclude_cols, "sex", "age", "usable")  # Füge weitere hinzu, wenn nötig
      
      # Finde alle numerischen Spalten, die nicht in exclude_cols sind
      all_cols <- colnames(filtered_row)
      category_cols <- setdiff(all_cols, exclude_cols)
      
      # Prüfe, ob überhaupt App-Kategorie-Spalten vorhanden sind
      if (length(category_cols) == 0) return(NULL)
      
      # Erstelle einen neuen Dataframe für die Pie-Chart-Daten
      pie_data <- data.frame(
        category = character(),
        value = numeric(),
        stringsAsFactors = FALSE
      )
      
      # Fülle den Dataframe manuell, anstatt pivot_longer zu verwenden
      for (col in category_cols) {
        value <- filtered_row[[col]]
        if (!is.na(value) && is.numeric(value) && value > 300000) {  # >5 Minuten (300,000 ms)
          pie_data <- rbind(pie_data, data.frame(category = col, value = value, stringsAsFactors = FALSE))
        }
      }
      
      # Wenn keine Daten die Bedingung erfüllen, NULL zurückgeben
      if (nrow(pie_data) == 0) return(NULL)
      
      return(pie_data)
    })
    
    # Daten für fehlende Segmente
    missing_segments <- reactive({
      req(filtered_data())
      
      filtered_data() %>%
        group_by(ID) %>%
        arrange(day) %>% 
        mutate(valid_day = if_else(!is.na(PHQ), day, NA_real_)) %>%
        mutate(
          prev_PHQ1 = if_else(day == min(day),
                              na.locf(PHQ, fromLast = TRUE, na.rm = FALSE),
                              na.locf(PHQ, na.rm = FALSE)),
          prev_day1 = if_else(day == min(day),
                              min(day),
                              na.locf(valid_day, na.rm = FALSE)),
          next_PHQ1 = if_else(day == max(day),
                              na.locf(PHQ, na.rm = FALSE),
                              na.locf(PHQ, fromLast = TRUE, na.rm = FALSE)),
          next_day1 = if_else(day == max(day),
                              max(day),
                              na.locf(valid_day, fromLast = TRUE, na.rm = FALSE))
        ) %>%
        mutate(
          prev_PHQ = if_else(is.na(PHQ), prev_PHQ1, PHQ),
          prev_day = if_else(is.na(PHQ), prev_day1, day),
          next_PHQ = if_else(is.na(PHQ), next_PHQ1, PHQ),
          next_day = if_else(is.na(PHQ), next_day1, day)
        ) %>%
        filter(is.na(PHQ)) %>%
        filter(!is.na(prev_PHQ)) %>% 
        filter(!is.na(next_PHQ)) %>% 
        ungroup() %>% 
        select(ID, prev_day, prev_PHQ, next_day, next_PHQ) %>% 
        rename(
          day = prev_day,
          PHQ = prev_PHQ) %>% 
        distinct(ID, day, PHQ, next_day, next_PHQ, .keep_all = TRUE)
    })
    
    # Segmentdaten
    segment_data <- reactive({
      filtered_data() %>%
        group_by(ID) %>%
        mutate(
          next_PHQ = dplyr::lead(PHQ),
          next_day = dplyr::lead(day),
          direction = case_when(
            next_PHQ > PHQ ~ "verschlechtert",
            next_PHQ < PHQ ~ "verbessert",
            TRUE ~ "unverändert"
          )
        ) %>%
        filter(!is.na(next_PHQ)) 
    })
    
    # Regressionslinien berechnen
    regression_lines <- reactive({
      req(filtered_data())
      
      filtered_data() %>%
        group_by(ID) %>%
        do({
          model <- tryCatch({
            lm(PHQ ~ day, data = ., na.action = na.exclude)
          }, error = function(e) {
            # Return a dummy model if there's an error
            return(NULL)
          })
          
          if(is.null(model)) {
            data.frame(day = numeric(0), pred = numeric(0))
          } else {
            days_range <- seq(min(.$day, na.rm = TRUE), max(.$day, na.rm = TRUE))
            preds <- predict(model, newdata = data.frame(day = days_range), na.action = na.exclude)
            data.frame(day = days_range, pred = preds)
          }
        }) %>%
        ungroup()
    })

    # Boxplot-Daten vorbereiten
    boxplot_data <- reactive({
      req(filtered_data())
      
      filtered_data() %>%
        mutate(PHQ = as.numeric(PHQ)) %>% 
        group_by(day) %>%
        summarise(
          has_data = sum(!is.na(PHQ)) > 0,
          q0 = if(has_data) min(PHQ, na.rm = TRUE) else NA_real_,
          q1 = if(has_data)quantile(PHQ, 0.25, na.rm = TRUE, names = FALSE) else NA_real_,
          q2 = if(has_data)median(PHQ, na.rm = TRUE) else NA_real_,
          q3 = if(has_data)quantile(PHQ, 0.75, na.rm = TRUE, names = FALSE) else NA_real_,
          q4 = if(has_data)max(PHQ, na.rm = TRUE) else NA_real_,
          .groups = "drop"
        ) %>% 
        filter(has_data) %>% 
        select(-has_data) %>% 
        filter_all(all_vars(!is.infinite(.)))
    })

    
      
    # Hauptplot mit Farbfix
    output$main_plot <- renderPlotly({
      p <- if(input$color_scheme == "id") {
        plot_ly(source = "A") %>% 
          event_register("plotly_hover") %>%
          add_trace(
            data = filtered_data(),
            x = ~day,
            y = ~PHQ,
            key = ~ID,
            color = ~ID,
            colors = id_colors(),
            type = "scatter",
            mode = "lines+markers",
            line = list(width = 2),
            marker = list(size = 8),
            hoverinfo = "text",
            text = ~paste("ID:", ID,
                          "<br>Tag:", day,
                          "<br>PHQ:", PHQ,
                          "<br>Screentime:", screentime)
          ) %>%
          layout(showlegend = FALSE,
                 xaxis = list(
                   title = "Day",
                   showline = TRUE,
                   linecolor = "#1c1c1e",
                   mirror = FALSE,
                   gridcolor = "#a9a9b7",
                   range = c(0.5, 14.5),
                   dtick = 1,
                   ticks = "outside",
                   fixedrange = TRUE
                   ),
                 yaxis = list(
                   title = "PHQ Score",
                   showline = TRUE,
                   linecolor = "#1c1c1e",
                   mirror = FALSE,
                   gridcolor = "#a9a9b7",
                   range = c(0, 27.5),
                   dtick = 3,
                   ticks = "outside",
                   fixedrange = TRUE
                   )
                 )
        
      } else {
        plot_ly(segment_data(), source = "A") %>%
          event_register("plotly_hover") %>%
          add_segments(
            x = ~day, y = ~PHQ,
            xend = ~next_day, yend = ~next_PHQ,
            color = ~direction,
            key = ~ID, #ähhm deepseek
            colors = c("verschlechtert" = "#69e963", "verbessert" = "#d83e3d", "unverändert" = "#0000fd"),
            line = list(width = 2),
            showlegend = FALSE,
            hoverinfo = "text",
            text = ~paste("ID:", ID, "<br>Von:", PHQ, "<br>Zu:", next_PHQ)
            ) %>%
          layout(showlegend = FALSE,
                 xaxis = list(
                   title = "Day",
                   showline = TRUE,
                   linecolor = "#1c1c1e",
                   mirror = FALSE,
                   gridcolor = "#a9a9b7",
                   range = c(0.5, 14.5),
                   dtick = 1,
                   ticks = "outside",
                   fixedrange = TRUE
                   ),
                 yaxis = list(
                   title = "PHQ Score",
                   showline = TRUE,
                   linecolor = "#1c1c1e",
                   mirror = FALSE,
                   gridcolor = "#a9a9b7",
                   range = c(0, 27.5),
                   dtick = 3,
                   fixedrange = TRUE
                 )
          )
        }
      
      p <- p %>%
        event_register("plotly_hover")
      
      # Boxplots hinzufügen
      if(input$show_boxplots && nrow(boxplot_data()) > 0) {
        p <- p %>%
          add_boxplot(
            data = boxplot_data(),
            orientation = "v",
            width = 0.5,
            x = ~day,
            y = ~q2,
            lowerfence = ~q0,
            q1 = ~q1,
            median = ~q2,
            q3 = ~q3,
            upperfence = ~q4,
            boxpoints = "all",
            jitter = 0.3,
            pointpos = -1.8,
            marker = list(size = 5, color = "black"),
            line = list(color = "#595959", width = 1),
            fillcolor = "rgba(200,200,200,0.5)",
            inherit = FALSE,
            hoverinfo = "text",
            text = ~paste("Tag:", day,
                          "<br>Median:", round(q2, 2),
                          "<br>Min:", q0,
                          "<br>Max:", q4)
          )
      }
      
      # Regressionslinien hinzufügen
      if(input$show_regression) {
        p <- p %>%
          add_trace(
            data = regression_lines(),
            x = ~day,
            y = ~pred,
            type = "scatter",
            mode = "lines",
            color = ~ID,
            colors = id_colors(),
            line = list(width = 1.5),
            inherit = FALSE,
            showlegend = FALSE
          )
      }
      
      if(input$show_missing) {
        if(input$color_scheme == "id") {
          p <- p %>%
            add_segments(
              data = missing_segments(),
              x = ~day, y = ~PHQ,
              xend = ~next_day, yend = ~next_PHQ,
              color = ~ID,
              colors = id_colors(),
              line = list(
                width = 1, 
                dash = "dot"
              ),
              inherit = FALSE,
              showlegend = FALSE
            )
        } else {
          p <- p %>%
            add_segments(
              data = missing_segments(),
              x = ~day, y = ~PHQ,
              xend = ~next_day, yend = ~next_PHQ,
              line = list(
                color = "#404040",
                width = 1,
                dash = "dot"
              ),
              inherit = FALSE,
              showlegend = FALSE
            )
        }
      }
      
      if(input$show_zones) {
        p <- p %>%
          layout(
            shapes = lapply(1:nrow(phq_zones), function(i) {
              list(
                type = "rect",
                fillcolor = phq_zones$color[i],
                line = list(color = phq_zones$color[i]),
                x0 = 0,
                x1 = 1,
                xref = "paper",
                y0 = phq_zones$y_start[i],
                y1 = phq_zones$y_end[i],
                layer = "below")
              }))
      } 
     p
     })
    
    # App category pie chart
    output$pie_chart <- renderPlotly({
      pie_data <- hovered_pie_data()
      
      req(pie_data)
      
      pie_data <- pie_data %>% 
        mutate(value_converted = round(value / 60000, 0)) %>% 
        mutate(label_text = paste0(value_converted, "min (",
                                   round(value / sum(value)*100, 1),"%)"))
      
      q <- plot_ly(
        pie_data,
        labels = ~category,
        values = ~value,
        type = "pie",
        textinfo = "text",
        text = ~label_text,
        textposition = "inside",
        insidetextorientation = "radial",
        marker = list(colors = color_list(12, "ID"))
        ) %>%
        layout(title = title_text())
    })
}
shinyApp(ui, server)
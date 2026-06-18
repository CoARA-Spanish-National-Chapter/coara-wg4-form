# =============================================================================
# CoARA WG4 - Formulario de Difusión y Formación
# Shiny app to collect dissemination and training activities from institutions
# participating in the CoARA Spanish National Chapter
# =============================================================================

library(shiny)
library(bslib)
library(shinyjs)
library(googlesheets4)
library(uuid)

# =============================================================================
# Configuration
# =============================================================================

SHEET_ID <- Sys.getenv(
  "COARA_SHEET_ID",
  unset = "11KyRgTZEF9co1YiFmWIsUdLcJOISAtAFPbAYIYyfOWo"
)

# Authentication: local JSON file or base64-encoded env var
if (file.exists("google-service-account.json")) {
  gs4_auth(path = "google-service-account.json")
} else if (nzchar(Sys.getenv("GOOGLE_SERVICE_ACCOUNT_KEY"))) {
  gs4_auth(path = rawToChar(
    jsonlite::base64_dec(Sys.getenv("GOOGLE_SERVICE_ACCOUNT_KEY"))
  ))
} else {
  gs4_deauth()
}

SHEET_COLUMNS <- c(
  "submission_id", "timestamp", "institution", "institution_type",
  "contact_name", "contact_email", "activity_type",
  "dissemination_category", "context", "activity_title",
  "description", "date_start", "date_end", "period_text",
  "governance", "target_audience", "format", "topics",
  "url", "url2", "additional_notes"
)

# Write header row to an empty Google Sheet
write_headers <- function() {
  empty_df <- data.frame(
    matrix(ncol = length(SHEET_COLUMNS), nrow = 0)
  )
  colnames(empty_df) <- SHEET_COLUMNS
  write_sheet(empty_df, ss = SHEET_ID, sheet = 1)
  message("Created header row in Google Sheet.")
}

ensure_sheet_headers <- function() {
  tryCatch({
    existing <- read_sheet(SHEET_ID, sheet = 1, n_max = 0)
    if ("submission_id" %in% colnames(existing)) {
      message("Sheet already has headers.")
      return(TRUE)
    }
    write_headers()
    TRUE
  }, error = function(e) {
    tryCatch({
      write_headers()
      TRUE
    }, error = function(e2) {
      message("Could not initialize Google Sheet: ",
              conditionMessage(e2))
      FALSE
    })
  })
}

SHEET_READY <- ensure_sheet_headers()

# =============================================================================
# Data: Institution list and dropdown choices
# =============================================================================

institutions <- c(
  "ACCUA", "ACCUEE", "ACPUA", "AMIT", "ANECA",
  "AQU Catalunya", "AQuAS", "AQUIB", "ARAID", "AVAP",
  "Biogipuzkoa", "CNEAI", "COMILLAS", "COSCE", "CREAF",
  "CRG", "CRUE", "CSIC", "DEUSTO", "EHU-UPV", "EU-LIFE",
  "FECYT", "FIBHULP", "FISABIO", "IBEC", "IBSAL",
  "IBS Granada", "IDIBAPS", "IIS Aragón", "IIS Navarra",
  "INCLIVA", "IR Sant Pau", "IREC", "ISCIII", "ISGlobal",
  "UA", "UAB", "UAL", "UAM", "UAX", "UB", "UC", "UCAM",
  "UCHCEU", "UCLM", "UCM", "UCO", "UdL", "UIB", "UJA",
  "UJI", "UMA", "UMH", "UMU", "UNIOVI", "UNIZAR", "UOC",
  "UPC", "UPCT", "UPO", "UPV", "URJC", "URL", "URV",
  "US", "UV", "UVIGO", "VIU"
)

institution_types <- c(
  "Agencia de evaluación",
  "Centro de investigación",
  "Universidad",
  "Asociación de universidades",
  "Otro"
)

dissemination_categories <- c(
  "Seminarios informativos y documentos",
  "Páginas web dedicadas a CoARA",
  "Noticias de CoARA en boletines institucionales",
  "Planes de acción CoARA",
  "Otro"
)

context_choices <- c("Nacional", "Internacional")

format_choices <- c(
  "Presencial", "En línea", "Híbrido",
  "Documento/Publicación", "Página web", "Boletín", "Otro"
)

audience_choices <- c(
  "Investigadores", "Evaluadores", "Personal administrativo",
  "Doctorandos", "Comités de selección", "Público general",
  "Otro"
)

topic_choices <- c(
  "Métricas responsables", "Ciencia abierta", "CV narrativo",
  "Principios CoARA", "Principios DORA", "HRS4R",
  "Ética e integridad", "Evaluación cualitativa",
  "Datos FAIR", "Otro"
)

# Helper: return value or empty string if NULL
null_to_empty <- function(x) {
  if (is.null(x)) "" else x
}

# =============================================================================
# UI
# =============================================================================

# CoARA National Chapter (Spain) brand palette, sampled from the logo:
#   orange  #E07030  (magnifying-glass mark; primary)
#   gold    #F0B050  (sun rays / "España")
#   red     #C01F2A  ("Spain")
#   charcoal #2A2A2A ("CoARA National Chapter" wordmark)
section_style <- paste0(
  "color: #2A2A2A; border-left: 4px solid #E07030; ",
  "padding: 2px 0 2px 14px; margin: 6px 0 18px 0; font-weight: 600;"
)

ui <- page_navbar(
  title = tags$span(
    "CoARA WG4",
    tags$span(class = "d-none d-md-inline",
              " - Formulario de Difusión y Formación")
  ),
  theme = bs_theme(
    version = 5,
    primary = "#C75B22",
    secondary = "#C01F2A",
    base_font = font_google("Public Sans", local = FALSE),
    heading_font = font_google("Fraunces", local = FALSE),
    font_scale = 0.95,
    "navbar-bg" = "#FFFFFF",
    "body-bg" = "#FAF6F0"
  ),
  header = tagList(
    useShinyjs(),
    tags$head(tags$style(HTML("
      body::before {
        content: ''; position: fixed; top: 0; left: 0; right: 0;
        height: 4px; z-index: 1080;
        background: linear-gradient(90deg, #C01F2A 0%, #E07030 50%, #F0B050 100%);
      }
      .navbar {
        border-bottom: 2px solid #F0B050;
        box-shadow: 0 1px 10px rgba(42, 42, 42, 0.05);
      }
      .navbar .nav-link.active, .navbar .nav-link:hover { color: #C75B22 !important; }
      .coara-logo-banner { text-align: center; padding: 24px 15px 2px; }
      .coara-logo-banner img { max-width: 320px; width: 100%; height: auto; }
      .coara-card {
        background: #ffffff; border: 1px solid #efe6da; border-radius: 14px;
        box-shadow: 0 8px 30px rgba(199, 91, 34, 0.07);
        max-width: 820px; margin: 8px auto 52px; padding: 28px 32px 36px;
      }
      .btn-primary {
        background-color: #E07030; border-color: #E07030;
        font-weight: 600; letter-spacing: 0.3px;
      }
      .btn-primary:hover, .btn-primary:focus, .btn-primary:active {
        background-color: #C75B22 !important; border-color: #C75B22 !important;
      }
      .form-control:focus, .form-select:focus, .selectize-input.focus {
        border-color: #E07030;
        box-shadow: 0 0 0 0.18rem rgba(224, 112, 48, 0.18);
      }
      a { color: #C75B22; }
      @media (max-width: 767px) {
        .container-fluid { padding-left: 15px; padding-right: 15px; }
        .navbar-brand { font-size: 0.9rem; }
        .coara-logo-banner img { max-width: 240px; }
        .coara-card { padding: 18px 16px 26px; border-radius: 10px; }
      }
    "))),
    div(
      class = "coara-logo-banner",
      tags$img(
        src = "coara-logo.png",
        alt = "CoARA National Chapter - Spain Espana"
      )
    )
  ),

  # -- Tab: Formulario --------------------------------------------------------
  nav_panel(
    title = "Formulario",
    icon = icon("clipboard-list"),

    div(
      class = "container-fluid coara-card",

      # Section A: Datos institucionales
      h4("Datos institucionales", style = section_style),
      fluidRow(
        column(6, selectizeInput(
          "institution", "Institución *", choices = NULL,
          options = list(
            create = TRUE,
            placeholder = "Seleccione o escriba su institución"
          )
        )),
        column(6, selectInput(
          "institution_type", "Tipo de institución *",
          choices = c("Seleccione..." = "", institution_types)
        ))
      ),
      fluidRow(
        column(6, textInput(
          "contact_name", "Persona de contacto"
        )),
        column(6, textInput(
          "contact_email", "Correo electrónico"
        ))
      ),

      br(),

      # Section B: Clasificación de la actividad
      h4("Clasificación de la actividad", style = section_style),
      fluidRow(
        column(12, selectInput(
          "activity_type", "Tipo de actividad *",
          choices = c(
            "Seleccione..." = "",
            "Difusión (sensibilización)" = "Difusion",
            "Formación" = "Formacion"
          )
        ))
      ),
      div(
        id = "dissemination_fields",
        fluidRow(
          column(6, selectInput(
            "dissemination_category", "Categoría de difusión",
            choices = c("Seleccione..." = "",
                        dissemination_categories)
          )),
          column(6, selectInput(
            "context", "Contexto",
            choices = c("Seleccione..." = "", context_choices)
          ))
        )
      ),

      br(),

      # Section C: Detalles de la actividad
      h4("Detalles de la actividad", style = section_style),
      textInput(
        "activity_title", "Nombre de la actividad *",
        width = "100%"
      ),
      textAreaInput(
        "description", "Descripción *", rows = 5,
        width = "100%",
        placeholder = paste(
          "Describa la actividad, incluyendo ponentes,",
          "temáticas, formato, etc."
        )
      ),
      fluidRow(
        column(4, div(
          id = "date_start_wrapper",
          dateInput(
            "date_start", "Fecha de inicio",
            value = NULL, language = "es"
          )
        )),
        column(4, div(
          id = "date_end_wrapper",
          dateInput(
            "date_end", "Fecha de fin",
            value = NULL, language = "es"
          )
        )),
        column(4, textInput(
          "period_text", "Período de implementación",
          placeholder = "Ej: Desde 2022, Curso 2024-2025"
        ))
      ),
      textAreaInput(
        "governance", "Gobernanza / Coordinación",
        rows = 2, width = "100%",
        placeholder = paste(
          "Quién coordina esta actividad",
          "dentro de la institución"
        )
      ),
      fluidRow(
        column(6, checkboxGroupInput(
          "target_audience", "Público objetivo",
          choices = audience_choices
        )),
        column(6, checkboxGroupInput(
          "topics", "Temáticas", choices = topic_choices
        ))
      ),
      selectInput(
        "format", "Formato",
        choices = c("Seleccione..." = "", format_choices)
      ),
      fluidRow(
        column(6, textInput(
          "url", "Enlace (URL)",
          placeholder = "https://..."
        )),
        column(6, textInput(
          "url2", "Enlace adicional",
          placeholder = "https://..."
        ))
      ),
      textAreaInput(
        "additional_notes", "Notas adicionales",
        rows = 3, width = "100%"
      ),

      # Submit button
      div(
        class = "text-center",
        style = "padding: 20px 0 40px 0;",
        actionButton(
          "submit", "Enviar",
          class = "btn-primary btn-lg",
          icon = icon("paper-plane")
        )
      )
    )
  ),

  # -- Tab: Ayuda -------------------------------------------------------------
  nav_panel(
    title = "Ayuda",
    icon = icon("circle-question"),

    div(
      class = "container-fluid coara-card",
      h4("Instrucciones", style = section_style),
      h5("Objetivo"),
      p(
        "Este formulario recoge información sobre actividades",
        " de ", strong("difusión"), " y ", strong("formación"),
        " relacionadas con la reforma de la evaluación de la",
        " investigación, en el marco del Capítulo Nacional",
        " Español de CoARA (NCS)."
      ),
      h5("Tipos de actividad"),
      tags$ul(
        tags$li(
          strong("Difusión (sensibilización):"),
          " Seminarios informativos, páginas web dedicadas",
          " a CoARA, noticias en boletines institucionales,",
          " planes de acción, etc."
        ),
        tags$li(
          strong("Formación:"),
          " Cursos, talleres, seminarios de capacitación,",
          " webinars, conferencias formativas sobre",
          " evaluación responsable, ciencia abierta,",
          " métricas responsables, etc."
        )
      ),
      h5("Campos obligatorios"),
      p(
        "Los campos marcados con * son obligatorios:",
        " Institución, Tipo de institución, Tipo de",
        " actividad, Nombre de la actividad y Descripción."
      ),
      h5("Instituciones"),
      p(
        "El desplegable incluye las instituciones miembros",
        " del NCS. Si su institución no aparece en la lista,",
        " puede escribir el nombre directamente."
      ),
      h5("Contacto"),
      p(
        "Para dudas o incidencias con el formulario,",
        " contacte con el Grupo de Trabajo 4 (WG4) del",
        " Capítulo Nacional Español de CoARA."
      )
    )
  )
)

# =============================================================================
# Server
# =============================================================================

server <- function(input, output, session) {

  # Populate institution selectize on startup
  updateSelectizeInput(
    session, "institution",
    choices = institutions,
    server = TRUE
  )

  # -- Conditional field visibility -------------------------------------------

  shinyjs::hide("dissemination_fields")

  observeEvent(input$activity_type, {
    if (input$activity_type == "Difusion") {
      shinyjs::show("dissemination_fields")
    } else {
      shinyjs::hide("dissemination_fields")
    }
  })

  observeEvent(input$dissemination_category, {
    hide_dates <- input$dissemination_category %in% c(
      "Páginas web dedicadas a CoARA",
      "Planes de acción CoARA"
    )
    if (hide_dates) {
      shinyjs::hide("date_start_wrapper")
      shinyjs::hide("date_end_wrapper")
    } else {
      shinyjs::show("date_start_wrapper")
      shinyjs::show("date_end_wrapper")
    }
  })

  # -- Validation helpers -----------------------------------------------------

  validate_email <- function(email) {
    if (is.null(email) || email == "") return(TRUE)
    grepl("^[^@\\s]+@[^@\\s]+\\.[^@\\s]+$", email, perl = TRUE)
  }

  validate_url <- function(url) {
    if (is.null(url) || url == "") return(TRUE)
    grepl("^https?://", url)
  }

  # -- Submission handler -----------------------------------------------------

  observeEvent(input$submit, {
    errors <- character(0)

    if (is.null(input$institution) || input$institution == "")
      errors <- c(errors, "Institución")
    if (is.null(input$institution_type) || input$institution_type == "")
      errors <- c(errors, "Tipo de institución")
    if (is.null(input$activity_type) || input$activity_type == "")
      errors <- c(errors, "Tipo de actividad")
    if (is.null(input$activity_title) || input$activity_title == "")
      errors <- c(errors, "Nombre de la actividad")
    if (is.null(input$description) || input$description == "")
      errors <- c(errors, "Descripción")
    if (!validate_email(input$contact_email))
      errors <- c(errors, "Correo electrónico (formato no válido)")
    if (!validate_url(input$url))
      errors <- c(errors, "URL (debe empezar con http:// o https://)")
    if (!validate_url(input$url2))
      errors <- c(errors, "URL adicional (debe empezar con http:// o https://)")

    date_start <- input$date_start
    date_end <- input$date_end
    dates_valid <- inherits(date_start, "Date") &&
      inherits(date_end, "Date") &&
      !is.na(date_start) && !is.na(date_end)
    if (dates_valid && date_end < date_start)
      errors <- c(errors, "La fecha de fin debe ser posterior a la de inicio")

    if (length(errors) > 0) {
      error_list <- paste0("<li>", errors, "</li>", collapse = "")
      showNotification(
        HTML(paste0(
          "<strong>Por favor, revise los siguientes campos:</strong>",
          "<ul>", error_list, "</ul>"
        )),
        type = "error",
        duration = 10
      )
      return()
    }

    # Build submission
    date_start_str <- if (dates_valid) as.character(date_start) else ""
    date_end_str <- if (dates_valid) as.character(date_end) else ""

    audience_str <- paste(
      null_to_empty(input$target_audience), collapse = ";"
    )
    topics_str <- paste(
      null_to_empty(input$topics), collapse = ";"
    )

    submission <- data.frame(
      submission_id          = UUIDgenerate(),
      timestamp              = as.character(Sys.time()),
      institution            = input$institution,
      institution_type       = input$institution_type,
      contact_name           = null_to_empty(input$contact_name),
      contact_email          = null_to_empty(input$contact_email),
      activity_type          = input$activity_type,
      dissemination_category = null_to_empty(input$dissemination_category),
      context                = null_to_empty(input$context),
      activity_title         = input$activity_title,
      description            = input$description,
      date_start             = date_start_str,
      date_end               = date_end_str,
      period_text            = null_to_empty(input$period_text),
      governance             = null_to_empty(input$governance),
      target_audience        = audience_str,
      format                 = null_to_empty(input$format),
      topics                 = topics_str,
      url                    = null_to_empty(input$url),
      url2                   = null_to_empty(input$url2),
      additional_notes       = null_to_empty(input$additional_notes),
      stringsAsFactors       = FALSE
    )

    # Write to Google Sheets
    if (!SHEET_READY) {
      showModal(modalDialog(
        title = "Error de configuración",
        p("La conexión con Google Sheets no está configurada.",
          " Contacte con el administrador del formulario."),
        easyClose = TRUE,
        footer = modalButton("Cerrar")
      ))
      return()
    }

    result <- tryCatch({
      sheet_append(ss = SHEET_ID, data = submission)
      TRUE
    }, error = function(e) {
      showModal(modalDialog(
        title = "Error al enviar",
        tagList(
          p("No se ha podido guardar el envío.",
            " Por favor, inténtelo de nuevo."),
          p("Si el problema persiste,",
            " contacte con el equipo de WG4."),
          p(class = "text-muted small",
            paste("Detalle:", conditionMessage(e)))
        ),
        easyClose = TRUE,
        footer = modalButton("Cerrar")
      ))
      FALSE
    })

    if (result) {
      showModal(modalDialog(
        title = "Envío realizado con éxito",
        tagList(
          p(icon("circle-check", class = "text-success"),
            " Su actividad ha sido registrada correctamente."),
          hr(),
          tags$dl(
            tags$dt("Institución"),
            tags$dd(input$institution),
            tags$dt("Tipo de actividad"),
            tags$dd(input$activity_type),
            tags$dt("Nombre"),
            tags$dd(input$activity_title)
          ),
          hr(),
          p("Puede enviar otra actividad pulsando",
            " el botón de abajo.")
        ),
        footer = tagList(
          actionButton(
            "reset_form", "Enviar otra actividad",
            class = "btn-primary"
          ),
          modalButton("Cerrar")
        ),
        easyClose = TRUE
      ))
    }
  })

  # -- Form reset -------------------------------------------------------------

  observeEvent(input$reset_form, {
    removeModal()
    updateSelectizeInput(session, "institution", selected = "")
    updateSelectInput(session, "institution_type", selected = "")
    updateTextInput(session, "contact_name", value = "")
    updateTextInput(session, "contact_email", value = "")
    updateSelectInput(session, "activity_type", selected = "")
    updateSelectInput(session, "dissemination_category", selected = "")
    updateSelectInput(session, "context", selected = "")
    updateTextInput(session, "activity_title", value = "")
    updateTextAreaInput(session, "description", value = "")
    updateDateInput(session, "date_start", value = character(0))
    updateDateInput(session, "date_end", value = character(0))
    updateTextInput(session, "period_text", value = "")
    updateTextAreaInput(session, "governance", value = "")
    updateCheckboxGroupInput(
      session, "target_audience", selected = character(0)
    )
    updateCheckboxGroupInput(
      session, "topics", selected = character(0)
    )
    updateSelectInput(session, "format", selected = "")
    updateTextInput(session, "url", value = "")
    updateTextInput(session, "url2", value = "")
    updateTextAreaInput(session, "additional_notes", value = "")
    shinyjs::hide("dissemination_fields")
  })
}

# =============================================================================
# Run app
# =============================================================================

shinyApp(ui = ui, server = server)

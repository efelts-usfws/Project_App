library(shiny)
library(shinyWidgets)
library(tidyverse)
library(readxl)
library(leaflet)
library(leafpm)
library(leafem)
library(leaflet.extras2)
library(bslib)
library(bsicons)
library(sf)
library(scales)

photos.dat <- read_rds("shiny_pieces/photo_table")

# make a vector of idfg regions

region_vector <- c(
  "Panhandle Region",
  "Clearwater Region",
  "Southwest Region",
  "Magic Valley Region",
  "Southeast Region",
  "Upper Snake Region",
  "Salmon Region"
)

# make a key with the region vector

region_key <- tibble(
  idfg_region = region_vector,
  region_number=seq(1,7,1)
)

# read in project data

projects.dat <- read_rds("shiny_pieces/project_table") %>%
  left_join(region_key, by = "idfg_region") %>%
  mutate(project_status = case_when(
    project_state == "Implementation Completed" ~ "Completed",
    TRUE ~ "Active"
  ),
  partner_agency=ifelse(is.na(partner_agency),"None",partner_agency))

# read in reference for project docs

project_docs <- read_rds("shiny_pieces/project_documents.rds")



# make a vector of managers

manager_vector <- projects.dat %>% 
  distinct(managing_org) %>% 
  arrange(managing_org) %>% 
  pull(managing_org)

# make a vector of partners

partner_vector <- projects.dat %>%
  separate_rows(partner_agency, sep = ",") %>%
  mutate(partner_agency = trimws(partner_agency)) %>%
  distinct(partner_agency) %>%
  arrange(partner_agency) %>%
  pull(partner_agency)


# make a vector of project types

projecttype_vector <- projects.dat %>%
  separate_rows(project_category, sep = ",") %>%
  mutate(project_category = trimws(project_category)) %>%
  distinct(project_category) %>%
  arrange(project_category) %>%
  pull(project_category)

# make a vector of primary species benefitted and
# arrange alphabetically

species_vector <- projects.dat %>%
  ungroup() %>% 
  select(primary_species) %>%
  distinct(primary_species) %>%
  arrange(primary_species) %>%
  pull(primary_species)


# read in layers to have as toggles on the map

hab_gpkg <- "data-raw/idfg_habitat_mapping.gpkg"

regions.sf <- st_read(
  dsn = hab_gpkg,
  layer = "idfg_regions"
)

streams.sf <- st_read(
  dsn = hab_gpkg,
  layer = "streams"
)

lakes.sf <- st_read(
  dsn = hab_gpkg,
  layer = "lakes"
)


counties.sf <- read_rds("data-raw/idaho_counties") %>%
  st_transform(crs = st_crs(streams.sf))


huc8.sf <- read_rds("data-raw/huc8") %>%
  st_transform(crs = st_crs(streams.sf))

fmp_regions.sf <- read_rds("data-raw/fmp_drainages.rds")

# make base leaflet map

leaflet_base <- leaflet() %>%
  addProviderTiles(providers$Esri.WorldTopoMap, group = "Topographic") %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "Imagery") %>%
  addProviderTiles(providers$OpenStreetMap.Mapnik, group = "Roads") %>%
  setView(lng = -114.60938, lat = 45.30580, zoom = 6) %>%
  addMouseCoordinates() %>%
  addPolygons(
    data = regions.sf,
    fill = "grey", color = "black",
    fillOpacity = 0.1,
    group = "IDFG Regions",
    popup = ~ str_c(region_name)
  ) %>%
  addPolygons(
    data = fmp_regions.sf,
    fill = "grey", color = "black",
    fillOpacity = 0.1,
    group = "FMP Drainages",
    popup = ~ str_c(fmp_drainage)
  ) |>
  addPolygons(
    data = counties.sf,
    fill = "grey", color = "black",
    fillOpacity = 0.1,
    group = "Counties",
    popup = ~ str_c(NAME, "County", sep = " ")
  ) %>%
  addPolygons(
    data = lakes.sf,
    label = ~ str_c(name),
    group = "Lakes/Reservoirs"
  ) %>%
  addPolylines(
    data = streams.sf,
    label = ~ str_c(name),
    group = "Streams"
  ) %>%
  addLayersControl(
    baseGroups = c("Topographic", "Imagery", "Roads"),
    overlayGroups = c(
      "Streams", "Lakes/Reservoirs",
      "IDFG Regions",
      "FMP Drainages",
      "Counties"
    ),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  hideGroup(c(
    "IDFG Regions",
    "FMP Drainages",
    "Counties"
  )) %>% 
  addMiniMap(
    tiles = providers$OpenStreetMap.Mapnik,
    toggleDisplay = TRUE,
    position = "bottomright",
    width = 180,
    height = 180,
    zoomLevelOffset = -6
  )
leaflet_base

status_pal <- colorFactor(
  palette = c("blue", "green"),
  levels = c("Completed", "Active")
)

# build the UI

ui <- page_navbar(
  title = "IDFG Fish Habitat Program Dashboard",
  theme = bs_theme(bootswatch = "flatly"),
  id = "nav",
  header = tags$head(
    tags$style(HTML("

 

  .bslib-value-box .value-box-title {
    font-weight: 700 !important;
    text-align: center !important;
    width: 100%;
  }
  
   .bslib-value-box .value-box-value {
    font-size: 0.8rem !important;
    text-align: center !important;
  }

  .bslib-value-box .value-box-subtitle,
  .bslib-value-box p {
    font-size: 0.7rem !important;
  }
  
  .selected-project-scroll {
  max-height: calc(100vh - 220px);
  overflow-y: auto;
  padding-right: 0.5rem;
  }

  .modal-backdrop {
    z-index: 20000 !important;
  }

  .modal {
    z-index: 20001 !important;
  }


")),
    tags$script(src = "https://unpkg.com/leaflet-easyprint@2.1.9/dist/bundle.js")
  ),
  sidebar = sidebar(
    width = 300,
    id = "sb",
    collapsible = T,
    open = T,
    accordion(
      open=c("Project Filters","Individual Projects"),
      accordion_panel(
        "Project Filters",
        pickerInput("primary_species_filter",
          label = "Select Primary Species Benefitted",
          choices = species_vector,
          selected = species_vector,
          multiple = T,
          options = list(
            `actions-box` = TRUE,
            `live-search` = TRUE
          )
        ),
        pickerInput("idfg_region_filter",
          label = "Choose IDFG Region",
          choices = region_vector,
          selected = region_vector,
          multiple = T,
          options = list(
            `actions-box` = TRUE,
            `live-search` = TRUE
          )
        ),
        pickerInput("managing_org_filter",
                    label="Choose Managing Organization",
                    choices=manager_vector,
                    selected=manager_vector,
                    multiple=T,
                    options = list(
                      `actions-box` = TRUE,
                      `live-search` = TRUE
                    )),
        pickerInput("partner_filter",
                    label="Choose Partner(s)",
                    choices=partner_vector,
                    selected=partner_vector,
                    multiple=T,
                    options = list(
                      `actions-box` = TRUE,
                      `live-search` = TRUE
                    )),
        pickerInput("projecttype_filter",
          label = "Choose Project Type",
          choices = projecttype_vector,
          selected = projecttype_vector,
          multiple = T,
          options = list(
            `actions-box` = TRUE,
            `live-search` = TRUE
          )
        )
      ),
      accordion_panel(
        "Individual Projects",
        uiOutput("ind_project_filter")
      )
    )
  ),
  nav_panel(
    "Main",
    layout_columns(
      col_widths = c(3,6,3),
      class="g-2",
      value_box(
        title="Projects Overview",
       uiOutput("project_overview"),
        max_height = "200px"
      ),
      value_box(
        title="Evaluation Metrics",
        uiOutput("eval_metrics"),
        max_height = "200px"
      ),
      value_box(
        title="Funding Summary",
        uiOutput("funding_summary"),
        max_height = "200px"
      )
    ),
    page_fillable(
      layout_columns(
        col_widths = c(8, 4),
        card(card_header("Project Map"),
          leafletOutput("project_map"),
          height="700px",
          max_height = "700px",
          full_screen = T
        ),
        card(
          card_header("Selected Project"),
          navset_card_tab(
            nav_panel(
              "Overview",
              uiOutput("selected_project_ui")
            ),
            nav_panel(
              "Photos",
              uiOutput("project_gallery_ui")
            ),
            nav_panel(
              "Timeline",
              uiOutput("project_timeline_ui")
            ),
            nav_panel(
              "Permitting",
              uiOutput("project_permitting_ui")
            )
          ),
          height = "700px",
          max_height = "700px",
          full_screen = TRUE
        )
      )
    )
  )
)

# build the server

server <- function(input, output, session) {
  
  projects_reactive <- reactive({
    req(input$primary_species_filter, input$idfg_region_filter, input$projecttype_filter,
        input$managing_org_filter,input$partner_filter)

    projects.dat %>%
      filter(
        primary_species %in% input$primary_species_filter,
        idfg_region %in% input$idfg_region_filter,
        managing_org %in% input$managing_org_filter
      ) %>%
      rowwise() %>%
      filter(any(trimws(unlist(strsplit(project_category, ","))) %in% input$projecttype_filter)) %>%
      ungroup() %>% 
      rowwise() %>% 
      filter(
          any(trimws(unlist(strsplit(partner_agency, ","))) %in% input$partner_filter)
      ) %>% 
      ungroup()
  })
  
  # render picker input of project names that
  # are included given the filters selected
 
  
  output$ind_project_filter <- renderUI({
    dat <- projects_reactive()
    
    project_vector <- dat |> 
      arrange(project_name) |> 
      pull(project_name)
    
    pickerInput(
      "project_filter",
      label = "Choose a Project",
      choices = c("Search or choose a project" = "", project_vector),
      selected = "",
      multiple = FALSE,
      options = list(
        `live-search` = TRUE
      )
    )
  })
  
  # project count after filters to 
  # render into the value box 
  output$project_overview <- renderUI({
    dat <- projects_reactive()
    
    project_count <- nrow(dat)
    completed_count <- sum(dat$project_status == "Completed", na.rm = TRUE)
    active_count <- sum(dat$project_status == "Active", na.rm = TRUE)
    
    n_partners <- dat |>
      select(partner_agency) |>
      separate_rows(partner_agency, sep = ",") |>
      mutate(partner_agency = trimws(partner_agency)) |>
      filter(!is.na(partner_agency), partner_agency != "") |>
      distinct(partner_agency) |>
      nrow()
    
    div(
      style = "
      display: flex;
      flex-direction: column;
      align-items: center;
      gap: 6px;
      text-align: center;
    ",
      
      tags$div(
        style = "font-size: 1rem; font-weight: 700;",
        str_c(scales::comma(project_count), " Projects")
      ),
      
      tags$div(
        style = "
        display: flex;
        gap: 6px;
        justify-content: center;
        flex-wrap: wrap;
        font-size: 1rem;
      ",
        tags$span(
          class = "badge rounded-pill",
          style= paste0(
            "background-color:", status_pal("Active"),
            "; color: white;"
          ),
          str_c(scales::comma(active_count), " Active")
        ),
        tags$span(
          class = "badge rounded-pill",
          style= paste0(
            "background-color:", status_pal("Completed"),
            "; color: white;"
          ),
          str_c(scales::comma(completed_count), " Completed")
        )
      ),
      
      tags$div(
        style = "font-size: 0.8rem;",
        str_c(scales::comma(n_partners), " Partners")
      )
    )
  })

  output$eval_metrics <- renderUI({
    dat <- projects_reactive()
    
    metrics <- tibble::tribble(
      ~metric, ~value,
      "Barriers Removed", sum(dat$barriers_removed, na.rm = TRUE),
      "Stream Miles Reconnected", round(sum(dat$stream_miles_reconnected, na.rm = TRUE)),
      "Acres Floodplain Reconnected", round(sum(dat$floodplain_reconnect, na.rm = TRUE)),
      "Enhancement Structures", round(sum(dat$enhancement_structures, na.rm = TRUE)),
      "Riparian Acres", round(sum(dat$riparian_area, na.rm = TRUE)),
      "Streambank Feet", round(sum(dat$streambank_linearfeet, na.rm = TRUE)),
      "Flow Restored Miles", round(sum(dat$flow_restored_miles, na.rm = TRUE)),
      "Wetland Acres", round(sum(dat$wetland_acres, na.rm = TRUE))
    )
    
    tags$div(
      style = "
      display: grid;
      grid-template-columns: 1fr 1fr;
      gap: 4px 12px;
      font-size: 0.75rem;
    ",
      purrr::pmap(
        metrics,
        \(metric, value) {
          tags$div(
            tags$b(scales::comma(value)),
            tags$span(paste0(' ', metric))
          )
        }
      )
    )
  })
  
  output$funding_summary <- renderUI({
    
    dat <- projects_reactive()
    
    fund_amount <- sum(dat$award_amount, na.rm = TRUE)
    
    n_funders <- dat |>
      select(funding_source) |>
      separate_rows(funding_source, sep = ";") |>
      mutate(funding_source = trimws(funding_source)) |>
      filter(!is.na(funding_source), funding_source != "") |>
      distinct(funding_source) |>
      nrow()
    
    tags$div(
      style = "
      display: flex;
      flex-direction: column;
      align-items: center;
      gap: 6px;
      text-align: center;
    ",
      tags$div(
        style = "font-size: 1rem; font-weight: 700;",
        str_c(scales::dollar(fund_amount, accuracy = 1), " Awarded")
      ),
      tags$div(
        style = "font-size: 0.8rem;",
        str_c(scales::comma(n_funders), " Funding Sources")
      )
    )
  })

  output$project_map <- renderLeaflet({
    dat <- projects_reactive()

    leaflet_base %>%
      addCircleMarkers(
        data = dat,
        fillColor = ~ status_pal(project_status),
        color = ~ status_pal(project_status),
        fillOpacity = 1,
        radius = 6,
        layerId = ~project_name
      ) %>%
      addLegend(
        pal = status_pal,
        values = c("Completed", "Active")
      )
  })

  selected_project <- reactiveVal(NULL)
  current_photo_index <- reactiveVal(NULL)

  selected_project_data <- reactive({
    req(selected_project())
    projects_reactive() %>% filter(project_name == selected_project())
  })

  output$selected_project_ui <- renderUI({
    if (is.null(selected_project())) {
      return(
        tagList(
          h5("No project selected"),
          p("Click a project on the map to view project details and photos.")
        )
      )
    }

    dat <- selected_project_data()
    req(nrow(dat) == 1)

    x <- dat[1, ]

    tagList(
      h4(x$project_name),
      tags$div(
        style = "margin-bottom: 12px;",
        span(
          class = "badge rounded-pill",
          style = paste0(
            "background-color:", ifelse(x$project_state == "Completed", "blue", "green"),
            "; color: white; margin-right: 6px;"
          ),
          x$project_state
        )
      ),
      p(tags$b("Project Type: "), x$project_category),
      p(tags$b("IDFG Region: "), x$idfg_region),
      p(tags$b("FMP Drainage: "), x$fmp_drainage),
      p(tags$b("County: "), x$county),
      p(tags$b("Primary Waterbody: "), x$stream_name),
      p(tags$b("Primary Species: "), x$primary_species),
      p(tags$b("Additional Species Benefitted: "), x$secondary_species),
      p(tags$b("Description: "), x$project_description)
    )
  })

  selected_project_photos <- reactive({
    req(selected_project())
    photos.dat %>%
      filter(project_name == selected_project()) %>%
      arrange(photo_order)
  })

  observeEvent(input$project_map_marker_click, {
    click <- input$project_map_marker_click
    req(click$id)

    selected_project(click$id)
    current_photo_index(NULL)

    selected_data <- projects.dat %>%
      filter(project_name == click$id)

    leafletProxy("project_map") %>%
      clearGroup("selection") %>%
      addCircleMarkers(
        data = selected_data,
        fillColor = NA,
        color = "red",
        radius = 10,
        weight = 3,
        group = "selection"
      )
  })
  
  observeEvent(input$project_filter, {
    req(input$project_filter != "")
    
    selected_project(input$project_filter)
    current_photo_index(NULL)
    
    selected_data <- projects_reactive() |>
      filter(project_name == input$project_filter)
    
    leafletProxy("project_map") |>
      clearGroup("selection") |>
      addCircleMarkers(
        data = selected_data,
        fillColor = NA,
        color = "red",
        radius = 10,
        weight = 3,
        group = "selection"
      )
  }, ignoreInit = TRUE)
  
  output$project_gallery_ui <- renderUI({
    if (is.null(selected_project())) {
      return(tags$p("Click a project on the map to view photos."))
    }
    
    photo_dat <- selected_project_photos()
    
    if (nrow(photo_dat) == 0) {
      return(tags$p("No photos available for this project."))
    }
    
    tagList(
      h5("Photos"),
      div(
        style = "display:grid; grid-template-columns:1fr 1fr; gap:12px;",
        lapply(seq_len(nrow(photo_dat)), function(i) {
          this_photo <- photo_dat[i, ]
          
          card(
            style = "padding:8px;",
            tags$a(
              href = "#",
              onclick = sprintf(
                "Shiny.setInputValue('photo_selected', %s, {priority: 'event'}); return false;",
                i
              ),
              tags$img(
                src = file.path("project_photos", this_photo$photo_file),
                style = "width:100%; height:180px; object-fit:cover; border-radius:8px;"
              )
            ),
            tags$div(
              style = "margin-top:8px;",
              if (!is.na(this_photo$caption) && nzchar(this_photo$caption)) {
                tags$p(this_photo$caption, style = "margin-bottom:4px; font-size:0.9rem;")
              },
              if (!is.na(this_photo$credit) && nzchar(this_photo$credit)) {
                tags$p(
                  paste("Photo:", this_photo$credit),
                  style = "margin-bottom:0; font-size:0.8rem; color:#666;"
                )
              }
            )
          )
        })
      )
    )
  })
  
  # helper function to show modal
  show_photo_modal <- function(i) {
    photo_dat <- selected_project_photos()
    req(nrow(photo_dat) > 0)
    req(i >= 1, i <= nrow(photo_dat))
    
    this_photo <- photo_dat[i, ]
    
    showModal(
      modalDialog(
        size = "l",
        easyClose = TRUE,
        footer = NULL,
        tags$img(
          src = file.path("project_photos", this_photo$photo_file),
          style = "display:block; margin:auto; max-width:100%; max-height:60vh; object-fit:contain;"
        ),
        tags$div(
          style = "margin-top:12px;",
          tags$p(
            paste(i, "of", nrow(photo_dat)),
            style = "color:#666; margin-bottom:8px;"
          ),
          if (!is.na(this_photo$caption) && nzchar(this_photo$caption)) {
            tags$p(tags$b("Caption: "), this_photo$caption)
          },
          if (!is.na(this_photo$credit) && nzchar(this_photo$credit)) {
            tags$p(tags$b("Credit: "), this_photo$credit)
          }
        ),
        tags$div(
          style = "display:flex; justify-content:space-between; margin-top:15px;",
          actionButton("prev_photo", "← Previous"),
          actionButton("next_photo", "Next →")
        )
      )
    )
  }
  
  observeEvent(input$photo_selected, {
    i <- as.integer(input$photo_selected)
    current_photo_index(i)
    show_photo_modal(i)
  }, ignoreInit = TRUE)
  
  # next button
  observeEvent(input$next_photo, {
    req(current_photo_index())
    photo_dat <- selected_project_photos()
    req(nrow(photo_dat) > 0)
    
    i <- current_photo_index()
    new_i <- ifelse(i == nrow(photo_dat), 1, i + 1)
    
    current_photo_index(new_i)
    show_photo_modal(new_i)
  })
  
  # previous button
  observeEvent(input$prev_photo, {
    req(current_photo_index())
    photo_dat <- selected_project_photos()
    req(nrow(photo_dat) > 0)
    
    i <- current_photo_index()
    new_i <- ifelse(i == 1, nrow(photo_dat), i - 1)
    
    current_photo_index(new_i)
    show_photo_modal(new_i)
  })
  
  # filter docs for selected project
  
  selected_project_docs <- reactive({
    req(selected_project())
    
    project_docs |>
      filter(
        project_name == selected_project(),
        doc_type == "permit"
      )
  })
  
  # track selected document
  
  selected_doc <- reactiveVal(NULL)
  
  # reset when project changes
  
  observeEvent(selected_project(), {
    docs <- selected_project_docs()
    
    if (nrow(docs) > 0) {
      selected_doc(docs$doc_file[1])
    } else {
      selected_doc(NULL)
    }
  })
  
  output$project_permitting_ui <- renderUI({
    if (is.null(selected_project())) {
      return(p("Select a project to view permitting documents."))
    }
    
    docs <- selected_project_docs()
    
    if (nrow(docs) == 0) {
      return(p("No permitting documents available."))
    }
    
    tagList(
      pickerInput(
        "permit_doc_select",
        "Select Permit Document",
        choices = setNames(docs$doc_file, docs$doc_title),
        selected = docs$doc_file[1],
        multiple = FALSE
      ),
      
      uiOutput("permit_pdf_viewer")
    )
  })
  
  observeEvent(input$permit_doc_select, {
    selected_doc(input$permit_doc_select)
  }, ignoreInit = TRUE)
  
  output$permit_pdf_viewer <- renderUI({
    req(selected_doc())
    
    pdf_src <- file.path("project_docs", "permits", selected_doc())
    
    tagList(
      tags$a(
        href = pdf_src,
        target = "_blank",
        download = selected_doc(),
        class = "btn btn-sm btn-primary",
        "Download PDF"
      ),
      
      tags$iframe(
        src = pdf_src,
        style = "
        width: 100%;
        height: 560px;
        border: 1px solid #ddd;
        margin-top: 10px;
        border-radius: 6px;
      "
      )
    )
  })
  
  # reset selected project when filters change
  observeEvent(
    list(
      input$primary_species_filter,
      input$idfg_region_filter,
      input$projecttype_filter,
      input$managing_org_filter,
      input$partner_filter
    ),
    {
      selected_project(NULL)
      current_photo_index(NULL)

      leafletProxy("project_map") %>%
        clearGroup("selection")
      
      
      
      
      updatePickerInput(session, "project_filter", selected = "")
    },
    ignoreInit = TRUE
  )

}


shinyApp(ui, server)

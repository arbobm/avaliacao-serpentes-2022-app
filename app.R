pacotes <- c("shiny", "readxl", "dplyr", "stringr", "leaflet", "sf", "readr",
             "shinybusy", "leaflet.extras", "raster")

to_install <- !pacotes %in% installed.packages()
if(any(to_install)) {
  install.packages(pacotes[to_install])
}



library(shiny)
library(readxl)
library(dplyr)
library(stringr)
library(leaflet)
library(sf)
library(readr)
library(shinybusy)
library(leaflet.extras)
library(raster)

ano_atual <- "2021"
ano_passado <- "2011"

proj_albers <- "+proj=aea +lat_0=-12 +lon_0=-54 +lat_1=-2 +lat_2=-22
+x_0=5000000 +y_0=10000000 +ellps=GRS80 +units=m +no_defs"
## carrega pontos de ocorrencia
occ <- readxl::read_xlsx("planilhas/Registros_serpentes_avaliacao_2022_v4.xlsx")

## carrega mpcs
list_files <- list.files("serpentes/mpcs", pattern = "_mpc.shp")
lista_mpcs <- list()

for (i in 1:length(list_files)) {
  
  pol <- sf::read_sf(paste0("serpentes/mpcs/", list_files[i]))
  specie <- stringr::str_replace(list_files[i], "_mpc.shp", "")
  pol <- pol %>% 
    dplyr::mutate(species = specie)
  lista_mpcs[[i]] <- pol
  
}

combined_distribution <- dplyr::bind_rows(lista_mpcs)
combined_distribution <- combined_distribution %>% 
  sf::st_transform(crs = proj_albers) %>% 
  dplyr::mutate(AREA_KM2 = as.numeric(st_area(.))/1000000,
                tipo = "mpc") %>% 
  sf::st_transform(crs = 4326) %>% 
  dplyr::rename_with(tolower) %>% 
  dplyr::select(species, area_km2)

combined_distribution_buf <- combined_distribution %>% 
  sf::st_transform(crs = proj_albers) %>% 
  sf::st_buffer(dist = 200000) %>% 
  dplyr::mutate(area_km2 = as.numeric(st_area(.))/1000000,
                tipo = "mpc buffer 10 km") %>% 
  sf::st_transform(crs = 4326)


ucs <- sf::st_read("shapes/UCsFedIcmb_EstMunicMMA.shp")
ucs <- sf::st_transform(ucs, crs = sf::st_crs(combined_distribution)) %>% 
  dplyr::mutate(SiglaGrupo =
                  dplyr::case_when(
                    SiglaGrupo == "US" ~ "Uso sustentável",
                    SiglaGrupo == "PI" ~ "Proteção integral"))


# shapes de atividades produtivas -----------------------------------------

peolicos <- sf::st_read("shapes/atividades/Poligono_do_Parque_Eolioeletrico_EOL.shp")
peolicos <- peolicos %>% 
  sf::st_transform(crs = st_crs(combined_distribution))
peolicos <- sf::st_make_valid(peolicos) %>% 
  dplyr::rename_with(tolower)

# irrigacao <- sf::st_read("shapes/atividades/mapbiomas_br_col6_irrigated_agriculture_ok.shp")
# mineracao <- sf::st_read("C:/Users/bruna/OneDrive/01_BaseSIG/Brasil/MapBiomas/usos2/mapbiomas_br_col7_mined_substance_ok.shp")




# estradas <- sf::st_read("shapes/atividades/rod_trecho_rodoviario_l.shp")
# estradas <- estradas %>% 
#   sf::st_transform(crs = sf::st_crs(combined_distribution))
# sf::st_is_valid(estradas) %>% unique

# reservatorios <- sf::st_read("C:/Users/bruna/OneDrive/01_BaseSIG/Brasil/ANA/geoft_bho_massa_dagua_v2019/geoft_bho_massa_dagua_v2019_art_wgs.shp")

ahe <- st_read("shapes/atividades/Aproveitamento_Hidreletricos_AHE.shp")
ahe <- st_transform(ahe, crs = st_crs(combined_distribution))


info <- readxl::read_xlsx("supplemental_material/SupplementalTable1.xlsx", 
                          sheet = 1)
modelinfo <- readxl::read_xlsx("supplemental_material/SupplementalTable1.xlsx", 
                               sheet = "final_results")

mudancainfo <- readr::read_csv("planilhas/FINAL_areas.csv")
avail_sdms <- info %>% 
  dplyr::filter(Tipo == "ADEQ") %>% 
  dplyr::arrange(species) %>% 
  dplyr::distinct() %>% 
  dplyr::pull(species)


species_list <- unique(sort(as.vector(info$species)))
countries_list <- sort(unique(unlist(strsplit(
  paste(unlist(info$countries), collapse = ", "), ", "
))))


# Define parametros da legenda

colors <- c(
  # "transparent", "navy", 
  # "transparent",
  "darkgreen",
  "chartreuse", "red", "palegoldenrod")

labels <- c(
  # titulo,
  # "Registros de Ocorrência",
  # "MPC",
  paste0("Vegetação Nativa (", ano_atual, ")"),
  paste0("Área Regenerada (", ano_passado, "-", ano_atual, ")"),
  paste0("Perda Recente (", ano_passado, "-", ano_atual, ")"),
  paste0("Perda Acumulada (até ", ano_passado, ")"))

sizes <- c(
  # 20, 5, 
  # 20, 
  20, 20, 20, 20)

shapes <- c(
  # "square",
  # "circle", "diamond", 
  "square",
  "square", "square", "square")

borders <- c(
  # "transparent",
  # "navy", "deeppink", 
  "darkgreen",
  "chartreuse", "red", "palegoldenrod")

addLegendCustom <- function(map, colors, labels, sizes, shapes, borders,
                            opacity = 0.9) {
  
  make_shapes <- function(colors, sizes, borders, shapes) {
    
    shapes <- gsub("circle", "50%", shapes)
    shapes <- gsub("square", "0%", shapes)
    shapes <- gsub("diamond", "0%", shapes)
    paste0(colors, "; width:", sizes, "px; height:", sizes, 
           "px; border:3px solid ", borders, "; border-radius:", shapes)
    
  }
  
  make_labels <- function(sizes, labels) {
    
    paste0("<div style='display: inline-block;height: ", sizes, 
           "px;margin-top: 4px;line-height: ", sizes, "px;'>", labels, "</div>"
    )
  }
  
  legend_colors <- make_shapes(colors,
                               sizes,
                               borders,
                               shapes)
  
  legend_labels <- make_labels(sizes,
                               labels)
  return(leaflet::addLegend(map,
                            colors = legend_colors,
                            labels = legend_labels,
                            opacity = opacity
  ))
}




# começa app --------------------------------------------------------------

# UI
ui <- navbarPage(
  "Oficina de Avaliação do Estado de Conservação das Serpentes Brasileiras - 2022 ", 
  id = "nav",
  
  ###################################
  ######### Interactive Map #########
  ###################################
  
  tabPanel("Mapa",
           tags$head(includeHTML(("google-analytics.html"))),
           div(class="outer",
               # Include custom styles and javascript
               tags$head(includeCSS("styles.css"), includeScript("gomap.js")),
               # If not using custom CSS, set height of leafletOutput to a number instead of percent
               leafletOutput("map", width = "100%", height = "100%"),
               
               absolutePanel(id = "controls", class="panel panel-default", 
                             fixed = TRUE, draggable = TRUE, top = 60, 
                             left = 20, right = "auto", bottom = 50,
                             width = 400, style = "overflow-y: scroll;",
                             
                             br(),
                             
                             img(src = "logo_horizontal.png", 
                                 # height = 300,
                                 width = 300, style = "display: block; 
                                  margin-left: auto; margin-right: auto"),
                             
                             br(),
                             
                             helpText("Melhor visualizado no computador!"),
                             helpText("Se estiver no celular, gire o aparelho para o modo paisagem."),
                             
                             # h5(HTML("<center><br>Melhor visualizado no computador!</center>")),
                             # h5(HTML("<center>Se estiver no celular, gire o aparelho para o modo paisagem.</center>")),
                             # 
                             # tags$div(HTML("<br><center><p><a target=\"_blank\" href=\"https://github.com/RhettRautsaw/VenomMaps\"><img src=\"https://img.shields.io/badge/User%20Guide-GitHub-blue\"></a></p></center>")),
                             # tags$div(HTML("<center><p><a target=\"_blank\" href=\"https://doi.org/10.1038/s41597-022-01323-4\"><img src=\"https://img.shields.io/badge/Citation-Scientific%20Data-blue\"></a></p></center>")),
                             # tags$div(HTML("<center><p><a target=\"_blank\" href=\"https://doi.org/10.5281/zenodo.5637094\"><img src=\"https://img.shields.io/badge/Archive-10.5281/zenodo.5637094-blue\"></a></p></center>")),
                             # tags$div(HTML("<center><p><a target=\"_blank\" href=\"https://creativecommons.org/licenses/by/4.0/\"><img src=\"https://img.shields.io/badge/License-CC%20BY-blue\"></a></p></center>")),
                             
                             selectizeInput(inputId = "species", 
                                            label = h4("Espécies:"), 
                                            choices = species_list, 
                                            multiple = FALSE # TÁ DANDO ERRO COM TRUE... CORRIGIR
                                            # multiple = TRUE
                             ),
                             
                             
                             checkboxInput("pontos", "Pontos de ocorrência", FALSE),
                             checkboxInput("eoo", "Extensão de ocorrência", FALSE),
                             checkboxInput("mudanca", "Mudança de hábitat", FALSE),
                             checkboxInput("ucs", "Unidades de conservação", FALSE),
                             # colocar o raster do mapbiomas com o habitat da especie
                             # checkboxInput("habitat", "Hábitat da espécie", FALSE),
                             
                             actionButton("update", "Atualizar"),
                             
                             br(),
                             
                             tableOutput('mudancatab'),
                             # DT::dataTableOutput('mudancatab'),
                             
                             br(),
                             br(),
                             br(),
                             
                             h5(strong("Potenciais ameaças:")),
                             # checkboxInput("estradas", "Estradas", FALSE),
                             checkboxInput("irrigacao", "Agricultura irrigada (ano base: 2020)", FALSE),
                             # checkboxInput("pastagem", "Pastagem (ano base: 2020 - Projeto MapBiomas, 2021)", FALSE),
                             checkboxInput("eolicos", "Parques eólicos (ANEEL, 2021)", FALSE),
                             checkboxInput("ahe", "Aproveitamentos hidrelétricos (ANEEL, 2022)", FALSE),
                             # checkboxInput("reservatorios", "Reservatórios (ANA, 2019)", FALSE),
                             # checkboxInput("mineracao", "Mineração (ano base: 2021)", FALSE),
                             # checkboxInput("fogo", "Fogo", FALSE),
                             
                             # actionButton("update", "Atualizar"),
                             
                             br(),
                             strong(
                               textOutput("irrigacao")
                             ),
                             
                             # br(),
                             strong(
                               textOutput("eolicos")
                             ),
                             
                             strong(
                               textOutput("reservatorios")
                             ),
                             
                             strong(
                               textOutput("ahe")
                             ),
                             
                             # br(),
                             strong(
                               textOutput("mineracao")
                             ),
                             
                             # br(),
                             strong(
                               textOutput("fogo")
                             )
                             
                             
                             
                             
                             
               )
           )
  ),
  
  ###################################
  ### Dados do modelo, se houver ####
  ###################################
  
  # tabPanel("Dados de modelo de distribuição",
  # ),
  
  ###################################
  ############ Créditos #############
  ###################################
  
  tabPanel("Créditos",
           p("Bruna"),
           p("Luciana"),
           p("Alejandro"),
           p("Carlos"),
           p("Lara"),
           
           br(),
           br(),
           br(),
           br(),
           
           # colocar em outra coluna? como?
           
           h4(strong("Referências:")),
           p("ANA. ",
             a("Base de Dados Nacional de Referência de Massas d'Água.",
               href="https://metadados.snirh.gov.br/geonetwork/srv/por/catalog.search#/metadata/7d054e5a-8cc9-403c-9f1a-085fd933610c",
               target="_blank", rel="noopener noreferrer"),
             "Acesso em: 2 jul. 2021."),
           p("ICMBIO. ",
             a("Limites das Unidades de Conservação Federais - shapefile. ",
               href="https://www.gov.br/icmbio/pt-br/servicos/geoprocessamento/mapa-tematico-e-dados-geoestatisticos-das-unidades-de-conservacao-federais",
               target="_blank", rel="noopener noreferrer"),
             "Acesso em: 4 maio. 2022. "),
           p("MMA. ",
             a("Cadastro Nacional de Unidades de Conservação - Unidades de Conservação do Brasil.",
               href="http://mapas.mma.gov.br/geonetwork/srv/br/metadata.show?id=1250",
               target="_blank", rel="noopener noreferrer"),
             "Acesso em: 4 maio. 2022."),
           p("PROJETO MAPBIOMAS. ",
             a("Coleção 6 da série anual de mapas de cobertura e uso de solo do Brasil.",
               href = "https://mapbiomas.org",
               target="_blank", rel="noopener noreferrer"),
             "Acesso em: 30 ago. 2021."),
           p("PROJETO MAPBIOMAS. ",
             a("Coleção 7 da série anual de mapas de cobertura e uso de solo do Brasil.",
               href="https://mapbiomas.org",
               target="_blank", rel="noopener noreferrer"),
             "Acesso em: 28 ago. 2022.")
  ),
  
  
  
  ###################################
  ######### Phylogeography ##########
  ###################################
  
  # tabPanel("Phylogeography")
  
  ###################################
  ############# Venom ###############
  ###################################
  
  # tabPanel("Venom")
  
)

server <- function(input, output, session) {
  
  ###################################
  ######### Interactive Map #########
  ###################################
  
  output$map <- renderLeaflet({
    
    leaflet() %>% 
      addMapPane("background", zIndex = 0) %>%        # Level 1: bottom
      addMapPane("polygons", zIndex = 1) %>%          # Level 2: middle
      addMapPane("rasters", zIndex = 100000) %>%      # Level 3: middle
      addMapPane("points", zIndex = 440) %>%          # Level 4: middle
      addMapPane("labels", zIndex = 450) %>%          # Level 5: top
      
      addWMSTiles('http://ows.mundialis.de/services/service?', layers='TOPO-WMS', group="Topography", options = pathOptions(pane = "background")) %>%
      addProviderTiles(providers$Esri.WorldStreetMap, group="Open Street Map", options = pathOptions(pane = "background")) %>%
      addProviderTiles(providers$Esri.WorldTerrain, group="Terrain", options = pathOptions(pane = "background")) %>%
      addProviderTiles(providers$Esri.WorldImagery, group="Satellite", options = pathOptions(pane = "background")) %>%
      
      addProviderTiles(providers$Stamen.TonerLines, group="Boundaries", options = pathOptions(pane = "labels")) %>%
      addProviderTiles(providers$Stamen.TonerLabels, group="Labels", options = pathOptions(pane = "labels")) %>%
      
      addLayersControl(
        baseGroups = c("Satellite","Terrain",  "Topography", "Open Street Map"), 
        overlayGroups = c("Mudanças", "Boundaries", "Labels"), 
        options = layersControlOptions(collapsed = F, position = "bottomright")
      ) %>%
      
      hideGroup("Labels") %>%
      addScaleBar(position = c("bottomleft"), options = scaleBarOptions()) 
    # addEasyprint(options = easyprintOptions(exportOnly = T, sizeModes = list("A4Landscape")))
  })
  
  # Update species list by country selection
  new_species_list <- reactive({
    sp_list  <-  info %>%
      dplyr::select(species) %>%
      arrange(species) %>%
      distinct()
    sp_list %>%
      pull(species)
    
  })
  
  observe({
    updateSelectizeInput(
      session,
      "species",
      choices = new_species_list(),
      selected = head(new_species_list(), 1)
    )
  })
  
  # Filter distributions
  distribution <- reactive({
    if (is.null(input$species)) {
      as(combined_distribution, "Spatial")
    } else{
      as(combined_distribution %>%
           filter(species %in% input$species),
         "Spatial")
    }
  })
  
  
  
  # Update map with distribution/points
  observeEvent(
    input$update,
    {
      show_modal_spinner()
      distribution <- distribution()
      bbox <- st_bbox(as(distribution, "sf")) %>%
        as.vector()
      
      
      leafletProxy("map", data = distribution) %>%
        clearGroup("distribution") %>%
        clearGroup("occpoints") %>%
        clearGroup("ucs") %>%
        clearGroup("atividades") %>%
        clearHeatmap() %>%
        clearMarkerClusters() %>%
        removeControl("distribution") %>%
        clearImages() %>%
        clearControls() %>% 
        
        
        fitBounds(bbox[1], bbox[2], bbox[3], bbox[4])
      
      
      
      if(input$pontos){
        # Filter points
        pointsData <- occ %>% 
          filter(species %in% input$species)
        
        if(nrow(pointsData)!=0) {
          
          leafletProxy("map", data = distribution) %>%
            addCircleMarkers(data = pointsData, group = "occpoints", 
                             lng =  ~ as.numeric(lon), 
                             lat =  ~ as.numeric(lat), 
                             radius = 4,
                             fill = TRUE, 
                             # fillColor = ~ point_pal(flag), 
                             fillOpacity = 1, weight = 1, 
                             stroke = TRUE, color = "black", opacity = 1, 
                             options = pathOptions(pane = "points"),
                             popup = paste(
                               "<em>",
                               input$species,
                               "</em>"
                               # "</br> Longitude:",
                               # pointsData$lon,
                               # "</br> Latitude:",
                               # pointsData$lat
                             )
                             #clusterOptions = markerClusterOptions(),
                             # popup = lapply(labs, htmltools::HTML)) #%>%
            )
        }
      }
      
      if(input$eoo){
        # Filter points
        mpc <- combined_distribution %>% 
          filter(species %in% input$species)
        # mutate(labs = paste0(
        #     ID, '<p></p>', 
        #     "recorded: ", taxonomy_updated_species, '</p><p>',
        #     "updated: ", final_species, '</p><p>',
        #     "accuracy: ", accuracy_m, ' m</p><p>',
        #     "flags: ", flag_detailed,
        #     '</p>'
        #   )
        # )
        # labs<-pointsData$labs
        if(nrow(mpc)!=0) {
          
          leafletProxy("map", data = distribution) %>%
            addPolygons(
              data = mpc,
              group = "occpoints",
              color = "black",
              fill = F,
              weight = 4,
              # dashArray= c(5,5),
              smoothFactor = 1,
              opacity = 1,
              popup = paste(
                "<em>",
                input$species,
                "</em>",
                "</br> Área (km²):",
                format(round(mpc$area_km2, digits = 2), decimal.mark = ",")
              )
            ) 
          # addCircleMarkers(data = pointsData, group = "occpoints", 
          #                  lng =  ~ as.numeric(lon), 
          #                  lat =  ~ as.numeric(lat), 
          #                  radius = 4,
          #                  fill = TRUE, 
          #                  # fillColor = ~ point_pal(flag), 
          #                  fillOpacity = 1, weight = 1, 
          #                  stroke = TRUE, color = "black", opacity = 1, 
          #                  options = pathOptions(pane = "points"),
          #                  #clusterOptions = markerClusterOptions(),
          #                  # popup = lapply(labs, htmltools::HTML)) #%>%
          # )
          
          
        }
      }
      
      if (input$mudanca) {
        
        if (input$species != "Bothrops germanoi") {
          
          # Define legenda
          
          # titulo <- paste("<strong><em>", input$species, "</em></strong>")
          
          mudanca <- raster::raster(paste0(
            "serpentes/uso_solo/", input$species, "_uso_wgs84.tif")
          ) 
          # mudanca <- raster(paste0(
          #   "serpentes/uso_solo/", species_list[1], "_uso_wgs84.tif")
          #   ) 
          
          
          if(nrow(mudanca)!=0) {
            leafletProxy("map", data = distribution) %>%
              addRasterImage(
                mudanca,
                colors = c(
                  # "transparent",
                  "palegoldenrod",
                  "red",
                  "green",
                  "palegreen4"),
                opacity = 1,
                project = T,
                method = "ngb",
                group = "Mudanças",
                # maxBytes = 1350000
                maxBytes = 2000000000
              ) %>%  
              # addLayersControl(
              #   # (baseGroups = c("Brasil", "Imagem de Satélite")),
              #   overlayGroups = c("Remanescente"
              #                     # "Unidades de Conservação"
              #   )) %>%
              
              
              addLegendCustom(colors, labels, sizes, shapes, borders)
            
            
            
            x <- mudancainfo %>%
              dplyr::filter(Especie %in% input$species) %>%
              dplyr::distinct()
            # x <- mudancainfo %>%
            #   filter(Especie %in% species_list[1]) %>%
            #   distinct()
            
            x$EOO_mpc <- as.numeric(x$EOO_mpc)
            
            # Coluna 1: Titulo
            
            a <- c(
              "Extensão de Ocorrência (EOO)",
              "Área de Ocupação (AOO)",
              paste0("Vegetação Nativa (", ano_atual, ")"),
              "",
              paste0("Perda Acumulada (até ", ano_atual, ")"),
              "",
              paste0("Área Regenerada (", ano_passado, "-", ano_atual, ")"),
              "",
              paste0("Perda Recente (", ano_passado, "-", ano_atual, ")"),
              "",
              paste0("Saldo de Vegetação (", ano_passado, "-", ano_atual, ")"),
              ""
            )
            
            
            # Coluna 2: Areas 
            
            mpc_t <- c(
              round(x$EOO_mpc, 1),
              round(x$AOO_mpc, 1),
              round(x$veg_nat_km2, 1),
              round(x$veg_nat_prop, 1),
              round(x$perda_acum_km2, 1),
              round(x$perda_acum_prop, 1),
              round(x$regener_km2, 1),
              round(x$regener_prop, 1),
              round(x$perda_rec_km2, 1),
              round(x$perda_rec_prop, 1),
              round(x$saldo_km2, 1),
              round(x$saldo_prop, 1)
            )
            
            mpc_t2 <- format(mpc_t, decimal.mark = ",") ## pra ficar em portugues na figura
            
            # Coluna 3: Unidade de medida
            
            colors <- c(
              rep("", 12)
            )
            
            c <- c("km²",
                   "km²",
                   "km²",
                   "%",
                   "km²",
                   "%",
                   "km²",
                   "%",
                   "km²",
                   "%",
                   "km²",
                   "%")
            
            tabela <- data.frame(
              # colors,
              a,
              mpc_t2,
              c)
            
            colnames(tabela) <- c(
              # "", 
              "", "", ""
            )
            
            
            
            
            output$mudancatab <- renderTable(
              tabela
            )
            
            
            
            
            # DT::renderDataTable(
            # # x %>% 
            # #   tidyr::pivot_longer(
            # #     cols = EOO_mpc:saldo_prop
            # #   ) %>%
            # #   dplyr::select(name,value),
            # # 
            # # colnames = FALSE,
            # tabela
            # 
            
            # )
            
            
            
          } 
          
          
        } else {
          output$mudancatab <- renderText({
            
            "</br>Não há dados de cobertura do solo disponíveis para essa espécie."
            
          })
        }
      } 
      
      if (input$ucs) {
        
        ucs2 <- st_filter(ucs, combined_distribution_buf %>% 
                            filter(species == input$species))
        
        
        leafletProxy("map", data = distribution) %>%
          addPolygons(
            # color = c("#00EE00"),
            color = c("#FFFFFF"),
            fill = TRUE,
            weight = 1,
            smoothFactor = 1,
            data = ucs2,
            # color = "red",
            # fill = F,
            # weight = 4,
            # dashArray = c(5, 5),
            # smoothFactor = 1,
            opacity = 1,
            group = "ucs",
            # popup = ~ paste0(nome)
            popup = paste(
              "<strong>",
              ucs2$nome,
              "</strong>",
              "</br>Administração: ",
              ucs2$administra,
              "</br>Grupo: ",
              ucs2$SiglaGrupo,
              "</br>Fonte: ",
              ucs2$fonte
              
            ),
            label = paste(ucs2$nome),
            labelOptions = labelOptions(noHide = F,
                                        style = list(
                                          "font-style" = "bold"
                                        ))
          )
        
        
      }
      
      
      if (input$eolicos) {
        
        peolicos2 <- st_filter(peolicos, combined_distribution_buf %>% 
                                 filter(species == input$species))
        # peolicos2 <- st_filter(peolicos, combined_distribution_buf %>% 
        #                          filter(species == species_list[3]))
        
        if (nrow(peolicos2) != 0) {
          
          leafletProxy("map", data = distribution) %>%
            addPolygons(
              # color = c("#00EE00"),
              # fill = FALSE,
              weight = 1,
              smoothFactor = 1,
              data = peolicos2,
              color = "red",
              fill = T,
              # color = "red",
              # fill = F,
              # weight = 4,
              # dashArray = c(5, 5),
              # smoothFactor = 1,
              opacity = 1,
              group = "atividades",
              # popup = ~ paste0(nome)
              popup = paste(
                "<strong>",
                peolicos2$nome_eol,
                "</strong>",
                "</br>Potência (MW): ",
                peolicos2$pot_mw,
                "</br>Fase: ",
                peolicos2$fase,
                "</br>Atualização: ",
                peolicos2$data_atual
                
              ))
        } else {
          
          output$eolicos <- renderText({
            
            "Não há informações sobre parques eólicos aqui."
            
          })
          
        }  
        
      }
      
      
      # começa reservatorios ----------------------------------------------------

      # if (input$reservatorios) {
      #   
      #   reservatorios2 <- st_filter(reservatorios, combined_distribution_buf %>% 
      #                                 filter(species == input$species))
      #   
      #   
      #   
      #   # reservatorios2 <- st_filter(reservatorios, combined_distribution_buf %>%
      #   #                          filter(species == species_list[3]))
      #   
      #   if (nrow(reservatorios2) != 0) {
      #     
      #     leafletProxy("map", data = distribution) %>%
      #       
      #       # leaflet() %>% 
      #       
      #       
      #       addPolygons(
      #         # color = c("#00EE00"),
      #         # fill = FALSE,
      #         weight = 1,
      #         smoothFactor = 2,
      #         data = reservatorios2,
      #         color = c("#009ACD"),
      #         fill = T,
      #         # color = "red",
      #         # fill = F,
      #         # weight = 4,
      #         # dashArray = c(5, 5),
      #         
      #         opacity = 1,
      #         group = "atividades",
      #         # popup = ~ paste0(nome)
      #         popup = paste(
      #           "<strong>",
      #           reservatorios2$nmoriginal,
      #           "</strong>",
      #           "</br>Uso principal: ",
      #           reservatorios2$usoprinc,
      #           "</br>Domínio: ",
      #           reservatorios2$dedominio,
      #           "</br>Fiscalização: ",
      #           reservatorios2$defiscaliz
      #           
      #         ))
      #   } else {
      #     
      #     output$reservatorios <- renderText({
      #       
      #       "Não há informações sobre reservatorios aqui."
      #       
      #     })
      #     
      #   }  
      #   
      # }
      # termina reservatorios
      
      
      if (input$ahe) {
        
        ahe2 <- st_filter(ahe, combined_distribution_buf %>% 
                            filter(species == input$species))
        
        
        # 
        # ahe2 <- st_filter(ahe, combined_distribution_buf %>%
        #                     filter(species == species_list[7]))
        
        if (nrow(ahe2) != 0) {
          
          leafletProxy("map", data = distribution) %>%
            
            # leaflet() %>%
            addCircleMarkers(data = ahe2, 
                             group = "atividades", 
                             lng =  ~ as.numeric(LONG_EIXO1), 
                             lat =  ~ as.numeric(LAT_EIXO_D), 
                             radius = 4,
                             fill = TRUE, 
                             # fillColor = ~ point_pal(flag), 
                             fillOpacity = 1, weight = 1, 
                             stroke = TRUE, color = "blue", opacity = 1, 
                             options = pathOptions(pane = "points"),
                             popup = paste(
                               "<strong>",
                               ahe2$NOME,
                               "</strong>",
                               "</br>Tipo de AHE:",
                               ahe2$TIPO_AHE,
                               "</br> Fase:",
                               ahe2$FASE,
                               "</br> Última atualização:",
                               ahe2$DATA_ATUAL
                             )
                             #clusterOptions = markerClusterOptions(),
                             # popup = lapply(labs, htmltools::HTML)) #%>%
            ) 
          
          
        } else {
          
          output$ahe <- renderText({
            
            "Não há informações sobre aproveitamentos hidrelétricos aqui."
            
          })
          
        }  
        
      }
      
      
      # começa estradas ---------------------------------------------------------
      
      # if (input$estradas) {
      #   
      #   estradas2 <- sf::st_filter(estradas, combined_distribution_buf %>% 
      #                                dplyr::filter(species == input$species))
      #   
      #   
      #   leafletProxy("map", data = distribution) %>%
      #     addPolylines(
      #       data = estradas2,
      #       color = c("#B22222"),
      #       opacity = 0.7,
      #       weight = 2,
      #       group = "atividades",
      #       popup = paste(
      #         "<strong>",
      #         estradas2$tipovia,
      #         "</strong>",
      #         "</br>Sigla: ",
      #         estradas2$sigla,
      #         "</br>Jurisdição: ",
      #         estradas2$jurisdicao,
      #         "</br>Administracao: ",
      #         estradas2$administra,
      #         "</br>Situação: ",
      #         estradas2$situacaofi,
      #         "</br>Pavimento: ",
      #         estradas2$tipopavime,
      #         "</br>Limite de velocidade: ",
      #         estradas2$limitevelo
      #         
      #       ))
      #   
      #   
      # }
      # termina estradas
      
      
      # começa irrigacao --------------------------------------------------------

      
      # if (input$irrigacao) {
      #   
      #   irrigacao2 <- sf::st_filter(irrigacao, combined_distribution_buf %>% 
      #                                 dplyr::filter(species == input$species))
      #   # irrigacao2 <- sf::st_filter(irrigacao, combined_distribution_buf %>% 
      #   #                              dplyr::filter(species == species_list[1]))
      #   
      #   if (nrow(irrigacao2) != 0) {
      #     
      #     
      #     
      #     leafletProxy("map", data = distribution) %>%
      #       addPolygons(
      #         # color = c("#00EE00"),
      #         color = c("#f505e8", "#3d0841", "#8b1d55"),
      #         fill = TRUE,
      #         weight = 1,
      #         smoothFactor = 1,
      #         data = irrigacao2,
      #         # color = "red",
      #         # fill = F,
      #         # weight = 4,
      #         # dashArray = c(5, 5),
      #         # smoothFactor = 1,
      #         opacity = 1,
      #         group = "atividades",
      #         # popup = ~ paste0(nome)
      #         # popup = paste(
      #         #   irrigacao2$classe
      #         #   
      #         # ),
      #         label = paste(irrigacao2$classe),
      #         labelOptions = labelOptions(noHide = F,
      #                                     style = list(
      #                                       "font-style" = "bold"
      #                                     ))
      #       )
      #     
      #     
      #   } else {
      #     # colocar texto aqui
      #     
      #     output$irrigacao <- renderText({
      #       
      #       "Não há informações sobre agricultura irrigada aqui."
      #       
      #     })
      #     
      #     
      #   }
      #   
      #   
      #   
      #   
      #   
      #   
      #   
      #   
      #   
      # }
      
      # termina irrigação
      
      
      # começa mineracao --------------------------------------------------------
      
      
      # if (input$mineracao) {
      #   
      #   mineracao2 <- sf::st_filter(mineracao, combined_distribution_buf %>% 
      #                                 dplyr::filter(species == input$species))
      #   # mineracao2 <- sf::st_filter(mineracao, combined_distribution_buf %>% 
      #   #                              dplyr::filter(species == species_list[1]))
      #   
      #   if (nrow(mineracao2) != 0) {
      #     
      #     
      #     
      #     leafletProxy("map", data = distribution) %>%
      #       addPolygons(
      #         # color = c("#00EE00"),
      #         color = c("#2803fc"),
      #         fill = TRUE,
      #         weight = 1,
      #         smoothFactor = 1,
      #         data = mineracao2,
      #         # color = "red",
      #         # fill = F,
      #         # weight = 4,
      #         # dashArray = c(5, 5),
      #         # smoothFactor = 1,
      #         opacity = 1,
      #         group = "atividades",
      #         # popup = ~ paste0(nome)
      #         # popup = paste(
      #         #   irrigacao2$classe
      #         #   
      #         # ),
      #         label = paste(mineracao2$GRIDCODE),
      #         labelOptions = labelOptions(noHide = F,
      #                                     style = list(
      #                                       "font-style" = "bold"
      #                                     ))
      #       )
      #     
      #     
      #   } else {
      #     # colocar texto aqui
      #     
      #     output$mineracao <- renderText({
      #       
      #       "Não há informações sobre mineração aqui."
      #       
      #     })
      #     
      #     
      #   }
      #   
      #   
      #   
      #   
      #   
      #   
      #   
      #   
      #   
      # }
      
      # termina mieracao
      
      # if(length(niche())!=0){
      #   if(input$nodist){
      #     leafletProxy("map") %>%
      #       clearGroup("distribution") %>% 
      #       removeControl("distribution") %>%
      #       addRasterImage2(niche(), colors = rev(pal), options = tileOptions(pane = "rasters"),
      #                       maxBytes = 800*1024*1024, project = T)
      #   }else{
      #     leafletProxy("map") %>%
      #       addRasterImage2(niche(), colors = rev(pal), options = tileOptions(pane = "rasters"),
      #                       maxBytes = 800*1024*1024, project = T)
      #   }
      # }
      remove_modal_spinner()
    })
  # 
  # ###################################
  # ####### General Information #######
  # ###################################
  # 
  # # output$infoPlot <- renderPlot({
  # #   if(is.null(input$species)){
  # #     tmpInfo<-info[1,]
  # #   }else{
  # #     tmpInfo<-info %>% filter(grepl(paste(input$species,collapse="|"), species))
  # #   }
  # #   
  # #   tmp<-infoGenerator(tmpInfo, info)
  # #   
  # #   wrap_plots(tmp, ncol=ifelse(length(tmp)<3,length(tmp),3))
  # # })
  # 
  # ###################################
  # ######### Phylogeography ##########
  
}

# Run the application 
shinyApp(ui = ui, server = server)

library(shiny)
library(bslib)
library(dplyr)
library(tidyr)
library(leaflet)
library(geobr)
library(sf)
library(osrm)

# Carregar os dados que estão na mesma pasta
dados <- readRDS("dados_app.rds")
todos_municipios <- dados$todos_municipios
df_final <- dados$df_final
cr_mun_filtrado <- dados$cr_mun_filtrado
malha_contornos <- dados$malha_contornos
malha_estados <- dados$malha_estados

# "Extrai" os objetos para o ambiente do R
todos_municipios <- dados$todos_municipios
df_final <- dados$df_final
cr_mun_filtrado <- dados$cr_mun_filtrado
malha_contornos <- dados$malha_contornos
malha_estados <- dados$malha_estados

# --- VERIFICAÇÃO AUTOMÁTICA DE DADOS ---
# Se os objetos não existirem, o script tenta carregar ou baixar
if (!exists("malha_estados")) {
  message("Carregando malha de estados (2020)...")
  malha_estados <- geobr::read_state(year = 2020, showProgress = FALSE) %>% st_transform(4326)
}

# Verifique se o seu objeto de municípios já está carregado, 
# se não, você pode baixá-lo aqui também:
if (!exists("malha_contornos")) {
  message("Aviso: Objeto 'malha_contornos' não encontrado no ambiente.")
}

# --- UI ---
ui <- page_sidebar(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  title = "Painel de Proximidade: Municípios e CRs",
  sidebar = sidebar(
    title = "Configurações",
    selectizeInput(
      "selected_mun", 
      "Selecione um Município:",
      choices = NULL, 
      options = list(placeholder = 'Busque por nome do município...')
    ),
    sliderInput("limiar", "Limiar de Proximidade Relativa (%)", 0, 100, 20),
    hr(),
    uiOutput("seletor_rota_cr"),
    helpText("A rota exibida considera o trajeto real por estradas entre os centróides.")
  ),
  
  layout_column_wrap(
    width = 1/2,
    card(
      card_header("Localização Geográfica"),
      leafletOutput("mapa_cr", height = "550px")
    ),
    layout_column_wrap(
      width = 1,
      card(
        card_header("Coordenações Regionais Próximas"),
        tableOutput("tabela_proximidade")
      ),
      card(
        card_header("Resumo da Rota Selecionada"),
        uiOutput("info_rota")
      )
    )
  )
)

# --- SERVER ---
server <- function(input, output, session) {
  
  # 1. Lista de municípios
  updateSelectizeInput(session, "selected_mun", 
                       choices = setNames(todos_municipios$municipio_id, 
                                          paste0(todos_municipios$nome_municipio, " - ", todos_municipios$uf_sigla)),
                       server = TRUE)
  
  # 2. Reativo de dados
  dados_municipio <- reactive({
    req(input$selected_mun)
    linha_dist <- df_final %>% 
      filter(as.character(code_muni) == as.character(input$selected_mun))
    
    df_long <- linha_dist %>%
      pivot_longer(cols = starts_with("Coordena"), 
                   names_to = "cr_nome", 
                   values_to = "distancia_km") %>%
      arrange(distancia_km) %>%
      mutate(
        dist_min = min(distancia_km, na.rm = TRUE),
        diff_relativa = (distancia_km - dist_min) / dist_min * 100
      )
    
    df_long %>% filter(diff_relativa <= input$limiar)
  })
  
  # 3. Seletor de CR (UI Dinâmica)
  output$seletor_rota_cr <- renderUI({
    res <- dados_municipio()
    if (is.null(res) || nrow(res) == 0) return(helpText("Nenhuma CR disponível."))
    
    selectInput("cr_para_rota", "Traçar Rota Terrestre para:", 
                choices = c("Selecione uma CR..." = "", sort(unique(res$cr_nome))))
  })
  
  # 4. Cálculo da Rota
  rota_reativa <- reactive({
    req(input$cr_para_rota, input$cr_para_rota != "")
    mun_info <- todos_municipios %>% filter(as.character(municipio_id) == as.character(input$selected_mun)) %>% slice(1)
    cr_info  <- cr_mun_filtrado %>% filter(nome == input$cr_para_rota) %>% slice(1)
    
    ponto_o <- c(mun_info$longitude, mun_info$latitude)
    ponto_d <- c(cr_info$longitude, cr_info$latitude)
    
    tryCatch({ osrmRoute(src = ponto_o, dst = ponto_d, overview = "full") }, 
             error = function(e) { return(NULL) })
  })
  
  # 5. MAPA BASE (Com Relevo e Estados)
  output$mapa_cr <- renderLeaflet({
    leaflet() %>%
      # Opções de Fundo (Base Groups)
      addTiles(group = "Mapa Padrão (OSM)") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satélite") %>%
      addProviderTiles(providers$OpenTopoMap, group = "Relevo") %>% # Camada de Relevo de volta
      
      # Camadas de Polígonos (Overlay Groups)
      # 1. Estados (Camada limpa e mais grossa)
      addPolygons(data = malha_estados, color = "#444444", weight = 2, 
                  fill = FALSE, group = "Limites Estaduais") %>%
      
      # 2. Municípios (Mais fina)
      addPolygons(data = malha_contornos, color = "black", weight = 0.5, 
                  fill = FALSE, group = "Limites Municipais", label = ~name_muni) %>%
      
      # 3. Pontos das CRs
      addCircleMarkers(data = cr_mun_filtrado, color = "blue", radius = 3, 
                       label = ~nome, group = "Todas as CRs") %>%
      
      # Controle de Camadas
      addLayersControl(
        baseGroups = c("Mapa Padrão (OSM)", "Satélite", "Relevo"),
        overlayGroups = c("Limites Estaduais", "Limites Municipais", "Todas as CRs"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      hideGroup("Limites Municipais") # Começa oculto para performance
  })
  
  # 6. Proxy para Município Selecionado
  observe({
    req(input$selected_mun)
    mun_shape <- read_municipality(code_muni = as.numeric(input$selected_mun), 
                                   year = 2022, showProgress = FALSE) %>% st_transform(4326)
    
    proxy <- leafletProxy("mapa_cr")
    proxy %>% clearGroup("selecao_ativa") %>%
      addPolygons(data = mun_shape, color = "red", weight = 3, fillOpacity = 0.2, group = "selecao_ativa")
    
    if(is.null(input$cr_para_rota) || input$cr_para_rota == "") {
      bbox <- st_bbox(mun_shape)
      proxy %>% fitBounds(bbox[["xmin"]], bbox[["ymin"]], bbox[["xmax"]], bbox[["ymax"]])
    }
  })
  
  # 7. Proxy para Rota
  observe({
    route <- rota_reativa()
    req(!is.null(route))
    leafletProxy("mapa_cr") %>%
      clearGroup("rota_ativa") %>%
      addPolylines(data = route, color = "#1b4d3e", weight = 6, opacity = 0.8, group = "rota_ativa") %>%
      fitBounds(st_bbox(route)[["xmin"]], st_bbox(route)[["ymin"]], st_bbox(route)[["xmax"]], st_bbox(route)[["ymax"]])
  })
  
  # 8. Tabelas e Infos
  output$tabela_proximidade <- renderTable({
    dados_municipio() %>%
      select(Coordenação = cr_nome, `Linha Reta (km)` = distancia_km, `Diferença (%)` = diff_relativa) %>%
      mutate(across(where(is.numeric), ~round(.x, 1)))
  })
  
  output$info_rota <- renderUI({
    req(input$cr_para_rota)
    route <- rota_reativa()
    req(!is.null(route))
    card(card_body(
      p(icon("road"), strong(" Distância real: "), round(route$distance, 1), " km"),
      p(icon("clock"), strong(" Tempo estimado: "), round(route$duration / 60, 1), " horas")
    ))
  })
}

shinyApp(ui, server)

# 1. INSTALAÇÃO E CARREGAMENTO DOS PACOTES ----------------------------------
# install.packages(c("osrm", "sf", "leaflet", "tidyverse", "geobr"))

library(osrm)
library(sf)
library(leaflet)
library(tidyverse)
library(geobr)

# 2. PROCESSAMENTO DE DADOS DE ENTRADA --------------------------------------
cat("Processando dados de entrada...\n")

# --- A. Preparar as SEDES DAS CRs (Destinos) ---
# Garantir numéricos e criar objeto SF
cr_mun_final <- cr_mun_final %>%
  mutate(longitude = as.numeric(longitude),
         latitude = as.numeric(latitude)) %>%
  filter(!is.na(longitude) & !is.na(latitude))

sf_sedes_crs <- st_as_sf(cr_mun_final,
                         coords = c("longitude", "latitude"),
                         crs = 4326)

# Preparar matriz de coordenadas para o OSRM
coords_sedes <- st_coordinates(sf_sedes_crs)
rownames(coords_sedes) <- cr_mun_final$nome # Nome da CR para identificação

# --- B. Preparar os CENTROIDES DOS MUNICÍPIOS (Origens) ---
todos_municipios <- todos_municipios %>%
  mutate(
    longitude = as.numeric(longitude),
    latitude = as.numeric(latitude),
    # Mudança aqui: de as.character para as.numeric
    municipio_id = as.numeric(municipio_id) 
  ) %>% 
  filter(!is.na(longitude) & !is.na(latitude))

sf_centroides_muni <- st_as_sf(todos_municipios,
                               coords = c("longitude", "latitude"),
                               crs = 4326)

# Preparar matriz de coordenadas para o OSRM
coords_centroides <- st_coordinates(sf_centroides_muni)

# 3. CÁLCULO DA MATRIZ EM LOTES (Muni -> CR) --------------------------------
cat(paste0("Calculando matriz viária para ", nrow(coords_centroides), " municípios em lotes...\n"))

# Configurações do Lote (Pode demorar bastante para o Brasil inteiro)
# Se der erro 502 frequente, reduza tamanho_lote para 50 ou 30.
tamanho_lote <- 80 
n_muni <- nrow(coords_centroides)
n_lotes <- ceiling(n_muni / tamanho_lote)

resultados_lista <- list()

# Loop de processamento em lotes (necessário para >5000 municípios)
for (i in 1:n_lotes) {
  inicio <- ((i - 1) * tamanho_lote) + 1
  fim <- min(i * tamanho_lote, n_muni)
  
  cat(paste0("Processando lote ", i, " de ", n_lotes, " (Muni ", inicio, " a ", fim, ")...\n"))
  
  tentativa <- tryCatch({
    osrmTable(
      src = coords_centroides[inicio:fim, , drop = FALSE], # Lote de municípios
      dst = coords_sedes,                                  # Todas as CRs
      measure = "duration"
    )
  }, error = function(e) {
    cat(paste0("!! Erro no lote ", i, ": ", e$message, "\n"))
    return(NULL)
  })
  
  if (!is.null(tentativa)) {
    # Guardamos apenas a matriz de durações
    resultados_lista[[i]] <- tentativa$durations
  } else {
    # Se falhar o lote inteiro, preenchemos com NA para manter a ordem
    n_linhas_lote <- fim - inicio + 1
    matriz_na <- matrix(NA, nrow = n_linhas_lote, ncol = nrow(coords_sedes))
    resultados_lista[[i]] <- matriz_na
  }
  
  # Pausa obrigatória para não ser bloqueado pelo servidor demo
  Sys.sleep(0.7) 
}

# Unificar resultados na matriz final (Ordem respeita 'todos_municipios')
matriz_tempo_final <- do.call(rbind, resultados_lista)

# ==============================================================================
# 4. ASSOCIAÇÃO E DISSOLVE (VERSÃO PARA MAPAS COMPLEXOS)
# ==============================================================================
cat("Processando união das geometrias (isso pode levar um momento)...\n")

# 1. Classificação (Matriz -> Município)
id_cr_proxima <- apply(matriz_tempo_final, 1, function(x) {
  if(all(is.na(x))) return(NA)
  which.min(x)
})
todos_municipios$cr_proxima_nome <- rownames(coords_sedes)[id_cr_proxima]

# 2. Join e Limpeza
poligonos_classificados <- brasil_poligonos_sf %>%
  mutate(code_muni = as.numeric(code_muni)) %>% 
  left_join(todos_municipios %>% select(municipio_id, cr_proxima_nome),
            by = c("code_muni" = "municipio_id")) %>%
  filter(!is.na(cr_proxima_nome))

# --- O SEGREDO TÉCNICO ---
# Desativamos o motor S2 para evitar os erros de 'duplicate edge' e 'loop edge'
sf::sf_use_s2(FALSE) 

# 3. UNIÃO ROBUSTA
regioes_voronoi_sf <- poligonos_classificados %>%
  st_make_valid() %>%             # Limpa cada município individualmente
  st_buffer(0.0001) %>%           # Expande um milímetro para colar as peças
  group_by(cr_proxima_nome) %>%
  summarise(geometry = st_union(st_make_valid(geom)), .groups = "drop") %>%
  st_make_valid() %>%             # Limpa o resultado da união
  st_collection_extract("POLYGON")

# Reativamos o motor S2 para manter a precisão do restante do R
sf::sf_use_s2(TRUE) 

# Verificação de segurança no Console: Se aparecer 0, o problema está no Join
cat(paste0("Sucesso: Foram geradas ", nrow(regioes_voronoi_sf), " regiões.\n"))

# ==============================================================================
# 5. PREPARAÇÃO DA VISUALIZAÇÃO
# ==============================================================================
n_crs <- length(unique(regioes_voronoi_sf$cr_proxima_nome))
cores_expandidas <- colorRampPalette(RColorBrewer::brewer.pal(12, "Set3"))(n_crs)
pal_funai <- colorFactor(palette = cores_expandidas, domain = regioes_voronoi_sf$cr_proxima_nome)

# ==============================================================================
# 6. RENDERIZAÇÃO DO MAPA (LEAFLET)
# ==============================================================================
mapa_final <- leaflet() %>%
  addTiles() %>%
  addPolygons(
    data = regioes_voronoi_sf,
    fillColor = ~pal_funai(cr_proxima_nome),
    fillOpacity = 0.6,
    # Voltamos com o stroke bem fino para você ter certeza que a região existe
    stroke = TRUE, 
    color = "white",
    weight = 0.5, 
    smoothFactor = 0.5,
    label = ~paste0("Regional: ", cr_proxima_nome),
    highlightOptions = highlightOptions(weight = 3, color = "#666", bringToFront = TRUE)
  ) %>%
  addCircleMarkers(
    data = sf_sedes_crs, 
    color = "black", fillColor = "red", fillOpacity = 1,
    radius = 5, weight = 1, popup = ~nome
  )

# Exibir o mapa
mapa_final

# EXPORTAR PARA HTML
# install.packages("htmlwidgets")
#library(htmlwidgets)
#saveWidget(mapa_final, file = "Mapa_Influencia_CRs_FUNAI.html", selfcontained = TRUE)

#cat("Sucesso! O arquivo 'Mapa_Influencia_CRs_FUNAI.html' foi gerado na sua pasta de trabalho.")
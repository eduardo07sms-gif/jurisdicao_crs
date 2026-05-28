# ==============================================================================
# 1. PARÂMETROS E TRAVAS
# ==============================================================================
VEL_ROD  <- 60.0   
VEL_HID  <- 20.0   
VEL_WALK <- 4.5    

MAX_WALK_KM <- 30.0   
MAX_RATIO   <- 3.0    
CRS_METRICO <- 5880 

PATH_ROD   <- "C:/Users/eduardo.silva/Documents/norte-260420-free.shp/gis_osm_roads_free_1.shp"
PATH_HIDRO <- "C:/Users/eduardo.silva/Documents/BaseHidroHidrovias/fc_hidro_hidrovia_antaq.shp"
PATH_PORTOS <- "C:/Users/eduardo.silva/Documents/BaseHidroPortos/BaseHidroPortos/BaseHidroPortos.shp"

# ==============================================================================
# 2. CARREGAMENTO COM CACHE (AMAZÔNIA LEGAL 2022)
# ==============================================================================
library(igraph)
library(sf)
library(leaflet)
library(tidyverse)
library(geobr)
library(sfnetworks)
library(units)
library(readr)

cr_geocode <- st_read("cr_geocode 1.gpkg")
cr_geocode <- cr_geocode %>%
  filter(startsWith(sigla, "CR"))
message("🛰️ Carregando Setores Censitários 2022...")

if(!file.exists("setores_amazonia_2022.rds")) {
  estados_ama <- c("AC", "AM", "AP", "MA", "MT", "PA", "RO", "RR", "TO")
  setores_sf <- lapply(estados_ama, function(uf) read_census_tract(code_tract = uf, year = 2022, showProgress = FALSE)) %>%
    bind_rows() %>% st_transform(CRS_METRICO)
  saveRDS(setores_sf, "setores_amazonia_2022.rds")
} else {
  setores_sf <- readRDS("setores_amazonia_2022.rds")
}

sf_setores_pt <- setores_sf %>% st_centroid() %>% st_zm(drop = TRUE)
# Certifique-se de carregar o cr_geocode antes, ex: 
# cr_geocode <- st_read("caminho_do_arquivo_se_for_gpkg_ou_shp") 
# ou readRDS("caminho_se_for_rds")

sf_sedes <- cr_geocode %>% 
  st_transform(CRS_METRICO) %>% 
  st_zm(drop = TRUE)

# ==============================================================================
# 3. CARREGAMENTO DAS MALHAS
# ==============================================================================
rede_rod <- st_read(PATH_ROD, quiet = TRUE) %>%
  filter(fclass %in% c('motorway', 'trunk', 'primary', 'secondary')) %>%
  st_transform(CRS_METRICO) %>% as_sfnetwork(directed = FALSE)

portos_sf <- st_read(PATH_PORTOS, quiet = TRUE) %>% st_transform(CRS_METRICO)
rede_hid <- st_read(PATH_HIDRO, quiet = TRUE) %>% 
  filter(cla_icacao == "Navegável") %>% st_transform(CRS_METRICO) %>% as_sfnetwork(directed = FALSE)

# ==============================================================================
# 4. MATRIZES DE CUSTO (CÁLCULO VETORIZADO)
# ==============================================================================
message("🧮 Calculando matrizes massivas...")

# Snapping e Acesso (km)
dist_s_rod <- drop_units(st_distance(sf_setores_pt, st_as_sf(rede_rod, "edges")[st_nearest_feature(sf_setores_pt, st_as_sf(rede_rod, "edges")),], by_element = TRUE)) / 1000
dist_cr_rod <- drop_units(st_distance(sf_sedes, st_as_sf(rede_rod, "edges")[st_nearest_feature(sf_sedes, st_as_sf(rede_rod, "edges")),], by_element = TRUE)) / 1000

dist_s_hid <- drop_units(st_distance(sf_setores_pt, portos_sf[st_nearest_feature(sf_setores_pt, portos_sf),], by_element = TRUE)) / 1000
dist_cr_hid <- drop_units(st_distance(sf_sedes, portos_sf[st_nearest_feature(sf_sedes, portos_sf),], by_element = TRUE)) / 1000

# Redes e Reta (km)
mat_rod_km  <- drop_units(st_network_cost(rede_rod, from = sf_setores_pt, to = sf_sedes)) / 1000
mat_hid_km  <- drop_units(st_network_cost(rede_hid, from = portos_sf[st_nearest_feature(sf_setores_pt, portos_sf),], to = portos_sf[st_nearest_feature(sf_sedes, portos_sf),])) / 1000
mat_reta_km <- drop_units(st_distance(sf_setores_pt, sf_sedes)) / 1000

# ==============================================================================
# 5. LÓGICA DE DECISÃO (Otimizada com max.col)
# ==============================================================================
message("🏆 Alocando e aplicando travas...")

# 5.1 Tempos em Horas
T_ROD  <- sweep(mat_rod_km / VEL_ROD, 1, dist_s_rod/VEL_WALK, "+") %>% sweep(2, dist_cr_rod/VEL_WALK, "+")
T_HID  <- sweep(mat_hid_km / VEL_HID, 1, dist_s_hid/VEL_WALK, "+") %>% sweep(2, dist_cr_hid/VEL_WALK, "+")
T_WALK <- mat_reta_km / VEL_WALK

# 5.2 Aplicar Travas (Inf)
T_ROD[dist_s_rod > MAX_WALK_KM, ] <- Inf
T_HID[dist_s_hid > MAX_WALK_KM, ] <- Inf

D_ROD_TOTAL <- sweep(mat_rod_km, 1, dist_s_rod, "+") %>% sweep(2, dist_cr_rod, "+")
D_HID_TOTAL <- sweep(mat_hid_km, 1, dist_s_hid, "+") %>% sweep(2, dist_cr_hid, "+")

T_ROD[D_ROD_TOTAL > (mat_reta_km * MAX_RATIO)] <- Inf
T_HID[D_HID_TOTAL > (mat_reta_km * MAX_RATIO)] <- Inf

# 5.3 Decisão de Melhor CR por Modal
# Para cada setor, qual a melhor sede se eu for por Rodovia?
best_rod_idx <- max.col(-T_ROD, ties.method = "first")
best_rod_t   <- T_ROD[cbind(1:nrow(T_ROD), best_rod_idx)]

best_hid_idx <- max.col(-T_HID, ties.method = "first")
best_hid_t   <- T_HID[cbind(1:nrow(T_HID), best_hid_idx)]

best_walk_idx <- max.col(-T_WALK, ties.method = "first")
best_walk_t   <- T_WALK[cbind(1:nrow(T_WALK), best_walk_idx)]

# 5.4 Decisão Final do Modal Vencedor
df_final_tempos <- data.frame(Rodovia = best_rod_t, Hidrovia = best_hid_t, Linha_Reta = best_walk_t)
modal_venc_idx  <- max.col(-df_final_tempos, ties.method = "first")

# Mapeando resultados de volta para o SF
setores_sf <- setores_sf %>%
  mutate(
    modal   = names(df_final_tempos)[modal_venc_idx],
    horas   = df_final_tempos[cbind(1:nrow(df_final_tempos), modal_venc_idx)],
    cr_nome = case_when(
      modal == "Rodovia"    ~ sf_sedes$nome[best_rod_idx],
      modal == "Hidrovia"   ~ sf_sedes$nome[best_hid_idx],
      TRUE                  ~ sf_sedes$nome[best_walk_idx]
    )
  )

# ==========================================================
# FUNÇÃO DE LIMPEZA TOPOLÓGICA (COM TOLERÂNCIA DE DISTÂNCIA)
# ==========================================================
limpar_enclaves_funai <- function(df_setores, sf_sedes, col_cr = "cr_nome", tolerancia_metros = 200) {
  
  message("🔍 Iniciando limpeza de enclaves com tolerância de pontes/rios...")
  
  # 1. IDENTIFICAR COMPONENTES CONECTADOS
  message("🔗 Calculando contiguidade por bloco...")
  
  lista_setores <- df_setores %>% group_split(!!sym(col_cr))
  
  df_processado <- lapply(lista_setores, function(sub_df) {
    
    # SOLUÇÃO AQUI: Em vez de st_touches, usamos st_is_within_distance.
    # Isso cria uma "ponte" invisível sobre rios, estradas ou erros do IBGE
    # de até 'tolerancia_metros' (ex: 200m).
    v <- st_is_within_distance(st_geometry(sub_df), dist = tolerancia_metros)
    
    g <- graph_from_adj_list(v, mode = "all")
    
    sub_df$id_componente <- paste0(sub_df[[col_cr]][1], "_", components(g)$membership)
    return(sub_df)
  }) %>% bind_rows()
  
  # 2. LOCALIZAR O 'CONTINENTE' (O QUE TEM A SEDE)
  message("📍 Identificando blocos oficiais via Sedes...")
  continentes_validos <- c()
  
  for(i in 1:nrow(sf_sedes)) {
    nome_cr <- sf_sedes$nome[i]
    ponto_sede <- sf_sedes[i, ]
    
    setores_cr <- df_processado %>% filter(!!sym(col_cr) == nome_cr)
    
    if(nrow(setores_cr) > 0) {
      idx_sede <- st_nearest_feature(ponto_sede, setores_cr)
      id_comp_sede <- setores_cr$id_componente[idx_sede]
      continentes_validos <- c(continentes_validos, id_comp_sede)
    }
  }
  
  # 3. REATRIBUIR ILHAS/ENCLAVES (AGORA DE FATO DESCONECTADAS)
  df_processado$is_ilha <- !df_processado$id_componente %in% continentes_validos
  
  n_ilhas <- sum(df_processado$is_ilha)
  if(n_ilhas == 0) {
    message("✅ Nenhuma ilha detectada.")
    return(df_processado %>% select(-id_componente, -is_ilha))
  }
  
  message(paste("🏝️ Reatribuindo", n_ilhas, "setores isolados..."))
  
  continentes <- df_processado %>% filter(!is_ilha)
  ilhas       <- df_processado %>% filter(is_ilha)
  
  indices_vizinhos <- st_nearest_feature(ilhas, continentes)
  df_processado[[col_cr]][df_processado$is_ilha] <- continentes[[col_cr]][indices_vizinhos]
  
  return(df_processado %>% select(-id_componente, -is_ilha))
}

setores_ama_limpos <- limpar_enclaves_funai(setores_sf, sf_sedes)

# ==============================================================================
# 6. SALVAMENTO E MAPA
# ==============================================================================
saveRDS(setores_ama_limpos, "setores_2022_final_regionalizados.rds")
message("✅ Base salva! Renderizando mapa...")

poligonos_mapa <- setores_ama_limpos %>% st_transform(4326) %>% select(code_tract, cr_nome, modal, horas)
pal <- colorFactor("turbo", poligonos_mapa$cr_nome)

leaflet(poligonos_mapa, options = leafletOptions(preferCanvas = TRUE)) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(fillColor = ~pal(cr_nome), fillOpacity = 0.7, weight = 0.1, color = "white",
              label = ~paste0("Setor: ", code_tract, " | CR: ", cr_nome, " | ", modal, " (", round(horas,1), "h)")) %>%
  addMarkers(data = st_transform(sf_sedes, 4326), label = ~nome)



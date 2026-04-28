# ==========================================================
# FUNÇÃO DE LIMPEZA TOPOLÓGICA (REMOÇÃO DE ENCLAVES)
# ==========================================================
library(sf)
library(igraph)
library(dplyr)
library(tidygraph)

## cr_mun_final é o arquivo com as localizações das CRs
## setores_sf é o shapefile com os setores atribuídos a cada CR. Deve ter a coluna 'cr_nome' com o nome da CR
## atribuída e a coluna geometry para os polígonos.CRS deveestarem EPSG ##

# Transforma as sedes em pontos espaciais
sf_sedes <- cr_mun_final %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(5880) %>%
  st_zm(drop = TRUE)

limpar_enclaves_funai <- function(df_setores, sf_sedes, col_cr = "cr_nome") {
  
  message("🔍 Iniciando limpeza de enclaves em 60k setores...")
  
  # 1. IDENTIFICAR COMPONENTES CONECTADOS
  # Usamos split para garantir que o sf não se perca com a geometria
  lista_setores <- df_setores %>% group_split(!!sym(col_cr))
  
  df_processado <- lapply(lista_setores, function(sub_df) {
    # v é a lista de adjacência (quem toca quem)
    v <- st_touches(st_geometry(sub_df))
    
    # Criar grafo para encontrar grupos de polígonos vizinhos
    g <- graph_from_adj_list(v, mode = "all")
    
    # Criar um ID de componente único combinando o nome da CR com o ID do grupo
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
  
  # 3. REATRIBUIR ILHAS/ENCLAVES
  df_processado$is_ilha <- !df_processado$id_componente %in% continentes_validos
  
  n_ilhas <- sum(df_processado$is_ilha)
  if(n_ilhas == 0) {
    message("✅ Nenhuma ilha detectada.")
    return(df_processado %>% select(-id_componente, -is_ilha))
  }
  
  message(paste("🏝️ Reatribuindo", n_ilhas, "setores isolados..."))
  
  # Separamos o que é bloco oficial (continente) do que é enclave (ilha)
  continentes <- df_processado %>% filter(!is_ilha)
  ilhas       <- df_processado %>% filter(is_ilha)
  
  # Reatribuição rápida via vizinho mais próximo 'oficial'
  indices_vizinhos <- st_nearest_feature(ilhas, continentes)
  df_processado[[col_cr]][df_processado$is_ilha] <- continentes[[col_cr]][indices_vizinhos]
  
  return(df_processado %>% select(-id_componente, -is_ilha))
}


# Assume que 'setores_ama_alocados' é o resultado do seu script de matrizes
setores_final_limpos <- limpar_enclaves_funai(setores_ama_alocados, sf_sedes)
library(readr)
cr_mun <- read_delim("C:/Users/eduardo.silva/OneDrive - FUNAI - Fundação Nacional dos Povos Indígenas/cr_mun.csv", 
                       +     delim = ";", escape_double = FALSE, trim_ws = TRUE)
library(geobr)
Carregando namespace exigido: sf
library(dplyr)

muns_referencia <- read_municipality(year = 2022, showProgress = FALSE) %>%
   sf::st_drop_geometry() %>%            # Remove a parte do mapa (geometria)
   select(code_muni, name_muni, abbrev_state) # Seleciona código, nome e UF

cr_mun_completo <- cr_mun %>%
  left_join(muns_referencia, by = c("municipio" = "code_muni"))
head(cr_mun_completo)

library(geobr)
library(dplyr)

  todos_municipios <- read_municipality(year = 2022, showProgress = FALSE) %>%
     # O geobr retorna um objeto espacial (sf). 
     # st_drop_geometry transforma o objeto em um dataframe comum (tibble).
     sf::st_drop_geometry() %>%
     # Selecionando e organizando as colunas mais importantes
     select(
           municipio_id = code_muni,    # Código de 7 dígitos do IBGE
           nome_municipio = name_muni,
           uf_sigla = abbrev_state,
           uf_nome = name_state,
           uf_codigo = code_state,
           regiao = name_region
       )
 # 2. Verificar o dataframe criado
print(paste("Total de municípios carregados:", nrow(todos_municipios)))


library(dplyr)
library(stringr)
cr_mun_filtrado <- cr_mun_completo %>%
     filter(str_starts(nome, "Coordena"))
 # Visualizando o resultado
head(cr_mun_filtrado)

View(cr_mun_filtrado)
library(geobr)
library(sf)
library(dplyr)
library(tidyr)

# 1. Obter a malha completa com geometria (necessário para o centroide)
muns_geo <- read_municipality(year = 2022, showProgress = FALSE)



# 3. Identificar os pontos das Coordenações Regionais (CRs)
 # Filtramos os centroides que correspondem aos códigos das CRs do seu 'cr_mun_filtrado'
 pontos_cr <- muns_centroides %>%
     filter(code_muni %in% cr_mun_filtrado$municipio) %>%
     # Unimos com o nome da CR para usar como cabeçalho depois
     left_join(select(cr_mun_filtrado, municipio, nome_cr = nome), by = c("code_muni" = "municipio"))

 # 4. Calcular a matriz de distância
 # st_distance retorna uma matriz em metros por padrão
 dist_matrix <- st_distance(muns_centroides, pontos_cr)

 # 5. Converter a matriz em um dataframe organizado
df_distancias <- as.data.frame(dist_matrix)
colnames(df_distancias) <- pontos_cr$nome_cr # Nomeia as colunas com as CRs

 # Adiciona o nome e código do município de cada linha
 df_final <- muns_centroides %>%
     st_drop_geometry() %>%
     select(code_muni, name_muni) %>%
     bind_cols(df_distancias)

 # 6. (Opcional) Converter de metros para quilômetros
 df_final <- df_final %>%
     mutate(across(starts_with("Coordena"), ~ as.numeric(. / 1000)))

 # Visualizando o resultado
 head(df_final)




library(dplyr)
library(tidyr)

# Definindo o limiar de 20% (0.20)
limiar_percentual <- 0.20

# 1. Preparação: Transformar os dados para identificar as distâncias por município
df_classificacao <- df_final %>%
  # Passamos para o formato 'longo' para ordenar as distâncias
  pivot_longer(cols = starts_with("Coordena"), 
               names_to = "cr_nome", 
               values_to = "distancia") %>%
  group_by(code_muni, name_muni) %>%
  # Ordenamos da mais perto para a mais longe
  arrange(distancia, .by_group = TRUE) %>%
  # Calculamos a diferença percentual em relação à CR mais próxima (rank 1)
  mutate(rank = row_number(),
         dist_minima = first(distancia),
         diff_relativa = (distancia - dist_minima) / dist_minima) %>%
  # Filtramos apenas as CRs que estão dentro do limite de proximidade
  filter(diff_relativa <= limiar_percentual) %>%
  # Contamos quantas CRs sobraram para este município
  mutate(total_crs_proximas = n()) %>%
  ungroup()

# --- CRIAÇÃO DOS TRÊS DATAFRAMES ---

# 1. Municípios com apenas 1 CR próxima
df_1_cr <- df_classificacao %>%
  filter(total_crs_proximas == 1) %>%
  select(code_muni, name_muni, cr_nome, distancia)

# 2. Municípios com duas CRs próximas
df_2_cr <- df_classificacao %>%
  filter(total_crs_proximas == 2) %>%
  select(code_muni, name_muni, cr_nome, distancia, rank) %>%
  pivot_wider(names_from = rank, 
              values_from = c(cr_nome, distancia),
              names_glue = "{.value}_{rank}") %>%
  rename(cr1_nome = cr_nome_1, dist1 = distancia_1,
         cr2_nome = cr_nome_2, dist2 = distancia_2)

# 3. Municípios com 3 ou mais CRs próximas (Apenas identificação)
df_3_plus_cr <- df_classificacao %>%
  filter(total_crs_proximas >= 3) %>%
  distinct(code_muni, name_muni)

# Verificação dos tamanhos
cat("Grupos criados:\n",
    "- 1 CR:", nrow(df_1_cr), "municípios\n",
    "- 2 CRs:", nrow(df_2_cr), "municípios\n",
    "- 3+ CRs:", nrow(df_3_plus_cr), "municípios\n")






### retirada das coordenações de suporte e cálculo dos centroides das coordenações

library(dplyr)
library(stringr)
library(geobr)
library(sf)

# 1. Filtrar para remover as Coordenações de Suporte
cr_mun_filtrado <- cr_mun_filtrado %>%
  filter(!str_starts(nome, "Coordenação Regional de Suporte"))

# 2. Buscar a malha de TODOS os municípios e filtrar localmente
# Isso evita o erro de "condition has length > 1"
sedes_geometria <- read_municipality(year = 2022, showProgress = FALSE) %>%
  # Filtramos apenas os códigos que estão na sua lista de CRs
  filter(code_muni %in% cr_mun_filtrado$municipio) %>%
  # Calculamos o centroide
  st_centroid() %>%
  # Convertemos para WGS84 (padrão Leaflet)
  st_transform(4326) %>%
  # Selecionamos apenas as colunas necessárias para o join
  select(code_muni, geom)

# 3. Unir com o seu dataframe original
# Agora o cr_mun_filtrado terá uma coluna 'geom' com os pontos
cr_mun_filtrado <- cr_mun_filtrado %>%
  inner_join(sedes_geometria, by = c("municipio" = "code_muni")) %>%
  st_as_sf() # Transforma o dataframe em um objeto espacial (sf)

cr_mun_final<-cr_mun_filtrado[,c(8,1,2,4,5,11,12)]


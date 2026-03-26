library(arrow)
library(geocodebr)
library(enderecobr)
library(geobr)
library(ggplot2)
library(sf)

consulta_uorgs_completo <- function(codigo){
  require(jsonlite)
  require(dplyr)
  require(tidyr)
  require(httr)
  
  url = paste0("https://estruturaorganizacional.dados.gov.br/doc/estrutura-organizacional/completa?codigoPoder=1&codigoEsfera=1&codigoUnidade=", codigo)
  b <- fromJSON(content(GET(url = url), 'text', encoding = 'UTF-8'))$unidades
  
  
  if(length(b) != 0){
    b <- b |>
      mutate(
        codigoUnidade = sub(".*/", "", codigoUnidade),
        codigoUnidadePai = sub(".*/", "", codigoUnidadePai),
        codigoOrgaoEntidade = sub(".*/", "", codigoOrgaoEntidade),
        codigoTipoUnidade = sub(".*/", "", codigoTipoUnidade),
        codigoEsfera = sub(".*/", "", codigoEsfera),
        codigoPoder = sub(".*/", "", codigoPoder),
        codigoNaturezaJuridica = sub(".*/", "", codigoNaturezaJuridica),
        codigoCategoriaUnidade = sub(".*/", "", codigoCategoriaUnidade)
      ) |> unnest(cols = c(contato, endereco)) 
    
    paste0("Sucesso em ",codigo)
  } else{
    paste0("Falha em ",codigo)
  }
  
  b <- b |>
    select(everything(), -email, -telefone, email, telefone) |> 
    unnest_wider(telefone, names_sep = '_') |>
    unnest_wider(email, names_sep = '_') |> 
    unnest_wider(atoNormativo, names_sep = '_') |>
    select(-site)
  
  return(b)
}

# CAMINHOS
path_dtb <- 'dados/brutos/DTB_2024.csv'

# aldeias |> as_tibble() |> select(nome_cr, undadm_codigo) |> View()

# URLs do GEOSERVER
url_tis <- "https://geoserver.funai.gov.br/geoserver/Funai/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=Funai%3Atis_poligonais&maxFeatures=10000"
url_aldeias <- "https://geoserver.funai.gov.br/geoserver/Funai/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=Funai%3Aaldeias_pontos&maxFeatures=10000"
url_aeroporto <- "https://geoservicos.ibge.gov.br/geoserverCCAR/CCAR/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=CCAR%3ABC250_2025_aer_complexo_aeroportuario_p&outputFormat=application%2Fjson&maxFeatures=600000"
utl_pista_pouso <- "https://geoservicos.ibge.gov.br/geoserverCCAR/CCAR/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=CCAR%3Apista_ponto_pouso&outputFormat=application%2Fjson&maxFeatures=600000"
url_hidroviario <- "https://geoservicos.ibge.gov.br/geoserverCCAR/CCAR/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=CCAR%3Ahidrovias_escala&outputFormat=application%2Fjson&maxFeatures=600000"

# IMPORTAÇÃO DOS DADOS ----
# GEOESPACIAIS
ti_poligons <- st_read(url_tis)
aldeias <- st_read(url_aldeias)
aeroporto <- st_read(url_aeroporto)
pista_pouso <- st_read(utl_pista_pouso)
hidroviario <- st_read(url_hidroviario)

# NÃO GEORREFERENCIADOS
municipios_ibge <- read.csv(path_dtb)

# CONVERTENDO PARA SIRGAS 2000 ----
# ti_poligons <- st_transform(ti_poligons, crs = 4674)
# aldeias <- st_transform(aldeias, crs = 4674)
aeroporto <- st_transform(aeroporto, crs = 4674)
pista_pouso <- st_transform(pista_pouso, crs = 4674)
hidroviario <- st_transform(hidroviario, crs = 4674)

# EXTRAINDO DADOS DO SIORG ----
siorg <- consulta_uorgs_completo(173)

# FILTRANDO PARA CRs e Unidades Técnicas Locais
siorg_cr <- siorg |> filter(grepl('^CR-', sigla) | grepl('^UTL-', sigla)
) |> distinct(codigoUnidade, .keep_all = T)

siorg_cr <- siorg_cr |> select(codigoUnidade, nome, sigla, logradouro, numero, complemento, cep, bairro, municipio, uf)

# Acrescentando nome do município
siorg_cr <- siorg_cr |> 
  left_join(
    municipios_ibge |> select(cd_mun_ibge, nm_mun_ibge, nome_uf), 
    by = join_by(municipio == cd_mun_ibge))


campos <- correspondencia_campos(
  logradouro = "logradouro",
  numero = "numero",
  complemento = "complemento",
  cep = "cep",
  bairro = "bairro",
  municipio = "nm_mun_ibge",
  estado = "nome_uf"
)

siorg_cr_padronizado <- padronizar_enderecos(siorg_cr, campos)

campos <- geocodebr::definir_campos(
  logradouro = "logradouro_padr",
  numero = "numero_padr",
  localidade = "bairro_padr",
  municipio = "municipio_padr",
  estado = "nome_uf"
)



df <- geocodebr::geocode(
  enderecos = siorg_cr_padronizado,
  campos_endereco = campos,
  resolver_empates = TRUE,
  resultado_sf = TRUE,
  verboso = FALSE
)



brasil <- read_country(year = 2020)



ggplot() +
  geom_sf(data = brasil, fill = "lightgray") +
  geom_sf(data = tis, fill = NA, color = "blue", size = 1) +
  geom_sf(data = df, color = "red", size = 1) +
  geom_sf(data = aldeias, color = "orange", size = .5) +
  geom_sf(data = dados, color = "yellow") +
  geom_sf(data = hidroviario, color = "cyan") +
  geom_sf_text(data = aldeias, aes(label = nome_aldeia), nudge_y = 0.5, size = .3) +
  theme_minimal()




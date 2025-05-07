# Prepara arquivos para visualização

library('tidyverse')
library('tidylog')
library('janitor')

pasta_dados <- '../dados'
pasta_publi <- sprintf('%s/tableau', pasta_dados)
dir.create(pasta_publi, recursive = TRUE, showWarnings = FALSE)


# ------------------------------------------------------------------------------
# Base de sinistros
# ------------------------------------------------------------------------------

sinistros <- list.files(pasta_dados, pattern = '_sinistros_', recursive = FALSE, full.names = TRUE)
sinistros <- map_df(sinistros, ~ read_delim(., delim = ';', col_types = cols(.default = "c"),
                     locale = locale(decimal_mark = ',', grouping_mark = '.', encoding = 'iso_8859-1')))
# sinistros <- sinistros %>% distinct()
# names(sinistros)

# Temos um id que está repetido, mas com informações conflitantes na base
# sinistros %>% group_by(id_sinistro) %>% tally() %>% filter(n > 1)
# id_sinistro     n
# <chr>       <int>
# 1 1749611         2
sinistros <- sinistros %>% distinct(id_sinistro, .keep_all = TRUE)


sinistros <-
  sinistros %>%
  select(id_sinistro,
         tipo_registro,
         data_sinistro,

         # ano_sinistro,
         # mes_sinistro,
         # dia_sinistro,
         # ano_mes_sinistro,

         hora_sinistro,
         logradouro,
         numero_logradouro,
         tipo_via,
         municipio,
         regiao_administrativa,

         tp_veiculo_bicicleta,
         tp_veiculo_caminhao,
         tp_veiculo_motocicleta,
         tp_veiculo_nao_disponivel,
         tp_veiculo_onibus,
         tp_veiculo_outros,
         tp_veiculo_automovel,
         gravidade_nao_disponivel,
         gravidade_leve,
         gravidade_fatal,
         gravidade_ileso,
         gravidade_grave,
         # administracao,
         # conservacao,
         jurisdicao,

         tipo_acidente_primario,
         tp_sinistro_atropelamento,
         tp_sinistro_colisao_frontal,
         tp_sinistro_colisao_traseira,
         tp_sinistro_colisao_lateral,
         tp_sinistro_colisao_transversal,
         tp_sinistro_colisao_outros,
         tp_sinistro_choque,
         tp_sinistro_capotamento,
         tp_sinistro_engavetamento,
         tp_sinistro_tombamento,
         tp_sinistro_outros,
         tp_sinistro_nao_disponivel,

         latitude,
         longitude
  ) %>%
  mutate(data_sinistro = dmy(data_sinistro),
         faixa_horario = as.numeric(str_sub(hora_sinistro, 1, 2)),
         # data_do_obito    = dmy(data_do_obito),
         # idade_da_vitima  = as.numeric(idade_da_vitima),
         numero_logradouro  = as.double(str_replace(numero_logradouro, ',', '.')),
         latitude         = as.double(str_replace(latitude, ',', '.')),
         longitude        = as.double(str_replace(longitude, ',', '.'))
  ) %>%
  mutate(faixa_horario = case_when(between(faixa_horario,  0,  5) ~ 'Madrugada',
                                   between(faixa_horario,  6, 11) ~ 'Manhã',
                                   between(faixa_horario, 12, 17) ~ 'Tarde',
                                   between(faixa_horario, 18, 23) ~ 'Noite',
                                   TRUE ~ 'Não disponível'),
         .after = 'hora_sinistro')




out_file <- sprintf('%s/obitos_tableau.csv', pasta_publi)
write_delim(obitos, out_file, delim = ';')


# compare_rows <- function(df) {
#   # Group by "id"
#   grouped_df <- df %>%
#     group_by(id_sinistro) %>%
#     arrange(id_sinistro) %>%
#     slice(1:2) # Select first two rows per group
#
#   # Compare columns
#   comparison_result <- sapply(2:ncol(grouped_df), function(i) {
#     # Compare values in each column for the two rows
#     if (!identical(grouped_df[1, i], grouped_df[2, i])) {
#       return(names(grouped_df)[i])  # Return column name if there's a difference
#     } else {
#       return(NULL)
#     }
#   })
#
#   comparison_result <- comparison_result[!sapply(comparison_result, is.null)]
#   # Add quotes around each column name
#   comparison_result <- paste("'", comparison_result, "'", sep = "")
#   # Combine them in a single string with commas, enclosed in square brackets
#   comparison_result <- paste("[", paste(comparison_result, collapse = ", "), "]", sep = "")
#
#   # comparison_result <- paste(comparison_result, collapse = ", ")
#   return(comparison_result)
# }
#
# # Call function to compare rows
# boo <- compare_rows(sinistros %>% filter(id_sinistro == '1749611'))



# ------------------------------------------------------------------------------
# Base de vítimas
# ------------------------------------------------------------------------------

vitimas <- list.files(pasta_dados, pattern = '_pessoas_', recursive = FALSE, full.names = TRUE)
vitimas <- map_df(vitimas, ~ read_delim(., delim = ';', col_types = cols(.default = "c"),
                                            locale = locale(decimal_mark = ',', grouping_mark = '.', encoding = 'iso_8859-1')))
# vitimas <- vitimas %>% distinct()

vitimas <- vitimas %>% clean_names()

vitimas <- vitimas %>%
  select(id_sinistro,
         # municipio,
         # tipo_via,
         tipo_veiculo_vitima,
         sexo,
         idade,
         data_obito,
         gravidade_lesao,
         tipo_de_vitima,
         # faixa_etaria_demografica,
         # faixa_etaria_legal,
         profissao,
         data_sinistro_vit = data_sinistro,
         # ano_sinistro,
         # mes_sinistro,
         # dia_sinistro,
         # ano_mes_sinistro,
         # ano_obito,
         # mes_obito,
         # dia_obito,
         # ano_mes_obito
  ) %>%
  mutate(data_obito = dmy(data_obito),
         idade  = as.numeric(idade)) %>%
  mutate(faixa_etaria = case_when(idade <= 3  ~ '00 a 03 anos',
                                  between(idade,  4,  6) ~ '04 a 06 anos',
                                  between(idade,  7, 10) ~ '07 a 10 anos',
                                  between(idade, 11, 14) ~ '11 a 14 anos',
                                  between(idade, 15, 17) ~ '15 a 17 anos',
                                  between(idade, 18, 22) ~ '18 a 22 anos',
                                  between(idade, 23, 29) ~ '23 a 29 anos',
                                  between(idade, 30, 39) ~ '30 a 39 anos',
                                  between(idade, 40, 49) ~ '40 a 49 anos',
                                  between(idade, 50, 59) ~ '50 a 59 anos',
                                  between(idade, 60, 69) ~ '60 a 69 anos',
                                  between(idade, 70, 79) ~ '70 a 79 anos',
                                  between(idade, 80, 999) ~ '80 anos ou mais',
                                  TRUE ~ 'Sem informação'),
         .after = 'idade')


# Selecionar colunas de interesse para visualização
sinistros_join <-
  sinistros %>%
  select(id_sinistro,
         tipo_registro,
         data_sinistro,
         hora_sinistro,
         faixa_horario,
         tipo_de_sinistro = tipo_acidente_primario,
         municipio,
         regiao_administrativa,
         tipo_via,
         logradouro,
         numero_logradouro,
         jurisdicao,
         latitude,
         longitude)


# Juntar dados de sinistros aos das vítimas
vitimas_sinistros <- vitimas %>% left_join(sinistros_join, by = 'id_sinistro')

vitimas_sinistros <-
  vitimas_sinistros %>%
  mutate(dias_ate_obito = as.numeric(data_obito - data_sinistro), .after = 'data_obito') %>%
  select(id_sinistro,
         data_sinistro,
         data_sinistro_vit,
         hora_sinistro,
         faixa_horario,
         tipo_de_sinistro,
         # local_do_obito # coluna ausente na base - recriar?
         tipo_registro,
         gravidade_lesao,
         data_obito,
         dias_ate_obito,
         sexo,
         idade,
         faixa_etaria,
         profissao,
         tipo_de_vitima,
         tipo_veiculo_vitima,
         municipio,
         regiao_administrativa,
         tipo_via,
         logradouro,
         numero_logradouro,
         jurisdicao,
         latitude,
         longitude) %>%
  arrange(id_sinistro)




obitos_sp <- vitimas_sinistros %>% filter(municipio == 'SAO PAULO')


# 1. Óbitos por ano e ano-mês (até 30 dias vs mais do que 30 dias)
#    - Incluir diferença para os anos anteriores, se aumentou ou diminuiu
# 2. Por tipo de veículo
# 3. Por faixa etária (alterar faixas existentes)
# 4. Por sexo
# 5. Por tipos de vias - municipais vs rodovias
# 6. Por horário


names(obitos_sp)

# Óbitos como o Infosiga está computando - números batem com os do site
obitos_sp %>%
  # Filtro utilizado para os dados oficiais
  filter(tipo_registro == 'SINISTRO FATAL' & gravidade_lesao == 'FATAL') %>%
  mutate(ano_obito = str_sub(data_obito, 1, 4), .before = 1) %>%
  group_by(ano_obito) %>%
  tally() %>%
  ungroup()
# ano_obito     n
# <chr>     <int>
# 1 2015       1101
# 2 2016        930
# 3 2017        824
# 4 2018        828
# 5 2019        800
# 6 2020        711
# 7 2021        725
# 8 2022        858
# 9 2023        927
# 10 2024      1033
# 11 2025       131

# Esses deveriam ser os números corretos
obitos_sp %>%
  # Filtro utilizado para os dados oficiais
  filter(tipo_registro == 'SINISTRO FATAL' & gravidade_lesao == 'FATAL') %>%
  filter(dias_ate_obito <= 30) %>%
  mutate(ano_obito = str_sub(data_obito, 1, 4), .before = 1) %>%
  group_by(ano_obito) %>%
  tally() %>%
  ungroup()
# ano_obito     n
# <chr>     <int>
# 1 2015       1101
# 2 2016        928
# 3 2017        822
# 4 2018        827
# 5 2019        799
# 6 2020        710
# 7 2021        725
# 8 2022        858
# 9 2023        927
# 10 2024       1029
# 11 2025       131

# Essas linhas não deveriam estar sendo computadas
obitos_sp %>%
  # Filtro utilizado para os dados oficiais
  filter(tipo_registro == 'SINISTRO FATAL' & gravidade_lesao == 'FATAL') %>%
  filter(dias_ate_obito > 30) %>%
  select(id_sinistro, data_sinistro, data_sinistro_vit, data_obito, dias_ate_obito) %>%
  mutate(ano_obito = str_sub(data_obito, 1, 4), .before = 1)
# ano_obito id_sinistro data_sinistro data_obito dias_ate_obito
# <chr>     <chr>       <date>        <date>              <dbl>
# 1 2024      2462712     2024-08-31    2024-10-15             45
# 2 2017      2462853     2016-04-11    2017-04-11            365
# 3 2016      2463745     2016-07-06    2016-08-24             49
# 4 2024      2465737     2023-08-01    2024-01-02            154
# 5 2024      2466684     2023-04-17    2024-04-17            366
# 6 2018      2467058     2018-07-01    2018-12-17            169
# 7 2016      2467386     2016-10-12    2016-11-23             42
# 8 2017      2468136     2016-11-25    2017-09-04            283
# 9 2024      2468242     2023-09-17    2024-03-23            188
# 10 2019      2477916     2018-12-31    2019-03-31             90
# 11 2020      2482832     2019-11-10    2020-01-16             67






obitos_sp %>%
  # Filtro utilizado para os dados oficiais
  filter(tipo_registro == 'SINISTRO FATAL' & gravidade_lesao == 'FATAL') %>%
  filter(dias_ate_obito <= 30) %>%
  mutate(ano_obito = str_sub(data_obito, 1, 4), .before = 1) %>%
  filter(str_detect(logradouro, 'TIET[EÊ]')) %>%

  # select(logradouro) %>%
  # distinct()
  group_by(ano_obito) %>%
  tally() %>%
  ungroup()



















this <-
  obitos_sp %>%
  # Filtro utilizado para os dados oficiais
  filter(tipo_registro == 'SINISTRO FATAL' & gravidade_lesao == 'FATAL') %>%
  filter(dias_ate_obito <= 30) %>%
  # filter(dias_ate_obito > 30) %>%
  # select(id_sinistro, data_sinistro, data_obito, dias_ate_obito)
  # filter(tipo_veiculo_vitima == 'MOTOCICLETA') %>%
  mutate(ano_obito = str_sub(data_obito, 1, 4), .before = 1) %>%
  group_by(ano_obito) %>%
  tally() %>%
  ungroup()

that <-
  obitos_sp %>%
  filter(tipo_registro == 'SINISTRO FATAL') %>%
  filter(is.na(data_obito)) %>%
  mutate(ano_sinistro = str_sub(data_sinistro, 1, 4), .before = 1) %>%
  select(-c(faixa_horario, tipo_de_sinistro, dias_ate_obito,
            id_sinistro, data_sinistro, hora_sinistro, municipio,
            regiao_administrativa, logradouro, numero_logradouro,
            faixa_etaria, profissao, sexo, latitude, longitude)) %>%
  sample_n(20) %>%
  select(1, 4, 5:length(names(.)))
  group_by(ano_sinistro) %>%
  tally() %>%
  ungroup()

this %>% left_join(that, by = c('ano_obito' = 'ano_sinistro')) %>%
  mutate(novo_total = n.x + n.y)


mortes_moto <-
  obitos_sp %>%
  filter(tempo_entre_o_sinistro_e_o_obito == 'ATÉ 30 DIAS') %>%
  filter(meio_de_locomocao_da_vitima == 'MOTOCICLETA') %>%
  mutate(ano_obito = str_sub(data_do_obito, 1, 4), .before = 1) %>%
  group_by(ano_obito) %>%
  tally() %>%
  as.data.frame()

out_file <- sprintf('%s/mortes_moto.csv', pasta_dados)
write_delim(mortes_moto, out_file, delim = ';')

filter(str_starts(data_do_obito, '2024')) %>%
  filter(str_starts(data_do_sinistro, '2024')) %>%
  filter(dias_morte > 30)

filter(tempo_entre_o_sinistro_e_o_obito == 'ATÉ 30 DIAS')

obitos_sp %>% select(faixa_etaria) %>% distinct() %>% arrange(faixa_etaria)
obitos_sp %>% select(tipo_de_vitima) %>% distinct() %>% arrange(tipo_de_vitima)
obitos_sp %>% select(meio_de_locomocao_da_vitima) %>% distinct() %>% arrange(meio_de_locomocao_da_vitima)

obitos_sp %>% select(dias_ate_obito) %>% hist(pch = 16, cex = 1, breaks = 100)
class(obitos_sp$dias_ate_obito)
obitos_sp %>% filter(is.na(dias_ate_obito))

this <- obitos_sp$dias_ate_obito
hist(this, pch = 16, cex = 1, breaks = 10000, xlim = c(0, 50))



# ------------------------------------------------------------------------------
# Base de sinistros não fatais
# ------------------------------------------------------------------------------

sinistros <- list.files(pasta_dados, pattern = '_nao_fatais_', recursive = FALSE, full.names = TRUE)

sinistros1 <- read_delim(sinistros[1], delim = ';', col_types = cols(.default = "c"),
                         locale = locale(decimal_mark = ',', grouping_mark = '.', encoding = 'iso_8859-1'))
sinistros2 <- read_delim(sinistros[2], delim = ';', col_types = cols(.default = "c"),
                         locale = locale(decimal_mark = ',', grouping_mark = '.', encoding = 'iso_8859-1'))

# names(sinistros1) == names(sinistros2)
sinistros <- rbind(sinistros1, sinistros2)
rm(sinistros1, sinistros2)

sinistros <- sinistros %>% clean_names()
names(sinistros)

sinistros %>%
  select(tipo_de_registro,
         data_do_sinistro,
         hora_do_sinistro,
         municipio,
         logradouro,
         numero_km,
         atropelamento:nao_disponivel,
         matches('_envolvid'),
         latitude,
         longitude
  ) %>%
  rename_with(~ gsub("_envolvid[ao][s]?", "", .), matches("_envolvid[ao][s]?"))

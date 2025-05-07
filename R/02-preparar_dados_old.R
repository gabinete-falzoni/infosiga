# Prepara arquivos para visualização

library('tidyverse')
library('tidylog')
library('janitor')

pasta_dados <- '../dados'
pasta_publi <- sprintf('%s/tableau', pasta_dados)
dir.create(pasta_publi, recursive = TRUE, showWarnings = FALSE)


# ------------------------------------------------------------------------------
# Base de óbitos
# ------------------------------------------------------------------------------

obitos <- list.files(pasta_dados, pattern = 'obitos\\.csv$', recursive = FALSE, full.names = TRUE)
obitos <- read_delim(obitos, delim = ';', col_types = cols(.default = "c"),
                     locale = locale(decimal_mark = ',', grouping_mark = '.', encoding = 'iso_8859-1'))
obitos <- obitos %>% clean_names()
# names(obitos)

obitos <-
  obitos %>%
  select(data_do_sinistro,
         hora_do_sinistro,
         tipo_de_sinistro,
         # tipo_de_local_do_sinistro,
         local_do_obito,
         data_do_obito,
         tempo_entre_o_sinistro_e_o_obito,
         sexo,
         idade_da_vitima,
         faixa_etaria_1 = faixa_etaria,
         tipo_de_vitima,
         meio_de_locomocao_da_vitima,
         outro_veiculo_envolvido,
         municipio,
         regiao_administrativa,
         tipo_de_via,
         logradouro,
         numeral_km,
         jurisdicao,
         # administracao,
         # conservacao,
         latitude,
         longitude,
         # id_da_delegacia,
         # numero_do_bo,
         ) %>%
  mutate(data_do_sinistro = dmy(data_do_sinistro),
         hora_do_sinistro = as.numeric(hora_do_sinistro),
         data_do_obito    = dmy(data_do_obito),
         idade_da_vitima  = as.numeric(idade_da_vitima),
         numeral_km       = as.double(str_replace(numeral_km, ',', '.')),
         latitude         = as.double(str_replace(latitude, ',', '.')),
         longitude        = as.double(str_replace(longitude, ',', '.'))
         ) %>%
  mutate(dias_ate_obito = as.numeric(data_do_obito - data_do_sinistro), .after = 'data_do_obito') %>%
  mutate(faixa_etaria_2 = case_when(idade_da_vitima <= 3  ~ '00 a 03 anos',
                                    between(idade_da_vitima,  4,  6) ~ '04 a 06 anos',
                                    between(idade_da_vitima,  7, 10) ~ '07 a 10 anos',
                                    between(idade_da_vitima, 11, 14) ~ '11 a 14 anos',
                                    between(idade_da_vitima, 15, 17) ~ '15 a 17 anos',
                                    between(idade_da_vitima, 18, 22) ~ '18 a 22 anos',
                                    between(idade_da_vitima, 23, 29) ~ '23 a 29 anos',
                                    between(idade_da_vitima, 30, 39) ~ '30 a 39 anos',
                                    between(idade_da_vitima, 40, 49) ~ '40 a 49 anos',
                                    between(idade_da_vitima, 50, 59) ~ '50 a 59 anos',
                                    between(idade_da_vitima, 60, 69) ~ '60 a 69 anos',
                                    between(idade_da_vitima, 70, 79) ~ '70 a 79 anos',
                                    between(idade_da_vitima, 80, 999) ~ '80 anos ou mais',
                                    TRUE ~ 'Sem informação'),
         .after = 'faixa_etaria_1') %>%
  mutate(faixa_horario = case_when(between(hora_do_sinistro,  0,  5) ~ 'Madrugada',
                                   between(hora_do_sinistro,  6, 11) ~ 'Manhã',
                                   between(hora_do_sinistro, 12, 17) ~ 'Tarde',
                                   between(hora_do_sinistro, 18, 23) ~ 'Noite',
                                   TRUE ~ 'Não disponível'),
         .after = 'hora_do_sinistro') %>%
  mutate(n = 1, .before = 'latitude')


out_file <- sprintf('%s/obitos_tableau.csv', pasta_publi)
write_delim(obitos, out_file, delim = ';')


obitos_sp <- obitos %>% filter(municipio == 'SAO PAULO')


# 1. Óbitos por ano e ano-mês (até 30 dias vs mais do que 30 dias)
#    - Incluir diferença para os anos anteriores, se aumentou ou diminuiu
# 2. Por tipo de veículo
# 3. Por faixa etária (alterar faixas existentes)
# 4. Por sexo
# 5. Por tipos de vias - municipais vs rodovias
# 6. Por horário


names(obitos_sp)

obitos_sp %>%
  filter(tempo_entre_o_sinistro_e_o_obito == 'ATÉ 30 DIAS') %>%
  filter(meio_de_locomocao_da_vitima == 'MOTOCICLETA' | outro_veiculo_envolvido == 'MOTOCICLETA') %>%
  mutate(ano_obito = str_sub(data_do_obito, 1, 4), .before = 1) %>%
  group_by(ano_obito) %>%
  tally()

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
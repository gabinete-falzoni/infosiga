# Baixar o arquivo dados_infosiga.zip em
# https://www.infosiga.sp.gov.br/ > Guia de Referência > Dados abertos

library('tidyverse')
library('tidylog')

pasta_downloads  <- sprintf('%s/Downloads', Sys.getenv("HOME"))
pasta_dados      <- sprintf('/mnt/fern/Dados/gitlab/infosiga/dados')
pasta_arquivos   <- sprintf('%s/arquivos', pasta_dados)
pasta_tmp_apagar <- sprintf('%s/tmp_apagar', pasta_dados)
dir.create(pasta_arquivos, recursive = TRUE, showWarnings = FALSE)
dir.create(pasta_tmp_apagar, recursive = TRUE, showWarnings = FALSE)


# Arquivos a serem baixados
# out_file1 <- sprintf('%s/obitos.zip', pasta_downloads)
# out_file2 <- sprintf('%s/sinistros_fatais.zip', pasta_downloads)
# out_file3 <- sprintf('%s/sinistros_nao_fatais.zip', pasta_downloads)
zip_file <- sprintf('%s/dados_infosiga.zip', pasta_downloads)


# ------------------------------------------------------------------------------
# Descompactar arquivos Infosiga
# ------------------------------------------------------------------------------

# Puxar nomes dos arquivos compactados para loop de mover arquivos, a seguir
unzipped_files <- sprintf('/usr/bin/7z l %s | awk "/^202[0-9]/ {print \\$6}"', zip_file)
out_files <- system(unzipped_files, intern = TRUE)
out_files <- out_files[out_files != 'files']

# Descompactar arquivo
unzip_string <- sprintf('/usr/bin/7z x -o%s -y %s', pasta_downloads, zip_file)
system(unzip_string)


# ------------------------------------------------------------------------------
# Mover arquivos descompactados para pasta de análise de dados
# ------------------------------------------------------------------------------

# Guardar todas as datas de modificação dos arquivos
last_mod_all <- data.frame(X = as.character(NA))

# Mover arquivos descompactados para pasta de trabalho
for (i in out_files[2:6]) {
  # i <- out_files[1]
  unzip_fullpath <- sprintf('%s/%s', pasta_downloads, i)

  # Atualizar o nome do arquivo a partir da última data de modificação
  unzip_lastmod <- file.info(unzip_fullpath)$mtime
  unzip_lastmod <- str_replace_all(str_sub(unzip_lastmod, 1, 10), '-', '')
  unzip_newname <- sprintf('%s_%s', unzip_lastmod, i)

  # Mover arquivo para pasta de dados, caso ainda não exista por lá
  dados_fullpath <- sprintf('%s/%s', pasta_dados, unzip_newname)
  if (!file.exists(dados_fullpath)) {

    # Caso exista uma versão anterior, movê-la para a pasta de arquivos
    prev_version <- list.files(pasta_dados, pattern = i, recursive = FALSE)
    if (length(prev_version) > 0) {
      file.copy(sprintf('%s/%s', pasta_dados, prev_version),
                sprintf('%s/%s', pasta_tmp_apagar, prev_version))

      file.remove(sprintf('%s/%s', pasta_dados, prev_version))
    }

    file.copy(unzip_fullpath, dados_fullpath)
    file.remove(unzip_fullpath)

  } else {
    warning(sprintf('Arquivo já existe na pasta: %s', unzip_newname))
  }

  # Atualizar listagem de datas de modificação
  last_mod_all <- rbind(last_mod_all, unzip_lastmod)

}

# Qual é a última data de modificação, considerando todos os arquivos?
last_mod_all <- last_mod_all %>% filter(!is.na(X)) %>% arrange(X) %>% tail(1) %>% pull()


# ------------------------------------------------------------------------------
# Mover arquivos .zip baixados para pasta de arquivos
# ------------------------------------------------------------------------------

# for (i in c(out_file1, out_file2, out_file3)) { file.remove(i) }
bkp_zip <- sprintf('%s/%s_%s', pasta_arquivos, last_mod_all, basename(zip_file))
file.copy(zip_file, bkp_zip)
file.remove(zip_file)

# Apagar arquivos descompactados que estavam na pasta de dados (os originais
# deverão estar como .zip na pasta de arquivos)
unlink(pasta_tmp_apagar, recursive = TRUE)
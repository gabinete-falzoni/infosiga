# Entra na página do Infosiga, baixa os arquivos de Óbitos, Sinistros fatais e
# Sinistros não fatais e joga na pasta de análise de dados

# Baixar GeckoDriver:
# https://github.com/mozilla/geckodriver/releases

# Baixar o Selenium Standalone Server:
# https://selenium-release.storage.googleapis.com/index.html

# Seguir os passos em Selenium Basics:
# https://docs.ropensci.org/RSelenium/articles/basics.html

# Rodar o Selenium e o GeckoDriver em um terminal:
# java -jar -Dwebdriver.gecko.driver="/home/livre/Desktop/Base_GtsRegionais/GitLab/infosiga/gecko/geckodriver-v0.31.0-linux64/geckodriver" /home/livre/Desktop/Base_GtsRegionais/GitLab/infosiga/gecko/selenium-server-standalone-3.9.1.jar

library('tidyverse')
library('tidylog')
library('RSelenium')

pasta_downloads <- '/home/livre/Downloads'
pasta_dados     <- '/home/livre/Desktop/Base_GtsRegionais/GitLab/infosiga/dados'
pasta_arquivos  <- sprintf('%s/arquivos', pasta_dados)
dir.create(pasta_arquivos, recursive = TRUE, showWarnings = FALSE)


# Arquivos a serem baixados
out_file1 <- sprintf('%s/obitos.zip', pasta_downloads)
out_file2 <- sprintf('%s/sinistros_fatais.zip', pasta_downloads)
out_file3 <- sprintf('%s/sinistros_nao_fatais.zip', pasta_downloads)


# ------------------------------------------------------------------------------
# Funções
# ------------------------------------------------------------------------------

# Baixar dados de acordo com o link de download
baixar_dados <- function(n, out_file_name, sleep_time = 7) {
  # n <- 1
  css_class <- sprintf("h3 > a[ng-click='download(%d)']", n)
  botao_download <- remDr$findElement("css", css_class)

  while (!file.exists(out_file_name)) {
    botao_download$clickElement()
    Sys.sleep(sleep_time)
  }

}


# ------------------------------------------------------------------------------
# Baixar arquivos Infosiga
# ------------------------------------------------------------------------------

# Rodar o Selenium e o GeckoDriver em um terminal:
# java -jar -Dwebdriver.gecko.driver="/home/livre/Desktop/Base_GtsRegionais/GitLab/infosiga/gecko/geckodriver-v0.31.0-linux64/geckodriver" /home/livre/Desktop/Base_GtsRegionais/GitLab/infosiga/gecko/selenium-server-standalone-3.9.1.jar

# Conectar a um servidor Selenium
remDr <- remoteDriver(remoteServerAddr = "localhost", port = 4444L, browserName = "firefox")

# Abrir o navegador:
remDr$open()

# Ir para a página do Infosiga onde estão os dados abertos
remDr$navigate("https://www.infosiga.sp.gov.br/#referencia")
# Esperar carrregar a página
Sys.sleep(15)
# remDr$getCurrentUrl()

# Para habilitar os botões de download, é preciso clicar na aba de "8. Dados Abertos"
botao_aba <- remDr$findElement("css", "span.fa.fa-angle-down[ng-click='abreGuia(7)']")
botao_aba$clickElement()

# Baixar arquivos
baixar_dados(1, out_file1)
baixar_dados(2, out_file2)
baixar_dados(3, out_file3, sleep_time = 25)


# Fechar navegador Selenium
if (file.exists(out_file1) & file.exists(out_file2) & file.exists(out_file3)) {
  remDr$close()
}


# ------------------------------------------------------------------------------
# Descompactar arquivos Infosiga
# ------------------------------------------------------------------------------

# Placeholder para gravar nomes dos arquivos de saída descompactados
out_file_names <- c()

if (version$os == 'linux-gnu') {
  for (i in c(out_file1, out_file2, out_file3)) {
    # Gravar nomes de saída dos arquivos descompactados
    unzipped_files <- sprintf('/usr/bin/7z l %s | awk "/^202[0-9]/ {print \\$6}"', i)
    out_files <- system(unzipped_files, intern = TRUE)
    out_file_names <- c(out_file_names, out_files)

    # Descompactararquivos
    unzip_string <- sprintf('/usr/bin/7z x -o%s -y %s', pasta_downloads, i)
    system(unzip_string)
  }
}

# Remover 'files' dos nomes de arquivo de saída
out_file_names <- out_file_names[out_file_names != 'files']


# ------------------------------------------------------------------------------
# Mover arquivos descompactados para pasta de análise de dados
# ------------------------------------------------------------------------------


for (i in out_file_names) {
  # i <- out_file_names[1]
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
                sprintf('%s/%s', pasta_arquivos, prev_version))

      file.remove(sprintf('%s/%s', pasta_dados, prev_version))
    }

    file.copy(unzip_fullpath, dados_fullpath)
    file.remove(unzip_fullpath)

  } else {
    warning(sprintf('Arquivo já existe na pasta: %s', unzip_newname))
  }

}


# ------------------------------------------------------------------------------
# Remover arquivos .zip baixados
# ------------------------------------------------------------------------------

for (i in c(out_file1, out_file2, out_file3)) { file.remove(i) }

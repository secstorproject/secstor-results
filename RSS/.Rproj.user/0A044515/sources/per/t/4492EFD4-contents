library(tidyverse)
library(ggplot2)
library(pbapply)
library(dplyr)
library(readr)

# Carrega funções auxiliares do arquivo externo
source("helper_functions.R")

# Define o caminho para as pastas de dados
data_path <- '../resultsss/'

# Lista de configurações
configurations <- c('c1-lib-i7-3624qm', 'c2-lib-i3-10100t', 'c3-lib-xeon-e2378g', 'c4-lib-ryzen7-5700x-windows', 
                    'c5-5lib- ryzen7-5700x-linux', 't1-api-local', 't2-api-ext', 't3-api-local-20pcs')

# Campos de agrupamento para os conjuntos de dados split e reconstruct
split_grouping_fields <- c('config', 'algorithm', 'n', 'k', 'ds', 'th')
reconstruct_grouping_fields <- c('config', 'algorithm', 'n', 'k', 'kused', 'ds', 'th')

# Imprime o tempo de início
current_time <- Sys.time()
cat("Início: ", format(current_time, "%Y-%m-%d %H:%M:%OS3"), "\n\n")

# Importa os dados
split_full_ds <- dplyr::bind_rows(lapply(configurations, import_split_data))

reconstruct_full_ds <- dplyr::bind_rows(lapply(configurations, import_reconstruct_data))

# Remove outliers usando ZScore
split_zscore <- remove_outliers_zscore_grouped(split_full_ds, split_grouping_fields)
reconstruct_zscore <- remove_outliers_zscore_grouped(reconstruct_full_ds, reconstruct_grouping_fields)

# Resume os dados
split_summary_zscore <- summarize_times(split_zscore, split_grouping_fields)
reconstruct_summary_zscore <- summarize_times(reconstruct_zscore, reconstruct_grouping_fields)

# Separa configurações com base no caractere inicial dos dataframes fornecidos
split_library <- filter(split_summary_zscore, grepl("^c", config))
split_api <- filter(split_summary_zscore, grepl("^t", config))
reconstruct_library <- filter(reconstruct_summary_zscore, grepl("^c", config))
reconstruct_api <- filter(reconstruct_summary_zscore, grepl("^t", config))

# Remove datasets não utilizados após esta etapa
rm(split_full_ds)
rm(split_zscore)
rm(reconstruct_full_ds)
rm(reconstruct_zscore)

# Salva os dataframes em arquivos CSV, caso seja necessário, importar a partir daqui
#write.csv(split_library, "split_library.csv", row.names = FALSE)
#write.csv(split_api, "split_api.csv", row.names = FALSE)
#write.csv(reconstruct_library, "reconstruct_library.csv", row.names = FALSE)
#write.csv(reconstruct_api, "reconstruct_api.csv", row.names = FALSE)

#########################LIB###############################################
# Para o conjunto de dados da biblioteca, obtenha uma visão geral do desempenho
split_conf_perf_lib <- performance_overview_all_configs(split_library)
print(split_conf_perf_lib)

# Calcula o desempenho médio para cada configuração do conjunto de dados da biblioteca
split_conf_perf_sum_lib <- compute_config_performance(split_library)
print(split_conf_perf_sum_lib)

# Visão geral do desempenho para C3
split_conf_perf_lib_c3 <- performance_overview_single_config(split_library, config = "c3-lib-xeon-e2378g")
print(split_conf_perf_lib_c3)

# Para o conjunto de dados da biblioteca, obtenha uma visão geral do desempenho
recons_conf_perf_lib <- performance_overview_all_configs(reconstruct_library)
print(recons_conf_perf_lib)

# Calcula o desempenho médio para cada configuração do conjunto de dados da biblioteca
recon_conf_perf_sum_lib <- compute_config_performance(reconstruct_library)
print(recon_conf_perf_sum_lib)

# Visão geral do desempenho para C3
recon_conf_perf_lib_c3 <- performance_overview_single_config(reconstruct_library, config = "c3-lib-xeon-e2378g")
print(recon_conf_perf_lib_c3)

#########################API###############################################
# Visão geral do desempenho
split_conf_perf_api <- performance_overview_all_configs(split_api)
print(split_conf_perf_api)

# Desempenho médio para cada configuração do conjunto de dados da API
split_conf_perf_sum_api <- compute_config_performance(split_api)
print(split_conf_perf_sum_api)

# Como o T3 foi testado usando apenas uma instância de n, k, ds e th, filtramos os dados relevantes
split_t3t2_perf_sum_api <- compute_ref_performance(split_api, "t3-api-local-20pcs")
print(split_t3t2_perf_sum_api)

# Visão geral do desempenho
recons_conf_perf_api <- performance_overview_all_configs(reconstruct_api)
print(recons_conf_perf_api)

# Desempenho médio para cada configuração do conjunto de dados da API
recon_conf_perf_sum_api <- compute_config_performance(reconstruct_api)
print(recon_conf_perf_sum_api)

# Como o T3 foi testado usando apenas uma instância de n, k, ds e th, filtramos os dados relevantes
recon_t3t2_perf_sum_api <- compute_ref_performance(reconstruct_api, "t3-api-local-20pcs")
print(recon_t3t2_perf_sum_api)

### Iniciando comparação entre LIB e API
split_baseline <- filter_baseline_lib(split_library)
reconstruct_baseline<- filter_baseline_lib(reconstruct_library)

# Compara a baseline com t1-api-local, pois ambos têm todos os testes
result_lib_API_T1 <- compare_lib_API_T1(split_api, reconstruct_api, split_baseline, reconstruct_baseline)
print(result_lib_API_T1)

# Gráficos de comparação
# Piores casos para Lib, T1 e T2
colors <- c("red", "green", "blue")  # Definindo cores para a visualização deste gráfico
plot_comparison(
  split_library, reconstruct_library, split_api, reconstruct_api, 
  10, 5, 10, 10, 10, 
  "Comparação de tempos piores casos",
  "c3-lib-xeon-e2378g", 
  "t1-api-local", 
  "t2-api-ext",
  colors
)

# Melhores casos para Lib, T1 e T2
plot_comparison(
  split_library, reconstruct_library, split_api, reconstruct_api, 
  3, 2, 1, 10, 2, 
  "Comparação de tempos melhores casos",
  "c3-lib-xeon-e2378g", 
  "t1-api-local", 
  "t2-api-ext",
  colors
)  

# Altera as cores para a próxima visualização (ciano para T3)
colors <- c("red", "green", "cyan")

# Caso único para Lib, T1 e T3
plot_comparison(
  split_library, reconstruct_library, split_api, reconstruct_api, 
  10, 5, 10, 100, 10, 
  "Comparação de caso único",  # Título do gráfico
  "c3-lib-xeon-e2378g", 
  "t1-api-local", 
  "t3-api-local-20pcs",
  colors
)

# Imprime o tempo de término
current_time <- Sys.time()
cat("Finalizado em: ", format(current_time, "%Y-%m-%d %H:%M:%OS3"))
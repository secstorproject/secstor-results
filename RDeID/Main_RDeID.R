library(tidyverse)
library(ggplot2)
library(pbapply)
library(dplyr)
library(readr)

# Carrega funções auxiliares do arquivo externo
source("helper_functions_deid.R")

# Setting the path to the data folders
data_path <- '../resultsdeid/'

# Lista de configurações
configurations <- c('c1-lib-i3-10100t', 'c2-lib-ryzen7-5700', 'c3-lib-xeon-e2378g', 
                    't1-lan-sec', 't2-ext-sec', 't1-api-local', 't3-ext-amazon', 't4-ext-amazon-wifi')
# Campos de agrupamento para os conjuntos de dados
grouping_fields <- c('config', 'threads', 'ds', 'parpay', 'requests')

# Imprime o tempo de início
current_time <- Sys.time()
cat("Início: ", format(current_time, "%Y-%m-%d %H:%M:%OS3"), "\n\n")

# Importa os dados
full_ds <- dplyr::bind_rows(lapply(configurations, import_data))

# Remove outliers usando ZScore
full_zscore <- remove_outliers_zscore_grouped(full_ds, grouping_fields)

# Resume os dados
full_summary_zscore <- summarize_times(full_zscore, grouping_fields)

# Separa configurações com base no caractere inicial dos dataframes fornecidos
anon_library <- filter(full_summary_zscore, grepl("^c", config))
anon_api <- filter(full_summary_zscore, grepl("^t", config))

# Remove datasets não utilizados após esta etapa
rm(full_ds)
rm(full_zscore)

# Salva os dataframes em arquivos CSV, caso seja necessário, importar a partir daqui
#write.csv(anon_library, "anon_library.csv", row.names = FALSE)
#write.csv(anon_api, "anon_api.csv", row.names = FALSE)

#########################LIB###############################################
# Para o conjunto de dados da biblioteca, obtenha uma visão geral do desempenho
anon_conf_perf_lib <- performance_overview_all_configs(anon_library)
print(anon_conf_perf_lib)

# Calcula o desempenho médio para cada configuração do conjunto de dados da biblioteca
anon_conf_perf_sum_lib <- compute_config_performance(anon_library)
print(anon_conf_perf_sum_lib)

# Visão geral do desempenho para C3
anon_c3_perf_sum_api <- performance_overview_single_config(anon_library, "c3-lib-xeon-e2378g")
print(anon_c3_perf_sum_api)


#########################API###############################################
# Visão geral do desempenho
anon_conf_perf_api <- performance_overview_all_configs(anon_api)
print(anon_conf_perf_api)

# Desempenho médio para cada configuração do conjunto de dados da API
anon_conf_perf_sum_api <- compute_config_performance(anon_api)
print(anon_conf_perf_sum_api)

# Desempenho do cenário onde o serviço está no IFSC e a conexão é externa
anon_t2_perf_sum_api <- performance_overview_single_config(anon_api, "t2-ext-sec")
print(anon_t2_perf_sum_api)

#Gera gráfico com todas as configurações da bilbioteca
generate_line_plots(anon_library)

#Comparação das configurações da API
generate_api_line_plots(anon_api)

#Um gráfico geral para cada payload
generate_config_parpay_plot(anon_api, "t2-ext-sec", 1)
generate_config_parpay_plot(anon_api, "t2-ext-sec", 2)
generate_config_parpay_plot(anon_api, "t2-ext-sec", 3)

cat("Finalizado em: ", format(current_time, "%Y-%m-%d %H:%M:%OS3"))


# Função para ler arquivos CSV com configurações brasileiras
read_brazilian_csv <- function(file_path) {
  # Usa o read.csv2 para ler arquivos com separadores de ponto e vírgula
  read.csv2(file_path, stringsAsFactors = FALSE)
}

# Função para extrair informações do nome do arquivo, como algoritmo, n e k
extract_file_info <- function(file_name) {
  # Divide o nome do arquivo por '_'
  parts <- unlist(strsplit(file_name, '_'))
  algorithm <- parts[2]
  nk_parts <- unlist(strsplit(parts[3], '-'))
  n <- nk_parts[1]
  k <- nk_parts[2]
  # Extrai números de partes do nome do arquivo
  ds <- as.numeric(gsub("[^0-9]", "", parts[4]))
  th <- as.numeric(gsub("[^0-9]", "", parts[5]))
  return(list(algorithm = algorithm, n = n, k = k, ds=ds, th=th))
}

# Função para renomear colunas conforme as traduções especificadas
rename_columns <- function(df) {
  # Renomeia colunas para nomes em inglês
  colnames(df)[colnames(df) == 'registro'] <- 'regid'
  colnames(df)[colnames(df) == 'número.de.chaves'] <- 'kused'
  colnames(df)[colnames(df) == 'tempo1'] <- 'instance1'
  colnames(df)[colnames(df) == 'tempo2'] <- 'instance2'
  colnames(df)[colnames(df) == 'tempo3'] <- 'instance3'
  colnames(df)[colnames(df) == 'tempo4'] <- 'instance4'
  colnames(df)[colnames(df) == 'tempo5'] <- 'instance5'
  return(df)
}

import_split_data <- function(config) {
  # Localiza todos os arquivos CSV na subpasta 'split' da pasta determinada pela configuração.
  files <- list.files(paste0(data_path, config, '/split'), full.names = TRUE, pattern = '*.csv')
  
  # Aplica a função anônima em cada arquivo listado para ler e processar os dados
  data_list <- lapply(files, function(file) {
    # Extrai informações do nome do arquivo usando a função `extract_file_info`
    info <- extract_file_info(basename(file))
    
    # Lê o arquivo CSV usando a função que considera as configurações brasileiras
    df <- read_brazilian_csv(file)
    
    # Inicializa ou atualiza colunas no dataframe com base nas informações extraídas
    df$algorithm <- info$algorithm
    df$n <- as.integer(info$n)
    df$k <- as.integer(info$k)
    df$ds <- info$ds
    df$th <- info$th
    df$config <- config
    
    # Renomeia as colunas do dataframe conforme especificado na função `rename_columns`
    df <- rename_columns(df)
    
    # Retorna o dataframe processado
    return(df)
  })
  
  # Combina todas as listas de dataframes em um único dataframe
  return(dplyr::bind_rows(data_list))
}

# Função para importar dados da operação reconstruct
import_reconstruct_data <- function(config) {
  # Localiza todos os arquivos CSV na subpasta 'reconstruct' da pasta determinada pela configuração.
  files <- list.files(paste0(data_path, config, '/reconstruct'), full.names = TRUE, pattern = '*.csv')
  
  # Aplica a função anônima em cada arquivo listado para ler e processar os dados
  data_list <- lapply(files, function(file) {
    # Lê o arquivo CSV usando a função que considera as configurações brasileiras
    df <- read_brazilian_csv(file)
    
    # Extrai informações do nome do arquivo usando a função `extract_file_info`
    info <- extract_file_info(basename(file))
    
    # Adiciona ou atualiza colunas no dataframe com base nas informações extraídas
    df$algorithm <- info$algorithm
    df$n <- as.numeric(info$n)
    df$k <- as.numeric(info$k)
    df$ds <- as.numeric(info$ds)
    df$th <- as.numeric(info$th)
    df$config <- config
    
    # Se a coluna "kused" existir no dataframe, ela é convertida para inteiro
    if("kused" %in% colnames(df)) {
      df$kused <- as.integer(df$kused)  # Converte kused para inteiro
    }
    
    # Renomeia as colunas do dataframe conforme especificado na função `rename_columns`
    df <- rename_columns(df)
    
    # Reordena as colunas do dataframe para uma sequência específica
    df <- df[, c('config', 'algorithm', 'n', 'k', 'kused', 'ds', 'th', colnames(df)[!colnames(df) %in% c('config', 'algorithm', 'n', 'k', 'kused','ds', 'th')])]
    
    # Retorna o dataframe processado
    return(df)
  })
  
  # Combina todas as listas de dataframes em um único dataframe
  return(dplyr::bind_rows(data_list))
}


# Função para remover outliers usando o método IQR (Intervalo Interquartil)
remove_outliers_iqr <- function(df, grouping_vars) {
  
  # Define uma função interna para calcular os limites inferior e superior do IQR para um vetor dado
  calculate_bounds <- function(x) {
    # Calcula o primeiro quartil (25%)
    Q1 <- quantile(x, 0.25, na.rm = TRUE)
    # Calcula o terceiro quartil (75%)
    Q3 <- quantile(x, 0.75, na.rm = TRUE)
    # Calcula o Intervalo Interquartil (IQR)
    IQR <- Q3 - Q1
    # Define o limite inferior (Q1 - 1,5 * IQR)
    lower_bound <- Q1 - 1.5 * IQR
    # Define o limite superior (Q3 + 1,5 * IQR)
    upper_bound <- Q3 + 1.5 * IQR
    # Retorna os limites como um vetor
    c(lower_bound, upper_bound)
  }
  
  # Itera sobre cada coluna de instância (tempo de execução para diferentes instâncias)
  for (instance_col in c("instance1", "instance2", "instance3", "instance4", "instance5")) {
    # Para cada grupo (definido pelas variáveis de agrupamento), calcula os limites do IQR
    bounds <- df %>%
      group_by(across(all_of(grouping_vars))) %>%
      summarize(lower = calculate_bounds(get(instance_col))[1],
                upper = calculate_bounds(get(instance_col))[2],
                .groups = 'drop')
    
    # Une o dataframe original com os limites calculados usando uma junção à esquerda
    df <- df %>%
      left_join(bounds, by = grouping_vars) %>%
      # Filtra linhas onde o valor da coluna de instância está dentro dos limites do IQR
      filter(get(instance_col) >= lower, get(instance_col) <= upper) %>%
      # Remove as colunas de limites do resultado final
      select(-c(lower, upper))
  }
  
  # Retorna o dataframe após a remoção de outliers
  return(df)
}

# Função para remover outliers usando o Z-Score
remove_outliers_zscore_grouped <- function(df, grouping_vars, z_threshold = 2) {
  
  # Define uma função interna para calcular os z-scores de um vetor dado
  # O z-score indica quantos desvios padrões um valor está em relação à média
  calculate_zscore <- function(x) {
    (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
  }
  
  # Itera sobre cada coluna de instância (tempo de execução para diferentes instâncias)
  for (instance_col in c("instance1", "instance2", "instance3", "instance4", "instance5")) {
    # Para cada grupo (definido pelas variáveis de agrupamento), calcula os z-scores
    z_scores <- df %>%
      group_by(across(all_of(grouping_vars))) %>%
      mutate(z_score = calculate_zscore(get(instance_col))) %>%
      ungroup() %>%
      pull(z_score)
    
    # Filtra linhas onde o valor absoluto do z-score é menor ou igual ao limiar (z_threshold)
    df <- df %>%
      filter(abs(z_scores) <= z_threshold)
  }
  
  # Lista de colunas que devem ser convertidas para numérico
  numeric_cols <- c("n", "k", "th", "ds")
  # Verifica se a coluna "kused" está presente no dataframe
  if ("kused" %in% colnames(df)) {
    numeric_cols <- c(numeric_cols, "kused")
  }
  
  # Converte as colunas especificadas para numérico
  df <- df %>%
    mutate(across(all_of(numeric_cols), as.numeric))
  
  # Retorna o dataframe após a remoção de outliers e conversão de colunas
  return(df)
}


# Função para sumarizar tempos
summarize_times <- function(df, grouping_fields) {
  df %>%
    # Agrupa os dados pelos campos especificados
    group_by(across(all_of(grouping_fields))) %>%
    # Calcula as estatísticas de resumo para os tempos
    summarise(
      mean_time = mean(c(instance1, instance2, instance3, instance4, instance5), na.rm = TRUE), # Média dos tempos
      sd_time = sd(c(instance1, instance2, instance3, instance4, instance5), na.rm = TRUE),     # Desvio padrão dos tempos
      median_time = median(c(instance1, instance2, instance3, instance4, instance5), na.rm = TRUE) # Mediana dos tempos
    )
}

# Função para calcular o resumo para tempos médios
compute_avg_time_summary <- function(dataset) {
  # Remove qualquer agrupamento do conjunto de dados
  dataset <- ungroup(dataset)
  # Calcula o tempo médio para cada linha
  avg_time <- rowMeans(select(dataset, starts_with("instance")), na.rm = TRUE)
  # Retorna as estatísticas de resumo para os tempos médios
  return(summary(avg_time))
}

# Função para calcular os tamanhos dos conjuntos de dados
calculate_dataset_sizes <- function() {
  # Lista todos os objetos no ambiente global
  all_objects <- ls(envir = .GlobalEnv)
  # Filtra apenas os dataframes
  data_frames <- all_objects[sapply(all_objects, function(x) is.data.frame(get(x, envir = .GlobalEnv)))]
  # Calcula os tamanhos em MB para cada dataframe
  sizes_in_MB <- sapply(data_frames, function(x) {
    size_in_bytes <- object.size(get(x, envir = .GlobalEnv))
    size_in_MB <- size_in_bytes / (1024^2)
    return(size_in_MB)
  })
  # Retorna os tamanhos em MB
  return(sizes_in_MB)
}

# Função para calcular o desempenho do algoritmo
compute_algorithm_performance <- function(df) {
  performance_overview <- df %>%
    # Agrupa os dados pelo algoritmo
    group_by(algorithm) %>%
    # Calcula as métricas de desempenho para cada algoritmo
    summarise(
      `Best Performance (ms)` = min(mean_time, na.rm = TRUE),  # Melhor desempenho
      `Median Performance (ms)` = median(mean_time, na.rm = TRUE), # Desempenho mediano
      `Worst Performance (ms)` = max(mean_time, na.rm = TRUE)  # Pior desempenho
    )
  
  # Retorna o resumo do desempenho
  return(performance_overview)
}

# Função para calcular o desempenho por configuração
compute_config_performance <- function(df) {
  # Calcula métricas de desempenho agrupadas por configuração
  config_performance <- df %>%
    group_by(config) %>%
    summarise(
      Average_Time = mean(mean_time, na.rm = TRUE),      # Tempo médio
      Median_Time = median(mean_time, na.rm = TRUE),     # Tempo mediano
      Minimum_Time = min(mean_time, na.rm = TRUE),       # Tempo mínimo
      Maximum_Time = max(mean_time, na.rm = TRUE),       # Tempo máximo
      Standard_Deviation = sd(mean_time, na.rm = TRUE)   # Desvio padrão do tempo
    ) %>%
    # Ordena os resultados pelo tempo médio em ordem decrescente
    arrange(desc(Average_Time))
  
  # Retorna o resumo do desempenho por configuração
  return(config_performance)
}

# Função para fornecer uma visão geral do desempenho para todas as configurações
performance_overview_all_configs <- function(df) {
  
  # Função interna para extrair as linhas baseadas no desempenho (melhor, mediano ou pior)
  extract_rows <- function(df, metric) {
    if (metric == "best") {
      idx <- df %>%
        group_by(algorithm) %>%
        slice(which.min(mean_time))
    } else if (metric == "median") {
      idx <- df %>%
        group_by(algorithm) %>%
        slice(which.min(abs(mean_time - median(mean_time))))
    } else if (metric == "worst") {
      idx <- df %>%
        group_by(algorithm) %>%
        slice(which.max(mean_time))
    }
    return(idx)
  }
  
  # Extração das linhas com base nos critérios
  best_rows <- extract_rows(df, "best")
  median_rows <- extract_rows(df, "median")
  worst_rows <- extract_rows(df, "worst")
  
  # Concatenando os resultados para criar um dataframe abrangente
  performance_overview <- bind_rows(
    mutate(best_rows, Performance = "Best"),
    mutate(median_rows, Performance = "Median"),
    mutate(worst_rows, Performance = "Worst")
  ) %>%
    select(Performance, everything()) %>%  # Mova Performance para a primeira coluna
    relocate(config, .after = algorithm) %>%  # Mova config para a terceira coluna
    mutate(across(c(mean_time, sd_time, median_time), round, 2))  # Arredonde as colunas especificadas para 2 casas decimais
  
  return(performance_overview)
}

# Função para fornecer uma visão geral do desempenho para uma única configuração
performance_overview_single_config <- function(df, config_name) {
  
  # Extraindo as melhores, medianas e piores linhas com base no tempo médio para cada algoritmo
  best_rows <- df %>% filter(config == config_name) %>%
    group_by(algorithm) %>%
    slice(which.min(mean_time)) %>%
    mutate(Performance = "Best")
  
  median_rows <- df %>% filter(config == config_name) %>%
    group_by(algorithm) %>%
    slice(which.min(abs(mean_time - median(mean_time)))) %>%
    mutate(Performance = "Median")
  
  worst_rows <- df %>% filter(config == config_name) %>%
    group_by(algorithm) %>%
    slice(which.max(mean_time)) %>%
    mutate(Performance = "Worst")
  
  # Combinando os resultados
  combined_df <- bind_rows(best_rows, median_rows, worst_rows) %>%
    select(Performance, algorithm, n, k, ds, th, mean_time, sd_time, median_time)
  
  return(combined_df)
}

# Função para calcular o desempenho com relação a uma configuração de referência
compute_ref_performance <- function(df, reference_config) {
  # Extraia combinações de parâmetros da configuração de referência especificada
  ref_params <- df %>%
    filter(config == reference_config) %>%
    select(algorithm, n, k, ds, th)
  
  # Filtra linhas no dataframe com base nos parâmetros da configuração de referência
  filtered_data <- df %>%
    semi_join(ref_params, by = c("algorithm", "n", "k", "ds", "th"))
  
  # Calcule métricas de desempenho agrupadas por configuração
  config_performance <- filtered_data %>%
    group_by(config) %>%
    summarise(
      Average_Time = mean(mean_time, na.rm = TRUE),
      Median_Time = median(mean_time, na.rm = TRUE),
      Minimum_Time = min(mean_time, na.rm = TRUE),
      Maximum_Time = max(mean_time, na.rm = TRUE),
      Standard_Deviation = sd(mean_time, na.rm = TRUE)
    ) %>%
    arrange(desc(Average_Time))
  
  # Retorna o resultado
  return(config_performance)
}

# Função para filtrar linhas onde a configuração é "c3-lib-xeon-e2378g" (para ser usada como linha de base)
filter_baseline_lib <- function(df) {
  filtered_df <- df %>% filter(config == "c3-lib-xeon-e2378g")
  return(filtered_df)
}

# Função para calcular estatísticas descritivas entre a biblioteca e a API
compare_lib_API_T1 <-  function(split_api, reconstruct_api, split_baseline, reconstruct_baseline) {
  
  # Filtra para as configurações relevantes
  split_api_local <- subset(split_api, config == "t1-api-local")
  reconstruct_api_local <- subset(reconstruct_api, config == "t1-api-local")
  
  # Estatísticas descritivas para mean_time
  stats_split_api_local <- summary(split_api_local$mean_time)
  stats_reconstruct_api_local <- summary(reconstruct_api_local$mean_time)
  stats_split_baseline <- summary(split_baseline$mean_time)
  stats_reconstruct_baseline <- summary(reconstruct_baseline$mean_time)
  
  # Calcule a diferença entre os tempos da API e da biblioteca
  difference_split <- as.vector(stats_split_api_local) - as.vector(stats_split_baseline)
  difference_reconstruct <- as.vector(stats_reconstruct_api_local) - as.vector(stats_reconstruct_baseline)
  
  # Combine estatísticas e diferenças em um dataframe
  combined_stats <- data.frame(
    Statistic = names(stats_split_api_local),
    split_api_local = as.vector(stats_split_api_local),
    reconstruct_api_local = as.vector(stats_reconstruct_api_local),
    split_baseline = as.vector(stats_split_baseline),
    reconstruct_baseline = as.vector(stats_reconstruct_baseline),
    api_vs_lib_difference_split = difference_split,
    api_vs_lib_difference_reconstruct = difference_reconstruct
  )
  
  return(combined_stats)
}

# Função para plotar dados
plot_data <- function(data, title, colors, n, k, ds, th, kused) {
  
  # Cria um gráfico de barras
  ggplot(data, aes(x=algorithm, y=mean_time, fill=config)) +
    geom_bar(stat="identity", position=position_dodge(width=0.9)) +
    labs(title=title,
         subtitle=sprintf("Parâmetros: n=%s, k=%s, ds=%s, th=%s, kused=%s", n, k, ds, th, kused),
         x="Algoritmo", y="Média (ms)") +
    theme_minimal() +
    theme(legend.position="top") +
    scale_fill_manual(values=colors, name="Comparação") +
    geom_text(aes(label=sprintf("%.2f", mean_time)), vjust=-1.0, position=position_dodge(width=0.9), size=3.5)
}

# Função para plotar comparações entre a biblioteca e a API
plot_comparison <- function(split_library, reconstruct_library, split_api, reconstruct_api, 
                            pn, pk, pds, pth, pkused, title, 
                            config_lib, 
                            config_api1, 
                            config_api2,
                            colors) {
  
  # Filtração dos dados
  split_lib_filtered <- split_library %>%
    filter(config == config_lib, n == pn, k == pk, ds == pds, th == pth)
  
  reconstruct_lib_filtered <- reconstruct_library %>%
    filter(config == config_lib, n == pn, k == pk, ds == pds, th == pth, kused == pkused)
  
  split_api1_filtered <- split_api %>%
    filter(config == config_api1, n == pn, k == pk, ds == pds, th == pth)
  
  reconstruct_api1_filtered <- reconstruct_api %>%
    filter(config == config_api1, n == pn, k == pk, ds == pds, th == pth, kused == pkused)
  
  split_api2_filtered <- split_api %>%
    filter(config == config_api2, n == pn, k == pk, ds == pds, th == pth)
  
  reconstruct_api2_filtered <- reconstruct_api %>%
    filter(config == config_api2, n == pn, k == pk, ds == pds, th == pth, kused == pkused)
  
  # Combinando dados filtrados
  split_combined <- bind_rows(
    split_lib_filtered %>% select(algorithm, mean_time, config),
    split_api1_filtered %>% select(algorithm, mean_time, config),
    split_api2_filtered %>% select(algorithm, mean_time, config)
  )
  
  reconstruct_combined <- bind_rows(
    reconstruct_lib_filtered %>% select(algorithm, mean_time, config),
    reconstruct_api1_filtered %>% select(algorithm, mean_time, config),
    reconstruct_api2_filtered %>% select(algorithm, mean_time, config)
  )
  
  # Mudança para melhor uso na legenda, extraindo os dois primeiros caracteres e tornando-os maiúsculos
  split_combined <- split_combined %>%
    mutate(config = substr(config, 1, 2) %>% toupper())
  
  reconstruct_combined <- reconstruct_combined %>%
    mutate(config = substr(config, 1, 2) %>% toupper())
  
  unique_configs <- unique(split_combined$config)
  names(colors) <- unique_configs[1:3]
  # Plotagem
  complete_title <- paste(title, "Split")
  print(plot_data(split_combined, complete_title, colors, pn, pk, pds, pth, pkused))
  complete_title <- paste(title, "Reconstruct")
  print(plot_data(reconstruct_combined, complete_title, colors, pn, pk, pds, pth, pkused))
}
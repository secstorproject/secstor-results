# Função para ler arquivos CSV com configurações brasileiras
read_brazilian_csv <- function(file_path) {
  # Usa o read.csv2 para ler arquivos com separadores de ponto e vírgula
  read.csv2(file_path, stringsAsFactors = FALSE)
}

# Função para extrair informações do nome do arquivo, como threads, parâmetros, payload
extract_file_info <- function(file_name) {
  
  parts <- unlist(strsplit(file_name, '_'))
  
  threads <- as.numeric(parts[which(parts == "threads") - 1])
  ds <- as.numeric(gsub("[^0-9]", "", parts[which(parts == "threads") + 1]))
  
  if ("parameters" %in% parts) {
    parpay <- as.numeric(gsub("[^0-9]", "", parts[which(parts == "parameters") + 1]))
    return(list(threads = threads, ds = ds, parpay = parpay))
  }
  
  if ("payload" %in% parts) {
    parpay <- as.numeric(parts[which(parts == "payload") + 1])
    request <- sub("\\..*$", "", parts[which(parts == "payload") + 2])
    return(list(threads = threads, ds = ds, parpay = parpay, request = request))
  }
  
  stop("Invalid file name format.")
}

import_data <- function(config) {
  # Localiza todos os arquivos CSV na subpasta 'split' da pasta determinada pela configuração.
  files <- list.files(paste0(data_path, config), full.names = TRUE, pattern = '*.csv')
  
  # Aplica a função anônima em cada arquivo listado para ler e processar os dados
  data_list <- lapply(files, function(file) {
    # Extrai informações do nome do arquivo usando a função `extract_file_info`
    info <- extract_file_info(basename(file))
    
    # Lê o arquivo CSV usando a função que considera as configurações brasileiras
    df <- read_brazilian_csv(file)
    
    # Inicializa ou atualiza colunas no dataframe com base nas informações extraídas
    df <- rename_columns(df)
    df$config <- config
    df$threads <- info$threads
    df$ds <- info$ds
    df$parpay <- info$parpay
    df$requests <- ifelse(!is.null(info$request), info$request, NA)
    
    # Reordena as colunas do dataframe
    columns_order <- c("config", "threads", "ds", "parpay", "requests", setdiff(colnames(df), c("config", "threads", "ds", "parpay", "requests")))
    df <- df[, columns_order]
    
    # Retorna o dataframe processado
    return(df)
  })

    # Combina todas as listas de dataframes em um único dataframe
  dplyr::bind_rows(data_list)
}

# Função para renomear colunas conforme as traduções especificadas
rename_columns <- function(df) {
  colnames(df)[colnames(df) == 'registro'] <- 'regid'
  colnames(df)[colnames(df) == 'tempo1'] <- 'instance1'
  colnames(df)[colnames(df) == 'tempo2'] <- 'instance2'
  colnames(df)[colnames(df) == 'tempo3'] <- 'instance3'
  colnames(df)[colnames(df) == 'tempo4'] <- 'instance4'
  colnames(df)[colnames(df) == 'tempo5'] <- 'instance5'
  return(df)
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
  numeric_cols <- c("threads", "ds", "parpay")
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

# Função para calcular métricas de desempenho agrupadas por configuração
compute_config_performance <- function(df) {
  # Calcula métricas de desempenho agrupando por configuração
  config_performance <- df %>%
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

# Função para fornecer uma visão geral do desempenho para todas as configurações
performance_overview_all_configs <- function(df) {
  
  # Função interna para extrair as linhas baseadas no desempenho (melhor, mediano ou pior)
  extract_rows <- function(df, metric) {
    if (metric == "best") {
      idx <- df %>%
        group_by(config) %>%
        slice(which.min(mean_time))
    } else if (metric == "median") {
      idx <- df %>%
        group_by(parpay) %>%
        slice(which.min(abs(mean_time - median(mean_time))))
    } else if (metric == "worst") {
      idx <- df %>%
        group_by(parpay) %>%
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
    relocate(config, .after = parpay) %>%  # Mova config para a terceira coluna
    mutate(across(c(mean_time, sd_time, median_time), round, 2))  # Arredonde as colunas especificadas para 2 casas decimais
  
  return(performance_overview)
}

# Função para obter uma visão geral do desempenho para uma única configuração
performance_overview_single_config <- function(df, config_name) {
  
  # Extrai as linhas de melhor, mediano e pior desempenho com base no tempo médio para cada algoritmo
  best_rows <- df %>% filter(config == config_name) %>%
    group_by(parpay) %>%
    slice(which.min(mean_time)) %>%
    mutate(Performance = "Best")
  
  median_rows <- df %>% filter(config == config_name) %>%
    group_by(parpay) %>%
    slice(which.min(abs(mean_time - median(mean_time)))) %>%
    mutate(Performance = "Median")
  
  worst_rows <- df %>% filter(config == config_name) %>%
    group_by(parpay) %>%
    slice(which.max(mean_time)) %>%
    mutate(Performance = "Worst")
  
  # Combina os resultados
  combined_df <- bind_rows(best_rows, median_rows, worst_rows) %>%
    select(Performance, ds, parpay, mean_time, sd_time, median_time)
  
  return(combined_df)
}

# Função para filtrar a configuração de base "c3-lib-xeon-e2378g"
#Não foi utilizado
filter_baseline_lib <- function(df) {
  # Filtra as linhas onde a configuração é "c3-lib-xeon-e2378g"
  filtered_df <- df %>% filter(config == "c3-lib-xeon-e2378g")
  return(filtered_df)
}

# Função para comparar o desempenho entre biblioteca (library) e API para o cenário T1
# Os valores foram muito discrepantes, não foi utilizado
# A implementação do multithreading para o teste da biblioteca não foi eficiente
compare_lib_API_T1 <-  function(anon_api, anon_baseline) {
  
  # Filtra os dados relevantes da configuração
  anon_api_local <- subset(anon_api, config == "t1-lan-sec")
  
  # Estatísticas descritivas para mean_time
  stats_anon_api_local <- summary(anon_api_local$mean_time)
  stats_anon_baseline <- summary(anon_baseline$mean_time)
  
  # Calcula a diferença entre os tempos da API e da biblioteca
  difference_anon <- as.vector(stats_anon_api_local) - as.vector(stats_anon_baseline)
  
  # Combina estatísticas e diferenças em um dataframe
  combined_stats <- data.frame(
    Statistic = names(stats_anon_api_local),
    anon_api_local = as.vector(stats_anon_api_local),
    anon_baseline = as.vector(stats_anon_baseline),
    api_vs_lib_difference_anon = difference_anon
  )
  
  return(combined_stats)
}

# Função para gerar gráficos de linha comparando os tempos de execução em relação ao tamanho dos conjuntos de dados para uma biblioteca
generate_line_plots <- function(anon_library) {
  
  # Obtém valores únicos de ds para os intervalos do eixo x
  ds_breaks <- unique(anon_library$ds)
  
  # Cria gráficos de linha
  plot <- ggplot(anon_library, aes(x = ds, y = mean_time, group = parpay, color = as.factor(parpay))) +
    geom_line() +
    geom_point() +
    scale_x_continuous(breaks = ds_breaks) +
    facet_wrap(~ config, scales = "free") +
    labs(title = "Comparação dos tempos vs Tamanho dos Datasets",
         x = "Tamanho do dataset (kb)",
         y = "Média (ms)",
         color = "Parpay") +
    theme_minimal()
  
  print(plot)
}

# Função para gerar gráficos de linha facetais para os dados da API
generate_api_line_plots <- function(anon_api) {
  plot <- ggplot(anon_api, aes(x = ds, y = mean_time, group = requests, color = requests)) +
    geom_line() +
    geom_point() +
    facet_wrap(~ config, scales = "free") +
    labs(title = "Comparação dos tempos vs Tamanho dos Datasets",
         x = "Tamanho do dataset (kb)",
         y = "Média (ms)",
         color = "Requisição") +
    theme_minimal()
  
  print(plot)
}

# Função para gerar um gráfico de barras com base na configuração e no valor parpay fornecidos
generate_config_parpay_plot <- function(anon_api_df, config_value, parpay_value) {
  # Filtra os dados com base nos valores de configuração e parpay fornecidos
  filtered_data <- subset(anon_api_df, config == config_value & parpay == parpay_value)
  
  # Gera o gráfico de barras
  plot <- ggplot(filtered_data, aes(x = as.factor(ds), y = mean_time, fill = requests)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_text(aes(label = round(mean_time, 2), group = requests), 
              position = position_dodge(width = 0.9), 
              vjust = -0.5, size = 3) +
    labs(title = paste0("Comparação dos tempos vs Tamanho dos Datasets (Payload=", parpay_value, ", ", config_value, ")"),
         x = "Tamanho do dataset (kb)",
         y = "Média (ms)",
         fill = "Requisição") +
    theme_minimal() +
    theme(
      axis.title.x = element_text(size = 10),
      axis.title.y = element_text(size = 10),
      axis.text.x = element_text(size = 8),
      axis.text.y = element_text(size = 8),
      legend.text = element_text(size = 8),
      legend.title = element_text(size = 10)
    )
  
  # Exibe o gráfico
  print(plot)
}
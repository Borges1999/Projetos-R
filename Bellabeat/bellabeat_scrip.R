# PACOTES ----
library(tidyverse)
library(lubridate)
library(dplyr)
library(ggplot2)
library(tidyr)

# IMPORTAÇÃO ----

## Importação Periodo 12/03/2016 à 12/04/2016
df_raw_atividade_diaria <- read.csv("dados_periodo_2016_03_12_a_2016_04_11/atividade_diaria.csv")
df_raw_calorias_horarias <- read.csv("dados_periodo_2016_03_12_a_2016_04_11/calorias_horarias.csv")
df_raw_intensidade_horaria <- read.csv("dados_periodo_2016_03_12_a_2016_04_11/intensidade_horaria.csv")
df_raw_passos_horarios <- read.csv("dados_periodo_2016_03_12_a_2016_04_11/passos_horarios.csv")
df_raw_peso_usuarios <- read.csv("dados_periodo_2016_03_12_a_2016_04_11/peso_usuarios.csv")



## Importação Periodo 12/04/2016 à 12/05/2016
df_raw_atividade_diaria_b <- read.csv("dados_periodo_2016_04_12_a_2016_05_12/atividade_diaria_b.csv")
df_raw_calorias_horarias_b <- read.csv("dados_periodo_2016_04_12_a_2016_05_12/calorias_horarias_b.csv")
df_raw_intensidade_horaria_b <- read.csv("dados_periodo_2016_04_12_a_2016_05_12/intensidade_horaria_b.csv")
df_raw_passos_horarios_b <- read.csv("dados_periodo_2016_04_12_a_2016_05_12/passos_horarios_b.csv")
df_raw_peso_usuarios_b <- read.csv("dados_periodo_2016_04_12_a_2016_05_12/peso_usuarios_b.csv")
df_raw_sono_diario_b <- read.csv("dados_periodo_2016_04_12_a_2016_05_12/sono_diario_b.csv")


# TRATAMENTO DOS DADOS ----

## Unindo as tabelas
df_stage_atividade <- bind_rows(
  df_raw_atividade_diaria,
  df_raw_atividade_diaria_b
)

df_stage_calorias_h <- bind_rows(
  df_raw_calorias_horarias,
  df_raw_calorias_horarias_b
)

df_stage_intensidade_h <- bind_rows(
  df_raw_intensidade_horaria,
  df_raw_intensidade_horaria_b
)

df_stage_passos_h <- bind_rows(
  df_raw_passos_horarios,
  df_raw_passos_horarios_b
)

df_stage_peso <- bind_rows(
  df_raw_peso_usuarios,
  df_raw_peso_usuarios_b
)



## Trocando nomes das colunas
df_stage_atividade <- df_stage_atividade %>%
  rename(
    id_usuario = Id,
    data = ActivityDate,
    total_passos = TotalSteps,
    distancia_total = TotalDistance,
    distancia_rastreador = TrackerDistance,
    distancia_atividades_registradas = LoggedActivitiesDistance,
    distancia_alta_ativa = VeryActiveDistance,
    distancia_moderada_ativa = ModeratelyActiveDistance,
    distancia_leve_ativa = LightActiveDistance,
    distancia_sedentaria_ativa = SedentaryActiveDistance,
    minutos_muito_ativos = VeryActiveMinutes,
    minutos_moderadamente_ativos = FairlyActiveMinutes,
    minutos_levemente_ativos = LightlyActiveMinutes,
    minutos_sedentarios = SedentaryMinutes,
    calorias = Calories
  )

df_stage_calorias_h <- df_stage_calorias_h %>%
  rename(
    id_usuario = Id,
    hora_da_atividade = ActivityHour,
    calorias = Calories
  )

df_stage_intensidade_h <- df_stage_intensidade_h %>%
  rename(
    id_usuario = Id,
    hora_da_atividade = ActivityHour,
    intensidade_total = TotalIntensity,
    intensidade_media = AverageIntensity
  )

df_stage_passos_h <- df_stage_passos_h %>%
  rename(
    id_usuario = Id,
    hora_da_atividade = ActivityHour,
    total_passos = StepTotal
  )

df_stage_peso <- df_stage_peso %>%
  rename(
    id_usuario = Id,
    data = Date,
    peso_em_kg = WeightKg,
    peso_em_libras = WeightPounds,
    gordura = Fat,
    IMC = BMI,
    relatótio_manual = IsManualReport,
    id_do_registro = LogId
  )

df_stage_sono_diario <- df_raw_sono_diario_b %>%
  rename(
    id_usuario = Id,
    data = SleepDay,
    total_registro_sono = TotalSleepRecords,
    total_minutos_dormidos = TotalMinutesAsleep,
    total_tempo_na_cama = TotalTimeInBed
  )


## Validação dos tipos de dados e identificação de valores nulos
glimpse(df_stage_atividade)

glimpse(df_stage_calorias_h)

glimpse(df_stage_intensidade_h)

glimpse(df_stage_passos_h)

glimpse(df_stage_peso)

glimpse(df_stage_sono_diario)


anyNA(df_stage_atividade)
colSums(is.na(df_stage_atividade))

anyNA(df_stage_calorias_h)
colSums(is.na(df_stage_calorias_h))

anyNA(df_stage_intensidade_h)
colSums(is.na(df_stage_intensidade_h))

anyNA(df_stage_passos_h)
colSums(is.na(df_stage_passos_h))

anyNA(df_stage_peso)
colSums(is.na(df_stage_peso))

anyNA(df_stage_sono_diario)
colSums(is.na(df_stage_sono_diario))


## Removendo data frames que já foram unificados
rm(df_raw_atividade_diaria)
rm(df_raw_atividade_diaria_b)
rm(df_raw_calorias_horarias)
rm(df_raw_calorias_horarias_b)
rm(df_raw_intensidade_horaria)
rm(df_raw_intensidade_horaria_b)
rm(df_raw_passos_horarios)
rm(df_raw_passos_horarios_b)
rm(df_raw_peso_usuarios)
rm(df_raw_peso_usuarios_b)
rm(df_raw_sono_diario_b)


## Conversão de tipos

## Alteração de chr para date
df_stage_atividade$data <-
  as.POSIXct(df_stage_atividade$data, 
             format = "%m/%d/%Y")

df_stage_calorias_h$hora_da_atividade <-
  as.POSIXct(df_stage_calorias_h$hora_da_atividade,
             format = "%m/%d/%Y %I:%M:%S %p")

df_stage_intensidade_h$hora_da_atividade <-
  as.POSIXct(df_stage_intensidade_h$hora_da_atividade,
             format = "%m/%d/%Y %I:%M:%S %p")

df_stage_passos_h$hora_da_atividade <-
  as.POSIXct(df_stage_passos_h$hora_da_atividade,
             format = "%m/%d/%Y %I:%M:%S %p")

df_stage_peso$data <-
  as.POSIXct(df_stage_peso$data,
             format = "%m/%d/%Y %I:%M:%S %p")

df_stage_sono_diario$data <-
  as.POSIXct(df_stage_sono_diario$data,
             format = "%m/%d/%Y %I:%M:%S %p")


## Alterando num para chr
df_stage_peso$id_do_registro <- 
  as.character(df_stage_peso$id_do_registro, format = "chr")


# ANÁLISE EXPLORATÓRIA ----

# Qtd. de usuárias usando a função de atividade física
n_distinct(df_stage_atividade$id_usuario)

# Qtd. de usuárias usando a função de calorias
n_distinct(df_stage_calorias_h$id_usuario)

# Qtd. de usuárias usando a função de intensidade
n_distinct(df_stage_intensidade_h$id_usuario)

# Qtd. de usuárias usando a função de contagem de passos
n_distinct(df_stage_passos_h$id_usuario)

# Qtd. de usuárias usando a função de registro de peso
n_distinct(df_stage_peso$id_usuario)

# Qtd. de usuárias usando a função de registro de sono
n_distinct(df_stage_sono_diario$id_usuario)

# Validação do range de datas para validar se a unidão das tabelas de dois períodos foi bem sucedida
range(as.Date(df_stage_atividade$data))


# ANÁLISE ESTATISTICA DESCRITIVA ----

df_stage_atividade %>%
  select(
    total_passos,
    distancia_total,
    minutos_sedentarios,
    calorias
  ) %>%
  summary()

df_stage_calorias_h %>%
  select(
    calorias
  ) %>%
  summary()

df_stage_passos_h %>%
  select(
    total_passos
  ) %>%
  summary()

df_stage_sono_diario %>%
  select(
    total_registro_sono,
    total_minutos_dormidos,
    total_tempo_na_cama
  ) %>%
  summary()

df_stage_peso %>%
  select(
    peso_em_kg,
    IMC
  ) %>%
  summary()


# MÉTRICAS ----

## Joins e data frames resumo

# Agregando total de passos por usuária
df_metric_total_passos_usuario <- df_stage_atividade %>%
  group_by(id_usuario) %>%
  summarise(total_passos = sum(total_passos, na.rm = TRUE)) %>%
  arrange(desc(total_passos))

# TOP 5 usuárias com maior número de passos
df_metric_top_5_total_passos <- df_stage_atividade %>%
  group_by(id_usuario) %>%
  summarise(total_passos = sum(total_passos, na.rm = TRUE)) %>%
  arrange(desc(total_passos)) %>%
  slice_head(n = 5)


# TOP 5 usuárias com menor número de passos
df_metric_top_5_menor_total_passos <- df_stage_atividade %>%
  group_by(id_usuario) %>%
  summarise(total_passos = sum(total_passos, na.rm = TRUE)) %>%
  arrange(total_passos) %>%
  slice_head(n = 5)


# Agregação média colunas da tabela df_stage_atividade
df_metric_media_atividade <- df_stage_atividade %>%
  group_by(id_usuario) %>%
  summarise(
    media_passos_dia = round(mean(total_passos, na.rm = TRUE)),
    media_calorias_dia = round(mean(calorias, na.rm = TRUE)),
    media_minu_ativo_dia = round(mean(minutos_muito_ativos, na.rm = TRUE)),
    media_minu_sedentario_dia = round(mean(minutos_sedentarios, na.rm = TRUE))
  ) %>%
  arrange(desc(media_passos_dia))


# Média de passos por hora agrupada por usuária
df_metric_media_passos_hora <- df_stage_passos_h %>%
  group_by(id_usuario) %>%
  summarise(
    media_passos_h = round(mean(total_passos, na.rm = TRUE))
  ) %>%
  arrange(desc(media_passos_h))


# Média de minutos de sono e na cama e criação de 2 colunas convertendo minutos em hora
df_metric_media_sono <- df_stage_sono_diario %>%
  group_by(id_usuario) %>%
  summarise(
    media_minutos_sono = round(mean(total_minutos_dormidos, na.rm = TRUE)),
    media_minutos_cama = round(mean(total_tempo_na_cama, na.rm = TRUE)),
    media_hora_sono = round(media_minutos_sono/60),
    media_hora_cama = round(media_minutos_cama/60)
  ) %>%
  arrange(desc(media_hora_sono))

# Categorizando a qualidade do sono com base no tempo médio de sono em horas
df_metric_classificacao_do_sono <- df_metric_media_sono %>%
  mutate(faixa_sono = case_when(
    media_hora_sono < 5 ~ "Sono insuficiente",
    media_hora_sono < 6 ~ "Sono curto",
    media_hora_sono < 7 ~ "Sono regular",
    media_hora_sono < 9 ~ "Sono adequado",
    TRUE ~ "Sono prolongado"
  )) %>%
  count(faixa_sono) %>%
  arrange(desc(n))
  

# Criando df para cruzamento de dados e analise de correlação entre minutos em atividade fisic. vs sono
df_metric_sono_minut_ativo <- df_metric_media_sono %>%
  inner_join(df_metric_media_atividade, by = "id_usuario") %>%
  arrange(desc(media_hora_sono))


# Média de gasto calórico hora por usuária
df_metric_calorias_usuario <- df_stage_calorias_h %>%
  group_by(id_usuario) %>%
  summarise(
    media_calorias_h = round(mean(calorias, na.rm = TRUE))
  ) %>%
  arrange(desc(media_calorias_h))

# Média de intensidade hora por usuária
df_metric_intensidade_usuario <- df_stage_intensidade_h %>%
  group_by(id_usuario) %>%
  summarise(
    media_intensidade = round(mean(intensidade_total, na.rm = TRUE))
  ) %>%
  arrange(desc(media_intensidade))

# Criando df para cruzamento de dados e analise de correlação entre intensidade vs gasto calórico
df_metric_calorias_intensidade <- df_metric_calorias_usuario %>%
  inner_join(df_metric_intensidade_usuario, by = "id_usuario")

# Agregação de dias ativos no app por usuária
df_metric_dias_ativos_por_usuario <- df_stage_atividade %>%
  group_by(id_usuario) %>%
  summarise(
    dias_ativos_usuario = n_distinct(data)
  ) %>%
  arrange(desc(dias_ativos_usuario))

# Criação de faixas de dias ativos
df_metric_dias_ativos_faixa <- df_metric_dias_ativos_por_usuario %>%
  mutate(faixa_dias = case_when(
    dias_ativos_usuario <= 38 ~ "Baixo engajamento",
    dias_ativos_usuario <= 42 ~ "Médio engajamento",
    TRUE ~ "Alto engajamento"
  )) %>%
  count(faixa_dias) %>%
  arrange(desc(n))
  

# Classificação dos tipos de usuárias
df_metric_tipos_de_usuarios <- df_metric_media_atividade %>%
  mutate(
    perfil_usuario = case_when(
      media_passos_dia < 5000 ~ "Sedentário",
      media_passos_dia < 7500 ~ "Pouco ativo",
      media_passos_dia < 10000 ~ "Moderadamente ativo",
      TRUE ~ "Muito ativo"
    )
  )


# Representação em % dos tipos de usuárias
df_metric_percent_perfil_usuarios <- df_metric_tipos_de_usuarios %>%
  group_by(perfil_usuario) %>%
  summarise(
    qtd_usuarios = n_distinct(id_usuario),
  ) %>%
  mutate(
    percentual = round(qtd_usuarios / sum(qtd_usuarios) * 100, 1)
  ) %>%
  arrange(desc(qtd_usuarios))


# Méticas de peso e IMC
df_metric_peso_imc <- df_stage_peso %>%
  group_by(id_usuario) %>%
  summarise(
    peso_em_kg = round(mean(peso_em_kg, na.rm = TRUE), 2),
    IMC = round(mean(IMC, na.rm = TRUE), 2)
  ) %>%
  mutate(
    classificacao = case_when(
      IMC < 18.5 ~ "Abaixo do peso",
      IMC < 24.9 ~ "Peso normal",
      IMC < 29.9 ~ "Sobrepeso",
      IMC > 30 ~ "Obesidade"
  )) %>%
  arrange(desc(IMC))


# Percentual IMC
df_metric_percent_imc <- df_metric_peso_imc %>%
  group_by(classificacao) %>%
  summarise(
    total_usuarios = n_distinct(id_usuario)
  ) %>%
  mutate(
    percentual = round(total_usuarios / sum(total_usuarios) * 100, 1)
  ) %>%
  arrange(desc(percentual))


# VISUALIZAÇÕES ----


#Distribuição de Usuárias por Perfil de Atividade
ggplot(
  df_metric_percent_perfil_usuarios,
  aes(
    x = reorder(perfil_usuario, -qtd_usuarios),
    y = qtd_usuarios,
    fill = perfil_usuario == "Sedentário"
  )) +
  geom_col(width = 0.6) +
  geom_text(aes(label = qtd_usuarios), vjust = -0.4, size = 4) +
  scale_fill_manual(
    values = c("TRUE" = "#D4D32B", "FALSE" = "grey70"),
    guide = "none"
  ) +
  labs(
    title = "Distribuição de Usuárias por Perfil de Atividade",
    subtitle = "Maior concentração de usuárias no perfil Sedentário",
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 12)+
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

# Tabela auxíliar de percentual 
df_metric_percent_perfil_usuarios %>%
  select(
    perfil_usuario,
    percentual
  )


# Distribuição percentual de usuárias por classificação de peso
ggplot(
  df_metric_percent_imc,
  aes(
    x = "",
    y = percentual,
    fill = reorder(classificacao, -percentual)
  )) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_text(
    aes(label = paste0(percentual, "%")),
    position = position_stack(vjust = 0.5),
    size = 4,
    color = "white"
  ) +
  scale_fill_manual(
    values = c(
      "#D4D32B",  
      "#006592",  
      "#e35c1c"
    )
  ) +
  labs(
    title = "Distribuição Percentual de Usuárias por Classificação de Peso",
    subtitle = "Usuárias com Sobrepeso representam a maior parcela da base",
    fill = NULL
  ) +
  theme_void(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  )

# Tabela auxíliar de qtd
df_metric_percent_imc %>%
  select(
    classificacao,
    total_usuarios
  )


# Distribuição de Usuárias por Dias Ativos no App

df_metric_dias_ativos_por_usuario %>%
  select(
    dias_ativos_usuario
  ) %>%
  summary()

ggplot(df_metric_dias_ativos_faixa, 
       aes(
         x = reorder(faixa_dias, -n), 
         y = n,
         fill = faixa_dias == "Médio engajamento"
       )) +
  geom_col(width = 0.6) +
  geom_text(aes(label = n), vjust = -0.4, size = 4) +
  scale_fill_manual(
    values = c("TRUE" = "#006592", "FALSE" = "grey70"),
    guide = "none"
  ) +
  labs(
    title = "Distribuição de Usuárias por Dias Ativos no App",
    subtitle = "Maior concentração de usuárias com Engajamento Médio",
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 12)+
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )


# Top 5 Usuárias com Maior número de Passos
ggplot(df_metric_top_5_total_passos, 
       aes(
         x = reorder(id_usuario, total_passos), 
         y = total_passos,
         fill = id_usuario == "8877689391"
       )) +
  geom_col(width = 0.6) +
  geom_text(
    aes(label = scales::label_number(big.mark = ".", decimal.mark = ",")(total_passos)),
    hjust = -0.2
  ) +
  coord_flip() +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.15))
  ) +
  scale_fill_manual(
    values = c("TRUE" = "#006592", "FALSE" = "grey70"),
    guide = "none"
  ) +
  labs(
    title = "Top 5 Usuárias com Maior número de Passos",
    x = "Usuária",
    y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    
  )


# Top 5 Usuárias com Menor número de Passos
ggplot(df_metric_top_5_menor_total_passos, 
       aes(
         x = reorder(id_usuario, -total_passos), 
         y = total_passos,
         fill = id_usuario == "2891001357"
       )) +
  geom_col(width = 0.6) +
  geom_text(
    aes(label = scales::label_number(big.mark = ".", decimal.mark = ",")(total_passos)),
    hjust = -0.2
  ) +
  coord_flip() +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.15))
  ) +
  scale_fill_manual(
    values = c("TRUE" = "#E74C3C", "FALSE" = "grey70"),
    guide = "none"
  ) +
  labs(
    title = "Top 5 Usuárias com Menor número de Passos",
    subtitle = NULL,
    x = "Usuária",
    y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )


## Analises de Correlações

# Passos vs Calorias
ggplot(
  df_metric_media_atividade,
  aes(
    x = media_passos_dia, 
    y = media_calorias_dia
  )) +
  geom_point(
    color = "#2C3E50",
    size = 2.5,
    alpha = 0.7
  ) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    color = "#E74C3C",
    linewidth = 1
  ) +
  labs(
    title = "Relação entre Passos e Gasto Calórico",
    subtitle = "Correlação de Pearson (r = 0.43)",
    x = "Média de Passos por Dia",
    y = "Média Calorias por Dia"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
  )

cor(
  df_metric_media_atividade$media_passos_dia,
  df_metric_media_atividade$media_calorias_dia,
  use = "complete.obs"
)


# Atividade Física vs Calorias
ggplot(
  df_metric_media_atividade,
  aes(
    x = media_minu_ativo_dia, 
    y = media_calorias_dia
  )) +
  geom_point(
    color = "#2C3E50",
    size = 2.5,
    alpha = 0.7
  ) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    color = "#E74C3C",
    linewidth = 1
  ) +
  labs(
    title = "Relação entre Atividade Física e Gasto Calorico",
    subtitle = "Correlação de Pearson (r = 0.63)",
    x = "Média Minutos de Atividade por Dia",
    y = "Média Calorias por Dia"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
  )


cor(
  df_metric_media_atividade$media_minu_ativo_dia,
  df_metric_media_atividade$media_calorias_dia,
  use = "complete.obs"
)


# Intensidade vs Calorias
ggplot(
  df_metric_calorias_intensidade,
  aes(
    x = media_intensidade, 
    y = media_calorias_h
  )) +
  geom_point(
    color = "#2C3E50",
    size = 2.5,
    alpha = 0.7
  ) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    color = "#E74C3C",
    linewidth = 1
  ) +
  labs(
    title = "Relação entre Intensidade e Gasto Calórico",
    subtitle = "Correlação de Pearson (r = 0.51)",
    x = "Média Intensidade por Hora",
    y = "Média Calorias por Hora"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
  )

cor(
  df_metric_calorias_intensidade$media_calorias_h,
  df_metric_calorias_intensidade$media_intensidade,
  use = "complete.obs"
)


# Atividade Física vs Sono
ggplot(
  df_metric_sono_minut_ativo,
  aes(
    x = media_minu_ativo_dia, 
    y = media_hora_sono
  )) +
  geom_point(
    color = "#2C3E50",
    size = 2.5,
    alpha = 0.7
  ) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    color = "#E74C3C",
    linewidth = 1
  ) +
  labs(
    title = "Relação entre Atividade Física e Horas de Sono",
    subtitle = "Correlação de Pearson (r = -0.085)",
    x = "Média Minutos de Atividade por Dia",
    y = "Média Horas de sono por Dia"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
  )

cor(
  df_metric_sono_minut_ativo$media_minu_ativo_dia,
  df_metric_sono_minut_ativo$media_hora_sono
)


# Distribuição de Usuárias por Classificação da qualidade do Sono
ggplot(
  df_metric_classificacao_do_sono, 
  aes(
    x = reorder(faixa_sono, -n), 
    y = n,
    fill = faixa_sono == "Sono adequado"
  ))+
  geom_col()+
  geom_text(aes(label = n), vjust = -0.4, size = 4)+
  scale_fill_manual(
    values = c("TRUE" = "#006592", "FALSE" = "grey70"),
    guide = "none"
  )+
  labs(
    title = "Distribuição de Usuárias por Classificação da qualidade do Sono",
    subtitle = "Maior concentração de usuárias categorizadas com Sono adequado",
    x = NULL,
    y = NULL
  )+
  theme_minimal(base_size = 12)+
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )






# EXPORTANDO DADOS TRATADOS ----

write.csv(df_stage_atividade, 
          "C:/Users/gabri/OneDrive/Documentos/R/Bellabeat/dados_tratados/atividade.csv", 
          row.names = FALSE)

write.csv(df_stage_calorias_h, 
          "C:/Users/gabri/OneDrive/Documentos/R/Bellabeat/dados_tratados/calorias.csv", 
          row.names = FALSE)

write.csv(df_stage_intensidade_h, 
          "C:/Users/gabri/OneDrive/Documentos/R/Bellabeat/dados_tratados/intensidade.csv", 
          row.names = FALSE)

write.csv(df_stage_passos_h, 
          "C:/Users/gabri/OneDrive/Documentos/R/Bellabeat/dados_tratados/passos.csv", 
          row.names = FALSE)

write.csv(df_stage_peso, 
          "C:/Users/gabri/OneDrive/Documentos/R/Bellabeat/dados_tratados/peso.csv", 
          row.names = FALSE)

write.csv(df_stage_sono_diario, 
          "C:/Users/gabri/OneDrive/Documentos/R/Bellabeat/dados_tratados/sono.csv", 
          row.names = FALSE)


# EXPORTANDO MÉTRICAS ----

write.csv(df_metric_calorias_intensidade, 
          "C:/Users/gabri/OneDrive/Documentos/R/Bellabeat/metricas/calorias_intensidade.csv", 
          row.names = FALSE)

write.csv(df_metric_calorias_usuario, 
          "C:/Users/gabri/OneDrive/Documentos/R/Bellabeat/metricas/calorias_usuario.csv", 
          row.names = FALSE)

write.csv(df_metric_classificacao_do_sono, 
          "C:/Users/gabri/OneDrive/Documentos/R/Bellabeat/metricas/classificacao_do_sono.csv", 
          row.names = FALSE)

write.csv(df_metric_dias_ativos_faixa, 
          "C:/Users/gabri/OneDrive/Documentos/R/Bellabeat/metricas/dias_ativos_faixa.csv", 
          row.names = FALSE)

write.csv(df_metric_dias_ativos_por_usuario, 
          "C:/Users/gabri/OneDrive/Documentos/R/Bellabeat/metricas/dias_ativos_por_usuario.csv", 
          row.names = FALSE)

write.csv(df_metric_intensidade_usuario, 
          "C:/Users/gabri/OneDrive/Documentos/R/Bellabeat/metricas/intensidade_usuario.csv", 
          row.names = FALSE)

write.csv(df_metric_media_atividade, 
          "C:/Users/gabri/OneDrive/Documentos/R/Bellabeat/metricas/media_atividade.csv", 
          row.names = FALSE)

write.csv(df_metric_media_passos_hora, 
          "C:/Users/gabri/OneDrive/Documentos/R/Bellabeat/metricas/media_passos_hora.csv", 
          row.names = FALSE)

write.csv(df_metric_media_sono, 
          "C:/Users/gabri/OneDrive/Documentos/R/Bellabeat/metricas/media_sono.csv", 
          row.names = FALSE)

write.csv(df_metric_percent_imc, 
          "C:/Users/gabri/OneDrive/Documentos/R/Bellabeat/metricas/percent_imc.csv", 
          row.names = FALSE)

write.csv(df_metric_percent_perfil_usuarios, 
          "C:/Users/gabri/OneDrive/Documentos/R/Bellabeat/metricas/percent_perfil_usuarios.csv", 
          row.names = FALSE)

write.csv(df_metric_peso_imc, 
          "C:/Users/gabri/OneDrive/Documentos/R/Bellabeat/metricas/peso_imc.csv", 
          row.names = FALSE)

write.csv(df_metric_sono_minut_ativo, 
          "C:/Users/gabri/OneDrive/Documentos/R/Bellabeat/metricas/sono_minut_ativo.csv", 
          row.names = FALSE)

write.csv(df_metric_tipos_de_usuarios, 
          "C:/Users/gabri/OneDrive/Documentos/R/Bellabeat/metricas/tipos_de_usuarios.csv", 
          row.names = FALSE)

write.csv(df_metric_top_5_menor_total_passos, 
          "C:/Users/gabri/OneDrive/Documentos/R/Bellabeat/metricas/top_5_menor_total_passos.csv", 
          row.names = FALSE)

write.csv(df_metric_top_5_total_passos, 
          "C:/Users/gabri/OneDrive/Documentos/R/Bellabeat/metricas/top_5_total_passos.csv", 
          row.names = FALSE)

write.csv(df_metric_total_passos_usuario, 
          "C:/Users/gabri/OneDrive/Documentos/R/Bellabeat/metricas/total_passos_usuario.csv", 
          row.names = FALSE)
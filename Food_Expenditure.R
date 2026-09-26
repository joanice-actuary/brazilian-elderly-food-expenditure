# -----------------------------------------------------------------------------
# Despesa Média Mensal com Alimentação na Terceira Idade obtido através da 
# Pesquisa de Orçamentos Familiares - POF 2017-2018 elaborada pelo IBGE e 
# atualização dos valores para setembro de 2026.
# Autora: Joanice
# -----------------------------------------------------------------------------

# 1. Carregar Pacotes
library(tidyverse)   # Para manipulação de dados
library(survey)      # Pacote oficial considerado na memória de cálculo do IBGE
library(openxlsx)    # Biblioteca otimizada para criação de arquivos Excel (.xlsx)
library(geobr)       # Para o mapa do Brasil 
library(sf)          # Para o mapa do Brasil
library(ggtext)      # Para melhorar a aparência do gráfico

# 2. Carregar as bases salvas no processo de leitura dos dados
MORADOR            <- readRDS("MORADOR.rds")
DESPESA_INDIVIDUAL <- readRDS("DESPESA_INDIVIDUAL.rds") 
CADERNETA_COLETIVA <- readRDS("CADERNETA_COLETIVA.rds") 


# 3. Verificação da Análise Exploratória dos Dados (AED)

# Gera o resumo descritivo para cada uma das bases 
skimr::skim(MORADOR)
skimr::skim(DESPESA_INDIVIDUAL)
skimr::skim(CADERNETA_COLETIVA)

# Algumas variáveis apresentam dados ausentes, mas não foram tratados por não 
# integrarem as variáveis utilizadas na análise, exceto a V9011, cujo tratamento
# não se aplica devido ao fato de incluir várias despesas além de alimentação.


# 4. Definição das Chaves de Domicílio
chave_domicilio <- c("COD_UPA", "NUM_DOM", "NUM_UC", "UF")


# 5. Processar Alimentação No Domicílio (Caderneta Coletiva)
caderneta_processada <- CADERNETA_COLETIVA |>
  mutate(
    COD_UPA         = as.character(COD_UPA),
    NUM_DOM         = as.character(NUM_DOM),
    NUM_UC          = as.character(NUM_UC),
    UF              = as.character(UF),
    V9001           = as.numeric(V9001),
    cod_tradutor_5d = floor(V9001 / 100)
  ) |>
  filter(cod_tradutor_5d < 86001 | cod_tradutor_5d > 89999) |>
  filter(V8000_DEFLA != 9999999.99) |>
  mutate(vlr_mensal = (V8000_DEFLA * FATOR_ANUALIZACAO) / 12) |>
  group_by(across(all_of(chave_domicilio))) |>
  summarise(alimentacao_no_dom = sum(vlr_mensal, na.rm = TRUE), .groups = "drop")


# 6. Processar Alimentação Fora do Domicílio (Despesa Individual)
individual_processada <- DESPESA_INDIVIDUAL |>
  mutate(
    COD_UPA         = as.character(COD_UPA),
    NUM_DOM         = as.character(NUM_DOM),
    NUM_UC          = as.character(NUM_UC),
    UF              = as.character(UF),
    QUADRO          = as.numeric(QUADRO),
    V9001           = as.numeric(V9001),
    cod_tradutor_5d = floor(V9001 / 100)
  ) |>
  filter(QUADRO == 24 | cod_tradutor_5d %in% c(41001, 48018, 49075, 49089)) |>
  filter(V8000_DEFLA != 9999999.99) |>
  mutate(
    vlr_mensal = ifelse(
      QUADRO == 24,
      (V8000_DEFLA * FATOR_ANUALIZACAO) / 12,
      (V8000_DEFLA * coalesce(as.numeric(V9011), 1) * FATOR_ANUALIZACAO) / 12
    )
  ) |>
  group_by(across(all_of(chave_domicilio))) |>
  summarise(alimentacao_fora_dom = sum(vlr_mensal, na.rm = TRUE), .groups = "drop")


# 7. Consolidar os Blocos por Domicílio
despesas_alimentacao <- full_join(caderneta_processada, individual_processada, by = chave_domicilio) |>
  mutate(
    alimentacao_no_dom   = replace_na(alimentacao_no_dom, 0),
    alimentacao_fora_dom = replace_na(alimentacao_fora_dom, 0),
    alimentacao_total    = alimentacao_no_dom + alimentacao_fora_dom
  )


# 8. Identificação da presença de idosos na família

# Passo A: Mapear a idade máxima existente em cada Unidade de Consumo (UC)
cadastro_idades_uc <- MORADOR |>
  mutate(
    COD_UPA = as.character(COD_UPA),
    NUM_DOM = as.character(NUM_DOM),
    NUM_UC  = as.character(NUM_UC),
    UF      = as.character(UF),  
    idade   = as.numeric(V0403)
  ) |>
  group_by(across(all_of(chave_domicilio))) |>
  summarise(
    contem_idoso  = if_else(any(idade >= 60, na.rm = TRUE), 1, 0),
    maior_idade_uc = max(idade, na.rm = TRUE),
    .groups = "drop"
  )

# Passo B: Pegar os dados amostrais (PESO_FINAL, ESTRATO) da Pessoa de Referência
ref_amostral <- MORADOR |>
  mutate(
    COD_UPA        = as.character(COD_UPA),
    NUM_DOM        = as.character(NUM_DOM),
    NUM_UC         = as.character(NUM_UC),
    UF             = as.character(UF),
    ESTRATO_POF    = as.character(ESTRATO_POF),
    COD_INFORMANTE = as.numeric(COD_INFORMANTE)
  ) |>
  filter(COD_INFORMANTE == 1) |> 
  select(all_of(chave_domicilio), ESTRATO_POF, PESO_FINAL)

# Passo C: Combinar as regras para gerar o novo perfil de moradores da UC
ref_moradores <- ref_amostral |>
  inner_join(cadastro_idades_uc, by = chave_domicilio)


# 9. Cruzar a amostra geral com as despesas e criar as Faixas Etárias da UC
base_domicilios <- ref_moradores |>
  left_join(despesas_alimentacao, by = chave_domicilio) |>
  mutate(
    alimentacao_no_dom   = replace_na(alimentacao_no_dom, 0),
    alimentacao_fora_dom = replace_na(alimentacao_fora_dom, 0),
    alimentacao_total    = replace_na(alimentacao_total, 0),
    
    # Criando faixas etárias baseadas na MAIOR idade de idoso presente na família
    faixa_etaria_60 = cut(
      maior_idade_uc,
      breaks = c(-Inf, 60, 65, 70, 75, 80, 85, 90, Inf),
      labels = c("Menos de 60 anos", "60 a 64 anos", "65 a 69 anos", "70 a 74 anos",
                 "75 a 79 anos", "80 a 84 anos", "85 a 89 anos", 
                 "90 anos ou mais"),
      right = FALSE
    )
  )


# 10. Configuração Amostral Amarrada à Memória do IBGE
options(survey.lonely.psu = "adjust")
pof_design <- svydesign(
  id      = ~COD_UPA,
  strata  = ~ESTRATO_POF,
  weights = ~PESO_FINAL,
  data    = base_domicilios,
  nest    = TRUE
)


# 11. Processamento dos Resultados Totais (Com arredondamento)
media_no_domicilio    <- svymean(~alimentacao_no_dom, pof_design)
media_fora_domicilio  <- svymean(~alimentacao_fora_dom, pof_design)
media_total_aliment   <- svymean(~alimentacao_total, pof_design)

# Extração dos valores numéricos para cálculo de proporção e correção
val_no_dom   <- as.numeric(media_no_domicilio)
val_fora_dom <- as.numeric(media_fora_domicilio)
val_total    <- as.numeric(media_total_aliment)

# Definição do fator acumulado do INPC (jan/2018 a ago/2026)
fator_inpc <- 1.5487

resultados_totais <- tibble(
  `Tipo de Despesa` = c("Alimentação no Domicílio", "Alimentação fora do Domicílio", "Alimentação Total"),
  `Despesa Média Mensal (R$)` = round(c(val_no_dom, val_fora_dom, val_total), 2),
  `Frequência Relativa (%)` = round(c((val_no_dom / val_total) * 100, (val_fora_dom / val_total) * 100, 100), 2),
  `Despesa Atualizada INPC (R$)` = round(c(val_no_dom * fator_inpc, val_fora_dom * fator_inpc, val_total * fator_inpc), 2)
)


# 12. Processamento dos Resultados por Idade (Famílias que CONTÊM Idosos)
pof_design_60 <- subset(pof_design, contem_idoso == 1)

# Agrupando idades de 90+ baseadas na idade do idoso da família
pof_design_60$variables <- pof_design_60$variables |>
  mutate(idade_exibicao = ifelse(maior_idade_uc >= 90, "90 anos ou mais", as.character(maior_idade_uc)))

# A. Despesa por Idade Exata do Idoso presente na família
despesas_por_idade_exata <- svyby(~alimentacao_no_dom + alimentacao_fora_dom + alimentacao_total,
                                  by = ~idade_exibicao, design = pof_design_60, FUN = svymean) |>
  as_tibble() |>
  rename(
    `Idade do Idoso na Família`      = idade_exibicao,
    `Alimentação no Domicílio`       = alimentacao_no_dom,
    `Alimentação fora do Domicílio`  = alimentacao_fora_dom,
    `Alimentação Total`              = alimentacao_total
  ) |>
  select(`Idade do Idoso na Família`, `Alimentação no Domicílio`, `Alimentação fora do Domicílio`, `Alimentação Total`) |>
  mutate(across(where(is.numeric), ~ round(., 2))) |>
  arrange(suppressWarnings(as.numeric(`Idade do Idoso na Família`)))

# B. Despesa por Faixa Etária Quinquenal da Família (Considerando presença de idoso)
despesas_por_faixa <- svyby(~alimentacao_no_dom + alimentacao_fora_dom + alimentacao_total,
                            by = ~faixa_etaria_60, design = pof_design, FUN = svymean) |>
  as_tibble() |>
  rename(
    `Faixa Etária da Família (Anos)` = faixa_etaria_60,
    `Alimentação no Domicílio`       = alimentacao_no_dom,
    `Alimentação fora do Domicílio`  = alimentacao_fora_dom,
    `Alimentação Total`              = alimentacao_total
  ) |>
  select(`Faixa Etária da Família (Anos)`, `Alimentação no Domicílio`, `Alimentação fora do Domicílio`, `Alimentação Total`) |>
  # CORREÇÃO: Cria a nova coluna atualizada pelo INPC multiplicando pelo fator_inpc (1.5537)
  mutate(
    `Despesa Atualizada INPC (R$)` = round(`Alimentação Total` * fator_inpc, 2)
  ) |>
  mutate(across(where(is.numeric), ~ round(., 2)))


# 13. Geração do Arquivo Excel (.xlsx) com Três Abas
wb <- createWorkbook()

# Adiciona as abas e dados
addWorksheet(wb, "Total Brasil")
writeData(wb, "Total Brasil", resultados_totais)

addWorksheet(wb, "Idade Exata")
writeData(wb, "Idade Exata", despesas_por_idade_exata)

addWorksheet(wb, "Faixa Etaria Quinquenal")
writeData(wb, "Faixa Etaria Quinquenal", despesas_por_faixa)

# Estilo para forçar 2 casas decimais (Formatos do Excel)
estilo_decimal <- createStyle(numFmt = "0.00")

# Aplica formatação decimal na aba "Total Brasil" (Colunas 3 e 4)
addStyle(wb, sheet = "Total Brasil", style = estilo_decimal, rows = 2:4, cols = 3, gridExpand = TRUE)
addStyle(wb, sheet = "Total Brasil", style = estilo_decimal, rows = 2:4, cols = 4, gridExpand = TRUE)

# CORREÇÃO: Aplica a formatação de 2 casas decimais para todas as colunas numéricas da aba quinquenal (Colunas 2 a 5)
addStyle(wb, sheet = "Faixa Etaria Quinquenal", style = estilo_decimal, rows = 2:(nrow(despesas_por_faixa) + 1), cols = 2:5, gridExpand = TRUE)

# Salva o arquivo
saveWorkbook(wb, "Resultado_Alimentacao.xlsx", overwrite = TRUE)
cat("✔ Arquivo gerado: 'Resultado_Alimentacao.xlsx'\n")


# 14. Gráfico Mapa de Calor do Brasil por estados
# A. Calcular a média de despesas com alimentação por UF considerando o desenho amostral
despesas_uf <- svyby(~alimentacao_total, by = ~UF, design = pof_design, FUN = svymean) |> 
  as_tibble()

# B. Baixar a malha dos Estados do Brasil 
estados_br <- read_state(year = 2025, showProgress = FALSE) |> 
  mutate(abbrev_state = as.character(abbrev_state)) 

# C. Unir e plotar com ggplot2
mapa_dados <- estados_br |> 
  mutate(code_state = as.character(code_state)) |> 
  left_join(despesas_uf, by = c("code_state" = "UF"))

grafico_mapa <- ggplot(data = mapa_dados) +
  geom_sf(aes(fill = alimentacao_total), color = "white") +
  # AJUSTE DA BARRA: Configurada para o formato horizontal na parte inferior
  scale_fill_viridis_c(
    name = "Despesa (R$)",
    guide = guide_colorbar(
      barheight = unit(0.7, "cm"),  # Altura da barra (fina na horizontal)
      barwidth = unit(12, "cm"),   # Largura da barra (esticada horizontalmente)
      title.position = "top",      # Título acima da barra
      title.hjust = 0.5            # Título centralizado
    )
  ) + 
  theme_minimal() +
  theme(
    # AJUSTE DA POSIÇÃO DA LEGENDA: Move para baixo do gráfico
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.title = element_text(size = 16, face = "bold"), # Tamanho da fonte do título ("Despesa R$")
    legend.text = element_text(size = 14),                 # Tamanho da fonte dos números da escala
    
    # AJUSTE: Alinha e formata o TÍTULO na parte SUPERIOR
    plot.title.position = "plot",
    plot.title = element_markdown(
      hjust = 0.5,
      lineheight = 1.3,
      size = 18,
      face = "plain",
      margin = margin(t = 15, r = 0, b = 25, l = 0)
    ),
    
    # AJUSTE: Alinha e formata a FONTE ampliada na parte INFERIOR
    plot.caption.position = "plot", 
    plot.caption = element_markdown(
      hjust = 0.5, 
      lineheight = 1.2,
      size = 14,              
      color = "#222222",      # Tom mais escuro para aumentar o contraste e a legibilidade
      face = "plain",
      margin = margin(t = 30, r = 0, b = 15, l = 0) 
    ),
    
    # Limpeza total de eixos e grades para evitar distorções de margem
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
  ) +
  labs(
    title = "**FIGURA 2** – Despesa Média Mensal com Alimentação por Estado",
    caption = "Fonte: Microdados da POF do IBGE (2017-2018). Elaborada pela autora (2026)."
  )

# SALVAMENTO PROPORCIONAL: Um formato ligeiramente mais estreito elimina as rebarbas laterais
ggsave("Mapa_Despesa_Alimentacao.png", plot = grafico_mapa, 
       width = 9, height = 8.5, dpi = 300, bg = "white")

cat("✔ Mapa corrigido gerado com sucesso: 'Mapa_Despesa_Alimentacao.png'\n")


# 15. Gráfico de barras com despesa por faixa etária (Versão Elegante Atualizada pelo INPC)

grafico_barras_etaria <- ggplot(data = despesas_por_faixa, 
                                aes(x = `Faixa Etária da Família (Anos)`, 
                                    y = `Despesa Atualizada INPC (R$)`)) + # REMOVIDO o fill por variável para cor única
  # ALTERADO: Define cor fixa (fill) em tom Azul Pastel e mantém cantos suaves
  geom_col(fill = "white", color = "#777777", linewidth = 0.4, width = 0.7) +
  
  # ALTERADO: Aumentado o tamanho da fonte dos valores (size de 3.5 para 5) sobre cada barra
  geom_text(aes(label = paste0("R$ ", round(`Despesa Atualizada INPC (R$)`, 0))), 
            vjust = -0.5, size = 5, fontface = "bold", color = "#333333") +
  
  # Adiciona uma folga de 12% no topo do eixo Y para garantir espaço livre ao texto
  scale_y_continuous(expand = expansion(mult = c(0, 0.12)),
                     labels = scales::comma_format(big.mark = ".", decimal.mark = ",")) +
  
  theme_minimal() +
  theme(
    # ALTERADO: Retirado o face = "bold" (padrão é plain) para tirar o negrito do eixo X
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14, face = "plain", color = "#000000"),
    
    # ALTERADO: Remove completamente os números do eixo Y
    axis.text.y = element_blank(),
    
    # ALTERADO: Aumenta o tamanho da fonte do título do eixo Y (definido tamanho 16)
    axis.title.y = element_text(size = 16, margin = margin(r = 15)),
    
    # Remove as linhas verticais e limpa o fundo
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    
    # Oculta a legenda lateral redundante
    legend.position = "none", 
    
    # AJUSTE: Alinha e formata o TÍTULO na parte SUPERIOR
    plot.title.position = "plot",
    plot.title = element_markdown(
      hjust = 0.5,
      lineheight = 1.3,
      size = 18,
      face = "plain",
      margin = margin(t = 15, r = 0, b = 25, l = 0)
    ),
    
    # AJUSTE: Alinha e formata a FONTE ampliada na parte INFERIOR
    plot.caption.position = "plot",
    plot.caption = element_markdown(
      hjust = 0.5,
      lineheight = 1.2,
      size = 15,                  
      color = "#222222",          
      face = "plain",
      margin = margin(t = 25, r = 0, b = 10, l = 0)
    )
  ) +
  labs(
    x = NULL, 
    y = "Despesa Média Mensal Atualizada (R$)",
    # Título movido para cima no padrão correto do MDT da UFSM
    title = "**FIGURA 3** – Despesa Média Mensal com Alimentação por Faixa Etária",
    # Fonte e notas movidas para baixo de forma clara e visível
    caption = "Fonte: Microdados da POF do IBGE (2017-2018). Elaborada pela autora (2026).<br>Nota: Valores originais atualizados pelo INPC para setembro de 2026 (Fator: 1.5487)."
  )

# Salva a imagem final com fundo branco sólido em alta resolução
ggsave("Grafico_Barras_Faixa_Etaria.png", plot = grafico_barras_etaria, 
       width = 11, height = 7, dpi = 300, bg = "white")

cat("✓ Gráfico gerado: 'Grafico_Barras_Faixa_Etaria.png'\n")


# 16. Calcular a Quantidade de Famílias por Faixa Etária
# A. Quantidade expandida (População estimada refletindo o peso do IBGE)
familias_expandido <- svytotal(~faixa_etaria_60, pof_design) |> 
  as_tibble(rownames = "Faixa Etária") |> 
  mutate(
    `Faixa Etária` = str_remove(`Faixa Etária`, "faixa_etaria_60"),
    total = round(total, 0)
  ) |> 
  rename(`Famílias na População (Expandido)` = total) |> 
  select(`Faixa Etária`, `Famílias na População (Expandido)`)

# B. Quantidade amostral (Contagem direta de observações na base de dados)
familias_amostra <- base_domicilios |> 
  group_by(faixa_etaria_60) |> 
  summarise(`Famílias na Amostra (n)` = n(), .groups = "drop") |> 
  rename(`Faixa Etária` = faixa_etaria_60)

# C. Consolidar os resultados em uma única tabela descritiva
tabela_familias_faixa <- familias_amostra |> 
  inner_join(familias_expandido, by = "Faixa Etária")

# Exibe o resultado no console
print(tabela_familias_faixa)

# Exportação da tabela de famílias por faixa etária para o Excel

# 1. Criar a estrutura do arquivo Excel
wb_faixas <- createWorkbook()
addWorksheet(wb_faixas, "Famílias por Faixa Etária")

# 2. Escrever os dados na planilha
writeData(wb_faixas, "Famílias por Faixa Etária", tabela_familias_faixa)

# 3. Criar estilo para números inteiros com separador de milhar (Ex: 1.250)
estilo_inteiro <- createStyle(numFmt = "#,##0")

# 4. Aplicar o estilo nas colunas numéricas (Colunas 2 e 3)
# rows = 2:(nrow(...) + 1) garante a formatação de todas as linhas de dados, pulando o cabeçalho
addStyle(
  wb_faixas, 
  sheet = "Famílias por Faixa Etária", 
  style = estilo_inteiro, 
  rows = 2:(nrow(tabela_familias_faixa) + 1), 
  cols = 2:3, 
  gridExpand = TRUE
)

# 5. Salvar o arquivo no diretório de trabalho
saveWorkbook(wb_faixas, "Familias_por_Faixa_Etaria.xlsx", overwrite = TRUE)
cat("✓ Arquivo gerado com sucesso: 'Familias_por_Faixa_Etaria.xlsx'\n")


# 17. Atualizado: Distribuição de Idosos 90+ por UF com Despesa Média Mensal

# Passo A: Criar a tabela de conversão das UFs (Código -> Sigla e Nome)
tabela_conversao_uf <- estados_br |> 
  sf::st_drop_geometry() |> 
  mutate(code_state = as.character(code_state)) |> 
  select(code_state, abbrev_state, name_state)

# Passo B: Processar a distribuição amostral e trazer a despesa média do Item 14
familias_90_mais_uf_despesa <- base_domicilios |> 
  filter(faixa_etaria_60 == "90 anos ou mais") |> 
  group_by(UF) |> 
  summarise(
    `Quantidade Amostral (N)` = n(),
    .groups = "drop"
  ) |> 
  # 1. Cruzar com a tabela de nomes das UFs
  left_join(tabela_conversao_uf, by = c("UF" = "code_state")) |> 
  # 2. Cruzar com as despesas por UF calculadas no Item 14 (objeto despesas_uf)
  left_join(despesas_uf |> mutate(UF = as.character(UF)), by = "UF") |> 
  mutate(
    `Proporção Amostral (%)` = round((`Quantidade Amostral (N)` / sum(`Quantidade Amostral (N)`)) * 100, 2),
    `Despesa Média Mensal (R$)` = round(alimentacao_total, 2)
  ) |> 
  # 3. Organizar e selecionar as colunas finais
  select(
    `Código IBGE` = UF, 
    `Sigla` = abbrev_state, 
    `Estado` = name_state, 
    `Quantidade Amostral (N)`, 
    `Proporção Amostral (%)`,
    `Despesa Média Mensal (R$)`
  ) |> 
  arrange(desc(`Quantidade Amostral (N)`))

# Passo C: Salvar o resultado atualizado no Excel
wb_uf_despesa <- createWorkbook()
addWorksheet(wb_uf_despesa, "Idosos 90+ por UF")

# Escreve a tabela na planilha
writeData(wb_uf_despesa, "Idosos 90+ por UF", familias_90_mais_uf_despesa)

# Definição dos formatos numéricos
estilo_inteiro <- createStyle(numFmt = "#,##0")
estilo_decimal <- createStyle(numFmt = "0.00")

# Aplicação dos estilos nas colunas correspondentes
# Coluna 4: Inteiro | Colunas 5 e 6: Decimal com 2 casas
num_linhas <- nrow(familias_90_mais_uf_despesa) + 1
addStyle(wb_uf_despesa, sheet = "Idosos 90+ por UF", style = estilo_inteiro, rows = 2:num_linhas, cols = 4, gridExpand = TRUE)
addStyle(wb_uf_despesa, sheet = "Idosos 90+ por UF", style = estilo_decimal, rows = 2:num_linhas, cols = 5:6, gridExpand = TRUE)

# Salva o arquivo final
saveWorkbook(wb_uf_despesa, "Familias_90_Mais_UF_Despesa.xlsx", overwrite = TRUE)
cat("✓ Arquivo atualizado gerado com sucesso: 'Familias_90_Mais_UF_Despesa.xlsx'\n")

rm(list=ls())

######## IMPORTANDO BIBLIOTECAS ########
library(prophet)
library(dplyr)
library('ggplot2')
library('forecast')
library('tseries')
library(TSA)
library(gtools)
library(astsa)
library("openxlsx")
########################################

########################################
################ LENDO BASES ################
# Fazendo a carga dos dados da base de faturamento
data = read.csv('20180903 - Base Faturamento v3.csv', header=TRUE, stringsAsFactors=FALSE, sep=";")

# Substituindo ponto por vírgula no volume e receita líquida
data$vol_M3 <- as.numeric(gsub(",", ".", gsub("\\.", "", data$vol_M3)))
data$rec_liq <- as.numeric(gsub(",", ".", gsub("\\.", "", data$rec_liq)))

# Fazendo a carga dos dados da base de faturamento
mercado_iba = read.csv('Mercado IBA (2014 - 2017).csv', header=TRUE, stringsAsFactors=FALSE, sep=";")

# Substituindo ponto por vírgula no volume
mercado_iba$Volume.M3 <- as.numeric(gsub(",", ".", gsub("\\.", "", mercado_iba$Volume.M3)))

# Fazendo a carga dos dados econômicos gerais
dados_economicos = read.csv('DADOS_ECONOMICOS_GERAL.csv', header=TRUE, stringsAsFactors=FALSE, sep=";")

# Renomeando a coluna ANOMES para dt_competencia
names(dados_economicos)[1]<-"dt_competencia"

# Convertendo a coluna dt_competencia com o formato padrão de
# datas no R e transformando em um objeto Date
data$dt_competencia = as.Date(paste0(as.character(data$dt_competencia), '01'), format='%Y%m%d')
mercado_iba$Competência = as.Date(paste0(as.character(mercado_iba$Competência), '01'), format='%Y%m%d')

# Convertendo a coluna dt_competencia com o formato padrão de
# datas no R e transformando em um objeto Date
dados_economicos$dt_competencia = as.Date(paste0(as.character(dados_economicos$dt_competencia), '01'), format='%Y%m%d')
########################################

########################################
######## CRIANDO DATAFRAMES PARA CADA 
################ PRODUTO/SEGMENTO DE MERCADO ########

##### MDP #####
# Criando o dataframe para todo o MDP
data_mdp <- subset(data , cd_linha == "MP        " & prd_classificacao == "Cru")

### Varejo ###
# Criando o dataframe para o MDP Varejo
data_mdp_var <- data_mdp[grep("Var", data_mdp$dc_filial),]

# Removendo Varejos que não contém muitos dados para o forecast
data_mdp_var <- data_mdp_var[-grep("Varejo - MG/DF/GO                       ", data_mdp_var$dc_filial),]
data_mdp_var <- data_mdp_var[-grep("Varejo - PR / SC                        ", data_mdp_var$dc_filial),]
data_mdp_var <- data_mdp_var[-grep("Varejo - RJ / ES                        ", data_mdp_var$dc_filial),]
data_mdp_var <- data_mdp_var[-grep("Varejo - NO / NE                        ", data_mdp_var$dc_filial),]
data_mdp_var <- data_mdp_var[-grep("Var - Funcionários                      ", data_mdp_var$dc_filial),]

# Somando a receita líquida de todas as regiões do MDP Varejo, por mês e ano
data_mdp_var_sum <- aggregate(x = data_mdp_var["rec_liq"],
                              FUN = sum,
                              by = list(dt_competencia = data_mdp_var$dt_competencia))

# Somando o volume de todas as regiões do MDP Varejo, por mês e ano
data_mdp_var_sum_volume <- aggregate(x = data_mdp_var["vol_M3"],
                                            FUN = sum,
                                            by = list(dt_competencia = data_mdp_var$dt_competencia))
## Varejo - SP (todos)
data_mdp_var_sp <- data_mdp_var[grep("SP", data_mdp_var$dc_filial),]

# Somando o volume de SP do MDP Varejo, por mês e ano
data_mdp_var_sum_volume_SP <- aggregate(x = data_mdp_var["vol_M3"],
                                     FUN = sum,
                                     by = list(dt_competencia = data_mdp_var$dt_competencia))

##### MDP #####
### Indústria ###
# Criando o dataframe para o MDP Industria
data_mdp_ind <- data_mdp[grep("Ind", data_mdp$dc_filial),]

# Removendo Vendas Direta do Forecast
data_mdp_ind <- data_mdp_ind[-grep("Vendas Diretas", data_mdp_ind$dc_filial),]

# Removendo SP - Rep do Forecast
data_mdp_ind <- data_mdp_ind[-grep("SP Capital - Rep                    ", data_mdp_ind$dc_filial),]

# Somando a receita líquida de todas as regiões do MDP Indústria, por mês e ano
data_mdp_ind_sum <- aggregate(x = data_mdp_ind["rec_liq"],
                              FUN = sum,
                              by = list(dt_competencia = data_mdp_ind$dt_competencia))
# Somando o volume de todas as regiões do MDP Indústria, por mês e ano
data_mdp_ind_sum_volume <- aggregate(x = data_mdp_ind["vol_M3"],
                              FUN = sum,
                              by = list(dt_competencia = data_mdp_ind$dt_competencia))

# MDP Indústria NO / NE
data_mdp_ind_no_ne <- data_mdp_ind[grep("NO / NE", data_mdp_ind$dc_filial),]
# Somando o volume do MDP Indústria NO / NE, por mês e ano
data_mdp_ind_no_ne_sum_volume <- aggregate(x = data_mdp_ind_no_ne["vol_M3"],
                                     FUN = sum,
                                     by = list(dt_competencia = data_mdp_ind_no_ne$dt_competencia))


# MDP Indústria PR/SC Volume
data_mdp_ind_pr_sc <- data_mdp_ind[grep("PR / SC", data_mdp_ind$dc_filial),]
#data_mdp_ind_pr_sc <- data_mdp_ind_pr_sc[-grep("Rep", data_mdp_ind_pr_sc$dc_filial),]
# Somando o volume do MDP Indústria PR/SC, por mês e ano
data_mdp_ind_pr_sc_sum_volume <- aggregate(x = data_mdp_ind_pr_sc["vol_M3"],
                                           FUN = sum,
                                           by = list(dt_competencia = data_mdp_ind_pr_sc$dt_competencia))

# MDP Indústria RS - Rep Volume
data_mdp_ind_rs_rep <- data_mdp_ind[grep("RS", data_mdp_ind$dc_filial),]
data_mdp_ind_rs_rep <- data_mdp_ind_rs_rep[grep("Rep", data_mdp_ind_rs_rep$dc_filial),]
# Somando o volume do MDP Indústria RS - Rep, por mês e ano
data_mdp_ind_rs_rep_volume <- aggregate(x = data_mdp_ind_rs_rep["vol_M3"],
                                           FUN = sum,
                                           by = list(dt_competencia = data_mdp_ind_rs_rep$dt_competencia))

# MDP Indústria SP Capital Volume
data_mdp_ind_sp_capital<- data_mdp_ind[grep("SP Capital", data_mdp_ind$dc_filial),]
#data_mdp_ind_sp_capital <- data_mdp_ind_sp_capital[grep("Capital", data_mdp_ind_sp_capital$dc_filial),]
#data_mdp_ind_sp_capital <- data_mdp_ind_sp_capital[-grep("Rep", data_mdp_ind_sp_capital$dc_filial),]

# Somando o volume do MDP Indústria RS - Rep, por mês e ano
data_mdp_ind_sp_capital_volume <- aggregate(x = data_mdp_ind_sp_capital["vol_M3"],
                                        FUN = sum,
                                        by = list(dt_competencia = data_mdp_ind_sp_capital$dt_competencia))


##### MDF #####
# Criando o dataframe para todo o MDF
data_mdf <- subset(data , cd_linha == "DB        " & prd_classificacao == "Branco Competitivo")

### Varejo ###
# Criando o dataframe para o MDF Varejo
data_mdf_var <- data_mdf[grep("Var", data_mdf$dc_filial),]

# Removendo Vendas Diretas e Funcionários do forecast
data_mdf_var <- data_mdf_var[-grep("Var - Funcionários                      ", data_mdf_var$dc_filial),]
data_mdf_var <- data_mdf_var[-grep("Var - Vendas Diretas                    ", data_mdf_var$dc_filial),]

# Somando a receita líquida de todas as regiões do MDF, por mês e ano
data_mdf_var_sum <- aggregate(x = data_mdf_var["rec_liq"],
                              FUN = sum,
                              by = list(dt_competencia = data_mdf_var$dt_competencia))

# Somando o volume todas as regiões do MDF Varejo, por mês e ano
data_mdf_var_sum_volume <- aggregate(x = data_mdf_var["vol_M3"],
                              FUN = sum,
                              by = list(dt_competencia = data_mdf_var$dt_competencia))

## Varejo - SP (todos)
data_mdf_var_sp <- data_mdf_var[grep("SP", data_mdf_var$dc_filial),]
# Somando o volume de SP do MDF Varejo por mês e ano
data_mdf_var_sum_volume_SP <- aggregate(x = data_mdf_var_sp["vol_M3"],
                                        FUN = sum,
                                        by = list(dt_competencia = data_mdf_var_sp$dt_competencia))

## MDF Varejo - MG/DF/GO ##
data_mdf_var_mg_df_go <- data_mdf_var[grep("MG/DF/GO", data_mdf_var$dc_filial),]
# Somando o volume de MG/DF/GO do MDF Varejo por mês e ano
data_mdf_var_mg_df_go_sum_volume <- aggregate(x = data_mdf_var_mg_df_go["vol_M3"],
                                        FUN = sum,
                                        by = list(dt_competencia = data_mdf_var_mg_df_go$dt_competencia))

## MDF Varejo - NO/NE ##
data_mdf_var_no_ne <- data_mdf_var[grep("NO / NE", data_mdf_var$dc_filial),]
# Somando o volume de MG/DF/GO do MDF Varejo por mês e ano
data_mdf_var_no_ne_sum_volume <- aggregate(x = data_mdf_var_no_ne["vol_M3"],
                                              FUN = sum,
                                              by = list(dt_competencia = data_mdf_var_no_ne$dt_competencia))
## MDF Varejo - PR/SC ##
data_mdf_var_pr_sc <- data_mdf_var[grep("PR / SC", data_mdf_var$dc_filial),]
# Somando o volume de PR/SC MDF Varejo por mês e ano
data_mdf_var_pr_sc_sum_volume <- aggregate(x = data_mdf_var_pr_sc["vol_M3"],
                                           FUN = sum,
                                           by = list(dt_competencia = data_mdf_var_pr_sc$dt_competencia))

## MDF Varejo - RJ / ES ##
data_mdf_var_rj_es <- data_mdf_var[grep("RJ / ES", data_mdf_var$dc_filial),]
# Somando o volume de RJ/ES do MDF Varejo por mês e ano
data_mdf_var_rj_es_sum_volume <- aggregate(x = data_mdf_var_rj_es["vol_M3"],
                                           FUN = sum,
                                           by = list(dt_competencia = data_mdf_var_rj_es$dt_competencia))
##### MDF #####
### Indústria ###
# Criando o dataframe para o MDF Industria
data_mdf_ind <- data_mdf[grep("Ind", data_mdf$dc_filial),]

# Removendo Vendas Diretas do MDF Indústria
data_mdf_ind <- data_mdf_ind[-grep("Vendas Diretas", data_mdf_ind$dc_filial),]

# Somando a receita líquida de todas as regiões do MDF Indústria, por mês e ano
data_mdf_ind_sum <- aggregate(x = data_mdf_ind["rec_liq"],
                              FUN = sum,
                              by = list(dt_competencia = data_mdf_ind$dt_competencia))

# Somando a receita líquida de todas as regiões do MDF Indústria, por mês e ano
data_mdf_ind_sum_volume <- aggregate(x = data_mdf_ind["vol_M3"],
                              FUN = sum,
                              by = list(dt_competencia = data_mdf_ind$dt_competencia))

## MDP Indústria NO/NE
data_mdf_ind_volume_no_ne <- data_mdf_ind[grep("NO / NE", data_mdf_ind$dc_filial),]

# Somando o volume de NO/NE do MDF Indústria por mês e ano
data_mdf_ind_sum_volume_no_ne <- aggregate(x = data_mdf_ind_volume_no_ne["vol_M3"],
                                        FUN = sum,
                                        by = list(dt_competencia = data_mdf_ind_volume_no_ne$dt_competencia))

# MDF Indústria PR/SC Volume
data_mdf_ind_pr_sc <- data_mdf_ind[grep("PR / SC", data_mdf_ind$dc_filial),]
#data_mdf_ind_pr_sc <- data_mdf_ind_pr_sc[-grep("Rep", data_mdf_ind_pr_sc$dc_filial),]
# Somando o volume do MDF Indústria PR/SC, por mês e ano
data_mdf_ind_pr_sc_sum_volume <- aggregate(x = data_mdf_ind_pr_sc["vol_M3"],
                                           FUN = sum,
                                           by = list(dt_competencia = data_mdf_ind_pr_sc$dt_competencia))

# MDF Indústria RS Rep Volume
data_mdf_ind_rs_rep <- data_mdf_ind[grep("RS", data_mdf_ind$dc_filial),]
data_mdf_ind_rs_rep <- data_mdf_ind_rs_rep[grep("Rep", data_mdf_ind_rs_rep$dc_filial),]
# Somando o volume do MDF Indústria PR/SC, por mês e ano
data_mdf_ind_rs_rep_sum_volume <- aggregate(x = data_mdf_ind_rs_rep["vol_M3"],
                                           FUN = sum,
                                           by = list(dt_competencia = data_mdf_ind_rs_rep$dt_competencia))

# MDF Indústria SP Capital Volume
data_mdf_ind_sp<- data_mdf_ind[grep("SP", data_mdf_ind$dc_filial),]
data_mdf_ind_sp_capital <- data_mdf_ind_sp[grep("Capital", data_mdf_ind_sp$dc_filial),]
# Somando o volume do MDP Indústria RS - Rep, por mês e ano
data_mdf_ind_sp_capital_volume <- aggregate(x = data_mdf_ind_sp_capital["vol_M3"],
                                            FUN = sum,
                                            by = list(dt_competencia = data_mdf_ind_sp_capital$dt_competencia))
########################################

########################################
######## REGRESSORES EXTERNOS ########
dados_economicos$PIB <- as.numeric(gsub(",", ".", gsub("\\.", "", dados_economicos$PIB)))
dados_economicos$SELIC <- as.numeric(gsub(",", ".", gsub("\\.", "", dados_economicos$SELIC)))
dados_economicos$INCC <- as.numeric(gsub(",", ".", gsub("\\.", "", dados_economicos$INCC)))
dados_economicos$IPCA <- as.numeric(gsub(",", ".", gsub("\\.", "", dados_economicos$IPCA)))
dados_economicos$FAT_PROD_DE_MADEIRA <- as.numeric(gsub(",", ".", gsub("\\.", "", dados_economicos$FAT_PROD_DE_MADEIRA)))
dados_economicos$INPC <- as.numeric(gsub(",", ".", gsub("\\.", "", dados_economicos$INPC)))
dados_economicos$SALARIO_MINIMO_REAL <- as.numeric(gsub(",", ".", gsub("\\.", "", dados_economicos$SALARIO_MINIMO_REAL))) 
dados_economicos$FAT_PROD_MAD_DEFLAC <- as.numeric(gsub(",", ".", gsub("\\.", "", dados_economicos$FAT_PROD_MAD_DEFLAC))) 
dados_economicos$LIC_AUTO_NACIONAIS <- as.numeric(gsub(",", ".", gsub("\\.", "", dados_economicos$LIC_AUTO_NACIONAIS))) 
dados_economicos$LIC_COM_LEVES_NACIONAIS <- as.numeric(gsub(",", ".", gsub("\\.", "", dados_economicos$LIC_COM_LEVES_NACIONAIS))) 
dados_economicos$ELETRODOMESTICOS <- as.numeric(gsub(",", ".", gsub("\\.", "", dados_economicos$ELETRODOMESTICOS))) 
dados_economicos$IND_EXP_NIVEL_DE_ATIV_CONSTRUCAO <- as.numeric(gsub(",", ".", gsub("\\.", "", dados_economicos$IND_EXP_NIVEL_DE_ATIV_CONSTRUCAO))) 
dados_economicos$IND_EXP_CONSUMIDOR <- as.numeric(gsub(",", ".", gsub("\\.", "", dados_economicos$IND_EXP_CONSUMIDOR))) 
dados_economicos$IND_CONF_CONSUMIDOR <- as.numeric(gsub(",", ".", gsub("\\.", "", dados_economicos$IND_CONF_CONSUMIDOR))) 
dados_economicos$IND_EXP_RENDA_MEDIA <- as.numeric(gsub(",", ".", gsub("\\.", "", dados_economicos$IND_EXP_RENDA_MEDIA))) 
dados_economicos$HRS_TRAB_PROD_MOVEIS <- as.numeric(gsub(",", ".", gsub("\\.", "", dados_economicos$HRS_TRAB_PROD_MOVEIS)))
dados_economicos$EMPREGO_MOVEIS <- as.numeric(gsub(",", ".", gsub("\\.", "", dados_economicos$EMPREGO_MOVEIS)))
dados_economicos$TAXA_DESEMPREGO <- as.numeric(gsub(",", ".", gsub("\\.", "", dados_economicos$TAXA_DESEMPREGO)))
dados_economicos$DIAS_UTEIS <- as.numeric(gsub(",", ".", gsub("\\.", "", dados_economicos$DIAS_UTEIS)))
dados_economicos$IGP.M <- as.numeric(gsub(",", ".", gsub("\\.", "", dados_economicos$IGP.M)))
dados_economicos$IPC_GERAL <- as.numeric(gsub(",", ".", gsub("\\.", "", dados_economicos$IPC_GERAL)))
dados_economicos$IPC_MADEIRA <- as.numeric(gsub(",", ".", gsub("\\.", "", dados_economicos$IPC_MADEIRA)))
dados_economicos$ICEI_BRASIL_COND_ECONOMIA_BRASILEIRA <- as.numeric(gsub(",", ".", gsub("\\.", "", dados_economicos$ICEI_BRASIL_COND_ECONOMIA_BRASILEIRA)))
dados_economicos$ICEI_MADEIRA <- as.numeric(gsub(",", ".", gsub("\\.", "", dados_economicos$ICEI_MADEIRA)))
dados_economicos$ICEI_BRASIL <- as.numeric(gsub(",", ".", gsub("\\.", "", dados_economicos$ICEI_BRASIL)))
dados_economicos$CUB_MEDIO_M2_BRASIL_CONSTRUCAO <- as.numeric(gsub(",", ".", gsub("\\.", "", dados_economicos$ICEI_BRASIL)))
dados_economicos$ACESSOS_PRE_PAGO <- as.numeric(gsub(",", ".", gsub("\\.", "", dados_economicos$ACESSOS_PRE_PAGO)))
dados_economicos$ACESSOS_POS_PAGO <- as.numeric(gsub(",", ".", gsub("\\.", "", dados_economicos$ACESSOS_POS_PAGO)))

# Substituindo NA por 0 no dataframe todo
dados_economicos[is.na(dados_economicos)] <- 0
########################################

########################################
######## REMOVENDO OUTLIERS DOS DADOS ########
# MDP Indústria - Receita Líquida
rec_ts = ts(data_mdp_ind_sum[, c('rec_liq')])
data_mdp_ind_sum$req_liq_clean = tsclean(rec_ts)

# MDP Indústria - Volume
volume_ts = ts(data_mdp_ind_sum_volume[, c('vol_M3')])
data_mdp_ind_sum_volume$volume_clean = tsclean(volume_ts)

# MDP Indústria NO / NE - Volume
volume_ts = ts(data_mdp_ind_no_ne_sum_volume[, c('vol_M3')])
data_mdp_ind_no_ne_sum_volume$volume_clean = tsclean(volume_ts)

# MDP Indústria PR / SC - Volume
volume_ts = ts(data_mdp_ind_pr_sc_sum_volume[, c('vol_M3')])
data_mdp_ind_pr_sc_sum_volume$volume_clean = tsclean(volume_ts)

# MDP Indústria RS Rep - Volume
volume_ts = ts(data_mdp_ind_rs_rep_volume[, c('vol_M3')])
data_mdp_ind_rs_rep_volume$volume_clean = tsclean(volume_ts)

# MDP Indústria SP Capital - Volume
volume_ts = ts(data_mdp_ind_sp_capital_volume[, c('vol_M3')])
data_mdp_ind_sp_capital_volume$volume_clean = tsclean(volume_ts)

#data_mdf_ind_sp_capital

# MDF Indústria - Receita Líquida
rec_ts = ts(data_mdf_ind_sum[, c('rec_liq')])
data_mdf_ind_sum$req_liq_clean = tsclean(rec_ts)

# MDF Indústria - Volume
volume_ts = ts(data_mdf_ind_sum_volume[, c('vol_M3')])
data_mdf_ind_sum_volume$volume_clean = tsclean(volume_ts)

# MDF Indústria NO/NE - Volume
volume_ts = ts(data_mdf_ind_sum_volume_no_ne[, c('vol_M3')])
data_mdf_ind_sum_volume_no_ne$volume_clean = tsclean(volume_ts)

# MDF Indústria PR/SC - Volume
volume_ts = ts(data_mdf_ind_pr_sc_sum_volume[, c('vol_M3')])
data_mdf_ind_pr_sc_sum_volume$volume_clean = tsclean(volume_ts)

# MDF Indústria RS Rep - Volume
volume_ts = ts(data_mdf_ind_rs_rep_sum_volume[, c('vol_M3')])
data_mdf_ind_rs_rep_sum_volume$volume_clean = tsclean(volume_ts)

# MDF Indústria SP Capital - Volume
volume_ts = ts(data_mdf_ind_sp_capital_volume[, c('vol_M3')])
data_mdf_ind_sp_capital_volume$volume_clean = tsclean(volume_ts)

# MDF Varejo - Receita Líquida
rec_ts = ts(data_mdf_var_sum[, c('rec_liq')])
data_mdf_var_sum$req_liq_clean = tsclean(rec_ts)

# MDF Varejo - Volume
volume_ts = ts(data_mdf_var_sum_volume[, c('vol_M3')])
data_mdf_var_sum_volume$volume_clean = tsclean(volume_ts)

# MDF Varejo SP - Volume
volume_ts = ts(data_mdf_var_sum_volume_SP[, c('vol_M3')])
data_mdf_var_sum_volume_SP$volume_clean = tsclean(volume_ts)

# MDF Varejo MG/DF/GO - Volume
volume_ts = ts(data_mdf_var_mg_df_go_sum_volume[, c('vol_M3')])
data_mdf_var_mg_df_go_sum_volume$volume_clean = tsclean(volume_ts)

# MDF Varejo NO/NE - Volume
volume_ts = ts(data_mdf_var_no_ne_sum_volume[, c('vol_M3')])
data_mdf_var_no_ne_sum_volume$volume_clean = tsclean(volume_ts)

# MDF Varejo PR/SC - Volume
volume_ts = ts(data_mdf_var_pr_sc_sum_volume[, c('vol_M3')])
data_mdf_var_pr_sc_sum_volume$volume_clean = tsclean(volume_ts)

# MDF Varejo RJ/ES - Volume
volume_ts = ts(data_mdf_var_rj_es_sum_volume[, c('vol_M3')])
data_mdf_var_rj_es_sum_volume$volume_clean = tsclean(volume_ts)

# MDP Varejo SP - Volume
volume_ts = ts(data_mdp_var_sum_volume_SP[, c('vol_M3')])
data_mdp_var_sum_volume_SP$volume_clean = tsclean(volume_ts)

# MDP Varejo - Volume
volume_ts = ts(data_mdp_var_sum_volume[, c('vol_M3')])
data_mdp_var_sum_volume$volume_clean = tsclean(volume_ts)
########################################

########################################
######## CÁLCULO DA MÉDIA MÓVEL ########
## MDP Indústria ##
# Média Móvel de 12 meses
data_mdp_ind_sum$req_liq_ma12 = ma(data_mdp_ind_sum$req_liq_clean, order=12)
data_mdp_ind_sum_volume$volume_ma12 = ma(data_mdp_ind_sum_volume$volume_clean, order=12)
data_mdp_ind_no_ne_sum_volume$volume_ma12 = ma(data_mdp_ind_no_ne_sum_volume$volume_clean, order=12)
data_mdp_ind_pr_sc_sum_volume$volume_ma12 = ma(data_mdp_ind_pr_sc_sum_volume$volume_clean, order=12)
data_mdp_ind_rs_rep_volume$volume_ma12 = ma(data_mdp_ind_rs_rep_volume$volume_clean, order=12)
data_mdp_ind_sp_capital_volume$volume_ma12 = ma(data_mdp_ind_sp_capital_volume$volume_clean, order=12)
#data_mdf_ind_sp_capital
# Média Móvel de 3 meses
data_mdp_ind_sum$req_liq_ma3 = ma(data_mdp_ind_sum$req_liq_clean, order=3)
data_mdp_ind_sum_volume$volume_ma3 = ma(data_mdp_ind_sum_volume$volume_clean, order=3)
data_mdp_ind_no_ne_sum_volume$volume_ma3 = ma(data_mdp_ind_no_ne_sum_volume$volume_clean, order=3)
data_mdp_ind_pr_sc_sum_volume$volume_ma3 = ma(data_mdp_ind_pr_sc_sum_volume$volume_clean, order=3)
data_mdp_ind_rs_rep_volume$volume_ma3 = ma(data_mdp_ind_rs_rep_volume$volume_clean, order=3)
data_mdp_ind_sp_capital_volume$volume_ma3 = ma(data_mdp_ind_sp_capital_volume$volume_clean, order=3)

## MDP Varejo ##
# Média Móvel de 12 meses
data_mdp_var_sum_volume$volume_ma12 = ma(data_mdp_var_sum_volume$volume_clean, order=12)
data_mdp_var_sum_volume_SP$volume_ma12 = ma(data_mdp_var_sum_volume_SP$volume_clean, order=12)
# Média Móvel de 3 meses
data_mdp_var_sum_volume$volume_ma3 = ma(data_mdp_var_sum_volume$volume_clean, order=3)
data_mdp_var_sum_volume_SP$volume_ma3 = ma(data_mdp_var_sum_volume_SP$volume_clean, order=3)

## MDF Indústria ##
# Média Móvel de 12 meses
data_mdf_ind_sum$req_liq_ma12 = ma(data_mdf_ind_sum$req_liq_clean, order=12)
data_mdf_ind_sum_volume$volume_ma12 = ma(data_mdf_ind_sum_volume$volume_clean, order=12)
data_mdf_ind_sum_volume_no_ne$volume_ma12 = ma(data_mdf_ind_sum_volume_no_ne$volume_clean, order=12)
data_mdf_ind_pr_sc_sum_volume$volume_ma12 = ma(data_mdf_ind_pr_sc_sum_volume$volume_clean, order=12)
data_mdf_ind_rs_rep_sum_volume$volume_ma12 = ma(data_mdf_ind_rs_rep_sum_volume$volume_clean, order=12)
data_mdf_ind_sp_capital_volume$volume_ma12 = ma(data_mdf_ind_sp_capital_volume$volume_clean, order=12)

# Média Móvel de 3 meses
data_mdf_ind_sum$req_liq_ma3 = ma(data_mdf_ind_sum$req_liq_clean, order=3)
data_mdf_ind_sum_volume$volume_ma3 = ma(data_mdf_ind_sum_volume$volume_clean, order=3)
data_mdf_ind_sum_volume_no_ne$volume_ma3 = ma(data_mdf_ind_sum_volume_no_ne$volume_clean, order=3)
data_mdf_ind_pr_sc_sum_volume$volume_ma3 = ma(data_mdf_ind_pr_sc_sum_volume$volume_clean, order=3)
data_mdf_ind_rs_rep_sum_volume$volume_ma3 = ma(data_mdf_ind_rs_rep_sum_volume$volume_clean, order=3)
data_mdf_ind_sp_capital_volume$volume_ma3 = ma(data_mdf_ind_sp_capital_volume$volume_clean, order=3)

## MDF Varejo ##
# Média Móvel de 12 meses
data_mdf_var_sum$req_liq_ma12 = ma(data_mdf_var_sum$req_liq_clean, order=12)
data_mdf_var_sum_volume$volume_ma12 = ma(data_mdf_var_sum_volume$volume_clean, order=12)
data_mdf_var_sum_volume_SP$volume_ma12 = ma(data_mdf_var_sum_volume_SP$volume_clean, order=12)
data_mdf_var_mg_df_go_sum_volume$volume_ma12 = ma(data_mdf_var_mg_df_go_sum_volume$volume_clean, order=12)
data_mdf_var_no_ne_sum_volume$volume_ma12 = ma(data_mdf_var_no_ne_sum_volume$volume_clean, order=12)
data_mdf_var_pr_sc_sum_volume$volume_ma12 = ma(data_mdf_var_pr_sc_sum_volume$volume_clean, order=12)
data_mdf_var_rj_es_sum_volume$volume_ma12 = ma(data_mdf_var_rj_es_sum_volume$volume_clean, order=12)

# Média Móvel de 3 meses
data_mdf_var_sum_volume$req_liq_ma3 = ma(data_mdf_var_sum$req_liq_clean, order=3)
data_mdf_var_sum_volume$volume_ma3 = ma(data_mdf_var_sum_volume$volume_clean, order=3)
data_mdf_var_sum_volume_SP$volume_ma3 = ma(data_mdf_var_sum_volume_SP$volume_clean, order=3)
data_mdf_var_mg_df_go_sum_volume$volume_ma3 = ma(data_mdf_var_mg_df_go_sum_volume$volume_clean, order=3)
data_mdf_var_no_ne_sum_volume$volume_ma3 = ma(data_mdf_var_no_ne_sum_volume$volume_clean, order=3)
data_mdf_var_pr_sc_sum_volume$volume_ma3 = ma(data_mdf_var_pr_sc_sum_volume$volume_clean, order=3)
data_mdf_var_rj_es_sum_volume$volume_ma3 = ma(data_mdf_var_rj_es_sum_volume$volume_clean, order=3)

# Substituindo NA por 0 no dataframe todo
data_mdp_ind_sum[is.na(data_mdp_ind_sum)] <- 0
data_mdf_ind_sum[is.na(data_mdf_ind_sum)] <- 0
data_mdf_var_sum[is.na(data_mdf_var_sum)] <- 0

data_mdp_ind_sum_volume[is.na(data_mdp_ind_sum_volume)] <- 0
data_mdf_ind_sum_volume[is.na(data_mdf_ind_sum_volume)] <- 0
data_mdf_var_sum_volume[is.na(data_mdf_var_sum_volume)] <- 0
data_mdp_var_sum_volume[is.na(data_mdf_var_sum_volume)] <- 0

data_mdp_var_sum_volume_SP[is.na(data_mdp_var_sum_volume_SP)] <- 0
data_mdf_var_sum_volume_SP[is.na(data_mdf_var_sum_volume_SP)] <- 0
data_mdp_ind_no_ne_sum_volume[is.na(data_mdp_ind_no_ne_sum_volume)] <- 0
data_mdf_ind_sum_volume_no_ne[is.na(data_mdf_ind_sum_volume_no_ne)] <- 0
data_mdp_ind_pr_sc_sum_volume[is.na(data_mdp_ind_pr_sc_sum_volume)] <- 0
data_mdf_ind_pr_sc_sum_volume[is.na(data_mdf_ind_pr_sc_sum_volume)] <- 0
data_mdp_ind_rs_rep_volume[is.na(data_mdp_ind_rs_rep_volume)] <- 0
data_mdf_ind_rs_rep_sum_volume[is.na(data_mdf_ind_rs_rep_sum_volume)] <- 0

data_mdf_ind_sp_capital_volume[is.na(data_mdf_ind_sp_capital_volume)] <- 0
data_mdp_ind_sp_capital_volume[is.na(data_mdp_ind_sp_capital_volume)] <- 0

data_mdf_var_mg_df_go_sum_volume[is.na(data_mdf_var_mg_df_go_sum_volume)] <- 0
data_mdf_var_no_ne_sum_volume[is.na(data_mdf_var_no_ne_sum_volume)] <- 0
data_mdf_var_pr_sc_sum_volume[is.na(data_mdf_var_pr_sc_sum_volume)] <- 0
data_mdf_var_rj_es_sum_volume[is.na(data_mdf_var_rj_es_sum_volume)] <- 0
########################################

########################################
######## CRIANDO SÉRIE TEMPORAL DA MÉDIA MÓVEL ########
# Criando a série temporal para a média móvel de 12 meses em todos os dataframes para receita líquida
# MDP Indústria - Receita Líquida
ts_moving_average_mdp_ind = ts(na.omit(data_mdp_ind_sum$req_liq_ma12), frequency=12,start=c(2014,1),end=c(2017,12))
decomp_mdp_ind = stl(ts_moving_average_mdp_ind, s.window="periodic")
deseasonal_cnt_mdp_ind <- seasadj(decomp_mdp_ind)

# MDP Indústria - Volume
ts_moving_average_mdp_ind_volume = ts(na.omit(data_mdp_ind_sum_volume$volume_ma12), frequency=12,start=c(2014,1),end=c(2017,12))
decomp_mdp_ind_volume = stl(ts_moving_average_mdp_ind_volume, s.window="periodic")
deseasonal_cnt_mdp_ind_volume <- seasadj(decomp_mdp_ind_volume)

# MDP Indústria NO / NE - Volume
ts_moving_average_mdp_ind_volume_no_ne = ts(na.omit(data_mdp_ind_no_ne_sum_volume$volume_ma12), frequency=12,start=c(2014,1),end=c(2017,12))
decomp_mdp_ind_volume_no_ne = stl(ts_moving_average_mdp_ind_volume_no_ne, s.window="periodic")
deseasonal_cnt_mdp_ind_volume_no_ne <- seasadj(decomp_mdp_ind_volume_no_ne)

# MDP Indústria PR/SC - Volume
ts_moving_average_mdp_ind_volume_pr_sc = ts(na.omit(data_mdp_ind_pr_sc_sum_volume$volume_ma12), frequency=12,start=c(2014,1),end=c(2017,12))
decomp_mdp_ind_volume_pr_sc = stl(ts_moving_average_mdp_ind_volume_pr_sc, s.window="periodic")
deseasonal_cnt_mdp_ind_volume_pr_sc <- seasadj(decomp_mdp_ind_volume_pr_sc)

# MDP Indústria RS Rep Volume
ts_moving_average_mdp_ind_volume_rs_rep = ts(na.omit(data_mdp_ind_rs_rep_volume$volume_ma12), frequency=12,start=c(2014,1),end=c(2017,12))
decomp_mdp_ind_volume_rs_rep = stl(ts_moving_average_mdp_ind_volume_rs_rep, s.window="periodic")
deseasonal_cnt_mdp_ind_volume_rs_rep <- seasadj(decomp_mdp_ind_volume_rs_rep)

# MDP Indústria SP Capital
ts_moving_average_mdp_ind_volume_sp_capital = ts(na.omit(data_mdp_ind_sp_capital_volume$volume_ma12), frequency=12,start=c(2014,1),end=c(2017,12))
decomp_mdp_ind_volume_sp_capital = stl(ts_moving_average_mdp_ind_volume_sp_capital, s.window="periodic")
deseasonal_cnt_mdp_ind_volume_sp_capital <- seasadj(decomp_mdp_ind_volume_sp_capital)


# MDP Varejo - Volume
ts_moving_average_mdp_var_volume = ts(na.omit(data_mdp_var_sum_volume$volume_ma12), frequency=12,start=c(2014,1),end=c(2017,12))
decomp_mdp_var_volume = stl(ts_moving_average_mdp_var_volume, s.window="periodic")
deseasonal_cnt_mdp_var_volume <- seasadj(decomp_mdp_var_volume)

# MDP Varejo SP - Volume
ts_moving_average_mdp_var_volume_SP = ts(na.omit(data_mdp_var_sum_volume_SP$volume_ma12), frequency=12,start=c(2014,1),end=c(2017,12))
decomp_mdp_var_volume_SP = stl(ts_moving_average_mdp_var_volume_SP, s.window="periodic")
deseasonal_cnt_mdp_var_volume_SP <- seasadj(decomp_mdp_var_volume_SP)

# MDF Indústria - Receita Líquida
ts_moving_average_mdf_ind = ts(na.omit(data_mdf_ind_sum$req_liq_ma12), frequency=12,start=c(2014,1),end=c(2017,12))
decomp_mdf_ind = stl(ts_moving_average_mdf_ind, s.window="periodic")
deseasonal_cnt_mdf_ind <- seasadj(decomp_mdf_ind)

# MDF Indústria - Volume
ts_moving_average_mdf_ind_volume = ts(na.omit(data_mdf_ind_sum_volume$volume_ma12), frequency=12,start=c(2014,1),end=c(2017,12))
decomp_mdf_ind_volume = stl(ts_moving_average_mdf_ind_volume, s.window="periodic")
deseasonal_cnt_mdf_ind_volume <- seasadj(decomp_mdf_ind_volume)

# MDF Indústria NO/NE - Volume
ts_moving_average_mdf_ind_volume_no_ne = ts(na.omit(data_mdf_ind_sum_volume_no_ne$volume_ma12), frequency=12,start=c(2014,1),end=c(2017,12))
decomp_mdf_ind_volume_no_ne = stl(ts_moving_average_mdf_ind_volume_no_ne, s.window="periodic")
deseasonal_cnt_mdf_ind_volume_no_ne <- seasadj(decomp_mdf_ind_volume_no_ne)

# MDF Indústria PR/SC - Volume
ts_moving_average_mdf_ind_volume_pr_sc = ts(na.omit(data_mdf_ind_pr_sc_sum_volume$volume_ma12), frequency=12,start=c(2014,1),end=c(2017,12))
decomp_mdf_ind_volume_pr_sc = stl(ts_moving_average_mdf_ind_volume_pr_sc, s.window="periodic")
deseasonal_cnt_mdf_ind_volume_pr_sc <- seasadj(decomp_mdf_ind_volume_pr_sc)

# MDF Indústria RS Rep - Volume
ts_moving_average_mdf_ind_volume_rs_rep = ts(na.omit(data_mdf_ind_rs_rep_sum_volume$volume_ma12), frequency=12,start=c(2014,1),end=c(2017,12))
decomp_mdf_ind_volume_rs_rep = stl(ts_moving_average_mdf_ind_volume_rs_rep, s.window="periodic")
deseasonal_cnt_mdf_ind_volume_rs_rep <- seasadj(decomp_mdf_ind_volume_rs_rep)

# MDF Indústria SP Capital - Volume
ts_moving_average_mdf_ind_volume_sp_capital = ts(na.omit(data_mdf_ind_sp_capital_volume$volume_ma12), frequency=12,start=c(2014,1),end=c(2017,12))
decomp_mdf_ind_volume_sp_capital = stl(ts_moving_average_mdf_ind_volume_sp_capital, s.window="periodic")
deseasonal_cnt_mdf_ind_volume_sp_capital <- seasadj(decomp_mdf_ind_volume_sp_capital)

# MDF Varejo - Receita Líquida
ts_moving_average_mdf_var = ts(na.omit(data_mdf_var_sum$req_liq_ma12), frequency=12,start=c(2014,1),end=c(2017,12))
decomp_mdf_var = stl(ts_moving_average_mdf_var, s.window="periodic")
deseasonal_cnt_mdf_var <- seasadj(decomp_mdf_var)

# MDF Varejo - Volume
ts_moving_average_mdf_var_volume = ts(na.omit(data_mdf_var_sum_volume$volume_ma12), frequency=12,start=c(2014,1),end=c(2017,12))
decomp_mdf_var_volume = stl(ts_moving_average_mdf_var_volume, s.window="periodic")
deseasonal_cnt_mdf_var_volume <- seasadj(decomp_mdf_var_volume)

# MDF Varejo SP - Volume
ts_moving_average_mdf_var_volume_SP = ts(na.omit(data_mdf_var_sum_volume_SP$volume_ma12), frequency=12,start=c(2014,1),end=c(2017,12))
decomp_mdf_var_volume_SP = stl(ts_moving_average_mdf_var_volume_SP, s.window="periodic")
deseasonal_cnt_mdf_var_volume_SP <- seasadj(decomp_mdf_var_volume_SP)

# MDF Varejo MG/DF/GO - Volume
ts_moving_average_mdf_var_volume_mg_df_go = ts(na.omit(data_mdf_var_mg_df_go_sum_volume$volume_ma12), frequency=12,start=c(2014,1),end=c(2017,12))
decomp_mdf_var_volume_mg_df_go = stl(ts_moving_average_mdf_var_volume_mg_df_go, s.window="periodic")
deseasonal_cnt_mdf_var_volume_mg_df_go <- seasadj(decomp_mdf_var_volume_mg_df_go)

# MDF Varejo NO/NE - Volume
ts_moving_average_mdf_var_volume_no_ne = ts(na.omit(data_mdf_var_no_ne_sum_volume$volume_ma12), frequency=12,start=c(2014,1),end=c(2017,12))
decomp_mdf_var_volume_no_ne = stl(ts_moving_average_mdf_var_volume_no_ne, s.window="periodic")
deseasonal_cnt_mdf_var_volume_no_ne <- seasadj(decomp_mdf_var_volume_no_ne)

# MDF Varejo PR/SC - Volume
ts_moving_average_mdf_var_volume_pr_sc = ts(na.omit(data_mdf_var_pr_sc_sum_volume$volume_ma12), frequency=12,start=c(2014,1),end=c(2017,12))
decomp_mdf_var_volume_pr_sc = stl(ts_moving_average_mdf_var_volume_pr_sc, s.window="periodic")
deseasonal_cnt_mdf_var_volume_pr_sc <- seasadj(decomp_mdf_var_volume_pr_sc)

# MDF Varejo RJ/ES - Volume
ts_moving_average_mdf_var_volume_rj_es = ts(na.omit(data_mdf_var_rj_es_sum_volume$volume_ma12), frequency=12,start=c(2014,1),end=c(2017,12))
decomp_mdf_var_volume_rj_es = stl(ts_moving_average_mdf_var_volume_rj_es, s.window="periodic")
deseasonal_cnt_mdf_var_volume_rj_es <- seasadj(decomp_mdf_var_volume_rj_es)

########################################

########################################
######## CRIANDO DF NOVO PARA REMOVER 6 PRIMEIROS E ÚLTIMOS MESES ZERADOS ########
# Criando uma base nova, removendo os 6 primeiros e últimos meses zerados
# MDP Indústria - Receita Líquida
df_ts_moving_average_mdp_ind <- as.data.frame(ts_moving_average_mdp_ind)
df_ts_moving_average_mdp_ind <- tail(df_ts_moving_average_mdp_ind, -6)
df_ts_moving_average_mdp_ind = head(df_ts_moving_average_mdp_ind, -6)
ts_moving_average_new_mdp_ind = ts(na.omit(df_ts_moving_average_mdp_ind$x), frequency=12,start=c(2014,6),end=c(2017,6))
decomp_mdp_ind = stl(ts_moving_average_new_mdp_ind, s.window="periodic")
deseasonal_cnt_mdp_ind <- seasadj(decomp_mdp_ind)

# MDP Indústria - Volume
df_ts_moving_average_mdp_ind_volume <- as.data.frame(ts_moving_average_mdp_ind_volume)
df_ts_moving_average_mdp_ind_volume <- tail(df_ts_moving_average_mdp_ind_volume, -6)
df_ts_moving_average_mdp_ind_volume = head(df_ts_moving_average_mdp_ind_volume, -6)
ts_moving_average_new_mdp_ind_volume = ts(na.omit(df_ts_moving_average_mdp_ind_volume$x), frequency=12,start=c(2014,6),end=c(2017,6))
decomp_mdp_ind_volume = stl(ts_moving_average_new_mdp_ind_volume, s.window="periodic")
deseasonal_cnt_mdp_ind_volume <- seasadj(decomp_mdp_ind_volume)

# MDP Indústria NO / NE - Volume
df_ts_moving_average_mdp_ind_volume_no_ne <- as.data.frame(ts_moving_average_mdp_ind_volume_no_ne)
df_ts_moving_average_mdp_ind_volume_no_ne <- tail(df_ts_moving_average_mdp_ind_volume_no_ne, -6)
df_ts_moving_average_mdp_ind_volume_no_ne = head(df_ts_moving_average_mdp_ind_volume_no_ne, -6)
ts_moving_average_new_mdp_ind_volume_no_ne = ts(na.omit(df_ts_moving_average_mdp_ind_volume_no_ne$x), frequency=12,start=c(2014,6),end=c(2017,6))
decomp_mdp_ind_volume_no_ne = stl(ts_moving_average_new_mdp_ind_volume_no_ne, s.window="periodic")
deseasonal_cnt_mdp_ind_volume_no_ne <- seasadj(decomp_mdp_ind_volume_no_ne)

# MDP Indústria PR/SC - Volume
df_ts_moving_average_mdp_ind_volume_pr_sc <- as.data.frame(ts_moving_average_mdp_ind_volume_pr_sc)
df_ts_moving_average_mdp_ind_volume_pr_sc <- tail(df_ts_moving_average_mdp_ind_volume_pr_sc, -6)
df_ts_moving_average_mdp_ind_volume_pr_sc = head(df_ts_moving_average_mdp_ind_volume_pr_sc, -6)
ts_moving_average_new_mdp_ind_volume_pr_sc = ts(na.omit(df_ts_moving_average_mdp_ind_volume_pr_sc$x), frequency=12,start=c(2014,6),end=c(2017,6))
decomp_mdp_ind_volume_pr_sc = stl(ts_moving_average_new_mdp_ind_volume_pr_sc, s.window="periodic")
deseasonal_cnt_mdp_ind_volume_pr_sc <- seasadj(decomp_mdp_ind_volume_pr_sc)

# MDP Indústria RS Rep - Volume
df_ts_moving_average_mdp_ind_volume_rs_rep <- as.data.frame(ts_moving_average_mdp_ind_volume_rs_rep)
df_ts_moving_average_mdp_ind_volume_rs_rep <- tail(df_ts_moving_average_mdp_ind_volume_rs_rep, -6)
df_ts_moving_average_mdp_ind_volume_rs_rep = head(df_ts_moving_average_mdp_ind_volume_rs_rep, -6)
ts_moving_average_new_mdp_ind_volume_rs_rep = ts(na.omit(df_ts_moving_average_mdp_ind_volume_rs_rep$x), frequency=12,start=c(2014,6),end=c(2017,6))
decomp_mdp_ind_volume_rs_rep = stl(ts_moving_average_new_mdp_ind_volume_rs_rep, s.window="periodic")
deseasonal_cnt_mdp_ind_volume_rs_rep <- seasadj(decomp_mdp_ind_volume_rs_rep)

# MDP Indústria SP Capital - Volume
df_ts_moving_average_mdp_ind_volume_sp_capital <- as.data.frame(ts_moving_average_mdp_ind_volume_sp_capital)
df_ts_moving_average_mdp_ind_volume_sp_capital <- tail(df_ts_moving_average_mdp_ind_volume_sp_capital, -6)
df_ts_moving_average_mdp_ind_volume_sp_capital = head(df_ts_moving_average_mdp_ind_volume_sp_capital, -6)
ts_moving_average_new_mdp_ind_volume_sp_capital = ts(na.omit(df_ts_moving_average_mdp_ind_volume_sp_capital$x), frequency=12,start=c(2014,6),end=c(2017,6))
decomp_mdp_ind_volume_sp_capital = stl(ts_moving_average_new_mdp_ind_volume_sp_capital, s.window="periodic")
deseasonal_cnt_mdp_ind_volume_sp_capital <- seasadj(decomp_mdp_ind_volume_sp_capital)

# MDP Varejo - Volume
df_ts_moving_average_mdp_var_volume <- as.data.frame(ts_moving_average_mdp_var_volume)
df_ts_moving_average_mdp_var_volume <- tail(df_ts_moving_average_mdp_var_volume, -6)
df_ts_moving_average_mdp_var_volume = head(df_ts_moving_average_mdp_var_volume, -6)
ts_moving_average_new_mdp_var_volume = ts(na.omit(df_ts_moving_average_mdp_var_volume$x), frequency=12,start=c(2014,6),end=c(2017,6))
decomp_mdp_var_volume = stl(ts_moving_average_new_mdp_var_volume, s.window="periodic")
deseasonal_cnt_mdp_var_volume <- seasadj(decomp_mdp_var_volume)

# MDP Varejo SP - Volume
df_ts_moving_average_mdp_var_volume_SP <- as.data.frame(ts_moving_average_mdp_var_volume_SP)
df_ts_moving_average_mdp_var_volume_SP <- tail(df_ts_moving_average_mdp_var_volume_SP, -6)
df_ts_moving_average_mdp_var_volume_SP = head(df_ts_moving_average_mdp_var_volume_SP, -6)
ts_moving_average_new_mdp_var_volume_SP = ts(na.omit(df_ts_moving_average_mdp_var_volume_SP$x), frequency=12,start=c(2014,6),end=c(2017,6))
decomp_mdp_var_volume_SP = stl(ts_moving_average_new_mdp_var_volume_SP, s.window="periodic")
deseasonal_cnt_mdp_var_volume_SP <- seasadj(decomp_mdp_var_volume_SP)

# MDF Indústria - Receita Líquida
df_ts_moving_average_mdf_ind <- as.data.frame(ts_moving_average_mdf_ind)
df_ts_moving_average_mdf_ind <- tail(df_ts_moving_average_mdf_ind, -6)
df_ts_moving_average_mdf_ind = head(df_ts_moving_average_mdf_ind, -6)
ts_moving_average_new_mdf_ind = ts(na.omit(df_ts_moving_average_mdf_ind$x), frequency=12,start=c(2014,6),end=c(2017,6))
decomp_mdf_ind = stl(ts_moving_average_new_mdf_ind, s.window="periodic")
deseasonal_cnt_mdf_ind <- seasadj(decomp_mdf_ind)

# MDF Indústria - Volume
df_ts_moving_average_mdf_ind_volume <- as.data.frame(ts_moving_average_mdf_ind_volume)
df_ts_moving_average_mdf_ind_volume <- tail(df_ts_moving_average_mdf_ind_volume, -6)
df_ts_moving_average_mdf_ind_volume = head(df_ts_moving_average_mdf_ind_volume, -6)
ts_moving_average_new_mdf_ind_volume = ts(na.omit(df_ts_moving_average_mdf_ind_volume$x), frequency=12,start=c(2014,6),end=c(2017,6))
decomp_mdf_ind_volume = stl(ts_moving_average_new_mdf_ind_volume, s.window="periodic")
deseasonal_cnt_mdf_ind_volume <- seasadj(decomp_mdf_ind_volume)

# MDF Indústria NO/NE - Volume
df_ts_moving_average_mdf_ind_volume_no_ne <- as.data.frame(ts_moving_average_mdf_ind_volume_no_ne)
df_ts_moving_average_mdf_ind_volume_no_ne <- tail(df_ts_moving_average_mdf_ind_volume_no_ne, -6)
df_ts_moving_average_mdf_ind_volume_no_ne = head(df_ts_moving_average_mdf_ind_volume_no_ne, -6)
ts_moving_average_new_mdf_ind_volume_no_ne = ts(na.omit(df_ts_moving_average_mdf_ind_volume_no_ne$x), frequency=12,start=c(2014,6),end=c(2017,6))
decomp_mdf_ind_volume_no_ne = stl(ts_moving_average_new_mdf_ind_volume_no_ne, s.window="periodic")
deseasonal_cnt_mdf_ind_volume_no_ne <- seasadj(decomp_mdf_ind_volume_no_ne)

# MDF Indústria PR/SC - Volume
df_ts_moving_average_mdf_ind_volume_pr_sc <- as.data.frame(ts_moving_average_mdf_ind_volume_pr_sc)
df_ts_moving_average_mdf_ind_volume_pr_sc <- tail(df_ts_moving_average_mdf_ind_volume_pr_sc, -6)
df_ts_moving_average_mdf_ind_volume_pr_sc = head(df_ts_moving_average_mdf_ind_volume_pr_sc, -6)
ts_moving_average_new_mdf_ind_volume_pr_sc = ts(na.omit(df_ts_moving_average_mdf_ind_volume_pr_sc$x), frequency=12,start=c(2014,6),end=c(2017,6))
decomp_mdf_ind_volume_pr_sc = stl(ts_moving_average_new_mdf_ind_volume_pr_sc, s.window="periodic")
deseasonal_cnt_mdf_ind_volume_pr_sc <- seasadj(decomp_mdf_ind_volume_pr_sc)

# MDF Indústria RS Rep - Volume
df_ts_moving_average_mdf_ind_volume_rs_rep <- as.data.frame(ts_moving_average_mdf_ind_volume_rs_rep)
df_ts_moving_average_mdf_ind_volume_rs_rep <- tail(df_ts_moving_average_mdf_ind_volume_rs_rep, -6)
df_ts_moving_average_mdf_ind_volume_rs_rep = head(df_ts_moving_average_mdf_ind_volume_rs_rep, -6)
ts_moving_average_new_mdf_ind_volume_rs_rep = ts(na.omit(df_ts_moving_average_mdf_ind_volume_rs_rep$x), frequency=12,start=c(2014,6),end=c(2017,6))
decomp_mdf_ind_volume_rs_rep = stl(ts_moving_average_new_mdf_ind_volume_rs_rep, s.window="periodic")
deseasonal_cnt_mdf_ind_volume_rs_rep <- seasadj(decomp_mdf_ind_volume_rs_rep)

# MDF Indústria SP Capital - Volume
df_ts_moving_average_mdf_ind_volume_sp_capital <- as.data.frame(ts_moving_average_mdf_ind_volume_sp_capital)
df_ts_moving_average_mdf_ind_volume_sp_capital <- tail(df_ts_moving_average_mdf_ind_volume_sp_capital, -6)
df_ts_moving_average_mdf_ind_volume_sp_capital = head(df_ts_moving_average_mdf_ind_volume_sp_capital, -6)
ts_moving_average_new_mdf_ind_volume_sp_capital= ts(na.omit(df_ts_moving_average_mdf_ind_volume_sp_capital$x), frequency=12,start=c(2014,6),end=c(2017,6))
decomp_mdf_ind_volume_sp_capital = stl(ts_moving_average_new_mdf_ind_volume_sp_capital, s.window="periodic")
deseasonal_cnt_mdf_ind_volume_sp_capital <- seasadj(decomp_mdf_ind_volume_sp_capital)

# MDF Varejo - Receita Líquida
df_ts_moving_average_mdf_var <- as.data.frame(ts_moving_average_mdf_var)
df_ts_moving_average_mdf_var <- tail(df_ts_moving_average_mdf_var, -6)
df_ts_moving_average_mdf_var = head(df_ts_moving_average_mdf_var, -6)
ts_moving_average_new_mdf_var = ts(na.omit(df_ts_moving_average_mdf_var$x), frequency=12,start=c(2014,6),end=c(2017,6))
decomp_mdf_var = stl(ts_moving_average_new_mdf_var, s.window="periodic")
deseasonal_cnt_mdf_var <- seasadj(decomp_mdf_var)

# MDF Varejo - Volume
df_ts_moving_average_mdf_var_volume <- as.data.frame(ts_moving_average_mdf_var_volume)
df_ts_moving_average_mdf_var_volume <- tail(df_ts_moving_average_mdf_var_volume, -6)
df_ts_moving_average_mdf_var_volume = head(df_ts_moving_average_mdf_var_volume, -6)
ts_moving_average_new_mdf_var_volume = ts(na.omit(df_ts_moving_average_mdf_var_volume$x), frequency=12,start=c(2014,6),end=c(2017,6))
decomp_mdf_var_volume = stl(ts_moving_average_new_mdf_var_volume, s.window="periodic")
deseasonal_cnt_mdf_var_volume <- seasadj(decomp_mdf_var_volume)

# MDF Varejo SP - Volume
df_ts_moving_average_mdf_var_volume_SP <- as.data.frame(ts_moving_average_mdf_var_volume_SP)
df_ts_moving_average_mdf_var_volume_SP <- tail(df_ts_moving_average_mdf_var_volume_SP, -6)
df_ts_moving_average_mdf_var_volume_SP = head(df_ts_moving_average_mdf_var_volume_SP, -6)
ts_moving_average_new_mdf_var_volume_SP = ts(na.omit(df_ts_moving_average_mdf_var_volume_SP$x), frequency=12,start=c(2014,6),end=c(2017,6))
decomp_mdf_var_volume_SP = stl(ts_moving_average_new_mdf_var_volume_SP, s.window="periodic")
deseasonal_cnt_mdf_var_volume_SP <- seasadj(decomp_mdf_var_volume_SP)

# MDF Varejo MG/DF/GO - Volume
df_ts_moving_average_mdf_var_volume_mg_df_go <- as.data.frame(ts_moving_average_mdf_var_volume_mg_df_go)
df_ts_moving_average_mdf_var_volume_mg_df_go <- tail(df_ts_moving_average_mdf_var_volume_mg_df_go, -6)
df_ts_moving_average_mdf_var_volume_mg_df_go = head(df_ts_moving_average_mdf_var_volume_mg_df_go, -6)
ts_moving_average_new_mdf_var_volume_mg_df_go = ts(na.omit(df_ts_moving_average_mdf_var_volume_mg_df_go$x), frequency=12,start=c(2014,6),end=c(2017,6))
decomp_mdf_var_volume_mg_df_go = stl(ts_moving_average_new_mdf_var_volume_mg_df_go, s.window="periodic")
deseasonal_cnt_mdf_var_volume_mg_df_go <- seasadj(decomp_mdf_var_volume_mg_df_go)

# MDF Varejo NO/NE - Volume
df_ts_moving_average_mdf_var_volume_no_ne <- as.data.frame(ts_moving_average_mdf_var_volume_no_ne)
df_ts_moving_average_mdf_var_volume_no_ne <- tail(df_ts_moving_average_mdf_var_volume_no_ne, -6)
df_ts_moving_average_mdf_var_volume_no_ne = head(df_ts_moving_average_mdf_var_volume_no_ne, -6)
ts_moving_average_new_mdf_var_volume_no_ne = ts(na.omit(df_ts_moving_average_mdf_var_volume_no_ne$x), frequency=12,start=c(2014,6),end=c(2017,6))
decomp_mdf_var_volume_no_ne = stl(ts_moving_average_new_mdf_var_volume_no_ne, s.window="periodic")
deseasonal_cnt_mdf_var_volume_no_ne <- seasadj(decomp_mdf_var_volume_no_ne)

# MDF Varejo PR/SC - Volume
df_ts_moving_average_mdf_var_volume_pr_sc <- as.data.frame(ts_moving_average_mdf_var_volume_pr_sc)
df_ts_moving_average_mdf_var_volume_pr_sc <- tail(df_ts_moving_average_mdf_var_volume_pr_sc, -6)
df_ts_moving_average_mdf_var_volume_pr_sc = head(df_ts_moving_average_mdf_var_volume_pr_sc, -6)
ts_moving_average_new_mdf_var_volume_pr_sc = ts(na.omit(df_ts_moving_average_mdf_var_volume_pr_sc$x), frequency=12,start=c(2014,6),end=c(2017,6))
decomp_mdf_var_volume_pr_sc = stl(ts_moving_average_new_mdf_var_volume_pr_sc, s.window="periodic")
deseasonal_cnt_mdf_var_volume_pr_sc <- seasadj(decomp_mdf_var_volume_pr_sc)

# MDF Varejo RJ/ES - Volume
df_ts_moving_average_mdf_var_volume_rj_es <- as.data.frame(ts_moving_average_mdf_var_volume_rj_es)
df_ts_moving_average_mdf_var_volume_rj_es <- tail(df_ts_moving_average_mdf_var_volume_rj_es, -6)
df_ts_moving_average_mdf_var_volume_rj_es = head(df_ts_moving_average_mdf_var_volume_rj_es, -6)
ts_moving_average_new_mdf_var_volume_rj_es = ts(na.omit(df_ts_moving_average_mdf_var_volume_rj_es$x), frequency=12,start=c(2014,6),end=c(2017,6))
decomp_mdf_var_volume_rj_es = stl(ts_moving_average_new_mdf_var_volume_rj_es, s.window="periodic")
deseasonal_cnt_mdf_var_volume_rj_es <- seasadj(decomp_mdf_var_volume_rj_es)



########################################

########################################
######## AGRUPANDO DF DE FATURAMENTO E DADOS ECONÔMICOS PELA DATA ########
# Agrupando os dataframes e dados_economicos pela data
data_mdp_ind_sum <- merge(data_mdp_ind_sum,dados_economicos,by="dt_competencia")
data_mdp_var_sum <- merge(data_mdp_var_sum,dados_economicos,by="dt_competencia")
data_mdf_ind_sum <- merge(data_mdf_ind_sum,dados_economicos,by="dt_competencia")
data_mdf_var_sum <- merge(data_mdf_var_sum,dados_economicos,by="dt_competencia")

data_mdp_ind_sum_volume <- merge(data_mdp_ind_sum_volume,dados_economicos,by="dt_competencia")
data_mdp_var_sum_volume <- merge(data_mdp_var_sum_volume,dados_economicos,by="dt_competencia")
data_mdf_ind_sum_volume <- merge(data_mdf_ind_sum_volume,dados_economicos,by="dt_competencia")
data_mdf_var_sum_volume <- merge(data_mdf_var_sum_volume,dados_economicos,by="dt_competencia")
data_mdp_var_sum_volume_SP <-  merge(data_mdp_var_sum_volume_SP,dados_economicos,by="dt_competencia")
data_mdf_var_sum_volume_SP <-  merge(data_mdf_var_sum_volume_SP,dados_economicos,by="dt_competencia")
data_mdp_ind_no_ne_sum_volume <-  merge(data_mdp_ind_no_ne_sum_volume,dados_economicos,by="dt_competencia")
data_mdp_ind_pr_sc_sum_volume <-  merge(data_mdp_ind_pr_sc_sum_volume,dados_economicos,by="dt_competencia")
data_mdf_ind_pr_sc_sum_volume <-  merge(data_mdf_ind_pr_sc_sum_volume,dados_economicos,by="dt_competencia")
data_mdp_ind_rs_rep_volume <-  merge(data_mdp_ind_rs_rep_volume,dados_economicos,by="dt_competencia")
data_mdf_ind_rs_rep_sum_volume <-  merge(data_mdf_ind_rs_rep_sum_volume,dados_economicos,by="dt_competencia")

data_mdf_ind_sp_capital_volume <-  merge(data_mdf_ind_sp_capital_volume,dados_economicos,by="dt_competencia")
data_mdp_ind_sp_capital_volume <-  merge(data_mdp_ind_sp_capital_volume,dados_economicos,by="dt_competencia")

data_mdf_var_mg_df_go_sum_volume <- merge(data_mdf_var_mg_df_go_sum_volume,dados_economicos,by="dt_competencia")
data_mdf_var_no_ne_sum_volume <- merge(data_mdf_var_no_ne_sum_volume,dados_economicos,by="dt_competencia")
data_mdf_var_pr_sc_sum_volume <- merge(data_mdf_var_pr_sc_sum_volume,dados_economicos,by="dt_competencia")
data_mdf_var_rj_es_sum_volume <- merge(data_mdf_var_rj_es_sum_volume,dados_economicos,by="dt_competencia")
########################################

########################################
######## CRIANDO SÉRIE TEMPORAL COM TODOS OS DADOS ECONÔMICOS ######## 
# Criando uma série temporal chamada ts_dados_economicos, com todos 
#   os dados econômicos
ts_dados_economicos <- ts(dados_economicos, start=c(2014,1), end=c(2017,12), frequency=12)

# Removendo os 6 primeiros dados e 6 últimos por causa da média móvel
# de 12 meses
df_ts_dados_economicos <- as.data.frame(ts_dados_economicos)
df_ts_dados_economicos <- tail(df_ts_dados_economicos, -6)
df_ts_dados_economicos = head(df_ts_dados_economicos, -6)

# Criando uma nova série temporal, sem os primeiros e últimos 6 meses
ts_dados_economicos_new = ts(df_ts_dados_economicos, frequency=12,start=c(2014,6),end=c(2017,6))

# Substituindo NA por 0 no dataframe todo
ts_dados_economicos_new[is.na(ts_dados_economicos_new)] <- 0

# Salvando o PIB
PIB <- ts_dados_economicos_new[,"PIB"]
PIB

# Salvando a SELIC
SELIC <- ts_dados_economicos_new[,"SELIC"]
SELIC

# Salvando o IPCA
IPCA <- ts_dados_economicos_new[,"IPCA"]
IPCA

# Salvando o FAT_PROD_DE_MADEIRA
FAT_PROD_DE_MADEIRA <- ts_dados_economicos_new[,"FAT_PROD_DE_MADEIRA"]
FAT_PROD_DE_MADEIRA

# Salvando o INCC
INCC <- ts_dados_economicos_new[,"INCC"]
INCC

# Salvando o INPC
INPC <- ts_dados_economicos_new[,"INPC"]
INPC

# Salvando o SALARIO_MINIMO_REAL
SALARIO_MINIMO_REAL <- ts_dados_economicos_new[,"SALARIO_MINIMO_REAL"]
SALARIO_MINIMO_REAL

FAT_PROD_MAD_DEFLAC <- ts_dados_economicos_new[,"FAT_PROD_MAD_DEFLAC"]
FAT_PROD_MAD_DEFLAC

LIC_AUTO_NACIONAIS <- ts_dados_economicos_new[,"LIC_AUTO_NACIONAIS"]
LIC_AUTO_NACIONAIS

LIC_COM_LEVES_NACIONAIS <- ts_dados_economicos_new[,"LIC_COM_LEVES_NACIONAIS"]
LIC_COM_LEVES_NACIONAIS

ELETRODOMESTICOS <- ts_dados_economicos_new[,"ELETRODOMESTICOS"]
ELETRODOMESTICOS

IND_EXP_NIVEL_DE_ATIV_CONSTRUCAO <- ts_dados_economicos_new[,"IND_EXP_NIVEL_DE_ATIV_CONSTRUCAO"]
IND_EXP_NIVEL_DE_ATIV_CONSTRUCAO

IND_EXP_CONSUMIDOR <- ts_dados_economicos_new[,"IND_EXP_CONSUMIDOR"]
IND_EXP_CONSUMIDOR

IND_CONF_CONSUMIDOR <- ts_dados_economicos_new[,"IND_CONF_CONSUMIDOR"]
IND_CONF_CONSUMIDOR

IND_EXP_RENDA_MEDIA <- ts_dados_economicos_new[,"IND_EXP_RENDA_MEDIA"]
IND_EXP_RENDA_MEDIA

HRS_TRAB_PROD_MOVEIS <- ts_dados_economicos_new[,"HRS_TRAB_PROD_MOVEIS"]
HRS_TRAB_PROD_MOVEIS

EMPREGO_MOVEIS <- ts_dados_economicos_new[,"EMPREGO_MOVEIS"]
EMPREGO_MOVEIS

TAXA_DESEMPREGO <- ts_dados_economicos_new[,"TAXA_DESEMPREGO"]
TAXA_DESEMPREGO

DIAS_UTEIS <- ts_dados_economicos_new[,"DIAS_UTEIS"]
DIAS_UTEIS

IGPM <- ts_dados_economicos_new[,"IGP.M"]
IGPM

IPC_GERAL <- ts_dados_economicos_new[,"IPC_GERAL"]
IPC_GERAL

CUB_MEDIO_M2_BRASIL_CONSTRUCAO <- ts_dados_economicos_new[,"CUB_MEDIO_M2_BRASIL_CONSTRUCAO"]
CUB_MEDIO_M2_BRASIL_CONSTRUCAO

ACESSOS_PRE_PAGO <- ts_dados_economicos_new[,"ACESSOS_PRE_PAGO"]
ACESSOS_PRE_PAGO

ACESSOS_POS_PAGO <- ts_dados_economicos_new[,"ACESSOS_POS_PAGO"]
ACESSOS_POS_PAGO

# ICEI
ICEI_MADEIRA <- ts_dados_economicos_new[,"ICEI_MADEIRA"]
ICEI_MADEIRA

ICEI_BRASIL <- ts_dados_economicos_new[,"ICEI_BRASIL"]
ICEI_BRASIL

ICEI_BRASIL_COND_ECONOMIA_BRASILEIRA <- ts_dados_economicos_new[,"ICEI_BRASIL_COND_ECONOMIA_BRASILEIRA"]
ICEI_BRASIL_COND_ECONOMIA_BRASILEIRA
########################################

########################### FORECAST ################################

########################################
######## ARIMA ########
# MDP Indústria - Receita Líquida #
fit_mdp_ind <- auto.arima(x = deseasonal_cnt_mdp_ind, seasonal=FALSE, xreg=cbind(PIB,SELIC,INCC,INPC,IPCA,SALARIO_MINIMO_REAL,IND_EXP_CONSUMIDOR,IND_CONF_CONSUMIDOR,IND_EXP_RENDA_MEDIA,DIAS_UTEIS,FAT_PROD_DE_MADEIRA,FAT_PROD_MAD_DEFLAC,EMPREGO_MOVEIS,HRS_TRAB_PROD_MOVEIS,LIC_AUTO_NACIONAIS,LIC_COM_LEVES_NACIONAIS,ELETRODOMESTICOS,IND_EXP_NIVEL_DE_ATIV_CONSTRUCAO,TAXA_DESEMPREGO,IGPM,IPC_GERAL,ICEI_BRASIL_COND_ECONOMIA_BRASILEIRA,ICEI_MADEIRA,ICEI_BRASIL,CUB_MEDIO_M2_BRASIL_CONSTRUCAO))
# Realizando o forecast e plotando o gráfico
fcast_mdp_ind <- forecast(fit_mdp_ind, h=12, xreg=cbind(PIB,SELIC,INCC,INPC,IPCA,SALARIO_MINIMO_REAL,IND_EXP_CONSUMIDOR,IND_CONF_CONSUMIDOR,IND_EXP_RENDA_MEDIA,DIAS_UTEIS,FAT_PROD_DE_MADEIRA,FAT_PROD_MAD_DEFLAC,EMPREGO_MOVEIS,HRS_TRAB_PROD_MOVEIS,LIC_AUTO_NACIONAIS,LIC_COM_LEVES_NACIONAIS,ELETRODOMESTICOS,IND_EXP_NIVEL_DE_ATIV_CONSTRUCAO,TAXA_DESEMPREGO,IGPM,IPC_GERAL,ICEI_BRASIL_COND_ECONOMIA_BRASILEIRA,ICEI_MADEIRA,ICEI_BRASIL,CUB_MEDIO_M2_BRASIL_CONSTRUCAO))
# Transformando o resultado do forecast em um dataframe
fcast_mdp_ind <- as.data.frame(fcast_mdp_ind)

# MDP Indústria - Volume #
fit_mdp_ind_volume <- auto.arima(x = deseasonal_cnt_mdp_ind_volume, seasonal=FALSE, xreg=cbind(PIB,SELIC,INCC,INPC,IPCA,SALARIO_MINIMO_REAL,IND_EXP_CONSUMIDOR,IND_CONF_CONSUMIDOR,IND_EXP_RENDA_MEDIA,DIAS_UTEIS,FAT_PROD_DE_MADEIRA,FAT_PROD_MAD_DEFLAC,EMPREGO_MOVEIS,HRS_TRAB_PROD_MOVEIS,LIC_AUTO_NACIONAIS,LIC_COM_LEVES_NACIONAIS,ELETRODOMESTICOS,IND_EXP_NIVEL_DE_ATIV_CONSTRUCAO,TAXA_DESEMPREGO,IGPM,IPC_GERAL,ICEI_BRASIL_COND_ECONOMIA_BRASILEIRA,ICEI_MADEIRA,ICEI_BRASIL,CUB_MEDIO_M2_BRASIL_CONSTRUCAO))
# Realizando o forecast e plotando o gráfico
fcast_mdp_ind_volume <- forecast(fit_mdp_ind_volume, h=12, xreg=cbind(PIB,SELIC,INCC,INPC,IPCA,SALARIO_MINIMO_REAL,IND_EXP_CONSUMIDOR,IND_CONF_CONSUMIDOR,IND_EXP_RENDA_MEDIA,DIAS_UTEIS,FAT_PROD_DE_MADEIRA,FAT_PROD_MAD_DEFLAC,EMPREGO_MOVEIS,HRS_TRAB_PROD_MOVEIS,LIC_AUTO_NACIONAIS,LIC_COM_LEVES_NACIONAIS,ELETRODOMESTICOS,IND_EXP_NIVEL_DE_ATIV_CONSTRUCAO,TAXA_DESEMPREGO,IGPM,IPC_GERAL,ICEI_BRASIL_COND_ECONOMIA_BRASILEIRA,ICEI_MADEIRA,ICEI_BRASIL,CUB_MEDIO_M2_BRASIL_CONSTRUCAO))
# Transformando o resultado do forecast em um dataframe
fcast_mdp_ind_volume <- as.data.frame(fcast_mdp_ind_volume)

# MDP Indústria NO / NE - Volume #
fit_mdp_ind_volume_no_ne <- auto.arima(x = deseasonal_cnt_mdp_ind_volume_no_ne, seasonal=FALSE, xreg=cbind(PIB,SELIC,INCC,INPC,IPCA,SALARIO_MINIMO_REAL,IND_EXP_CONSUMIDOR,IND_CONF_CONSUMIDOR,IND_EXP_RENDA_MEDIA,DIAS_UTEIS,FAT_PROD_DE_MADEIRA,FAT_PROD_MAD_DEFLAC,EMPREGO_MOVEIS,HRS_TRAB_PROD_MOVEIS,LIC_AUTO_NACIONAIS,LIC_COM_LEVES_NACIONAIS,ELETRODOMESTICOS,IND_EXP_NIVEL_DE_ATIV_CONSTRUCAO,TAXA_DESEMPREGO,IGPM,IPC_GERAL,ICEI_BRASIL_COND_ECONOMIA_BRASILEIRA,ICEI_MADEIRA,ICEI_BRASIL,CUB_MEDIO_M2_BRASIL_CONSTRUCAO))
# Realizando o forecast e plotando o gráfico
fcast_mdp_ind_volume_no_ne <- forecast(fit_mdp_ind_volume_no_ne, h=12, xreg=cbind(PIB,SELIC,INCC,INPC,IPCA,SALARIO_MINIMO_REAL,IND_EXP_CONSUMIDOR,IND_CONF_CONSUMIDOR,IND_EXP_RENDA_MEDIA,DIAS_UTEIS,FAT_PROD_DE_MADEIRA,FAT_PROD_MAD_DEFLAC,EMPREGO_MOVEIS,HRS_TRAB_PROD_MOVEIS,LIC_AUTO_NACIONAIS,LIC_COM_LEVES_NACIONAIS,ELETRODOMESTICOS,IND_EXP_NIVEL_DE_ATIV_CONSTRUCAO,TAXA_DESEMPREGO,IGPM,IPC_GERAL,ICEI_BRASIL_COND_ECONOMIA_BRASILEIRA,ICEI_MADEIRA,ICEI_BRASIL,CUB_MEDIO_M2_BRASIL_CONSTRUCAO))
# Transformando o resultado do forecast em um dataframe
fcast_mdp_ind_volume_no_ne <- as.data.frame(fcast_mdp_ind_volume_no_ne)

# MDP Indústria PR / SC - Volume #
fit_mdp_ind_volume_pr_sc <- auto.arima(x = deseasonal_cnt_mdp_ind_volume_pr_sc, seasonal=FALSE, xreg=cbind(PIB,SELIC,INCC,INPC,IPCA,SALARIO_MINIMO_REAL,IND_EXP_CONSUMIDOR,IND_CONF_CONSUMIDOR,IND_EXP_RENDA_MEDIA,DIAS_UTEIS,FAT_PROD_DE_MADEIRA,FAT_PROD_MAD_DEFLAC,EMPREGO_MOVEIS,HRS_TRAB_PROD_MOVEIS,LIC_AUTO_NACIONAIS,LIC_COM_LEVES_NACIONAIS,ELETRODOMESTICOS,IND_EXP_NIVEL_DE_ATIV_CONSTRUCAO,TAXA_DESEMPREGO,IGPM,IPC_GERAL,ICEI_BRASIL_COND_ECONOMIA_BRASILEIRA,ICEI_MADEIRA,ICEI_BRASIL,CUB_MEDIO_M2_BRASIL_CONSTRUCAO))
# Realizando o forecast e plotando o gráfico
fcast_mdp_ind_volume_pr_sc <- forecast(fit_mdp_ind_volume_pr_sc, h=12, xreg=cbind(PIB,SELIC,INCC,INPC,IPCA,SALARIO_MINIMO_REAL,IND_EXP_CONSUMIDOR,IND_CONF_CONSUMIDOR,IND_EXP_RENDA_MEDIA,DIAS_UTEIS,FAT_PROD_DE_MADEIRA,FAT_PROD_MAD_DEFLAC,EMPREGO_MOVEIS,HRS_TRAB_PROD_MOVEIS,LIC_AUTO_NACIONAIS,LIC_COM_LEVES_NACIONAIS,ELETRODOMESTICOS,IND_EXP_NIVEL_DE_ATIV_CONSTRUCAO,TAXA_DESEMPREGO,IGPM,IPC_GERAL,ICEI_BRASIL_COND_ECONOMIA_BRASILEIRA,ICEI_MADEIRA,ICEI_BRASIL,CUB_MEDIO_M2_BRASIL_CONSTRUCAO))
# Transformando o resultado do forecast em um dataframe
fcast_mdp_ind_volume_pr_sc <- as.data.frame(fcast_mdp_ind_volume_pr_sc)

# MDP Indústria RS Rep - Volume #
fit_mdp_ind_volume_rs_rep <- auto.arima(x = deseasonal_cnt_mdp_ind_volume_rs_rep, seasonal=FALSE, xreg=cbind(PIB,SELIC,INCC,INPC,IPCA,SALARIO_MINIMO_REAL,IND_EXP_CONSUMIDOR,IND_CONF_CONSUMIDOR,IND_EXP_RENDA_MEDIA,DIAS_UTEIS,FAT_PROD_DE_MADEIRA,FAT_PROD_MAD_DEFLAC,EMPREGO_MOVEIS,HRS_TRAB_PROD_MOVEIS,LIC_AUTO_NACIONAIS,LIC_COM_LEVES_NACIONAIS,ELETRODOMESTICOS,IND_EXP_NIVEL_DE_ATIV_CONSTRUCAO,TAXA_DESEMPREGO,IGPM,IPC_GERAL,ICEI_BRASIL_COND_ECONOMIA_BRASILEIRA,ICEI_MADEIRA,ICEI_BRASIL,CUB_MEDIO_M2_BRASIL_CONSTRUCAO))
# Realizando o forecast e plotando o gráfico
fcast_mdp_ind_volume_rs_rep <- forecast(fit_mdp_ind_volume_rs_rep, h=12, xreg=cbind(PIB,SELIC,INCC,INPC,IPCA,SALARIO_MINIMO_REAL,IND_EXP_CONSUMIDOR,IND_CONF_CONSUMIDOR,IND_EXP_RENDA_MEDIA,DIAS_UTEIS,FAT_PROD_DE_MADEIRA,FAT_PROD_MAD_DEFLAC,EMPREGO_MOVEIS,HRS_TRAB_PROD_MOVEIS,LIC_AUTO_NACIONAIS,LIC_COM_LEVES_NACIONAIS,ELETRODOMESTICOS,IND_EXP_NIVEL_DE_ATIV_CONSTRUCAO,TAXA_DESEMPREGO,IGPM,IPC_GERAL,ICEI_BRASIL_COND_ECONOMIA_BRASILEIRA,ICEI_MADEIRA,ICEI_BRASIL,CUB_MEDIO_M2_BRASIL_CONSTRUCAO))
# Transformando o resultado do forecast em um dataframe
fcast_mdp_ind_volume_rs_rep <- as.data.frame(fcast_mdp_ind_volume_rs_rep)

# MDP Indústria SP Capital - Volume #
fit_mdp_ind_volume_sp_capital <- auto.arima(x = deseasonal_cnt_mdp_ind_volume_sp_capital, seasonal=FALSE, xreg=cbind(PIB,SELIC,INCC,INPC,IPCA,SALARIO_MINIMO_REAL,IND_EXP_CONSUMIDOR,IND_CONF_CONSUMIDOR,IND_EXP_RENDA_MEDIA,DIAS_UTEIS,FAT_PROD_DE_MADEIRA,FAT_PROD_MAD_DEFLAC,EMPREGO_MOVEIS,HRS_TRAB_PROD_MOVEIS,LIC_AUTO_NACIONAIS,LIC_COM_LEVES_NACIONAIS,ELETRODOMESTICOS,IND_EXP_NIVEL_DE_ATIV_CONSTRUCAO,TAXA_DESEMPREGO,IGPM,IPC_GERAL,ICEI_BRASIL_COND_ECONOMIA_BRASILEIRA,ICEI_MADEIRA,ICEI_BRASIL,CUB_MEDIO_M2_BRASIL_CONSTRUCAO))
# Realizando o forecast e plotando o gráfico
fcast_mdp_ind_volume_sp_capital <- forecast(fit_mdp_ind_volume_sp_capital, h=12, xreg=cbind(PIB,SELIC,INCC,INPC,IPCA,SALARIO_MINIMO_REAL,IND_EXP_CONSUMIDOR,IND_CONF_CONSUMIDOR,IND_EXP_RENDA_MEDIA,DIAS_UTEIS,FAT_PROD_DE_MADEIRA,FAT_PROD_MAD_DEFLAC,EMPREGO_MOVEIS,HRS_TRAB_PROD_MOVEIS,LIC_AUTO_NACIONAIS,LIC_COM_LEVES_NACIONAIS,ELETRODOMESTICOS,IND_EXP_NIVEL_DE_ATIV_CONSTRUCAO,TAXA_DESEMPREGO,IGPM,IPC_GERAL,ICEI_BRASIL_COND_ECONOMIA_BRASILEIRA,ICEI_MADEIRA,ICEI_BRASIL,CUB_MEDIO_M2_BRASIL_CONSTRUCAO))
# Transformando o resultado do forecast em um dataframe
fcast_mdp_ind_volume_sp_capital <- as.data.frame(fcast_mdp_ind_volume_sp_capital)

# MDF Indústria - Receita Líquida
fit_mdf_ind <- auto.arima(x = deseasonal_cnt_mdf_ind, seasonal=FALSE, xreg=cbind(PIB,SELIC,INCC,INPC,IPCA,SALARIO_MINIMO_REAL,IND_EXP_CONSUMIDOR,IND_CONF_CONSUMIDOR,IND_EXP_RENDA_MEDIA,DIAS_UTEIS,FAT_PROD_DE_MADEIRA,FAT_PROD_MAD_DEFLAC,EMPREGO_MOVEIS,HRS_TRAB_PROD_MOVEIS,LIC_AUTO_NACIONAIS,LIC_COM_LEVES_NACIONAIS,ELETRODOMESTICOS,IND_EXP_NIVEL_DE_ATIV_CONSTRUCAO,TAXA_DESEMPREGO,IGPM,IPC_GERAL,ICEI_BRASIL_COND_ECONOMIA_BRASILEIRA,ICEI_MADEIRA,ICEI_BRASIL,CUB_MEDIO_M2_BRASIL_CONSTRUCAO))
# Realizando o forecast e plotando o gráfico
fcast_mdf_ind <- forecast(fit_mdf_ind, h=12, xreg=cbind(PIB,SELIC,INCC,INPC,IPCA,SALARIO_MINIMO_REAL,IND_EXP_CONSUMIDOR,IND_CONF_CONSUMIDOR,IND_EXP_RENDA_MEDIA,DIAS_UTEIS,FAT_PROD_DE_MADEIRA,FAT_PROD_MAD_DEFLAC,EMPREGO_MOVEIS,HRS_TRAB_PROD_MOVEIS,LIC_AUTO_NACIONAIS,LIC_COM_LEVES_NACIONAIS,ELETRODOMESTICOS,IND_EXP_NIVEL_DE_ATIV_CONSTRUCAO,TAXA_DESEMPREGO,IGPM,IPC_GERAL,ICEI_BRASIL_COND_ECONOMIA_BRASILEIRA,ICEI_MADEIRA,ICEI_BRASIL,CUB_MEDIO_M2_BRASIL_CONSTRUCAO))
# Transformando o resultado do forecast em um dataframe
fcast_mdf_ind <- as.data.frame(fcast_mdf_ind)

# MDF Indústria - Volume #
fit_mdf_ind_volume <- auto.arima(x = deseasonal_cnt_mdf_ind_volume, seasonal=FALSE, xreg=cbind(PIB,SELIC,INCC,INPC,IPCA,SALARIO_MINIMO_REAL,IND_EXP_CONSUMIDOR,IND_CONF_CONSUMIDOR,IND_EXP_RENDA_MEDIA,DIAS_UTEIS,FAT_PROD_DE_MADEIRA,FAT_PROD_MAD_DEFLAC,EMPREGO_MOVEIS,HRS_TRAB_PROD_MOVEIS,LIC_AUTO_NACIONAIS,LIC_COM_LEVES_NACIONAIS,ELETRODOMESTICOS,IND_EXP_NIVEL_DE_ATIV_CONSTRUCAO,TAXA_DESEMPREGO,IGPM,IPC_GERAL,ICEI_BRASIL_COND_ECONOMIA_BRASILEIRA,ICEI_MADEIRA,ICEI_BRASIL,CUB_MEDIO_M2_BRASIL_CONSTRUCAO))
# Realizando o forecast e plotando o gráfico
fcast_mdf_ind_volume <- forecast(fit_mdf_ind_volume, h=12, xreg=cbind(PIB,SELIC,INCC,INPC,IPCA,SALARIO_MINIMO_REAL,IND_EXP_CONSUMIDOR,IND_CONF_CONSUMIDOR,IND_EXP_RENDA_MEDIA,DIAS_UTEIS,FAT_PROD_DE_MADEIRA,FAT_PROD_MAD_DEFLAC,EMPREGO_MOVEIS,HRS_TRAB_PROD_MOVEIS,LIC_AUTO_NACIONAIS,LIC_COM_LEVES_NACIONAIS,ELETRODOMESTICOS,IND_EXP_NIVEL_DE_ATIV_CONSTRUCAO,TAXA_DESEMPREGO,IGPM,IPC_GERAL,ICEI_BRASIL_COND_ECONOMIA_BRASILEIRA,ICEI_MADEIRA,ICEI_BRASIL,CUB_MEDIO_M2_BRASIL_CONSTRUCAO))
# Transformando o resultado do forecast em um dataframe
fcast_mdf_ind_volume <- as.data.frame(fcast_mdf_ind_volume)















# MDF Indústria SARIMA - Volume #
fit_mdf_ind_volume_sarima <- sarima.for(deseasonal_cnt_mdf_ind_volume, 14, 1,0,0,0,1,1,12,newxreg=cbind(PIB,SELIC,INCC,INPC,IPCA,SALARIO_MINIMO_REAL,IND_EXP_CONSUMIDOR,IND_CONF_CONSUMIDOR,IND_EXP_RENDA_MEDIA,DIAS_UTEIS,FAT_PROD_DE_MADEIRA,FAT_PROD_MAD_DEFLAC,EMPREGO_MOVEIS,HRS_TRAB_PROD_MOVEIS,LIC_AUTO_NACIONAIS,LIC_COM_LEVES_NACIONAIS,ELETRODOMESTICOS,IND_EXP_NIVEL_DE_ATIV_CONSTRUCAO,TAXA_DESEMPREGO,IGPM,IPC_GERAL,ICEI_BRASIL_COND_ECONOMIA_BRASILEIRA,ICEI_MADEIRA,ICEI_BRASIL,CUB_MEDIO_M2_BRASIL_CONSTRUCAO))
# Diagnóstico de Resíduos
#plot.ts(fit_mdf_ind_volume$residuals)
#Box.test(fit_mdf_ind_volume$residuals,lag=20,type="Ljung-Box")



  
  
  
# Criando a série temporal do MDF Indústria SARIMA
#ts_mdf_ind_sarima = ts(na.omit(fit_mdf_ind_volume_sarima$pred), frequency=12,start=c(2017,7),end=c(2018,7))
#fcast_mdf_ind_volume_sarima <- as.data.frame(ts_mdf_ind_sarima)

# MDF Varejo SARIMA - Volume #
fit_mdf_var_volume_sarima <- sarima.for(deseasonal_cnt_mdf_var_volume, 14, 1,0,0,0,1,1,15)

# MDF Indústria SARIMA - SP Capital - Volume #
fit_mdf_ind_volume_sp_capital <- sarima.for(deseasonal_cnt_mdf_ind_volume_sp_capital, 14, 1,0,0,0,1,1,15,newxreg=cbind(PIB,SELIC,INCC,INPC,IPCA,SALARIO_MINIMO_REAL,IND_EXP_CONSUMIDOR,IND_CONF_CONSUMIDOR,IND_EXP_RENDA_MEDIA,DIAS_UTEIS,FAT_PROD_DE_MADEIRA,FAT_PROD_MAD_DEFLAC,EMPREGO_MOVEIS,HRS_TRAB_PROD_MOVEIS,LIC_AUTO_NACIONAIS,LIC_COM_LEVES_NACIONAIS,ELETRODOMESTICOS,IND_EXP_NIVEL_DE_ATIV_CONSTRUCAO,TAXA_DESEMPREGO,IGPM,IPC_GERAL,ICEI_BRASIL_COND_ECONOMIA_BRASILEIRA,ICEI_MADEIRA,ICEI_BRASIL,CUB_MEDIO_M2_BRASIL_CONSTRUCAO))

# MDF Indústria SARIMA - PR SC - Volume #
fit_mdf_ind_volume_pr_sc_sarima <- sarima.for(deseasonal_cnt_mdf_ind_volume_pr_sc, 14, 1,0,0,0,1,1,15,newxreg=cbind(PIB,SELIC,INCC,INPC,IPCA,SALARIO_MINIMO_REAL,IND_EXP_CONSUMIDOR,IND_CONF_CONSUMIDOR,IND_EXP_RENDA_MEDIA,DIAS_UTEIS,FAT_PROD_DE_MADEIRA,FAT_PROD_MAD_DEFLAC,EMPREGO_MOVEIS,HRS_TRAB_PROD_MOVEIS,LIC_AUTO_NACIONAIS,LIC_COM_LEVES_NACIONAIS,ELETRODOMESTICOS,IND_EXP_NIVEL_DE_ATIV_CONSTRUCAO,TAXA_DESEMPREGO,IGPM,IPC_GERAL,ICEI_BRASIL_COND_ECONOMIA_BRASILEIRA,ICEI_MADEIRA,ICEI_BRASIL,CUB_MEDIO_M2_BRASIL_CONSTRUCAO))

# MDF Indústria SARIMA - RS Rep - Volume #
fit_mdf_ind_volume_rs_rep_sarima <- sarima.for(deseasonal_cnt_mdf_ind_volume_rs_rep, 14, 0,0,0,0,1,1,5,newxreg=cbind(PIB,SELIC,INCC,INPC,IPCA,SALARIO_MINIMO_REAL,IND_EXP_CONSUMIDOR,IND_CONF_CONSUMIDOR,IND_EXP_RENDA_MEDIA,DIAS_UTEIS,FAT_PROD_DE_MADEIRA,FAT_PROD_MAD_DEFLAC,EMPREGO_MOVEIS,HRS_TRAB_PROD_MOVEIS,LIC_AUTO_NACIONAIS,LIC_COM_LEVES_NACIONAIS,ELETRODOMESTICOS,IND_EXP_NIVEL_DE_ATIV_CONSTRUCAO,TAXA_DESEMPREGO,IGPM,IPC_GERAL,ICEI_BRASIL_COND_ECONOMIA_BRASILEIRA,ICEI_MADEIRA,ICEI_BRASIL,CUB_MEDIO_M2_BRASIL_CONSTRUCAO))
fit_mdf_ind_volume_rs_rep_sarima["pred"]

# MDP Indústria SARIMA - Volume #
fit_mdp_var_volume_sarima <- sarima.for(deseasonal_cnt_mdp_var_volume, 14, 0,0,0,0,1,1,5,newxreg=cbind(PIB,SELIC,INCC,INPC,IPCA,SALARIO_MINIMO_REAL,IND_EXP_CONSUMIDOR,IND_CONF_CONSUMIDOR,IND_EXP_RENDA_MEDIA,DIAS_UTEIS,FAT_PROD_DE_MADEIRA,FAT_PROD_MAD_DEFLAC,EMPREGO_MOVEIS,HRS_TRAB_PROD_MOVEIS,LIC_AUTO_NACIONAIS,LIC_COM_LEVES_NACIONAIS,ELETRODOMESTICOS,IND_EXP_NIVEL_DE_ATIV_CONSTRUCAO,TAXA_DESEMPREGO,IGPM,IPC_GERAL,ICEI_BRASIL_COND_ECONOMIA_BRASILEIRA,ICEI_MADEIRA,ICEI_BRASIL,CUB_MEDIO_M2_BRASIL_CONSTRUCAO))
fit_mdp_var_volume_sarima["pred"]


# MDF Indústria NO/NE - Volume #
fit_mdf_ind_volume_no_ne <- auto.arima(x = deseasonal_cnt_mdf_ind_volume_no_ne, seasonal=FALSE, xreg=cbind(PIB,SELIC,INCC,INPC,IPCA,SALARIO_MINIMO_REAL,IND_EXP_CONSUMIDOR,IND_CONF_CONSUMIDOR,IND_EXP_RENDA_MEDIA,DIAS_UTEIS,FAT_PROD_DE_MADEIRA,FAT_PROD_MAD_DEFLAC,EMPREGO_MOVEIS,HRS_TRAB_PROD_MOVEIS,LIC_AUTO_NACIONAIS,LIC_COM_LEVES_NACIONAIS,ELETRODOMESTICOS,IND_EXP_NIVEL_DE_ATIV_CONSTRUCAO,TAXA_DESEMPREGO,IGPM,IPC_GERAL,ICEI_BRASIL_COND_ECONOMIA_BRASILEIRA,ICEI_MADEIRA,ICEI_BRASIL,CUB_MEDIO_M2_BRASIL_CONSTRUCAO))
# Realizando o forecast e plotando o gráfico
fcast_mdf_ind_volume_no_ne <- forecast(fit_mdf_ind_volume_no_ne, h=12, xreg=cbind(PIB,SELIC,INCC,INPC,IPCA,SALARIO_MINIMO_REAL,IND_EXP_CONSUMIDOR,IND_CONF_CONSUMIDOR,IND_EXP_RENDA_MEDIA,DIAS_UTEIS,FAT_PROD_DE_MADEIRA,FAT_PROD_MAD_DEFLAC,EMPREGO_MOVEIS,HRS_TRAB_PROD_MOVEIS,LIC_AUTO_NACIONAIS,LIC_COM_LEVES_NACIONAIS,ELETRODOMESTICOS,IND_EXP_NIVEL_DE_ATIV_CONSTRUCAO,TAXA_DESEMPREGO,IGPM,IPC_GERAL,ICEI_BRASIL_COND_ECONOMIA_BRASILEIRA,ICEI_MADEIRA,ICEI_BRASIL,CUB_MEDIO_M2_BRASIL_CONSTRUCAO))
# Transformando o resultado do forecast em um dataframe
fcast_mdf_ind_volume_no_ne <- as.data.frame(fcast_mdf_ind_volume_no_ne)

# MDF Indústria PR/SC - Volume #
fit_mdf_ind_volume_pr_sc_arima <- auto.arima(x = deseasonal_cnt_mdf_ind_volume_pr_sc, seasonal=FALSE, xreg=cbind(PIB,SELIC,INCC,INPC,IPCA,SALARIO_MINIMO_REAL,IND_EXP_CONSUMIDOR,IND_CONF_CONSUMIDOR,IND_EXP_RENDA_MEDIA,DIAS_UTEIS,FAT_PROD_DE_MADEIRA,FAT_PROD_MAD_DEFLAC,EMPREGO_MOVEIS,HRS_TRAB_PROD_MOVEIS,LIC_AUTO_NACIONAIS,LIC_COM_LEVES_NACIONAIS,ELETRODOMESTICOS,IND_EXP_NIVEL_DE_ATIV_CONSTRUCAO,TAXA_DESEMPREGO,IGPM,IPC_GERAL,ICEI_BRASIL_COND_ECONOMIA_BRASILEIRA,ICEI_MADEIRA,ICEI_BRASIL,CUB_MEDIO_M2_BRASIL_CONSTRUCAO))
# Realizando o forecast e plotando o gráfico
fcast_mdf_ind_volume_pr_sc_arima <- forecast(fit_mdf_ind_volume_pr_sc_arima, h=12, xreg=cbind(PIB,SELIC,INCC,INPC,IPCA,SALARIO_MINIMO_REAL,IND_EXP_CONSUMIDOR,IND_CONF_CONSUMIDOR,IND_EXP_RENDA_MEDIA,DIAS_UTEIS,FAT_PROD_DE_MADEIRA,FAT_PROD_MAD_DEFLAC,EMPREGO_MOVEIS,HRS_TRAB_PROD_MOVEIS,LIC_AUTO_NACIONAIS,LIC_COM_LEVES_NACIONAIS,ELETRODOMESTICOS,IND_EXP_NIVEL_DE_ATIV_CONSTRUCAO,TAXA_DESEMPREGO,IGPM,IPC_GERAL,ICEI_BRASIL_COND_ECONOMIA_BRASILEIRA,ICEI_MADEIRA,ICEI_BRASIL,CUB_MEDIO_M2_BRASIL_CONSTRUCAO))
# Transformando o resultado do forecast em um dataframe
fcast_mdf_ind_volume_pr_sc_arima <- as.data.frame(fcast_mdf_ind_volume_pr_sc_arima)

# MDF Indústria RS Rep - Volume #
fit_mdf_ind_volume_rs_rep <- auto.arima(x = deseasonal_cnt_mdf_ind_volume_rs_rep, seasonal=FALSE, xreg=cbind(PIB,SELIC,INCC,INPC,IPCA,SALARIO_MINIMO_REAL,IND_EXP_CONSUMIDOR,IND_CONF_CONSUMIDOR,IND_EXP_RENDA_MEDIA,DIAS_UTEIS,FAT_PROD_DE_MADEIRA,FAT_PROD_MAD_DEFLAC,EMPREGO_MOVEIS,HRS_TRAB_PROD_MOVEIS,LIC_AUTO_NACIONAIS,LIC_COM_LEVES_NACIONAIS,ELETRODOMESTICOS,IND_EXP_NIVEL_DE_ATIV_CONSTRUCAO,TAXA_DESEMPREGO,IGPM,IPC_GERAL,ICEI_BRASIL_COND_ECONOMIA_BRASILEIRA,ICEI_MADEIRA,ICEI_BRASIL,CUB_MEDIO_M2_BRASIL_CONSTRUCAO))
# Realizando o forecast e plotando o gráfico
fcast_mdf_ind_volume_rs_rep <- forecast(fit_mdf_ind_volume_rs_rep, h=12, xreg=cbind(PIB,SELIC,INCC,INPC,IPCA,SALARIO_MINIMO_REAL,IND_EXP_CONSUMIDOR,IND_CONF_CONSUMIDOR,IND_EXP_RENDA_MEDIA,DIAS_UTEIS,FAT_PROD_DE_MADEIRA,FAT_PROD_MAD_DEFLAC,EMPREGO_MOVEIS,HRS_TRAB_PROD_MOVEIS,LIC_AUTO_NACIONAIS,LIC_COM_LEVES_NACIONAIS,ELETRODOMESTICOS,IND_EXP_NIVEL_DE_ATIV_CONSTRUCAO,TAXA_DESEMPREGO,IGPM,IPC_GERAL,ICEI_BRASIL_COND_ECONOMIA_BRASILEIRA,ICEI_MADEIRA,ICEI_BRASIL,CUB_MEDIO_M2_BRASIL_CONSTRUCAO))
# Transformando o resultado do forecast em um dataframe
fcast_mdf_ind_volume_rs_rep <- as.data.frame(fcast_mdf_ind_volume_rs_rep)


# MDF Indústria SP Capital - Volume - ARIMA #
fit_mdf_ind_volume_sp_capital_arima <- auto.arima(x = deseasonal_cnt_mdf_ind_volume_sp_capital, seasonal=FALSE, xreg=cbind(PIB,SELIC,INCC,INPC,IPCA,SALARIO_MINIMO_REAL,IND_EXP_CONSUMIDOR,IND_CONF_CONSUMIDOR,IND_EXP_RENDA_MEDIA,DIAS_UTEIS,FAT_PROD_DE_MADEIRA,FAT_PROD_MAD_DEFLAC,EMPREGO_MOVEIS,HRS_TRAB_PROD_MOVEIS,LIC_AUTO_NACIONAIS,LIC_COM_LEVES_NACIONAIS,ELETRODOMESTICOS,IND_EXP_NIVEL_DE_ATIV_CONSTRUCAO,TAXA_DESEMPREGO,IGPM,IPC_GERAL,ICEI_BRASIL_COND_ECONOMIA_BRASILEIRA,ICEI_MADEIRA,ICEI_BRASIL,CUB_MEDIO_M2_BRASIL_CONSTRUCAO))
# Realizando o forecast e plotando o gráfico
fcast_mdf_ind_volume_sp_capital_arima <- forecast(fit_mdf_ind_volume_sp_capital_arima, h=12, xreg=cbind(PIB,SELIC,INCC,INPC,IPCA,SALARIO_MINIMO_REAL,IND_EXP_CONSUMIDOR,IND_CONF_CONSUMIDOR,IND_EXP_RENDA_MEDIA,DIAS_UTEIS,FAT_PROD_DE_MADEIRA,FAT_PROD_MAD_DEFLAC,EMPREGO_MOVEIS,HRS_TRAB_PROD_MOVEIS,LIC_AUTO_NACIONAIS,LIC_COM_LEVES_NACIONAIS,ELETRODOMESTICOS,IND_EXP_NIVEL_DE_ATIV_CONSTRUCAO,TAXA_DESEMPREGO,IGPM,IPC_GERAL,ICEI_BRASIL_COND_ECONOMIA_BRASILEIRA,ICEI_MADEIRA,ICEI_BRASIL,CUB_MEDIO_M2_BRASIL_CONSTRUCAO))
# Transformando o resultado do forecast em um dataframe
fcast_mdf_ind_volume_sp_capital_arima <- as.data.frame(fcast_mdf_ind_volume_sp_capital_arima)


# MDF Varejo - Receita Líquida #
fit_mdf_var <- auto.arima(x = deseasonal_cnt_mdf_var, seasonal=FALSE, xreg=cbind(PIB,SELIC,INCC,INPC,IPCA,SALARIO_MINIMO_REAL,IND_EXP_CONSUMIDOR,IND_CONF_CONSUMIDOR,IND_EXP_RENDA_MEDIA,DIAS_UTEIS,FAT_PROD_DE_MADEIRA,FAT_PROD_MAD_DEFLAC,EMPREGO_MOVEIS,HRS_TRAB_PROD_MOVEIS,LIC_AUTO_NACIONAIS,LIC_COM_LEVES_NACIONAIS,ELETRODOMESTICOS,IND_EXP_NIVEL_DE_ATIV_CONSTRUCAO,TAXA_DESEMPREGO,IGPM,IPC_GERAL,ICEI_BRASIL_COND_ECONOMIA_BRASILEIRA,ICEI_MADEIRA,ICEI_BRASIL,CUB_MEDIO_M2_BRASIL_CONSTRUCAO))
# Realizando o forecast e plotando o gráfico
fcast_mdf_var <- forecast(fit_mdf_var, h=12, xreg=cbind(PIB,SELIC,INCC,INPC,IPCA,SALARIO_MINIMO_REAL,IND_EXP_CONSUMIDOR,IND_CONF_CONSUMIDOR,IND_EXP_RENDA_MEDIA,DIAS_UTEIS,FAT_PROD_DE_MADEIRA,FAT_PROD_MAD_DEFLAC,EMPREGO_MOVEIS,HRS_TRAB_PROD_MOVEIS,LIC_AUTO_NACIONAIS,LIC_COM_LEVES_NACIONAIS,ELETRODOMESTICOS,IND_EXP_NIVEL_DE_ATIV_CONSTRUCAO,TAXA_DESEMPREGO,IGPM,IPC_GERAL,ICEI_BRASIL_COND_ECONOMIA_BRASILEIRA,ICEI_MADEIRA,ICEI_BRASIL,CUB_MEDIO_M2_BRASIL_CONSTRUCAO))
# Transformando o resultado do forecast em um dataframe
fcast_mdf_var <- as.data.frame(fcast_mdf_var)

# MDF Varejo - Volume #
fit_mdf_var_volume <- auto.arima(x = deseasonal_cnt_mdf_var_volume, seasonal=FALSE, xreg=cbind(PIB,SELIC,INCC,INPC,IPCA,SALARIO_MINIMO_REAL,IND_EXP_CONSUMIDOR,IND_CONF_CONSUMIDOR,IND_EXP_RENDA_MEDIA,DIAS_UTEIS,FAT_PROD_DE_MADEIRA,FAT_PROD_MAD_DEFLAC,EMPREGO_MOVEIS,HRS_TRAB_PROD_MOVEIS,LIC_AUTO_NACIONAIS,LIC_COM_LEVES_NACIONAIS,ELETRODOMESTICOS,IND_EXP_NIVEL_DE_ATIV_CONSTRUCAO,TAXA_DESEMPREGO,IGPM,IPC_GERAL,ICEI_BRASIL_COND_ECONOMIA_BRASILEIRA,ICEI_MADEIRA,ICEI_BRASIL,CUB_MEDIO_M2_BRASIL_CONSTRUCAO))
# Realizando o forecast e plotando o gráfico
fcast_mdf_var_volume <- forecast(fit_mdf_var_volume, h=12, xreg=cbind(PIB,SELIC,INCC,INPC,IPCA,SALARIO_MINIMO_REAL,IND_EXP_CONSUMIDOR,IND_CONF_CONSUMIDOR,IND_EXP_RENDA_MEDIA,DIAS_UTEIS,FAT_PROD_DE_MADEIRA,FAT_PROD_MAD_DEFLAC,EMPREGO_MOVEIS,HRS_TRAB_PROD_MOVEIS,LIC_AUTO_NACIONAIS,LIC_COM_LEVES_NACIONAIS,ELETRODOMESTICOS,IND_EXP_NIVEL_DE_ATIV_CONSTRUCAO,TAXA_DESEMPREGO,IGPM,IPC_GERAL,ICEI_BRASIL_COND_ECONOMIA_BRASILEIRA,ICEI_MADEIRA,ICEI_BRASIL,CUB_MEDIO_M2_BRASIL_CONSTRUCAO))
# Transformando o resultado do forecast em um dataframe
fcast_mdf_var_volume <- as.data.frame(fcast_mdf_var_volume)

# MDP Varejo - Volume #
fit_mdp_var_volume <- auto.arima(x = deseasonal_cnt_mdp_var_volume, seasonal=FALSE, xreg=cbind(PIB,SELIC,INCC,INPC,IPCA,SALARIO_MINIMO_REAL,IND_EXP_CONSUMIDOR,IND_CONF_CONSUMIDOR,IND_EXP_RENDA_MEDIA,DIAS_UTEIS,FAT_PROD_DE_MADEIRA,FAT_PROD_MAD_DEFLAC,EMPREGO_MOVEIS,HRS_TRAB_PROD_MOVEIS,LIC_AUTO_NACIONAIS,LIC_COM_LEVES_NACIONAIS,ELETRODOMESTICOS,IND_EXP_NIVEL_DE_ATIV_CONSTRUCAO,TAXA_DESEMPREGO,IGPM,IPC_GERAL,ICEI_BRASIL_COND_ECONOMIA_BRASILEIRA,ICEI_MADEIRA,ICEI_BRASIL,CUB_MEDIO_M2_BRASIL_CONSTRUCAO))
# Realizando o forecast e plotando o gráfico
fcast_mdp_var_volume <- forecast(fit_mdp_var_volume, h=12, xreg=cbind(PIB,SELIC,INCC,INPC,IPCA,SALARIO_MINIMO_REAL,IND_EXP_CONSUMIDOR,IND_CONF_CONSUMIDOR,IND_EXP_RENDA_MEDIA,DIAS_UTEIS,FAT_PROD_DE_MADEIRA,FAT_PROD_MAD_DEFLAC,EMPREGO_MOVEIS,HRS_TRAB_PROD_MOVEIS,LIC_AUTO_NACIONAIS,LIC_COM_LEVES_NACIONAIS,ELETRODOMESTICOS,IND_EXP_NIVEL_DE_ATIV_CONSTRUCAO,TAXA_DESEMPREGO,IGPM,IPC_GERAL,ICEI_BRASIL_COND_ECONOMIA_BRASILEIRA,ICEI_MADEIRA,ICEI_BRASIL,CUB_MEDIO_M2_BRASIL_CONSTRUCAO))
# Transformando o resultado do forecast em um dataframe
fcast_mdp_var_volume <- as.data.frame(fcast_mdp_var_volume)

# MDP Varejo SP - Volume #
fit_mdp_var_volume_SP <- auto.arima(x = deseasonal_cnt_mdp_var_volume_SP, seasonal=FALSE, xreg=cbind(PIB,SELIC,INCC,INPC,IPCA,SALARIO_MINIMO_REAL,IND_EXP_CONSUMIDOR,IND_CONF_CONSUMIDOR,IND_EXP_RENDA_MEDIA,DIAS_UTEIS,FAT_PROD_DE_MADEIRA,FAT_PROD_MAD_DEFLAC,EMPREGO_MOVEIS,HRS_TRAB_PROD_MOVEIS,LIC_AUTO_NACIONAIS,LIC_COM_LEVES_NACIONAIS,ELETRODOMESTICOS,IND_EXP_NIVEL_DE_ATIV_CONSTRUCAO,TAXA_DESEMPREGO,IGPM,IPC_GERAL,ICEI_BRASIL_COND_ECONOMIA_BRASILEIRA,ICEI_MADEIRA,ICEI_BRASIL,CUB_MEDIO_M2_BRASIL_CONSTRUCAO))
# Realizando o forecast e plotando o gráfico
fcast_mdp_var_volume_SP <- forecast(fit_mdp_var_volume_SP, h=12, xreg=cbind(PIB,SELIC,INCC,INPC,IPCA,SALARIO_MINIMO_REAL,IND_EXP_CONSUMIDOR,IND_CONF_CONSUMIDOR,IND_EXP_RENDA_MEDIA,DIAS_UTEIS,FAT_PROD_DE_MADEIRA,FAT_PROD_MAD_DEFLAC,EMPREGO_MOVEIS,HRS_TRAB_PROD_MOVEIS,LIC_AUTO_NACIONAIS,LIC_COM_LEVES_NACIONAIS,ELETRODOMESTICOS,IND_EXP_NIVEL_DE_ATIV_CONSTRUCAO,TAXA_DESEMPREGO,IGPM,IPC_GERAL,ICEI_BRASIL_COND_ECONOMIA_BRASILEIRA,ICEI_MADEIRA,ICEI_BRASIL,CUB_MEDIO_M2_BRASIL_CONSTRUCAO))
# Transformando o resultado do forecast em um dataframe
fcast_mdp_var_volume_SP <- as.data.frame(fcast_mdp_var_volume_SP)

# MDF Varejo SP - Volume #
#fit_mdf_var_volume_SP <- auto.arima(x = deseasonal_cnt_mdf_var_volume_SP, seasonal=FALSE, xreg=cbind(PIB,SELIC,INCC,INPC,IPCA,SALARIO_MINIMO_REAL,IND_EXP_CONSUMIDOR,IND_CONF_CONSUMIDOR,IND_EXP_RENDA_MEDIA,DIAS_UTEIS,FAT_PROD_DE_MADEIRA,FAT_PROD_MAD_DEFLAC,EMPREGO_MOVEIS,HRS_TRAB_PROD_MOVEIS,LIC_AUTO_NACIONAIS,LIC_COM_LEVES_NACIONAIS,ELETRODOMESTICOS,IND_EXP_NIVEL_DE_ATIV_CONSTRUCAO,TAXA_DESEMPREGO,IGPM,IPC_GERAL,ICEI_BRASIL_COND_ECONOMIA_BRASILEIRA,ICEI_MADEIRA,ICEI_BRASIL,CUB_MEDIO_M2_BRASIL_CONSTRUCAO))
# Realizando o forecast e plotando o gráfico
#fcast_mdf_var_volume_SP <- forecast(fit_mdf_var_volume_SP, h=12, xreg=cbind(PIB,SELIC,INCC,INPC,IPCA,SALARIO_MINIMO_REAL,IND_EXP_CONSUMIDOR,IND_CONF_CONSUMIDOR,IND_EXP_RENDA_MEDIA,DIAS_UTEIS,FAT_PROD_DE_MADEIRA,FAT_PROD_MAD_DEFLAC,EMPREGO_MOVEIS,HRS_TRAB_PROD_MOVEIS,LIC_AUTO_NACIONAIS,LIC_COM_LEVES_NACIONAIS,ELETRODOMESTICOS,IND_EXP_NIVEL_DE_ATIV_CONSTRUCAO,TAXA_DESEMPREGO,IGPM,IPC_GERAL,ICEI_BRASIL_COND_ECONOMIA_BRASILEIRA,ICEI_MADEIRA,ICEI_BRASIL,CUB_MEDIO_M2_BRASIL_CONSTRUCAO))
# Transformando o resultado do forecast em um dataframe
#fcast_mdf_var_volume_SP <- as.data.frame(fcast_mdf_var_volume_SP)

# MDF Varejo SP - Volume SARIMA #
fit_mdf_var_volume_sp_sarima <- sarima.for(deseasonal_cnt_mdf_var_volume_SP, 14, 0,0,0,0,1,1,5,newxreg=cbind(PIB,SELIC,INCC,INPC,IPCA,SALARIO_MINIMO_REAL,IND_EXP_CONSUMIDOR,IND_CONF_CONSUMIDOR,IND_EXP_RENDA_MEDIA,DIAS_UTEIS,FAT_PROD_DE_MADEIRA,FAT_PROD_MAD_DEFLAC,EMPREGO_MOVEIS,HRS_TRAB_PROD_MOVEIS,LIC_AUTO_NACIONAIS,LIC_COM_LEVES_NACIONAIS,ELETRODOMESTICOS,IND_EXP_NIVEL_DE_ATIV_CONSTRUCAO,TAXA_DESEMPREGO,IGPM,IPC_GERAL,ICEI_BRASIL_COND_ECONOMIA_BRASILEIRA,ICEI_MADEIRA,ICEI_BRASIL,CUB_MEDIO_M2_BRASIL_CONSTRUCAO))

# MDF Varejo Sarima MG/DF/GO - Volume #
fit_mdf_var_volume_mg_df_go <- sarima.for(deseasonal_cnt_mdf_var_volume_mg_df_go, 14, 0,0,0,0,1,1,5,newxreg=cbind(PIB,SELIC,INCC,INPC,IPCA,SALARIO_MINIMO_REAL,IND_EXP_CONSUMIDOR,IND_CONF_CONSUMIDOR,IND_EXP_RENDA_MEDIA,DIAS_UTEIS,FAT_PROD_DE_MADEIRA,FAT_PROD_MAD_DEFLAC,EMPREGO_MOVEIS,HRS_TRAB_PROD_MOVEIS,LIC_AUTO_NACIONAIS,LIC_COM_LEVES_NACIONAIS,ELETRODOMESTICOS,IND_EXP_NIVEL_DE_ATIV_CONSTRUCAO,TAXA_DESEMPREGO,IGPM,IPC_GERAL,ICEI_BRASIL_COND_ECONOMIA_BRASILEIRA,ICEI_MADEIRA,ICEI_BRASIL,CUB_MEDIO_M2_BRASIL_CONSTRUCAO))
#fit_mdf_var_volume_mg_df_go["pred"]

# MDF Varejo Sarima NO/NE - Volume #
fit_mdf_var_volume_no_ne <- sarima.for(deseasonal_cnt_mdf_var_volume_no_ne, 14, 0,0,0,0,1,1,5,newxreg=cbind(PIB,SELIC,INCC,INPC,IPCA,SALARIO_MINIMO_REAL,IND_EXP_CONSUMIDOR,IND_CONF_CONSUMIDOR,IND_EXP_RENDA_MEDIA,DIAS_UTEIS,FAT_PROD_DE_MADEIRA,FAT_PROD_MAD_DEFLAC,EMPREGO_MOVEIS,HRS_TRAB_PROD_MOVEIS,LIC_AUTO_NACIONAIS,LIC_COM_LEVES_NACIONAIS,ELETRODOMESTICOS,IND_EXP_NIVEL_DE_ATIV_CONSTRUCAO,TAXA_DESEMPREGO,IGPM,IPC_GERAL,ICEI_BRASIL_COND_ECONOMIA_BRASILEIRA,ICEI_MADEIRA,ICEI_BRASIL,CUB_MEDIO_M2_BRASIL_CONSTRUCAO))
#fit_mdf_var_volume_no_ne["pred"]

# MDF Varejo Sarima PR/SC - Volume #
fit_mdf_var_volume_pr_sc <- sarima.for(deseasonal_cnt_mdf_var_volume_pr_sc, 14, 0,0,0,0,1,1,5,newxreg=cbind(PIB,SELIC,INCC,INPC,IPCA,SALARIO_MINIMO_REAL,IND_EXP_CONSUMIDOR,IND_CONF_CONSUMIDOR,IND_EXP_RENDA_MEDIA,DIAS_UTEIS,FAT_PROD_DE_MADEIRA,FAT_PROD_MAD_DEFLAC,EMPREGO_MOVEIS,HRS_TRAB_PROD_MOVEIS,LIC_AUTO_NACIONAIS,LIC_COM_LEVES_NACIONAIS,ELETRODOMESTICOS,IND_EXP_NIVEL_DE_ATIV_CONSTRUCAO,TAXA_DESEMPREGO,IGPM,IPC_GERAL,ICEI_BRASIL_COND_ECONOMIA_BRASILEIRA,ICEI_MADEIRA,ICEI_BRASIL,CUB_MEDIO_M2_BRASIL_CONSTRUCAO))
#fit_mdf_var_volume_pr_sc["pred"]

# MDF Varejo Sarima RJ/ES - Volume #
fit_mdf_var_volume_rj_es <- sarima.for(deseasonal_cnt_mdf_var_volume_rj_es, 14, 0,0,0,0,1,1,5,newxreg=cbind(PIB,SELIC,INCC,INPC,IPCA,SALARIO_MINIMO_REAL,IND_EXP_CONSUMIDOR,IND_CONF_CONSUMIDOR,IND_EXP_RENDA_MEDIA,DIAS_UTEIS,FAT_PROD_DE_MADEIRA,FAT_PROD_MAD_DEFLAC,EMPREGO_MOVEIS,HRS_TRAB_PROD_MOVEIS,LIC_AUTO_NACIONAIS,LIC_COM_LEVES_NACIONAIS,ELETRODOMESTICOS,IND_EXP_NIVEL_DE_ATIV_CONSTRUCAO,TAXA_DESEMPREGO,IGPM,IPC_GERAL,ICEI_BRASIL_COND_ECONOMIA_BRASILEIRA,ICEI_MADEIRA,ICEI_BRASIL,CUB_MEDIO_M2_BRASIL_CONSTRUCAO))
#fit_mdf_var_volume_rj_es["pred"]

########################################

########################################
######## FACEBOOK PROPHET - MDP VAREJO RECEITA LÍQUIDA ########
# Mudando o nome das colunas para ds e y, requisito obrigatório para o Prophet
colnames(data_mdp_var_sum)[1] <- "ds"
colnames(data_mdp_var_sum)[2] <- "y"

# Criando um dataframe apenas com as datas, com 365 dias
pfdat <- data.frame(ds=max(data_mdp_var_sum$ds) + 1:365)

# Criando uma coluna no dataframe pfdat com o formato de data yyyy-mm
pfdat$Month_Yr <- format(as.Date(pfdat$ds), "%Y%m")

# Criando um novo dataframe com as data únicas para o forecast dos regressores
df_dates <- data.frame(unique(
  as.Date(paste0(as.character(pfdat$Month_Yr), '01'), format='%Y%m%d')
))

# Renomeando a coluna do dataframe df_dates
colnames(df_dates) <- "ds"

## Realizando o fit e o forecast de cada regressor ##
# PIB
pPIB <-  data_mdp_var_sum %>% 
  select(ds,y=PIB) %>% 
  prophet() %>%
  predict(df_dates)

# SELIC
pSELIC <-  data_mdp_var_sum %>% 
  select(ds,y=SELIC) %>% 
  prophet() %>%
  predict(df_dates)

# INCC
pINCC <-  data_mdp_var_sum %>% 
  select(ds,y=INCC) %>% 
  prophet() %>%
  predict(df_dates)

# IPCA
pIPCA <-  data_mdp_var_sum %>% 
  select(ds,y=IPCA) %>% 
  prophet() %>%
  predict(df_dates)

# FAT_PROD_DE_MADEIRA
pFAT_PROD_DE_MADEIRA <-  data_mdp_var_sum %>% 
  select(ds,y=FAT_PROD_DE_MADEIRA) %>% 
  prophet() %>%
  predict(df_dates)

# INPC
pINPC <-  data_mdp_var_sum %>% 
  select(ds,y=INPC) %>% 
  prophet() %>%
  predict(df_dates)

# SALARIO_MINIMO_REAL
pSALARIO_MINIMO_REAL <-  data_mdp_var_sum %>% 
  select(ds,y=SALARIO_MINIMO_REAL) %>% 
  prophet() %>%
  predict(df_dates)

# FAT_PROD_MAD_DEFLAC
pFAT_PROD_MAD_DEFLAC <-  data_mdp_var_sum %>% 
  select(ds,y=FAT_PROD_MAD_DEFLAC) %>% 
  prophet() %>%
  predict(df_dates)

# LIC_AUTO_NACIONAIS
pLIC_AUTO_NACIONAIS <-  data_mdp_var_sum %>% 
  select(ds,y=LIC_AUTO_NACIONAIS) %>% 
  prophet() %>%
  predict(df_dates)

# LIC_COM_LEVES_NACIONAIS
pLIC_COM_LEVES_NACIONAIS <-  data_mdp_var_sum %>% 
  select(ds,y=LIC_COM_LEVES_NACIONAIS) %>% 
  prophet() %>%
  predict(df_dates)

# ELETRODOMESTICOS
pELETRODOMESTICOS <-  data_mdp_var_sum %>% 
  select(ds,y=ELETRODOMESTICOS) %>% 
  prophet() %>%
  predict(df_dates)

# IND_EXP_NIVEL_DE_ATIV_CONSTRUCAO
pIND_EXP_NIVEL_DE_ATIV_CONSTRUCAO <-  data_mdp_var_sum %>% 
  select(ds,y=IND_EXP_NIVEL_DE_ATIV_CONSTRUCAO) %>% 
  prophet() %>%
  predict(df_dates)

# IND_EXP_CONSUMIDOR
pIND_EXP_CONSUMIDOR <-  data_mdp_var_sum %>% 
  select(ds,y=IND_EXP_CONSUMIDOR) %>% 
  prophet() %>%
  predict(df_dates)

# IND_CONF_CONSUMIDOR
pIND_CONF_CONSUMIDOR <-  data_mdp_var_sum %>% 
  select(ds,y=IND_CONF_CONSUMIDOR) %>% 
  prophet() %>%
  predict(df_dates)

# IND_EXP_RENDA_MEDIA
pIND_EXP_RENDA_MEDIA <-  data_mdp_var_sum %>% 
  select(ds,y=IND_EXP_RENDA_MEDIA) %>% 
  prophet() %>%
  predict(df_dates)

# HRS_TRAB_PROD_MOVEIS
pHRS_TRAB_PROD_MOVEIS <-  data_mdp_var_sum %>% 
  select(ds,y=HRS_TRAB_PROD_MOVEIS) %>% 
  prophet() %>%
  predict(df_dates)

# EMPREGO_MOVEIS
pEMPREGO_MOVEIS <-  data_mdp_var_sum %>% 
  select(ds,y=EMPREGO_MOVEIS) %>% 
  prophet() %>%
  predict(df_dates)

# TAXA_DESEMPREGO
pTAXA_DESEMPREGO <-  data_mdp_var_sum %>% 
  select(ds,y=TAXA_DESEMPREGO) %>% 
  prophet() %>%
  predict(df_dates)

# DIAS_UTEIS
pDIAS_UTEIS <-  data_mdp_var_sum %>% 
  select(ds,y=DIAS_UTEIS) %>% 
  prophet() %>%
  predict(df_dates)

# IGPM.M
pIGPM <-  data_mdp_var_sum %>% 
  select(ds,y=IGP.M) %>% 
  prophet() %>%
  predict(df_dates)

# IPC_GERAL
pIPC_GERAL <-  data_mdp_var_sum %>% 
  select(ds,y=IPC_GERAL) %>% 
  prophet() %>%
  predict(df_dates)

# IPC_MADEIRA
pIPC_MADEIRA <-  data_mdp_var_sum %>% 
  select(ds,y=IPC_MADEIRA) %>% 
  prophet() %>%
  predict(df_dates)

# ICEI_BRASIL_COND_ECONOMIA_BRASILEIRA
pICEI_BRASIL_COND_ECONOMIA_BRASILEIRA <-  data_mdp_var_sum %>% 
  select(ds,y=ICEI_BRASIL_COND_ECONOMIA_BRASILEIRA) %>% 
  prophet() %>%
  predict(df_dates)

# ICEI_MADEIRA
pICEI_MADEIRA <-  data_mdp_var_sum %>% 
  select(ds,y=ICEI_MADEIRA) %>% 
  prophet() %>%
  predict(df_dates)

# ICEI_BRASIL
pICEI_BRASIL <-  data_mdp_var_sum %>% 
  select(ds,y=ICEI_BRASIL) %>% 
  prophet() %>%
  predict(df_dates)

# CUB_MEDIO_M2_BRASIL_CONSTRUCAO
pCUB_MEDIO_M2_BRASIL_CONSTRUCAO <-  data_mdp_var_sum %>% 
  select(ds,y=CUB_MEDIO_M2_BRASIL_CONSTRUCAO) %>% 
  prophet() %>%
  predict(df_dates)

# ACESSOS_PRE_PAGO
pACESSOS_PRE_PAGO <-  data_mdp_var_sum %>% 
  select(ds,y=ACESSOS_PRE_PAGO) %>% 
  prophet() %>%
  predict(df_dates)

# ACESSOS_POS_PAGO
pACESSOS_POS_PAGO <-data_mdp_var_sum %>% 
  select(ds,y=ACESSOS_POS_PAGO) %>% 
  prophet() %>%
  predict(df_dates)

fdat <-  data.frame(ds=df_dates$ds,
                    PIB=pPIB$yhat,
                    SELIC=pSELIC$yhat,
                    INCC=pINCC$yhat,
                    IPCA=pIPCA$yhat,
                    FAT_PROD_DE_MADEIRA=pFAT_PROD_DE_MADEIRA$yhat,
                    INPC=pINPC$yhat,
                    SALARIO_MINIMO_REAL=pSALARIO_MINIMO_REAL$yhat,
                    FAT_PROD_MAD_DEFLAC=pFAT_PROD_MAD_DEFLAC$yhat,
                    LIC_AUTO_NACIONAIS=pLIC_AUTO_NACIONAIS$yhat,
                    LIC_COM_LEVES_NACIONAIS=pLIC_COM_LEVES_NACIONAIS$yhat,
                    ELETRODOMESTICOS=pELETRODOMESTICOS$yhat,
                    IND_EXP_NIVEL_DE_ATIV_CONSTRUCAO=pIND_EXP_NIVEL_DE_ATIV_CONSTRUCAO$yhat,
                    IND_EXP_CONSUMIDOR=pIND_EXP_CONSUMIDOR$yhat,
                    IND_CONF_CONSUMIDOR=pIND_CONF_CONSUMIDOR$yhat,
                    IND_EXP_RENDA_MEDIA=pIND_EXP_RENDA_MEDIA$yhat,
                    HRS_TRAB_PROD_MOVEIS=pHRS_TRAB_PROD_MOVEIS$yhat,
                    EMPREGO_MOVEIS=pEMPREGO_MOVEIS$yhat,
                    TAXA_DESEMPREGO=pTAXA_DESEMPREGO$yhat,
                    DIAS_UTEIS=pDIAS_UTEIS$yhat,
                    IGP.M=pIGPM$yhat,
                    IPC_GERAL=pIPC_GERAL$yhat,
                    ICEI_BRASIL_COND_ECONOMIA_BRASILEIRA=pICEI_BRASIL_COND_ECONOMIA_BRASILEIRA$yhat,
                    ICEI_MADEIRA=pICEI_MADEIRA$yhat,
                    ICEI_BRASIL=pICEI_BRASIL$yhat,
                    CUB_MEDIO_M2_BRASIL_CONSTRUCAO=pCUB_MEDIO_M2_BRASIL_CONSTRUCAO$yhat,
                    ACESSOS_PRE_PAGO=pACESSOS_PRE_PAGO$yhat,
                    ACESSOS_POS_PAGO=pACESSOS_POS_PAGO$yhat
)

# Realizando o fit do prophet, com os regressores externos
fit_mdp_var <- prophet() %>% 
  add_regressor('PIB') %>% 
  add_regressor('SELIC') %>% 
  add_regressor('IPCA') %>% 
  add_regressor('INCC') %>% 
  add_regressor('FAT_PROD_DE_MADEIRA') %>% 
  add_regressor('INPC') %>%
  add_regressor('SALARIO_MINIMO_REAL') %>% 
  add_regressor('FAT_PROD_MAD_DEFLAC') %>% 
  add_regressor('LIC_AUTO_NACIONAIS') %>% 
  add_regressor('LIC_COM_LEVES_NACIONAIS') %>% 
  add_regressor('ELETRODOMESTICOS') %>%
  add_regressor('IND_EXP_NIVEL_DE_ATIV_CONSTRUCAO') %>%
  add_regressor('IND_EXP_CONSUMIDOR') %>%
  add_regressor('IND_CONF_CONSUMIDOR') %>% 
  add_regressor('IND_EXP_RENDA_MEDIA') %>% 
  add_regressor('HRS_TRAB_PROD_MOVEIS') %>% 
  add_regressor('EMPREGO_MOVEIS') %>% 
  add_regressor('TAXA_DESEMPREGO') %>% 
  add_regressor('DIAS_UTEIS') %>% 
  add_regressor('IGP.M') %>% 
  add_regressor('IPC_GERAL') %>% 
  add_regressor('ICEI_BRASIL_COND_ECONOMIA_BRASILEIRA') %>%
  add_regressor('ICEI_MADEIRA') %>% 
  add_regressor('ICEI_BRASIL') %>% 
  add_regressor('CUB_MEDIO_M2_BRASIL_CONSTRUCAO') %>% 
  add_regressor('ACESSOS_PRE_PAGO') %>% 
  add_regressor('ACESSOS_POS_PAGO') %>% 
  fit.prophet(data_mdp_var_sum)

# Forecast
fcast_mdp_var <- predict(fit_mdp_var, fdat)
fpred_mdp_var <- predict(fit_mdp_var)
fpred_mdp_var$ds <- as.Date(fpred_mdp_var$ds)

# Transformando o resultado do forecast em um dataframe
fcast_mdp_var <- as.data.frame(fcast_mdp_var)
########################################


#######################################
########################################
######## FACEBOOK PROPHET - MDP VAREJO Volume ########
# Mudando o nome das colunas para ds e y, requisito obrigatório para o Prophet
colnames(data_mdp_var_sum_volume)[1] <- "ds"
colnames(data_mdp_var_sum_volume)[2] <- "y"

# Criando um dataframe apenas com as datas, com 365 dias
pfdat <- data.frame(ds=max(data_mdp_var_sum_volume$ds) + 1:365)

# Criando uma coluna no dataframe pfdat com o formato de data yyyy-mm
pfdat$Month_Yr <- format(as.Date(pfdat$ds), "%Y%m")

# Criando um novo dataframe com as data únicas para o forecast dos regressores
df_dates <- data.frame(unique(
  as.Date(paste0(as.character(pfdat$Month_Yr), '01'), format='%Y%m%d')
))

# Renomeando a coluna do dataframe df_dates
colnames(df_dates) <- "ds"

## Realizando o fit e o forecast de cada regressor ##
# PIB
pPIB <-  data_mdp_var_sum_volume %>% 
  select(ds,y=PIB) %>% 
  prophet() %>%
  predict(df_dates)

# SELIC
pSELIC <-  data_mdp_var_sum_volume %>% 
  select(ds,y=SELIC) %>% 
  prophet() %>%
  predict(df_dates)

# INCC
pINCC <-  data_mdp_var_sum_volume %>% 
  select(ds,y=INCC) %>% 
  prophet() %>%
  predict(df_dates)

# IPCA
pIPCA <-  data_mdp_var_sum_volume %>% 
  select(ds,y=IPCA) %>% 
  prophet() %>%
  predict(df_dates)

# FAT_PROD_DE_MADEIRA
pFAT_PROD_DE_MADEIRA <-  data_mdp_var_sum_volume %>% 
  select(ds,y=FAT_PROD_DE_MADEIRA) %>% 
  prophet() %>%
  predict(df_dates)

# INPC
pINPC <-  data_mdp_var_sum_volume %>% 
  select(ds,y=INPC) %>% 
  prophet() %>%
  predict(df_dates)

# SALARIO_MINIMO_REAL
pSALARIO_MINIMO_REAL <-  data_mdp_var_sum_volume %>% 
  select(ds,y=SALARIO_MINIMO_REAL) %>% 
  prophet() %>%
  predict(df_dates)

# FAT_PROD_MAD_DEFLAC
pFAT_PROD_MAD_DEFLAC <-  data_mdp_var_sum_volume %>% 
  select(ds,y=FAT_PROD_MAD_DEFLAC) %>% 
  prophet() %>%
  predict(df_dates)

# LIC_AUTO_NACIONAIS
pLIC_AUTO_NACIONAIS <-  data_mdp_var_sum_volume %>% 
  select(ds,y=LIC_AUTO_NACIONAIS) %>% 
  prophet() %>%
  predict(df_dates)

# LIC_COM_LEVES_NACIONAIS
pLIC_COM_LEVES_NACIONAIS <-  data_mdp_var_sum_volume %>% 
  select(ds,y=LIC_COM_LEVES_NACIONAIS) %>% 
  prophet() %>%
  predict(df_dates)

# ELETRODOMESTICOS
pELETRODOMESTICOS <-  data_mdp_var_sum_volume %>% 
  select(ds,y=ELETRODOMESTICOS) %>% 
  prophet() %>%
  predict(df_dates)

# IND_EXP_NIVEL_DE_ATIV_CONSTRUCAO
pIND_EXP_NIVEL_DE_ATIV_CONSTRUCAO <-  data_mdp_var_sum_volume %>% 
  select(ds,y=IND_EXP_NIVEL_DE_ATIV_CONSTRUCAO) %>% 
  prophet() %>%
  predict(df_dates)

# IND_EXP_CONSUMIDOR
pIND_EXP_CONSUMIDOR <-  data_mdp_var_sum_volume %>% 
  select(ds,y=IND_EXP_CONSUMIDOR) %>% 
  prophet() %>%
  predict(df_dates)

# IND_CONF_CONSUMIDOR
pIND_CONF_CONSUMIDOR <-  data_mdp_var_sum_volume %>% 
  select(ds,y=IND_CONF_CONSUMIDOR) %>% 
  prophet() %>%
  predict(df_dates)

# IND_EXP_RENDA_MEDIA
pIND_EXP_RENDA_MEDIA <-  data_mdp_var_sum_volume %>% 
  select(ds,y=IND_EXP_RENDA_MEDIA) %>% 
  prophet() %>%
  predict(df_dates)

# HRS_TRAB_PROD_MOVEIS
pHRS_TRAB_PROD_MOVEIS <-  data_mdp_var_sum_volume %>% 
  select(ds,y=HRS_TRAB_PROD_MOVEIS) %>% 
  prophet() %>%
  predict(df_dates)

# EMPREGO_MOVEIS
pEMPREGO_MOVEIS <-  data_mdp_var_sum_volume %>% 
  select(ds,y=EMPREGO_MOVEIS) %>% 
  prophet() %>%
  predict(df_dates)

# TAXA_DESEMPREGO
pTAXA_DESEMPREGO <-  data_mdp_var_sum_volume %>% 
  select(ds,y=TAXA_DESEMPREGO) %>% 
  prophet() %>%
  predict(df_dates)

# DIAS_UTEIS
pDIAS_UTEIS <-  data_mdp_var_sum_volume %>% 
  select(ds,y=DIAS_UTEIS) %>% 
  prophet() %>%
  predict(df_dates)

# IGPM.M
pIGPM <-  data_mdp_var_sum_volume %>% 
  select(ds,y=IGP.M) %>% 
  prophet() %>%
  predict(df_dates)

# IPC_GERAL
pIPC_GERAL <-  data_mdp_var_sum_volume %>% 
  select(ds,y=IPC_GERAL) %>% 
  prophet() %>%
  predict(df_dates)

# IPC_MADEIRA
pIPC_MADEIRA <-  data_mdp_var_sum_volume %>% 
  select(ds,y=IPC_MADEIRA) %>% 
  prophet() %>%
  predict(df_dates)

# ICEI_BRASIL_COND_ECONOMIA_BRASILEIRA
pICEI_BRASIL_COND_ECONOMIA_BRASILEIRA <-  data_mdp_var_sum_volume %>% 
  select(ds,y=ICEI_BRASIL_COND_ECONOMIA_BRASILEIRA) %>% 
  prophet() %>%
  predict(df_dates)

# ICEI_MADEIRA
pICEI_MADEIRA <-  data_mdp_var_sum_volume %>% 
  select(ds,y=ICEI_MADEIRA) %>% 
  prophet() %>%
  predict(df_dates)

# ICEI_BRASIL
pICEI_BRASIL <-  data_mdp_var_sum_volume %>% 
  select(ds,y=ICEI_BRASIL) %>% 
  prophet() %>%
  predict(df_dates)

# CUB_MEDIO_M2_BRASIL_CONSTRUCAO
pCUB_MEDIO_M2_BRASIL_CONSTRUCAO <-  data_mdp_var_sum_volume %>% 
  select(ds,y=CUB_MEDIO_M2_BRASIL_CONSTRUCAO) %>% 
  prophet() %>%
  predict(df_dates)

# ACESSOS_PRE_PAGO
pACESSOS_PRE_PAGO <-  data_mdp_var_sum_volume %>% 
  select(ds,y=ACESSOS_PRE_PAGO) %>% 
  prophet() %>%
  predict(df_dates)

# ACESSOS_POS_PAGO
pACESSOS_POS_PAGO <-data_mdp_var_sum_volume %>% 
  select(ds,y=ACESSOS_POS_PAGO) %>% 
  prophet() %>%
  predict(df_dates)

fdat <-  data.frame(ds=df_dates$ds,
                    PIB=pPIB$yhat,
                    SELIC=pSELIC$yhat,
                    INCC=pINCC$yhat,
                    IPCA=pIPCA$yhat,
                    FAT_PROD_DE_MADEIRA=pFAT_PROD_DE_MADEIRA$yhat,
                    INPC=pINPC$yhat,
                    SALARIO_MINIMO_REAL=pSALARIO_MINIMO_REAL$yhat,
                    FAT_PROD_MAD_DEFLAC=pFAT_PROD_MAD_DEFLAC$yhat,
                    LIC_AUTO_NACIONAIS=pLIC_AUTO_NACIONAIS$yhat,
                    LIC_COM_LEVES_NACIONAIS=pLIC_COM_LEVES_NACIONAIS$yhat,
                    ELETRODOMESTICOS=pELETRODOMESTICOS$yhat,
                    IND_EXP_NIVEL_DE_ATIV_CONSTRUCAO=pIND_EXP_NIVEL_DE_ATIV_CONSTRUCAO$yhat,
                    IND_EXP_CONSUMIDOR=pIND_EXP_CONSUMIDOR$yhat,
                    IND_CONF_CONSUMIDOR=pIND_CONF_CONSUMIDOR$yhat,
                    IND_EXP_RENDA_MEDIA=pIND_EXP_RENDA_MEDIA$yhat,
                    HRS_TRAB_PROD_MOVEIS=pHRS_TRAB_PROD_MOVEIS$yhat,
                    EMPREGO_MOVEIS=pEMPREGO_MOVEIS$yhat,
                    TAXA_DESEMPREGO=pTAXA_DESEMPREGO$yhat,
                    DIAS_UTEIS=pDIAS_UTEIS$yhat,
                    IGP.M=pIGPM$yhat,
                    IPC_GERAL=pIPC_GERAL$yhat,
                    ICEI_BRASIL_COND_ECONOMIA_BRASILEIRA=pICEI_BRASIL_COND_ECONOMIA_BRASILEIRA$yhat,
                    ICEI_MADEIRA=pICEI_MADEIRA$yhat,
                    ICEI_BRASIL=pICEI_BRASIL$yhat,
                    CUB_MEDIO_M2_BRASIL_CONSTRUCAO=pCUB_MEDIO_M2_BRASIL_CONSTRUCAO$yhat,
                    ACESSOS_PRE_PAGO=pACESSOS_PRE_PAGO$yhat,
                    ACESSOS_POS_PAGO=pACESSOS_POS_PAGO$yhat
)

# Realizando o fit do prophet, com os regressores externos
fit_mdp_var_volume <- prophet() %>% 
  add_regressor('PIB') %>% 
  add_regressor('SELIC') %>% 
  add_regressor('IPCA') %>% 
  add_regressor('INCC') %>% 
  add_regressor('FAT_PROD_DE_MADEIRA') %>% 
  add_regressor('INPC') %>%
  add_regressor('SALARIO_MINIMO_REAL') %>% 
  add_regressor('FAT_PROD_MAD_DEFLAC') %>% 
  add_regressor('LIC_AUTO_NACIONAIS') %>% 
  add_regressor('LIC_COM_LEVES_NACIONAIS') %>% 
  add_regressor('ELETRODOMESTICOS') %>%
  add_regressor('IND_EXP_NIVEL_DE_ATIV_CONSTRUCAO') %>%
  add_regressor('IND_EXP_CONSUMIDOR') %>%
  add_regressor('IND_CONF_CONSUMIDOR') %>% 
  add_regressor('IND_EXP_RENDA_MEDIA') %>% 
  add_regressor('HRS_TRAB_PROD_MOVEIS') %>% 
  add_regressor('EMPREGO_MOVEIS') %>% 
  add_regressor('TAXA_DESEMPREGO') %>% 
  add_regressor('DIAS_UTEIS') %>% 
  add_regressor('IGP.M') %>% 
  add_regressor('IPC_GERAL') %>% 
  add_regressor('ICEI_BRASIL_COND_ECONOMIA_BRASILEIRA') %>%
  add_regressor('ICEI_MADEIRA') %>% 
  add_regressor('ICEI_BRASIL') %>% 
  add_regressor('CUB_MEDIO_M2_BRASIL_CONSTRUCAO') %>% 
  add_regressor('ACESSOS_PRE_PAGO') %>% 
  add_regressor('ACESSOS_POS_PAGO') %>% 
  fit.prophet(data_mdp_var_sum_volume)

# Forecast do Volume Duratex
fcast_mdp_var_volume <- predict(fit_mdp_var_volume, fdat)
fpred_mdp_var_volume <- predict(fit_mdp_var_volume)
fpred_mdp_var_volume$ds <- as.Date(fpred_mdp_var_volume$ds)

# Transformando o resultado do forecast em um dataframe
fcast_mdp_var_volume <- as.data.frame(fcast_mdp_var_volume)
########################################



#######################################
########################################
######## FACEBOOK PROPHET - MDP INDUSTRIA NO/NE Volume ########
# Mudando o nome das colunas para ds e y, requisito obrigatório para o Prophet
colnames(data_mdp_ind_no_ne_sum_volume)[1] <- "ds"
colnames(data_mdp_ind_no_ne_sum_volume)[2] <- "y"

# Criando um dataframe apenas com as datas, com 365 dias
pfdat <- data.frame(ds=max(data_mdp_ind_no_ne_sum_volume$ds) + 1:365)

# Criando uma coluna no dataframe pfdat com o formato de data yyyy-mm
pfdat$Month_Yr <- format(as.Date(pfdat$ds), "%Y%m")

# Criando um novo dataframe com as data únicas para o forecast dos regressores
df_dates <- data.frame(unique(
  as.Date(paste0(as.character(pfdat$Month_Yr), '01'), format='%Y%m%d')
))

# Renomeando a coluna do dataframe df_dates
colnames(df_dates) <- "ds"

## Realizando o fit e o forecast de cada regressor ##
# PIB
pPIB <-  data_mdp_ind_no_ne_sum_volume %>% 
  select(ds,y=PIB) %>% 
  prophet() %>%
  predict(df_dates)

# SELIC
pSELIC <-  data_mdp_ind_no_ne_sum_volume %>% 
  select(ds,y=SELIC) %>% 
  prophet() %>%
  predict(df_dates)

# INCC
pINCC <-  data_mdp_ind_no_ne_sum_volume %>% 
  select(ds,y=INCC) %>% 
  prophet() %>%
  predict(df_dates)

# IPCA
pIPCA <-  data_mdp_ind_no_ne_sum_volume %>% 
  select(ds,y=IPCA) %>% 
  prophet() %>%
  predict(df_dates)

# FAT_PROD_DE_MADEIRA
pFAT_PROD_DE_MADEIRA <-  data_mdp_ind_no_ne_sum_volume %>% 
  select(ds,y=FAT_PROD_DE_MADEIRA) %>% 
  prophet() %>%
  predict(df_dates)

# INPC
pINPC <-  data_mdp_ind_no_ne_sum_volume %>% 
  select(ds,y=INPC) %>% 
  prophet() %>%
  predict(df_dates)

# SALARIO_MINIMO_REAL
pSALARIO_MINIMO_REAL <-  data_mdp_ind_no_ne_sum_volume %>% 
  select(ds,y=SALARIO_MINIMO_REAL) %>% 
  prophet() %>%
  predict(df_dates)

# FAT_PROD_MAD_DEFLAC
pFAT_PROD_MAD_DEFLAC <-  data_mdp_ind_no_ne_sum_volume %>% 
  select(ds,y=FAT_PROD_MAD_DEFLAC) %>% 
  prophet() %>%
  predict(df_dates)

# LIC_AUTO_NACIONAIS
pLIC_AUTO_NACIONAIS <-  data_mdp_ind_no_ne_sum_volume %>% 
  select(ds,y=LIC_AUTO_NACIONAIS) %>% 
  prophet() %>%
  predict(df_dates)

# LIC_COM_LEVES_NACIONAIS
pLIC_COM_LEVES_NACIONAIS <-  data_mdp_ind_no_ne_sum_volume %>% 
  select(ds,y=LIC_COM_LEVES_NACIONAIS) %>% 
  prophet() %>%
  predict(df_dates)

# ELETRODOMESTICOS
pELETRODOMESTICOS <-  data_mdp_ind_no_ne_sum_volume %>% 
  select(ds,y=ELETRODOMESTICOS) %>% 
  prophet() %>%
  predict(df_dates)

# IND_EXP_NIVEL_DE_ATIV_CONSTRUCAO
pIND_EXP_NIVEL_DE_ATIV_CONSTRUCAO <-  data_mdp_ind_no_ne_sum_volume %>% 
  select(ds,y=IND_EXP_NIVEL_DE_ATIV_CONSTRUCAO) %>% 
  prophet() %>%
  predict(df_dates)

# IND_EXP_CONSUMIDOR
pIND_EXP_CONSUMIDOR <-  data_mdp_ind_no_ne_sum_volume %>% 
  select(ds,y=IND_EXP_CONSUMIDOR) %>% 
  prophet() %>%
  predict(df_dates)

# IND_CONF_CONSUMIDOR
pIND_CONF_CONSUMIDOR <-  data_mdp_ind_no_ne_sum_volume %>% 
  select(ds,y=IND_CONF_CONSUMIDOR) %>% 
  prophet() %>%
  predict(df_dates)

# IND_EXP_RENDA_MEDIA
pIND_EXP_RENDA_MEDIA <-  data_mdp_ind_no_ne_sum_volume %>% 
  select(ds,y=IND_EXP_RENDA_MEDIA) %>% 
  prophet() %>%
  predict(df_dates)

# HRS_TRAB_PROD_MOVEIS
pHRS_TRAB_PROD_MOVEIS <-  data_mdp_ind_no_ne_sum_volume %>% 
  select(ds,y=HRS_TRAB_PROD_MOVEIS) %>% 
  prophet() %>%
  predict(df_dates)

# EMPREGO_MOVEIS
pEMPREGO_MOVEIS <-  data_mdp_ind_no_ne_sum_volume %>% 
  select(ds,y=EMPREGO_MOVEIS) %>% 
  prophet() %>%
  predict(df_dates)

# TAXA_DESEMPREGO
pTAXA_DESEMPREGO <-  data_mdp_ind_no_ne_sum_volume %>% 
  select(ds,y=TAXA_DESEMPREGO) %>% 
  prophet() %>%
  predict(df_dates)

# DIAS_UTEIS
pDIAS_UTEIS <-  data_mdp_ind_no_ne_sum_volume %>% 
  select(ds,y=DIAS_UTEIS) %>% 
  prophet() %>%
  predict(df_dates)

# IGPM.M
pIGPM <-  data_mdp_ind_no_ne_sum_volume %>% 
  select(ds,y=IGP.M) %>% 
  prophet() %>%
  predict(df_dates)

# IPC_GERAL
pIPC_GERAL <-  data_mdp_ind_no_ne_sum_volume %>% 
  select(ds,y=IPC_GERAL) %>% 
  prophet() %>%
  predict(df_dates)

# IPC_MADEIRA
pIPC_MADEIRA <-  data_mdp_ind_no_ne_sum_volume %>% 
  select(ds,y=IPC_MADEIRA) %>% 
  prophet() %>%
  predict(df_dates)

# ICEI_BRASIL_COND_ECONOMIA_BRASILEIRA
pICEI_BRASIL_COND_ECONOMIA_BRASILEIRA <-  data_mdp_ind_no_ne_sum_volume %>% 
  select(ds,y=ICEI_BRASIL_COND_ECONOMIA_BRASILEIRA) %>% 
  prophet() %>%
  predict(df_dates)

# ICEI_MADEIRA
pICEI_MADEIRA <-  data_mdp_ind_no_ne_sum_volume %>% 
  select(ds,y=ICEI_MADEIRA) %>% 
  prophet() %>%
  predict(df_dates)

# ICEI_BRASIL
pICEI_BRASIL <-  data_mdp_ind_no_ne_sum_volume %>% 
  select(ds,y=ICEI_BRASIL) %>% 
  prophet() %>%
  predict(df_dates)

# CUB_MEDIO_M2_BRASIL_CONSTRUCAO
pCUB_MEDIO_M2_BRASIL_CONSTRUCAO <-  data_mdp_ind_no_ne_sum_volume %>% 
  select(ds,y=CUB_MEDIO_M2_BRASIL_CONSTRUCAO) %>% 
  prophet() %>%
  predict(df_dates)

# ACESSOS_PRE_PAGO
pACESSOS_PRE_PAGO <-  data_mdp_ind_no_ne_sum_volume %>% 
  select(ds,y=ACESSOS_PRE_PAGO) %>% 
  prophet() %>%
  predict(df_dates)

# ACESSOS_POS_PAGO
pACESSOS_POS_PAGO <-data_mdp_ind_no_ne_sum_volume %>% 
  select(ds,y=ACESSOS_POS_PAGO) %>% 
  prophet() %>%
  predict(df_dates)

fdat <-  data.frame(ds=df_dates$ds,
                    PIB=pPIB$yhat,
                    SELIC=pSELIC$yhat,
                    INCC=pINCC$yhat,
                    IPCA=pIPCA$yhat,
                    FAT_PROD_DE_MADEIRA=pFAT_PROD_DE_MADEIRA$yhat,
                    INPC=pINPC$yhat,
                    SALARIO_MINIMO_REAL=pSALARIO_MINIMO_REAL$yhat,
                    FAT_PROD_MAD_DEFLAC=pFAT_PROD_MAD_DEFLAC$yhat,
                    LIC_AUTO_NACIONAIS=pLIC_AUTO_NACIONAIS$yhat,
                    LIC_COM_LEVES_NACIONAIS=pLIC_COM_LEVES_NACIONAIS$yhat,
                    ELETRODOMESTICOS=pELETRODOMESTICOS$yhat,
                    IND_EXP_NIVEL_DE_ATIV_CONSTRUCAO=pIND_EXP_NIVEL_DE_ATIV_CONSTRUCAO$yhat,
                    IND_EXP_CONSUMIDOR=pIND_EXP_CONSUMIDOR$yhat,
                    IND_CONF_CONSUMIDOR=pIND_CONF_CONSUMIDOR$yhat,
                    IND_EXP_RENDA_MEDIA=pIND_EXP_RENDA_MEDIA$yhat,
                    HRS_TRAB_PROD_MOVEIS=pHRS_TRAB_PROD_MOVEIS$yhat,
                    EMPREGO_MOVEIS=pEMPREGO_MOVEIS$yhat,
                    TAXA_DESEMPREGO=pTAXA_DESEMPREGO$yhat,
                    DIAS_UTEIS=pDIAS_UTEIS$yhat,
                    IGP.M=pIGPM$yhat,
                    IPC_GERAL=pIPC_GERAL$yhat,
                    ICEI_BRASIL_COND_ECONOMIA_BRASILEIRA=pICEI_BRASIL_COND_ECONOMIA_BRASILEIRA$yhat,
                    ICEI_MADEIRA=pICEI_MADEIRA$yhat,
                    ICEI_BRASIL=pICEI_BRASIL$yhat,
                    CUB_MEDIO_M2_BRASIL_CONSTRUCAO=pCUB_MEDIO_M2_BRASIL_CONSTRUCAO$yhat,
                    ACESSOS_PRE_PAGO=pACESSOS_PRE_PAGO$yhat,
                    ACESSOS_POS_PAGO=pACESSOS_POS_PAGO$yhat
)

# Realizando o fit do prophet, com os regressores externos
fit_mdp_ind_volume_no_ne <- prophet() %>% 
  add_regressor('PIB') %>% 
  add_regressor('SELIC') %>% 
  add_regressor('IPCA') %>% 
  add_regressor('INCC') %>% 
  add_regressor('FAT_PROD_DE_MADEIRA') %>% 
  add_regressor('INPC') %>%
  add_regressor('SALARIO_MINIMO_REAL') %>% 
  add_regressor('FAT_PROD_MAD_DEFLAC') %>% 
  add_regressor('LIC_AUTO_NACIONAIS') %>% 
  add_regressor('LIC_COM_LEVES_NACIONAIS') %>% 
  add_regressor('ELETRODOMESTICOS') %>%
  add_regressor('IND_EXP_NIVEL_DE_ATIV_CONSTRUCAO') %>%
  add_regressor('IND_EXP_CONSUMIDOR') %>%
  add_regressor('IND_CONF_CONSUMIDOR') %>% 
  add_regressor('IND_EXP_RENDA_MEDIA') %>% 
  add_regressor('HRS_TRAB_PROD_MOVEIS') %>% 
  add_regressor('EMPREGO_MOVEIS') %>% 
  add_regressor('TAXA_DESEMPREGO') %>% 
  add_regressor('DIAS_UTEIS') %>% 
  add_regressor('IGP.M') %>% 
  add_regressor('IPC_GERAL') %>% 
  add_regressor('ICEI_BRASIL_COND_ECONOMIA_BRASILEIRA') %>%
  add_regressor('ICEI_MADEIRA') %>% 
  add_regressor('ICEI_BRASIL') %>% 
  add_regressor('CUB_MEDIO_M2_BRASIL_CONSTRUCAO') %>% 
  add_regressor('ACESSOS_PRE_PAGO') %>% 
  add_regressor('ACESSOS_POS_PAGO') %>% 
  fit.prophet(data_mdp_ind_no_ne_sum_volume)

# Forecast do Volume Duratex
fcast_mdp_ind_volume_no_ne <- predict(fit_mdp_ind_volume_no_ne, fdat)
fpred_mdp_ind_volume_no_ne <- predict(fit_mdp_ind_volume_no_ne)
fpred_mdp_ind_volume_no_ne$ds <- as.Date(fpred_mdp_ind_volume_no_ne$ds)

# Transformando o resultado do forecast em um dataframe
fcast_mdp_ind_volume_no_ne <- as.data.frame(fcast_mdp_ind_volume_no_ne)
########################################









##################################################################
######## VOLUME BRASIL ########
##### MDP #####
# Criando o dataframe para todo o MDP
iba_mdp <- subset(mercado_iba , Produto == "MDP")

# Somando o volume do MDP, por mês e ano
iba_mdp_sum_volume <- aggregate(x = iba_mdp["Volume.M3"],
                                FUN = sum,
                                by = list(dt_competencia = iba_mdp$Competência))

##### MDF #####
# Criando o dataframe para todo o MDF
iba_mdf <- subset(mercado_iba , Produto == "MDF")

# Substituindo NA por 0 no dataframe todo
iba_mdf[is.na(iba_mdf)] <- 0

# Somando o volume por mês e ano
iba_mdf_sum_volume <- aggregate(x = iba_mdf["Volume.M3"],
                                FUN = sum,
                                by = list(dt_competencia = iba_mdf$Competência))

##### REMOVENDO OUTLIERS DOS DADOS ####
# MDP - Volume
volume_ts = ts(iba_mdp_sum_volume[, c('Volume.M3')])
iba_mdp_sum_volume$volume_clean = tsclean(volume_ts)

# MDF - Volume
volume_ts = ts(iba_mdf_sum_volume[, c('Volume.M3')])
iba_mdf_sum_volume$volume_clean = tsclean(volume_ts)

#### CÁLCULO DA MÉDIA MÓVEL ####
## MDP Volume ##
# Média Móvel de 12 meses
iba_mdp_sum_volume$volume_ma12 = ma(iba_mdp_sum_volume$volume_clean, order=12)
# Média Móvel de 3 meses
iba_mdp_sum_volume$volume_ma3 = ma(iba_mdp_sum_volume$volume_clean, order=3)

## MDF Volume ##
# Média Móvel de 12 meses
iba_mdf_sum_volume$volume_ma12 = ma(iba_mdf_sum_volume$volume_clean, order=12)
# Média Móvel de 3 meses
iba_mdf_sum_volume$volume_ma3 = ma(iba_mdf_sum_volume$volume_clean, order=3)

# Substituindo NA por 0 no dataframe todo
iba_mdp_sum_volume[is.na(iba_mdp_sum_volume)] <- 0
iba_mdf_sum_volume[is.na(iba_mdf_sum_volume)] <- 0

#### CRIANDO SÉRIE TEMPORAL DA MÉDIA MÓVEL ####
# Criando a série temporal para a média móvel de 12 meses em todos os dataframes para receita líquida
# MDP - Volume
ts_moving_average_mdp_iba_volume = ts(na.omit(iba_mdp_sum_volume$volume_ma12), frequency=12,start=c(2014,1),end=c(2017,12))
decomp_mdp_iba_volume = stl(ts_moving_average_mdp_iba_volume, s.window="periodic")
deseasonal_cnt_mdp_iba_volume <- seasadj(decomp_mdp_iba_volume)

# MDF - Volume
ts_moving_average_mdf_iba_volume = ts(na.omit(iba_mdf_sum_volume$volume_ma12), frequency=12,start=c(2014,1),end=c(2017,12))
decomp_mdf_iba_volume = stl(ts_moving_average_mdf_iba_volume, s.window="periodic")
deseasonal_cnt_mdf_iba_volume <- seasadj(decomp_mdf_iba_volume)

#### CRIANDO DF NOVO PARA REMOVER 6 PRIMEIROS E ÚLTIMOS MESES ZERADOS ####
# Criando uma base nova, removendo os 6 primeiros e últimos meses zerados
# MDP - Volume
df_ts_moving_average_mdp_iba_volume <- as.data.frame(ts_moving_average_mdp_iba_volume)
df_ts_moving_average_mdp_iba_volume <- tail(df_ts_moving_average_mdp_iba_volume, -6)
df_ts_moving_average_mdp_iba_volume = head(df_ts_moving_average_mdp_iba_volume, -6)
ts_moving_average_new_mdp_iba_volume = ts(na.omit(df_ts_moving_average_mdp_iba_volume$x), frequency=12,start=c(2014,6),end=c(2017,6))
decomp_mdp_iba_volume = stl(ts_moving_average_new_mdp_iba_volume, s.window="periodic")
deseasonal_cnt_mdp_iba_volume <- seasadj(decomp_mdp_iba_volume)

# MDF - Volume
df_ts_moving_average_mdf_iba_volume <- as.data.frame(ts_moving_average_mdf_iba_volume)
df_ts_moving_average_mdf_iba_volume <- tail(df_ts_moving_average_mdf_iba_volume, -6)
df_ts_moving_average_mdf_iba_volume = head(df_ts_moving_average_mdf_iba_volume, -6)
ts_moving_average_new_mdf_iba_volume = ts(na.omit(df_ts_moving_average_mdf_iba_volume$x), frequency=12,start=c(2014,6),end=c(2017,6))
decomp_mdf_iba_volume = stl(ts_moving_average_new_mdf_iba_volume, s.window="periodic")
deseasonal_cnt_mdf_iba_volume <- seasadj(decomp_mdf_iba_volume)

#### AGRUPANDO DF DE FATURAMENTO E DADOS ECONÔMICOS PELA DATA ####
# Agrupando os dataframes e dados_economicos pela data
iba_mdp_sum_volume <- merge(iba_mdp_sum_volume,dados_economicos,by="dt_competencia")

########
#### ARIMA ####
# MDP - Volume #
fit_mdp_iba_volume <- auto.arima(x = deseasonal_cnt_mdp_iba_volume, seasonal=FALSE, xreg=cbind(PIB,SELIC,INCC,INPC,IPCA,SALARIO_MINIMO_REAL,IND_EXP_CONSUMIDOR,IND_CONF_CONSUMIDOR,IND_EXP_RENDA_MEDIA,DIAS_UTEIS,FAT_PROD_DE_MADEIRA,FAT_PROD_MAD_DEFLAC,EMPREGO_MOVEIS,HRS_TRAB_PROD_MOVEIS,LIC_AUTO_NACIONAIS,LIC_COM_LEVES_NACIONAIS,ELETRODOMESTICOS,IND_EXP_NIVEL_DE_ATIV_CONSTRUCAO,TAXA_DESEMPREGO,IGPM,IPC_GERAL,ICEI_BRASIL_COND_ECONOMIA_BRASILEIRA,ICEI_MADEIRA,ICEI_BRASIL,CUB_MEDIO_M2_BRASIL_CONSTRUCAO))
# Realizando o forecast e plotando o gráfico
fcast_mdp_iba_volume <- forecast(fit_mdp_iba_volume, h=12, xreg=cbind(PIB,SELIC,INCC,INPC,IPCA,SALARIO_MINIMO_REAL,IND_EXP_CONSUMIDOR,IND_CONF_CONSUMIDOR,IND_EXP_RENDA_MEDIA,DIAS_UTEIS,FAT_PROD_DE_MADEIRA,FAT_PROD_MAD_DEFLAC,EMPREGO_MOVEIS,HRS_TRAB_PROD_MOVEIS,LIC_AUTO_NACIONAIS,LIC_COM_LEVES_NACIONAIS,ELETRODOMESTICOS,IND_EXP_NIVEL_DE_ATIV_CONSTRUCAO,TAXA_DESEMPREGO,IGPM,IPC_GERAL,ICEI_BRASIL_COND_ECONOMIA_BRASILEIRA,ICEI_MADEIRA,ICEI_BRASIL,CUB_MEDIO_M2_BRASIL_CONSTRUCAO))
# Transformando o resultado do forecast em um dataframe
fcast_mdp_iba_volume <- as.data.frame(fcast_mdp_iba_volume)

# MDF - Volume #
fit_mdf_iba_volume <- auto.arima(x = deseasonal_cnt_mdf_iba_volume, seasonal=FALSE, xreg=cbind(PIB,SELIC,INCC,INPC,IPCA,SALARIO_MINIMO_REAL,IND_EXP_CONSUMIDOR,IND_CONF_CONSUMIDOR,IND_EXP_RENDA_MEDIA,DIAS_UTEIS,FAT_PROD_DE_MADEIRA,FAT_PROD_MAD_DEFLAC,EMPREGO_MOVEIS,HRS_TRAB_PROD_MOVEIS,LIC_AUTO_NACIONAIS,LIC_COM_LEVES_NACIONAIS,ELETRODOMESTICOS,IND_EXP_NIVEL_DE_ATIV_CONSTRUCAO,TAXA_DESEMPREGO,IGPM,IPC_GERAL,ICEI_BRASIL_COND_ECONOMIA_BRASILEIRA,ICEI_MADEIRA,ICEI_BRASIL,CUB_MEDIO_M2_BRASIL_CONSTRUCAO))
# Realizando o forecast e plotando o gráfico
fcast_mdf_iba_volume <- forecast(fit_mdf_iba_volume, h=12, xreg=cbind(PIB,SELIC,INCC,INPC,IPCA,SALARIO_MINIMO_REAL,IND_EXP_CONSUMIDOR,IND_CONF_CONSUMIDOR,IND_EXP_RENDA_MEDIA,DIAS_UTEIS,FAT_PROD_DE_MADEIRA,FAT_PROD_MAD_DEFLAC,EMPREGO_MOVEIS,HRS_TRAB_PROD_MOVEIS,LIC_AUTO_NACIONAIS,LIC_COM_LEVES_NACIONAIS,ELETRODOMESTICOS,IND_EXP_NIVEL_DE_ATIV_CONSTRUCAO,TAXA_DESEMPREGO,IGPM,IPC_GERAL,ICEI_BRASIL_COND_ECONOMIA_BRASILEIRA,ICEI_MADEIRA,ICEI_BRASIL,CUB_MEDIO_M2_BRASIL_CONSTRUCAO))
# Transformando o resultado do forecast em um dataframe
fcast_mdf_iba_volume <- as.data.frame(fcast_mdf_iba_volume)
##################################################################


########################################
######## SALVANDO ARQUIVO XLSX ########

# Mês/Ano | Produto | Mercado | Receita Líquida
# Mês/Ano = formato da data: YYYY-MM
# Produto = MDF Branco Competitivo; MDP Cru
# Mercado = IND; VAREJO
# Receita Líquida = Faturamento em R$

### MDF INDÚSTRIA ###
# Transformando a predição de MDF Indústria do ARIMA para uma lista com meses e anos
fcast_mdf_ind_volume_sarima <- tapply(fit_mdf_ind_volume_sarima$pred, 
                                      list(year = floor(time(fit_mdf_ind_volume_sarima$pred)), 
                                           month = month.abb[cycle(fit_mdf_ind_volume_sarima$pred)]),
                                      c)

# Transformando a variável fcast_mdf_ind_volume_sarima para o formato dataframe
fcast_mdf_ind_volume_sarima <- as.data.frame(fcast_mdf_ind_volume_sarima)

# Renomeando as colunas do dataframe fcast_mdf_ind_volume_sarima com os meses - MDF INDÚSTRIA
names(fcast_mdf_ind_volume_sarima)[names(fcast_mdf_ind_volume_sarima) == 'Jan'] <- '01'
names(fcast_mdf_ind_volume_sarima)[names(fcast_mdf_ind_volume_sarima) == 'Feb'] <- '02'
names(fcast_mdf_ind_volume_sarima)[names(fcast_mdf_ind_volume_sarima) == 'Mar'] <- '03'
names(fcast_mdf_ind_volume_sarima)[names(fcast_mdf_ind_volume_sarima) == 'Apr'] <- '04'
names(fcast_mdf_ind_volume_sarima)[names(fcast_mdf_ind_volume_sarima) == 'May'] <- '05'
names(fcast_mdf_ind_volume_sarima)[names(fcast_mdf_ind_volume_sarima) == 'Jun'] <- '06'
names(fcast_mdf_ind_volume_sarima)[names(fcast_mdf_ind_volume_sarima) == 'Jul'] <- '07'
names(fcast_mdf_ind_volume_sarima)[names(fcast_mdf_ind_volume_sarima) == 'Aug'] <- '08'
names(fcast_mdf_ind_volume_sarima)[names(fcast_mdf_ind_volume_sarima) == 'Sep'] <- '09'
names(fcast_mdf_ind_volume_sarima)[names(fcast_mdf_ind_volume_sarima) == 'Oct'] <- '10'
names(fcast_mdf_ind_volume_sarima)[names(fcast_mdf_ind_volume_sarima) == 'Nov'] <- '11'
names(fcast_mdf_ind_volume_sarima)[names(fcast_mdf_ind_volume_sarima) == 'Dec'] <- '12'

# Ordenando as colunas por ordem crescente dos meses - MDF INDÚSTRIA
fcast_mdf_ind_volume_sarima <- fcast_mdf_ind_volume_sarima[ , order(names(fcast_mdf_ind_volume_sarima))]

# Transpondo a linha dos anos para a coluna - MDF INDÚSTRIA
fcast_mdf_ind_volume_sarima <- as.data.frame(t(fcast_mdf_ind_volume_sarima))

# Transformando o index de meses em uma coluna
fcast_mdf_ind_volume_sarima <- cbind(meses = rownames(fcast_mdf_ind_volume_sarima), fcast_mdf_ind_volume_sarima)
rownames(fcast_mdf_ind_volume_sarima) <- 1:nrow(fcast_mdf_ind_volume_sarima)

# Criando dataframe 2017 e 2018
df_new_2017 <- as.data.frame(fcast_mdf_ind_volume_sarima[,1:2])
df_new_2018 <- as.data.frame(fcast_mdf_ind_volume_sarima[,c(1,3)])

# Juntando mês com ano - 2017
df_new_2017 <- as.data.frame(paste("2017", df_new_2017$meses, sep = "-"))
df_new_2017 <- cbind(df_new_2017, fcast_mdf_ind_volume_sarima[,2])

# Juntando mês com ano - 2018
df_new_2018 <- as.data.frame(paste("2018", df_new_2018$meses, sep = "-"))
df_new_2018 <- cbind(df_new_2018, fcast_mdf_ind_volume_sarima[,3])

# Renomeando as colunas
names(df_new_2017) <- c("dt_competencia","forecast")
names(df_new_2018) <- c("dt_competencia","forecast")

# Concatenando os dois dataframes do MDF INDÚSTRIA
fcast_mdf_ind_volume_sarima <- rbind(df_new_2017, df_new_2018)

# Removendo linhas onde o volume é NA, ou seja, não existe volume
fcast_mdf_ind_volume_sarima <- fcast_mdf_ind_volume_sarima[complete.cases(fcast_mdf_ind_volume_sarima[ , 2]),]
###

### MDP VAREJO ###
# Transformando a predição de MDF Varejo do ARIMA para uma lista com meses e anos
fcast_mdp_var_volume_sarima <- tapply(fit_mdp_var_volume_sarima$pred, 
                                      list(year = floor(time(fit_mdp_var_volume_sarima$pred)), 
                                           month = month.abb[cycle(fit_mdp_var_volume_sarima$pred)]),
                                      c)

# Transformando a variável fit_mdp_var_volume_sarima para o formato dataframe
fcast_mdp_var_volume_sarima <- as.data.frame(fcast_mdp_var_volume_sarima)

# Renomeando as colunas do dataframe fcast_mdp_var_volume_sarima com os meses - MDP VAREJO
names(fcast_mdp_var_volume_sarima)[names(fcast_mdp_var_volume_sarima) == 'Jan'] <- '01'
names(fcast_mdp_var_volume_sarima)[names(fcast_mdp_var_volume_sarima) == 'Feb'] <- '02'
names(fcast_mdp_var_volume_sarima)[names(fcast_mdp_var_volume_sarima) == 'Mar'] <- '03'
names(fcast_mdp_var_volume_sarima)[names(fcast_mdp_var_volume_sarima) == 'Apr'] <- '04'
names(fcast_mdp_var_volume_sarima)[names(fcast_mdp_var_volume_sarima) == 'May'] <- '05'
names(fcast_mdp_var_volume_sarima)[names(fcast_mdp_var_volume_sarima) == 'Jun'] <- '06'
names(fcast_mdp_var_volume_sarima)[names(fcast_mdp_var_volume_sarima) == 'Jul'] <- '07'
names(fcast_mdp_var_volume_sarima)[names(fcast_mdp_var_volume_sarima) == 'Aug'] <- '08'
names(fcast_mdp_var_volume_sarima)[names(fcast_mdp_var_volume_sarima) == 'Sep'] <- '09'
names(fcast_mdp_var_volume_sarima)[names(fcast_mdp_var_volume_sarima) == 'Oct'] <- '10'
names(fcast_mdp_var_volume_sarima)[names(fcast_mdp_var_volume_sarima) == 'Nov'] <- '11'
names(fcast_mdp_var_volume_sarima)[names(fcast_mdp_var_volume_sarima) == 'Dec'] <- '12'

# Ordenando as colunas por ordem crescente dos meses - MDP VAREJO
fcast_mdp_var_volume_sarima <- fcast_mdp_var_volume_sarima[ , order(names(fcast_mdp_var_volume_sarima))]

# Transpondo a linha dos anos para a coluna - MDP VAREJO
fcast_mdp_var_volume_sarima <- as.data.frame(t(fcast_mdp_var_volume_sarima))

# Transformando o index de meses em uma coluna
fcast_mdp_var_volume_sarima <- cbind(meses = rownames(fcast_mdp_var_volume_sarima), fcast_mdp_var_volume_sarima)
rownames(fcast_mdp_var_volume_sarima) <- 1:nrow(fcast_mdp_var_volume_sarima)

# Criando dataframe 2017 e 2018
df_new_2017 <- as.data.frame(fcast_mdp_var_volume_sarima[,1:2])
df_new_2018 <- as.data.frame(fcast_mdp_var_volume_sarima[,c(1,3)])

# Juntando mês com ano - 2017
df_new_2017 <- as.data.frame(paste("2017", df_new_2017$meses, sep = "-"))
df_new_2017 <- cbind(df_new_2017, fcast_mdp_var_volume_sarima[,2])

# Juntando mês com ano - 2018
df_new_2018 <- as.data.frame(paste("2018", df_new_2018$meses, sep = "-"))
df_new_2018 <- cbind(df_new_2018, fcast_mdp_var_volume_sarima[,3])

# Renomeando as colunas
names(df_new_2017) <- c("dt_competencia","forecast")
names(df_new_2018) <- c("dt_competencia","forecast")

# Concatenando os dois dataframes do MDP VAREJO
fcast_mdp_var_volume_sarima <- rbind(df_new_2017, df_new_2018)

# Removendo linhas onde o volume é NA, ou seja, não existe volume
fcast_mdp_var_volume_sarima <- fcast_mdp_var_volume_sarima[complete.cases(fcast_mdp_var_volume_sarima[ , 2]),]
###


### MDF VAREJO ###
# Transformando a predição de MDF Varejo do ARIMA para uma lista com meses e anos
fcast_mdf_var_volume_sarima <- tapply(fit_mdf_var_volume_sarima$pred, 
                                      list(year = floor(time(fit_mdf_var_volume_sarima$pred)), 
                                           month = month.abb[cycle(fit_mdf_var_volume_sarima$pred)]),
                                      c)

# Transformando a variável fcast_mdf_var_volume_sarima para o formato dataframe
fcast_mdf_var_volume_sarima <- as.data.frame(fcast_mdf_var_volume_sarima)

# Renomeando as colunas do dataframe fcast_mdf_var_volume_sarima com os meses - MDF VAREJO
names(fcast_mdf_var_volume_sarima)[names(fcast_mdf_var_volume_sarima) == 'Jan'] <- '01'
names(fcast_mdf_var_volume_sarima)[names(fcast_mdf_var_volume_sarima) == 'Feb'] <- '02'
names(fcast_mdf_var_volume_sarima)[names(fcast_mdf_var_volume_sarima) == 'Mar'] <- '03'
names(fcast_mdf_var_volume_sarima)[names(fcast_mdf_var_volume_sarima) == 'Apr'] <- '04'
names(fcast_mdf_var_volume_sarima)[names(fcast_mdf_var_volume_sarima) == 'May'] <- '05'
names(fcast_mdf_var_volume_sarima)[names(fcast_mdf_var_volume_sarima) == 'Jun'] <- '06'
names(fcast_mdf_var_volume_sarima)[names(fcast_mdf_var_volume_sarima) == 'Jul'] <- '07'
names(fcast_mdf_var_volume_sarima)[names(fcast_mdf_var_volume_sarima) == 'Aug'] <- '08'
names(fcast_mdf_var_volume_sarima)[names(fcast_mdf_var_volume_sarima) == 'Sep'] <- '09'
names(fcast_mdf_var_volume_sarima)[names(fcast_mdf_var_volume_sarima) == 'Oct'] <- '10'
names(fcast_mdf_var_volume_sarima)[names(fcast_mdf_var_volume_sarima) == 'Nov'] <- '11'
names(fcast_mdf_var_volume_sarima)[names(fcast_mdf_var_volume_sarima) == 'Dec'] <- '12'

# Ordenando as colunas por ordem crescente dos meses - MDF VAREJO
fcast_mdf_var_volume_sarima <- fcast_mdf_var_volume_sarima[ , order(names(fcast_mdf_var_volume_sarima))]

# Transpondo a linha dos anos para a coluna - MDF VAREJO
fcast_mdf_var_volume_sarima <- as.data.frame(t(fcast_mdf_var_volume_sarima))

# Transformando o index de meses em uma coluna
fcast_mdf_var_volume_sarima <- cbind(meses = rownames(fcast_mdf_var_volume_sarima), fcast_mdf_var_volume_sarima)
rownames(fcast_mdf_var_volume_sarima) <- 1:nrow(fcast_mdf_var_volume_sarima)

# Criando dataframe 2017 e 2018
df_new_2017 <- as.data.frame(fcast_mdf_var_volume_sarima[,1:2])
df_new_2018 <- as.data.frame(fcast_mdf_var_volume_sarima[,c(1,3)])

# Juntando mês com ano - 2017
df_new_2017 <- as.data.frame(paste("2017", df_new_2017$meses, sep = "-"))
df_new_2017 <- cbind(df_new_2017, fcast_mdf_var_volume_sarima[,2])

# Juntando mês com ano - 2018
df_new_2018 <- as.data.frame(paste("2018", df_new_2018$meses, sep = "-"))
df_new_2018 <- cbind(df_new_2018, fcast_mdf_var_volume_sarima[,3])

# Renomeando as colunas
names(df_new_2017) <- c("dt_competencia","forecast")
names(df_new_2018) <- c("dt_competencia","forecast")

# Concatenando os dois dataframes do MDF VAREJO
fcast_mdf_var_volume_sarima <- rbind(df_new_2017, df_new_2018)

# Removendo linhas onde o volume é NA, ou seja, não existe volume
fcast_mdf_var_volume_sarima <- fcast_mdf_var_volume_sarima[complete.cases(fcast_mdf_var_volume_sarima[ , 2]),]
###

# Salvando apenas a data e o forecast do MDP Indústria (RECEITA LÍQUIDA)
fcast_mdp_ind_csv <- fcast_mdp_ind %>%
  select(`Point Forecast`) 

# Salvando apenas a data e o forecast do MDF Indústria (VOLUME) - SARIMA
fcast_mdf_ind_csv <- fcast_mdf_ind_volume_sarima %>%
  select(dt_competencia,forecast)

# Salvando apenas a data e o forecast do MDP Varejo (RECEITA LÍQUIDA)
fcast_mdp_var_csv <- fcast_mdp_var %>%
  select(ds, yhat)

# Salvando apenas o forecast do MDP Industria (VOLUME)
fcast_mdp_ind_volume_csv <- fcast_mdp_ind_volume %>%
  select(`Point Forecast`) 

# Salvando apenas o forecast do MDP Varejo (VOLUME)
fcast_mdp_var_volume_csv <- fcast_mdp_var_volume_sarima %>%
  select(dt_competencia, forecast) 

## Colocando a data em uma coluna do dataframe MDP Indústria - RECEITA LÍQUIDA ##
fcast_mdp_ind_csv <- cbind(dt_competencia = rownames(fcast_mdp_ind_csv), fcast_mdp_ind_csv)
rownames(fcast_mdp_ind_csv) <- 1:nrow(fcast_mdp_ind_csv)

## Colocando a data em uma coluna do dataframe MDP Indústria - VOLUME ##
fcast_mdp_ind_volume_csv <- cbind(dt_competencia = rownames(fcast_mdp_ind_volume_csv), fcast_mdp_ind_volume_csv)
rownames(fcast_mdp_ind_volume_csv) <- 1:nrow(fcast_mdp_ind_volume_csv)


colnames(fcast_mdp_var_csv)[2] <- c("forecast")
colnames(fcast_mdp_var_csv)[1] <- c("dt_competencia")

colnames(fcast_mdp_var_volume_csv) <- c("dt_competencia", "forecast")

names(fcast_mdp_ind_csv)[2] <- c("forecast")

names(fcast_mdp_ind_volume_csv)[2] <- c("forecast")

# Salvando apenas a data e o forecast do MDF Varejo (VOLUME) - SARIMA
fcast_mdf_var_csv <- fcast_mdf_var_volume_sarima %>%
  select(dt_competencia, forecast) 

# MDF VAREJO
fcast_mdf_var_csv$Produto <- "Branco Competitivo"
fcast_mdf_var_csv$Mercado <- "Varejo"
# MDF INDUSTRIA
fcast_mdf_ind_csv$Produto <- "Branco Competitivo"
fcast_mdf_ind_csv$Mercado <- "Indústria"

# Receita Líquida MDP VAREJO
fcast_mdp_var_csv$Produto <- "MP Cru"
fcast_mdp_var_csv$Mercado <- "Varejo"

# Volume MDP VAREJO SARIMA
fcast_mdp_var_volume_csv$Produto <- "MP Cru"
fcast_mdp_var_volume_csv$Mercado <- "Varejo"


# Volume MDP VAREJO
fcast_mdp_var_volume_csv$Produto <- "MP Cru"
fcast_mdp_var_volume_csv$Mercado <- "Varejo"

# Receita Líquida  MDP INDUSTRIA
fcast_mdp_ind_csv$Produto <- "MP Cru"
fcast_mdp_ind_csv$Mercado <- "Indústria"

# Volume MDP INDUSTRIA 
fcast_mdp_ind_volume_csv$Produto <- "MP Cru"
fcast_mdp_ind_volume_csv$Mercado <- "Indústria"


# Criando uma coluna em todos os dataframes com o data no 
# formato yyyy-mm
#fcast_mdp_ind_csv$Month_Yr <- format(as.Date(fcast_mdp_ind_csv$dt_competencia), "%Y%m")
#as.Date(paste0(as.character(fcast_mdp_ind_csv$Month_Yr), 'Jan'), format='%Y%m%')

# Agrupando todos os forecasts da DURATEX em uma variável
forecast_duratex <- smartbind(fcast_mdf_ind_csv, fcast_mdf_var_csv, fcast_mdp_ind_volume_csv, fcast_mdp_var_volume_csv)

# Renomeando colunas do dataframe
colnames(forecast_duratex) <- c("dt_competencia", "Volume", "Produto", "Mercado")

# Transformando . em vírgula para o Volume
forecast_duratex$Volume <- gsub("\\.", ",", forecast_duratex$Volume)

# Salvando apenas a data e o forecast do MDF (VOLUME BRASIL) - ARIMA
fcast_mdf_iba_volume_csv <- fcast_mdf_iba_volume %>%
  select(`Point Forecast`) 

## Colocando a data em uma coluna do dataframe MDF - VOLUME BRASIL ##
fcast_mdf_iba_volume_csv <- cbind(dt_competencia = rownames(fcast_mdf_iba_volume_csv), fcast_mdf_iba_volume_csv)
rownames(fcast_mdf_iba_volume_csv) <- 1:nrow(fcast_mdf_iba_volume_csv)


# Salvando apenas a data e o forecast do MDP (VOLUME BRASIL) - ARIMA
fcast_mdp_iba_volume_csv <- fcast_mdp_iba_volume %>%
  select(`Point Forecast`) 

## Colocando a data em uma coluna do dataframe MDP - VOLUME BRASIL ##
fcast_mdp_iba_volume_csv <- cbind(dt_competencia = rownames(fcast_mdp_iba_volume_csv), fcast_mdp_iba_volume_csv)
rownames(fcast_mdp_iba_volume_csv) <- 1:nrow(fcast_mdp_iba_volume_csv)

# Volume BRASIL - MDF e MDP
fcast_mdf_iba_volume_csv$Produto <- "MDF"
fcast_mdp_iba_volume_csv$Produto <- "MDP"

# Agrupando todos os forecasts do BRASIL em uma variável
forecast_brasil <- smartbind(fcast_mdf_iba_volume_csv, fcast_mdp_iba_volume_csv)

# Renomeando colunas do dataframe
colnames(forecast_brasil) <- c("dt_competencia", "Volume", "Produto")

# Transformando . em vírgula para o Volume
forecast_brasil$Volume <- gsub("\\.", ",", forecast_brasil$Volume)






### FILIAIS ###
# Salvando apenas a data e o forecast do MDP Indústria
fcast_mdp_ind_pr_sc_csv <- fcast_mdp_ind_volume_pr_sc %>%
  select(`Point Forecast`) 
## Colocando a data em uma coluna do dataframe MDP Indústria 
fcast_mdp_ind_pr_sc_csv <- cbind(dt_competencia = rownames(fcast_mdp_ind_pr_sc_csv), fcast_mdp_ind_pr_sc_csv)
rownames(fcast_mdp_ind_pr_sc_csv) <- 1:nrow(fcast_mdp_ind_pr_sc_csv)
names(fcast_mdp_ind_pr_sc_csv)[2] <- c("forecast")
# MDP INDUSTRIA
fcast_mdp_ind_pr_sc_csv$Produto <- "MP Cru"
fcast_mdp_ind_pr_sc_csv$Mercado <- "Indústria"

#
fcast_mdp_ind_rs_rep_csv <- fcast_mdp_ind_volume_rs_rep %>%
  select(`Point Forecast`) 
## Colocando a data em uma coluna do dataframe MDP Indústria 
fcast_mdp_ind_rs_rep_csv <- cbind(dt_competencia = rownames(fcast_mdp_ind_rs_rep_csv), fcast_mdp_ind_rs_rep_csv)
rownames(fcast_mdp_ind_rs_rep_csv) <- 1:nrow(fcast_mdp_ind_rs_rep_csv)
names(fcast_mdp_ind_rs_rep_csv)[2] <- c("forecast")
# MDP INDUSTRIA
fcast_mdp_ind_rs_rep_csv$Produto <- "MP Cru"
fcast_mdp_ind_rs_rep_csv$Mercado <- "Indústria"
###

#
fcast_mdp_ind_sp_capital_csv <- fcast_mdp_ind_volume_sp_capital %>%
  select(`Point Forecast`) 
## Colocando a data em uma coluna do dataframe MDP Indústria 
fcast_mdp_ind_sp_capital_csv <- cbind(dt_competencia = rownames(fcast_mdp_ind_sp_capital_csv), fcast_mdp_ind_sp_capital_csv)
rownames(fcast_mdp_ind_sp_capital_csv) <- 1:nrow(fcast_mdp_ind_sp_capital_csv)
names(fcast_mdp_ind_sp_capital_csv)[2] <- c("forecast")
# MDP INDUSTRIA
fcast_mdp_ind_sp_capital_csv$Produto <- "MP Cru"
fcast_mdp_ind_sp_capital_csv$Mercado <- "Indústria"
###

#
fcast_mdf_ind_sp_capital_csv <- fcast_mdf_ind_volume_sp_capital_arima %>%
  select(`Point Forecast`) 
## Colocando a data em uma coluna do dataframe MDP Indústria 
fcast_mdf_ind_sp_capital_csv <- cbind(dt_competencia = rownames(fcast_mdf_ind_sp_capital_csv), fcast_mdf_ind_sp_capital_csv)
rownames(fcast_mdf_ind_sp_capital_csv) <- 1:nrow(fcast_mdf_ind_sp_capital_csv)
names(fcast_mdf_ind_sp_capital_csv)[2] <- c("forecast")
# MDP INDUSTRIA
fcast_mdf_ind_sp_capital_csv$Produto <- "Branco Competitivo"
fcast_mdf_ind_sp_capital_csv$Mercado <- "Indústria"

#
fcast_mdf_ind_no_ne_csv <- fcast_mdf_ind_volume_sp_capital_arima %>%
  select(`Point Forecast`) 
## Colocando a data em uma coluna do dataframe MDP Indústria 
fcast_mdf_ind_no_ne_csv <- cbind(dt_competencia = rownames(fcast_mdf_ind_no_ne_csv), fcast_mdf_ind_no_ne_csv)
rownames(fcast_mdf_ind_no_ne_csv) <- 1:nrow(fcast_mdf_ind_no_ne_csv)
names(fcast_mdf_ind_no_ne_csv)[2] <- c("forecast")
# MDP INDUSTRIA
fcast_mdf_ind_no_ne_csv$Produto <- "Branco Competitivo"
fcast_mdf_ind_no_ne_csv$Mercado <- "Indústria"
###

# rs- rep
fcast_mdf_ind_rs_rep_csv <- fcast_mdf_ind_volume_no_ne %>%
  select(`Point Forecast`) 
## Colocando a data em uma coluna do dataframe MDP Indústria 
fcast_mdf_ind_rs_rep_csv <- cbind(dt_competencia = rownames(fcast_mdf_ind_rs_rep_csv), fcast_mdf_ind_rs_rep_csv)
rownames(fcast_mdf_ind_rs_rep_csv) <- 1:nrow(fcast_mdf_ind_rs_rep_csv)
names(fcast_mdf_ind_rs_rep_csv)[2] <- c("forecast")
# MDP INDUSTRIA
fcast_mdf_ind_rs_rep_csv$Produto <- "Branco Competitivo"
fcast_mdf_ind_rs_rep_csv$Mercado <- "Indústria"
###

#
# Transformando a predição de MDF Indústria do sARIMA para uma lista com meses e anos
fcast_mdf_ind_pr_sc_sarima <- tapply(fit_mdf_ind_volume_pr_sc_sarima$pred, 
                                     list(year = floor(time(fit_mdf_ind_volume_pr_sc_sarima$pred)), 
                                          month = month.abb[cycle(fit_mdf_ind_volume_pr_sc_sarima$pred)]),
                                     c)

# Transformando a variável fcast_mdf_ind_pr_sc_sarima para o formato dataframe
fcast_mdf_ind_pr_sc_sarima <- as.data.frame(fcast_mdf_ind_pr_sc_sarima)

# Renomeando as colunas do dataframe fcast_mdf_ind_pr_sc_sarima com os meses - MDF INDÚSTRIA
names(fcast_mdf_ind_pr_sc_sarima)[names(fcast_mdf_ind_pr_sc_sarima) == 'Jan'] <- '01'
names(fcast_mdf_ind_pr_sc_sarima)[names(fcast_mdf_ind_pr_sc_sarima) == 'Feb'] <- '02'
names(fcast_mdf_ind_pr_sc_sarima)[names(fcast_mdf_ind_pr_sc_sarima) == 'Mar'] <- '03'
names(fcast_mdf_ind_pr_sc_sarima)[names(fcast_mdf_ind_pr_sc_sarima) == 'Apr'] <- '04'
names(fcast_mdf_ind_pr_sc_sarima)[names(fcast_mdf_ind_pr_sc_sarima) == 'May'] <- '05'
names(fcast_mdf_ind_pr_sc_sarima)[names(fcast_mdf_ind_pr_sc_sarima) == 'Jun'] <- '06'
names(fcast_mdf_ind_pr_sc_sarima)[names(fcast_mdf_ind_pr_sc_sarima) == 'Jul'] <- '07'
names(fcast_mdf_ind_pr_sc_sarima)[names(fcast_mdf_ind_pr_sc_sarima) == 'Aug'] <- '08'
names(fcast_mdf_ind_pr_sc_sarima)[names(fcast_mdf_ind_pr_sc_sarima) == 'Sep'] <- '09'
names(fcast_mdf_ind_pr_sc_sarima)[names(fcast_mdf_ind_pr_sc_sarima) == 'Oct'] <- '10'
names(fcast_mdf_ind_pr_sc_sarima)[names(fcast_mdf_ind_pr_sc_sarima) == 'Nov'] <- '11'
names(fcast_mdf_ind_pr_sc_sarima)[names(fcast_mdf_ind_pr_sc_sarima) == 'Dec'] <- '12'

# Ordenando as colunas por ordem crescente dos meses - MDF VAREJO
fcast_mdf_ind_pr_sc_sarima <- fcast_mdf_ind_pr_sc_sarima[ , order(names(fcast_mdf_ind_pr_sc_sarima))]

# Transpondo a linha dos anos para a coluna - MDF INDÚSTRIA
fcast_mdf_ind_pr_sc_sarima <- as.data.frame(t(fcast_mdf_ind_pr_sc_sarima))

# Transformando o index de meses em uma coluna
fcast_mdf_ind_pr_sc_sarima <- cbind(meses = rownames(fcast_mdf_ind_pr_sc_sarima), fcast_mdf_ind_pr_sc_sarima)
rownames(fcast_mdf_ind_pr_sc_sarima) <- 1:nrow(fcast_mdf_ind_pr_sc_sarima)

# Criando dataframe 2017 e 2018
df_new_2017 <- as.data.frame(fcast_mdf_ind_pr_sc_sarima[,1:2])
df_new_2018 <- as.data.frame(fcast_mdf_ind_pr_sc_sarima[,c(1,3)])

# Juntando mês com ano - 2017
df_new_2017 <- as.data.frame(paste("2017", df_new_2017$meses, sep = "-"))
df_new_2017 <- cbind(df_new_2017, fcast_mdf_ind_pr_sc_sarima[,2])

# Juntando mês com ano - 2018
df_new_2018 <- as.data.frame(paste("2018", df_new_2018$meses, sep = "-"))
df_new_2018 <- cbind(df_new_2018, fcast_mdf_ind_pr_sc_sarima[,3])

# Renomeando as colunas
names(df_new_2017) <- c("dt_competencia","forecast")
names(df_new_2018) <- c("dt_competencia","forecast")

# Concatenando os dois dataframes do MDF INDÚSTRIA
fcast_mdf_ind_pr_sc_sarima <- rbind(df_new_2017, df_new_2018)

# Removendo linhas onde o volume é NA, ou seja, não existe volume
fcast_mdf_ind_pr_sc_sarima <- fcast_mdf_ind_pr_sc_sarima[complete.cases(fcast_mdf_ind_pr_sc_sarima[ , 2]),]

fcast_mdf_ind_pr_sc_sarima$Produto <- "Branco Competitivo"
fcast_mdf_ind_pr_sc_sarima$Mercado <- "Industria"
###

#
# Transformando a predição de MDF Varejo do SARIMA para uma lista com meses e anos
fcast_mdf_var_mg_df_go_sarima <- tapply(fit_mdf_var_volume_mg_df_go$pred, 
                                        list(year = floor(time(fit_mdf_var_volume_mg_df_go$pred)), 
                                             month = month.abb[cycle(fit_mdf_var_volume_mg_df_go$pred)]),
                                        c)

# Transformando a variável fcast_mdf_var_mg_df_go_sarima para o formato dataframe
fcast_mdf_var_mg_df_go_sarima <- as.data.frame(fcast_mdf_var_mg_df_go_sarima)

# Renomeando as colunas do dataframe fcast_mdf_var_mg_df_go_sarima com os meses - MDF INDÚSTRIA
names(fcast_mdf_var_mg_df_go_sarima)[names(fcast_mdf_var_mg_df_go_sarima) == 'Jan'] <- '01'
names(fcast_mdf_var_mg_df_go_sarima)[names(fcast_mdf_var_mg_df_go_sarima) == 'Feb'] <- '02'
names(fcast_mdf_var_mg_df_go_sarima)[names(fcast_mdf_var_mg_df_go_sarima) == 'Mar'] <- '03'
names(fcast_mdf_var_mg_df_go_sarima)[names(fcast_mdf_var_mg_df_go_sarima) == 'Apr'] <- '04'
names(fcast_mdf_var_mg_df_go_sarima)[names(fcast_mdf_var_mg_df_go_sarima) == 'May'] <- '05'
names(fcast_mdf_var_mg_df_go_sarima)[names(fcast_mdf_var_mg_df_go_sarima) == 'Jun'] <- '06'
names(fcast_mdf_var_mg_df_go_sarima)[names(fcast_mdf_var_mg_df_go_sarima) == 'Jul'] <- '07'
names(fcast_mdf_var_mg_df_go_sarima)[names(fcast_mdf_var_mg_df_go_sarima) == 'Aug'] <- '08'
names(fcast_mdfcast_mdf_var_mg_df_go_sarimaf_var_volume_sarima)[names(fcast_mdf_var_mg_df_go_sarima) == 'Sep'] <- '09'
names(fcast_mdf_var_mg_df_go_sarima)[names(fcast_mdf_var_mg_df_go_sarima) == 'Oct'] <- '10'
names(fcast_mdf_var_mg_df_go_sarima)[names(fcast_mdf_var_mg_df_go_sarima) == 'Nov'] <- '11'
names(fcast_mdf_var_mg_df_go_sarima)[names(fcast_mdf_var_mg_df_go_sarima) == 'Dec'] <- '12'

# Ordenando as colunas por ordem crescente dos meses - MDF VAREJO
fcast_mdf_var_mg_df_go_sarima <- fcast_mdf_var_mg_df_go_sarima[ , order(names(fcast_mdf_var_mg_df_go_sarima))]

# Transpondo a linha dos anos para a coluna - MDF INDÚSTRIA
fcast_mdf_var_mg_df_go_sarima <- as.data.frame(t(fcast_mdf_var_mg_df_go_sarima))

# Transformando o index de meses em uma coluna
fcast_mdf_var_mg_df_go_sarima <- cbind(meses = rownames(fcast_mdf_var_mg_df_go_sarima), fcast_mdf_var_mg_df_go_sarima)
rownames(fcast_mdf_var_mg_df_go_sarima) <- 1:nrow(fcast_mdf_var_mg_df_go_sarima)

# Criando dataframe 2017 e 2018
df_new_2017 <- as.data.frame(fcast_mdf_var_mg_df_go_sarima[,1:2])
df_new_2018 <- as.data.frame(fcast_mdf_var_mg_df_go_sarima[,c(1,3)])

# Juntando mês com ano - 2017
df_new_2017 <- as.data.frame(paste("2017", df_new_2017$meses, sep = "-"))
df_new_2017 <- cbind(df_new_2017, fcast_mdf_var_mg_df_go_sarima[,2])

# Juntando mês com ano - 2018
df_new_2018 <- as.data.frame(paste("2018", df_new_2018$meses, sep = "-"))
df_new_2018 <- cbind(df_new_2018, fcast_mdf_var_mg_df_go_sarima[,3])

# Renomeando as colunas
names(df_new_2017) <- c("dt_competencia","forecast")
names(df_new_2018) <- c("dt_competencia","forecast")

# Concatenando os dois dataframes do MDF INDÚSTRIA
fcast_mdf_var_mg_df_go_sarima <- rbind(df_new_2017, df_new_2018)

# Removendo linhas onde o volume é NA, ou seja, não existe volume
fcast_mdf_var_mg_df_go_sarima <- fcast_mdf_var_mg_df_go_sarima[complete.cases(fcast_mdf_var_mg_df_go_sarima[ , 2]),]

fcast_mdf_var_mg_df_go_sarima$Produto <- "Branco Competitivo"
fcast_mdf_var_mg_df_go_sarima$Mercado <- "Varejo"
###

# Transformando a predição de MDF Varejo NO/NE do SARIMA para uma lista com meses e anos
fcast_mdf_var_no_ne_sarima <- tapply(fit_mdf_var_volume_no_ne$pred, 
                                     list(year = floor(time(fit_mdf_var_volume_no_ne$pred)), 
                                          month = month.abb[cycle(fit_mdf_var_volume_no_ne$pred)]),
                                     c)

# Transformando a variável fcast_mdf_var_no_ne_sarima para o formato dataframe
fcast_mdf_var_no_ne_sarima <- as.data.frame(fcast_mdf_var_no_ne_sarima)

# Renomeando as colunas do dataframe fcast_mdf_var_no_ne_sarima com os meses - MDF VAREJO NO/NE
names(fcast_mdf_var_no_ne_sarima)[names(fcast_mdf_var_no_ne_sarima) == 'Jan'] <- '01'
names(fcast_mdf_var_no_ne_sarima)[names(fcast_mdf_var_no_ne_sarima) == 'Feb'] <- '02'
names(fcast_mdf_var_no_ne_sarima)[names(fcast_mdf_var_no_ne_sarima) == 'Mar'] <- '03'
names(fcast_mdf_var_no_ne_sarima)[names(fcast_mdf_var_no_ne_sarima) == 'Apr'] <- '04'
names(fcast_mdf_var_no_ne_sarima)[names(fcast_mdf_var_no_ne_sarima) == 'May'] <- '05'
names(fcast_mdf_var_no_ne_sarima)[names(fcast_mdf_var_no_ne_sarima) == 'Jun'] <- '06'
names(fcast_mdf_var_no_ne_sarima)[names(fcast_mdf_var_no_ne_sarima) == 'Jul'] <- '07'
names(fcast_mdf_var_no_ne_sarima)[names(fcast_mdf_var_no_ne_sarima) == 'Aug'] <- '08'
names(fcast_mdfcast_mdf_var_no_ne_sarimaf_var_volume_sarima)[names(fcast_mdf_var_no_ne_sarima) == 'Sep'] <- '09'
names(fcast_mdf_var_no_ne_sarima)[names(fcast_mdf_var_no_ne_sarima) == 'Oct'] <- '10'
names(fcast_mdf_var_no_ne_sarima)[names(fcast_mdf_var_no_ne_sarima) == 'Nov'] <- '11'
names(fcast_mdf_var_no_ne_sarima)[names(fcast_mdf_var_no_ne_sarima) == 'Dec'] <- '12'

# Ordenando as colunas por ordem crescente dos meses - MDF VAREJO NO/NE
fcast_mdf_var_no_ne_sarima <- fcast_mdf_var_no_ne_sarima[ , order(names(fcast_mdf_var_no_ne_sarima))]

# Transpondo a linha dos anos para a coluna - MDF VAREJO NO/NE
fcast_mdf_var_no_ne_sarima <- as.data.frame(t(fcast_mdf_var_no_ne_sarima))

# Transformando o index de meses em uma coluna
fcast_mdf_var_no_ne_sarima <- cbind(meses = rownames(fcast_mdf_var_no_ne_sarima), fcast_mdf_var_no_ne_sarima)
rownames(fcast_mdf_var_no_ne_sarima) <- 1:nrow(fcast_mdf_var_no_ne_sarima)

# Criando dataframe 2017 e 2018
df_new_2017 <- as.data.frame(fcast_mdf_var_no_ne_sarima[,1:2])
df_new_2018 <- as.data.frame(fcast_mdf_var_no_ne_sarima[,c(1,3)])

# Juntando mês com ano - 2017
df_new_2017 <- as.data.frame(paste("2017", df_new_2017$meses, sep = "-"))
df_new_2017 <- cbind(df_new_2017, fcast_mdf_var_no_ne_sarima[,2])

# Juntando mês com ano - 2018
df_new_2018 <- as.data.frame(paste("2018", df_new_2018$meses, sep = "-"))
df_new_2018 <- cbind(df_new_2018, fcast_mdf_var_no_ne_sarima[,3])

# Renomeando as colunas
names(df_new_2017) <- c("dt_competencia","forecast")
names(df_new_2018) <- c("dt_competencia","forecast")

# Concatenando os dois dataframes do MDF VAREJO NO/NE
fcast_mdf_var_no_ne_sarima <- rbind(df_new_2017, df_new_2018)

# Removendo linhas onde o volume é NA, ou seja, não existe volume
fcast_mdf_var_no_ne_sarima <- fcast_mdf_var_no_ne_sarima[complete.cases(fcast_mdf_var_no_ne_sarima[ , 2]),]

fcast_mdf_var_no_ne_sarima$Produto <- "Branco Competitivo"
fcast_mdf_var_no_ne_sarima$Mercado <- "Varejo"
###

#
# Transformando a predição de MDF Varejo PR/SC do SARIMA para uma lista com meses e anos
fcast_mdf_var_pr_sc_sarima <- tapply(fit_mdf_var_volume_pr_sc$pred, 
                                     list(year = floor(time(fit_mdf_var_volume_pr_sc$pred)), 
                                          month = month.abb[cycle(fit_mdf_var_volume_pr_sc$pred)]),
                                     c)

# Transformando a variável fcast_mdf_var_pr_sc_sarima para o formato dataframe
fcast_mdf_var_pr_sc_sarima <- as.data.frame(fcast_mdf_var_pr_sc_sarima)

# Renomeando as colunas do dataframe fcast_mdf_var_pr_sc_sarima com os meses - MDF VAREJO PR/SC
names(fcast_mdf_var_pr_sc_sarima)[names(fcast_mdf_var_pr_sc_sarima) == 'Jan'] <- '01'
names(fcast_mdf_var_pr_sc_sarima)[names(fcast_mdf_var_pr_sc_sarima) == 'Feb'] <- '02'
names(fcast_mdf_var_pr_sc_sarima)[names(fcast_mdf_var_pr_sc_sarima) == 'Mar'] <- '03'
names(fcast_mdf_var_pr_sc_sarima)[names(fcast_mdf_var_pr_sc_sarima) == 'Apr'] <- '04'
names(fcast_mdf_var_pr_sc_sarima)[names(fcast_mdf_var_pr_sc_sarima) == 'May'] <- '05'
names(fcast_mdf_var_pr_sc_sarima)[names(fcast_mdf_var_pr_sc_sarima) == 'Jun'] <- '06'
names(fcast_mdf_var_pr_sc_sarima)[names(fcast_mdf_var_pr_sc_sarima) == 'Jul'] <- '07'
names(fcast_mdf_var_pr_sc_sarima)[names(fcast_mdf_var_pr_sc_sarima) == 'Aug'] <- '08'
names(fcast_mdfcast_mdf_var_pr_sc_sarimaf_var_volume_sarima)[names(fcast_mdf_var_pr_sc_sarima) == 'Sep'] <- '09'
names(fcast_mdf_var_pr_sc_sarima)[names(fcast_mdf_var_pr_sc_sarima) == 'Oct'] <- '10'
names(fcast_mdf_var_pr_sc_sarima)[names(fcast_mdf_var_pr_sc_sarima) == 'Nov'] <- '11'
names(fcast_mdf_var_pr_sc_sarima)[names(fcast_mdf_var_pr_sc_sarima) == 'Dec'] <- '12'

# Ordenando as colunas por ordem crescente dos meses - MDF VAREJO PR/SC
fcast_mdf_var_pr_sc_sarima <- fcast_mdf_var_pr_sc_sarima[ , order(names(fcast_mdf_var_pr_sc_sarima))]

# Transpondo a linha dos anos para a coluna - MDF VAREJO PR/SC
fcast_mdf_var_pr_sc_sarima <- as.data.frame(t(fcast_mdf_var_pr_sc_sarima))

# Transformando o index de meses em uma coluna
fcast_mdf_var_pr_sc_sarima <- cbind(meses = rownames(fcast_mdf_var_pr_sc_sarima), fcast_mdf_var_pr_sc_sarima)
rownames(fcast_mdf_var_pr_sc_sarima) <- 1:nrow(fcast_mdf_var_pr_sc_sarima)

# Criando dataframe 2017 e 2018
df_new_2017 <- as.data.frame(fcast_mdf_var_pr_sc_sarima[,1:2])
df_new_2018 <- as.data.frame(fcast_mdf_var_pr_sc_sarima[,c(1,3)])

# Juntando mês com ano - 2017
df_new_2017 <- as.data.frame(paste("2017", df_new_2017$meses, sep = "-"))
df_new_2017 <- cbind(df_new_2017, fcast_mdf_var_pr_sc_sarima[,2])

# Juntando mês com ano - 2018
df_new_2018 <- as.data.frame(paste("2018", df_new_2018$meses, sep = "-"))
df_new_2018 <- cbind(df_new_2018, fcast_mdf_var_pr_sc_sarima[,3])

# Renomeando as colunas
names(df_new_2017) <- c("dt_competencia","forecast")
names(df_new_2018) <- c("dt_competencia","forecast")

# Concatenando os dois dataframes do MDF VAREJO PR/SC
fcast_mdf_var_pr_sc_sarima <- rbind(df_new_2017, df_new_2018)

# Removendo linhas onde o volume é NA, ou seja, não existe volume
fcast_mdf_var_pr_sc_sarima <- fcast_mdf_var_pr_sc_sarima[complete.cases(fcast_mdf_var_pr_sc_sarima[ , 2]),]
fcast_mdf_var_pr_sc_sarima$Produto <- "Branco Competitivo"
fcast_mdf_var_pr_sc_sarima$Mercado <- "Varejo"

###


#
# Transformando a predição de MDF Varejo PR/SC do SARIMA para uma lista com meses e anos
fcast_mdf_var_rj_es_sarima <- tapply(fit_mdf_var_volume_rj_es$pred, 
                                     list(year = floor(time(fit_mdf_var_volume_rj_es$pred)), 
                                          month = month.abb[cycle(fit_mdf_var_volume_rj_es$pred)]),
                                     c)

# Transformando a variável fcast_mdf_var_rj_es_sarima para o formato dataframe
fcast_mdf_var_rj_es_sarima <- as.data.frame(fcast_mdf_var_rj_es_sarima)

# Renomeando as colunas do dataframe fcast_mdf_var_rj_es_sarima com os meses - MDF VAREJO PR/SC
names(fcast_mdf_var_rj_es_sarima)[names(fcast_mdf_var_rj_es_sarima) == 'Jan'] <- '01'
names(fcast_mdf_var_rj_es_sarima)[names(fcast_mdf_var_rj_es_sarima) == 'Feb'] <- '02'
names(fcast_mdf_var_rj_es_sarima)[names(fcast_mdf_var_rj_es_sarima) == 'Mar'] <- '03'
names(fcast_mdf_var_rj_es_sarima)[names(fcast_mdf_var_rj_es_sarima) == 'Apr'] <- '04'
names(fcast_mdf_var_rj_es_sarima)[names(fcast_mdf_var_rj_es_sarima) == 'May'] <- '05'
names(fcast_mdf_var_rj_es_sarima)[names(fcast_mdf_var_rj_es_sarima) == 'Jun'] <- '06'
names(fcast_mdf_var_rj_es_sarima)[names(fcast_mdf_var_rj_es_sarima) == 'Jul'] <- '07'
names(fcast_mdf_var_rj_es_sarima)[names(fcast_mdf_var_rj_es_sarima) == 'Aug'] <- '08'
names(fcast_mdfcast_mdf_var_rj_es_sarimaf_var_volume_sarima)[names(fcast_mdf_var_rj_es_sarima) == 'Sep'] <- '09'
names(fcast_mdf_var_rj_es_sarima)[names(fcast_mdf_var_rj_es_sarima) == 'Oct'] <- '10'
names(fcast_mdf_var_rj_es_sarima)[names(fcast_mdf_var_rj_es_sarima) == 'Nov'] <- '11'
names(fcast_mdf_var_rj_es_sarima)[names(fcast_mdf_var_rj_es_sarima) == 'Dec'] <- '12'

# Ordenando as colunas por ordem crescente dos meses - MDF VAREJO PR/SC
fcast_mdf_var_rj_es_sarima <- fcast_mdf_var_rj_es_sarima[ , order(names(fcast_mdf_var_rj_es_sarima))]

# Transpondo a linha dos anos para a coluna - MDF VAREJO PR/SC
fcast_mdf_var_rj_es_sarima <- as.data.frame(t(fcast_mdf_var_rj_es_sarima))

# Transformando o index de meses em uma coluna
fcast_mdf_var_rj_es_sarima <- cbind(meses = rownames(fcast_mdf_var_rj_es_sarima), fcast_mdf_var_rj_es_sarima)
rownames(fcast_mdf_var_rj_es_sarima) <- 1:nrow(fcast_mdf_var_rj_es_sarima)

# Criando dataframe 2017 e 2018
df_new_2017 <- as.data.frame(fcast_mdf_var_rj_es_sarima[,1:2])
df_new_2018 <- as.data.frame(fcast_mdf_var_rj_es_sarima[,c(1,3)])

# Juntando mês com ano - 2017
df_new_2017 <- as.data.frame(paste("2017", df_new_2017$meses, sep = "-"))
df_new_2017 <- cbind(df_new_2017, fcast_mdf_var_rj_es_sarima[,2])

# Juntando mês com ano - 2018
df_new_2018 <- as.data.frame(paste("2018", df_new_2018$meses, sep = "-"))
df_new_2018 <- cbind(df_new_2018, fcast_mdf_var_rj_es_sarima[,3])

# Renomeando as colunas
names(df_new_2017) <- c("dt_competencia","forecast")
names(df_new_2018) <- c("dt_competencia","forecast")

# Concatenando os dois dataframes do MDF VAREJO PR/SC
fcast_mdf_var_rj_es_sarima <- rbind(df_new_2017, df_new_2018)

# Removendo linhas onde o volume é NA, ou seja, não existe volume
fcast_mdf_var_rj_es_sarima <- fcast_mdf_var_rj_es_sarima[complete.cases(fcast_mdf_var_rj_es_sarima[ , 2]),]

fcast_mdf_var_rj_es_sarima$Produto <- "Branco Competitivo"
fcast_mdf_var_rj_es_sarima$Mercado <- "Varejo"
###

#
# Transformando a predição de MDF Varejo SP do SARIMA para uma lista com meses e anos
fcast_mdf_var_sp_sarima <- tapply(fit_mdf_var_volume_sp_sarima$pred, 
                                  list(year = floor(time(fit_mdf_var_volume_sp_sarima$pred)), 
                                       month = month.abb[cycle(fit_mdf_var_volume_sp_sarima$pred)]),
                                  c)

# Transformando a variável fcast_mdf_var_sp_sarima para o formato dataframe
fcast_mdf_var_sp_sarima <- as.data.frame(fcast_mdf_var_sp_sarima)

# Renomeando as colunas do dataframe fcast_mdf_var_sp_sarima com os meses - MDF VAREJO SP
names(fcast_mdf_var_sp_sarima)[names(fcast_mdf_var_sp_sarima) == 'Jan'] <- '01'
names(fcast_mdf_var_sp_sarima)[names(fcast_mdf_var_sp_sarima) == 'Feb'] <- '02'
names(fcast_mdf_var_sp_sarima)[names(fcast_mdf_var_sp_sarima) == 'Mar'] <- '03'
names(fcast_mdf_var_sp_sarima)[names(fcast_mdf_var_sp_sarima) == 'Apr'] <- '04'
names(fcast_mdf_var_sp_sarima)[names(fcast_mdf_var_sp_sarima) == 'May'] <- '05'
names(fcast_mdf_var_sp_sarima)[names(fcast_mdf_var_sp_sarima) == 'Jun'] <- '06'
names(fcast_mdf_var_sp_sarima)[names(fcast_mdf_var_sp_sarima) == 'Jul'] <- '07'
names(fcast_mdf_var_sp_sarima)[names(fcast_mdf_var_sp_sarima) == 'Aug'] <- '08'
names(fcast_mdf_var_sp_sarima)[names(fcast_mdf_var_sp_sarima) == 'Sep'] <- '09'
names(fcast_mdf_var_sp_sarima)[names(fcast_mdf_var_sp_sarima) == 'Oct'] <- '10'
names(fcast_mdf_var_sp_sarima)[names(fcast_mdf_var_sp_sarima) == 'Nov'] <- '11'
names(fcast_mdf_var_sp_sarima)[names(fcast_mdf_var_sp_sarima) == 'Dec'] <- '12'

# Ordenando as colunas por ordem crescente dos meses - MDF VAREJO SP
fcast_mdf_var_sp_sarima <- fcast_mdf_var_sp_sarima[ , order(names(fcast_mdf_var_sp_sarima))]

# Transpondo a linha dos anos para a coluna - MDF VAREJO SP
fcast_mdf_var_sp_sarima <- as.data.frame(t(fcast_mdf_var_sp_sarima))

# Transformando o index de meses em uma coluna
fcast_mdf_var_sp_sarima <- cbind(meses = rownames(fcast_mdf_var_sp_sarima), fcast_mdf_var_sp_sarima)
rownames(fcast_mdf_var_sp_sarima) <- 1:nrow(fcast_mdf_var_sp_sarima)

# Criando dataframe 2017 e 2018
df_new_2017 <- as.data.frame(fcast_mdf_var_sp_sarima[,1:2])
df_new_2018 <- as.data.frame(fcast_mdf_var_sp_sarima[,c(1,3)])

# Juntando mês com ano - 2017
df_new_2017 <- as.data.frame(paste("2017", df_new_2017$meses, sep = "-"))
df_new_2017 <- cbind(df_new_2017, fcast_mdf_var_sp_sarima[,2])

# Juntando mês com ano - 2018
df_new_2018 <- as.data.frame(paste("2018", df_new_2018$meses, sep = "-"))
df_new_2018 <- cbind(df_new_2018, fcast_mdf_var_sp_sarima[,3])

# Renomeando as colunas
names(df_new_2017) <- c("dt_competencia","forecast")
names(df_new_2018) <- c("dt_competencia","forecast")

# Concatenando os dois dataframes do MDF VAREJO SP
fcast_mdf_var_sp_sarima <- rbind(df_new_2017, df_new_2018)

# Removendo linhas onde o volume é NA, ou seja, não existe volume
fcast_mdf_var_sp_sarima <- fcast_mdf_var_sp_sarima[complete.cases(fcast_mdf_var_sp_sarima[ , 2]),]

fcast_mdf_var_sp_sarima$Produto <- "Branco Competitivo"
fcast_mdf_var_sp_sarima$Mercado <- "Varejo"
###

#
# Salvando apenas a data e o forecast do MDP Varejo NO/NE (VOLUME)
fcast_mdp_ind_volume_no_ne_csv <- fcast_mdp_ind_volume_no_ne %>%
  select(ds, yhat)

colnames(fcast_mdp_ind_volume_no_ne_csv)[2] <- c("forecast")
colnames(fcast_mdp_ind_volume_no_ne_csv)[1] <- c("dt_competencia")

fcast_mdp_ind_volume_no_ne_csv$Produto <- "MP Cru"
fcast_mdp_ind_volume_no_ne_csv$Mercado <- "Industria"
###

#
# Salvando apenas a data e o forecast do MDP Industria SP (VOLUME)
fcast_mdp_var_volume_SP <- fcast_mdp_var_volume_SP %>%
  select(`Point Forecast`) 
## Colocando a data em uma coluna do dataframe MDP Indústria 
fcast_mdp_var_volume_SP <- cbind(dt_competencia = rownames(fcast_mdp_var_volume_SP), fcast_mdp_var_volume_SP)
rownames(fcast_mdp_var_volume_SP) <- 1:nrow(fcast_mdp_var_volume_SP)
names(fcast_mdp_var_volume_SP)[2] <- c("forecast")
# MDP vAREJO
fcast_mdp_var_volume_SP$Produto <- "MP Cru"
fcast_mdp_var_volume_SP$Mercado <- "Varejo"
###

#
# Transformando a predição de MDF Varejo SP do SARIMA para uma lista com meses e anos
fcast_mdf_ind_volume_sp_capital_sarima <- tapply(fit_mdf_ind_volume_sp_capital$pred, 
                                                 list(year = floor(time(fit_mdf_ind_volume_sp_capital$pred)), 
                                                      month = month.abb[cycle(fit_mdf_ind_volume_sp_capital$pred)]),
                                                 c)

# Transformando a variável fcast_mdf_ind_volume_sp_capital_sarima para o formato dataframe
fcast_mdf_ind_volume_sp_capital_sarima <- as.data.frame(fcast_mdf_ind_volume_sp_capital_sarima)

# Renomeando as colunas do dataframe fcast_mdf_ind_volume_sp_capital_sarima com os meses - MDF VAREJO SP
names(fcast_mdf_ind_volume_sp_capital_sarima)[names(fcast_mdf_ind_volume_sp_capital_sarima) == 'Jan'] <- '01'
names(fcast_mdf_ind_volume_sp_capital_sarima)[names(fcast_mdf_ind_volume_sp_capital_sarima) == 'Feb'] <- '02'
names(fcast_mdf_ind_volume_sp_capital_sarima)[names(fcast_mdf_ind_volume_sp_capital_sarima) == 'Mar'] <- '03'
names(fcast_mdf_ind_volume_sp_capital_sarima)[names(fcast_mdf_ind_volume_sp_capital_sarima) == 'Apr'] <- '04'
names(fcast_mdf_ind_volume_sp_capital_sarima)[names(fcast_mdf_ind_volume_sp_capital_sarima) == 'May'] <- '05'
names(fcast_mdf_ind_volume_sp_capital_sarima)[names(fcast_mdf_ind_volume_sp_capital_sarima) == 'Jun'] <- '06'
names(fcast_mdf_ind_volume_sp_capital_sarima)[names(fcast_mdf_ind_volume_sp_capital_sarima) == 'Jul'] <- '07'
names(fcast_mdf_ind_volume_sp_capital_sarima)[names(fcast_mdf_ind_volume_sp_capital_sarima) == 'Aug'] <- '08'
names(fcast_mdf_ind_volume_sp_capital_sarima)[names(fcast_mdf_ind_volume_sp_capital_sarima) == 'Sep'] <- '09'
names(fcast_mdf_ind_volume_sp_capital_sarima)[names(fcast_mdf_ind_volume_sp_capital_sarima) == 'Oct'] <- '10'
names(fcast_mdf_ind_volume_sp_capital_sarima)[names(fcast_mdf_ind_volume_sp_capital_sarima) == 'Nov'] <- '11'
names(fcast_mdf_ind_volume_sp_capital_sarima)[names(fcast_mdf_ind_volume_sp_capital_sarima) == 'Dec'] <- '12'

# Ordenando as colunas por ordem crescente dos meses - MDF VAREJO SP
fcast_mdf_ind_volume_sp_capital_sarima <- fcast_mdf_ind_volume_sp_capital_sarima[ , order(names(fcast_mdf_ind_volume_sp_capital_sarima))]

# Transpondo a linha dos anos para a coluna - MDF VAREJO SP
fcast_mdf_ind_volume_sp_capital_sarima <- as.data.frame(t(fcast_mdf_ind_volume_sp_capital_sarima))

# Transformando o index de meses em uma coluna
fcast_mdf_ind_volume_sp_capital_sarima <- cbind(meses = rownames(fcast_mdf_ind_volume_sp_capital_sarima), fcast_mdf_ind_volume_sp_capital_sarima)
rownames(fcast_mdf_ind_volume_sp_capital_sarima) <- 1:nrow(fcast_mdf_ind_volume_sp_capital_sarima)

# Criando dataframe 2017 e 2018
df_new_2017 <- as.data.frame(fcast_mdf_ind_volume_sp_capital_sarima[,1:2])
df_new_2018 <- as.data.frame(fcast_mdf_ind_volume_sp_capital_sarima[,c(1,3)])

# Juntando mês com ano - 2017
df_new_2017 <- as.data.frame(paste("2017", df_new_2017$meses, sep = "-"))
df_new_2017 <- cbind(df_new_2017, fcast_mdf_ind_volume_sp_capital_sarima[,2])

# Juntando mês com ano - 2018
df_new_2018 <- as.data.frame(paste("2018", df_new_2018$meses, sep = "-"))
df_new_2018 <- cbind(df_new_2018, fcast_mdf_ind_volume_sp_capital_sarima[,3])

# Renomeando as colunas
names(df_new_2017) <- c("dt_competencia","forecast")
names(df_new_2018) <- c("dt_competencia","forecast")

# Concatenando os dois dataframes do MDF VAREJO SP
fcast_mdf_ind_volume_sp_capital_sarima <- rbind(df_new_2017, df_new_2018)

# Removendo linhas onde o volume é NA, ou seja, não existe volume
fcast_mdf_ind_volume_sp_capital_sarima <- fcast_mdf_ind_volume_sp_capital_sarima[complete.cases(fcast_mdf_ind_volume_sp_capital_sarima[ , 2]),]

fcast_mdf_ind_volume_sp_capital_sarima$Produto <- "Branco Competitivo"
fcast_mdf_ind_volume_sp_capital_sarima$Mercado <- "Industria"
###

##### SALVANDO ARQUIVO XLSX ####
# Salvando o arquivo XLSX com as previsões para Volume MDF, MDP (DURATEX) e Volume BRASIL
list_of_datasets <- list("Forecast Volume Duratex" = forecast_duratex, 
                         "Forecast Volume Brasil" = forecast_brasil, 
                         "MDP Ind PR-SC" = fcast_mdp_ind_pr_sc_csv,
                         "MDP Ind RS-Rep" = fcast_mdp_ind_rs_rep_csv,
                         "MDP Ind SP-Capital" = fcast_mdp_ind_sp_capital_csv,
                         "MDP Ind NO-NE" = fcast_mdp_ind_volume_no_ne_csv,
                         "MDP Var SP-I" = fcast_mdp_var_volume_SP,
                         "MDF Ind SP-Capital" = fcast_mdf_ind_volume_sp_capital_sarima,
                         "MDF Ind NO-NE" = fcast_mdf_ind_no_ne_csv,
                         "MDF Ind RS-Rep" = fcast_mdf_ind_rs_rep_csv,
                         "MDF Ind PR-SC" = fcast_mdf_ind_pr_sc_sarima,
                         "MDF Var MG-DF-GO" = fcast_mdf_var_mg_df_go_sarima,
                         "MDF Var NO-NE" = fcast_mdf_var_no_ne_sarima,
                         "MDF Var PR-SC" = fcast_mdf_var_pr_sc_sarima,
                         "MDF Var RJ-ES" = fcast_mdf_var_rj_es_sarima,
                         "MDF Var SP" = fcast_mdf_var_sp_sarima
                        )

# Escrevendo arquivo XLSX
write.xlsx(list_of_datasets, file = "Forecast.xlsx")


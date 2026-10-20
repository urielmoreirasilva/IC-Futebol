import pandas as pd
import numpy as np
from scipy.stats import poisson
from datetime import timedelta
from scipy.optimize import minimize

# =====================================================
# CONFIGURAÇÕES INICIAIS
# =====================================================
JANELA_DIAS = 730 
MAX_GOLS = 10      
XI = 0.0018       

# =====================================================
# TRATAMENTO DE DADOS E MAPEAMENTO GLOBAL
# =====================================================
df = pd.read_csv("serie_A.csv")

# Remove qualquer linha com data ou placar nulo/vazio
df = df.dropna(subset=["data", "mandante_Placar", "visitante_Placar"])

# Padroniza o nome dos times
df["mandante"] = df["mandante"].str.lower().str.strip()
df["visitante"] = df["visitante"].str.lower().str.strip()

# Converte e ordena os jogos por data cronológica
df["data"] = pd.to_datetime(df["data"], dayfirst=True)
df = df.sort_values("data").reset_index(drop=True)

# Mapeia todos os times únicos para garantir consistência de índices
TODOS_TIMES = sorted(set(df["mandante"]).union(set(df["visitante"])))
TOTAL_TIMES_BANCO = len(TODOS_TIMES)
MAPA_TIMES_GLOBAL = {t: i for i, t in enumerate(TODOS_TIMES)}

# =====================================================
# FUNÇÃO: FORÇA DOS TIMES (COM DIXON-COLES E GRADIENTE CORRIGIDO)
# =====================================================
def calcular_forcas(df_janela, data_referencia, parametros_anteriores=None):
    N = TOTAL_TIMES_BANCO
    mapa_times = MAPA_TIMES_GLOBAL

    # Cálculo dos pesos temporais para cada jogo na janela
    dias_passados = (data_referencia - df_janela["data"]).dt.days.values
    pesos_temporais = np.exp(-XI * dias_passados)
    soma_pesos = np.sum(pesos_temporais)

    if soma_pesos == 0:
        soma_pesos = 1.0

    # Conta jogos por time na janela
    jogos_mandante = df_janela["mandante"].value_counts()
    jogos_visitante = df_janela["visitante"].value_counts()
   
    n_i = np.zeros(N)
    for t, idx in mapa_times.items():
        n_i[idx] = jogos_mandante.get(t, 0) + jogos_visitante.get(t, 0)

    # Chute inicial (Warm-start)
    if parametros_anteriores is None:
        gols_totais = np.sum((df_janela["mandante_Placar"] + df_janela["visitante_Placar"]) * pesos_temporais)
        media_gols = gols_totais / (2 * soma_pesos) if soma_pesos > 0 else 1.0
       
        mu_inicial = np.log(media_gols)
        ataque_inicial = np.zeros(N)
        defesa_inicial = np.zeros(N)
        mando_inicial = 0.2
        rho_inicial = 0.0
    else:
        ataque_inicial = parametros_anteriores["ataque"]
        defesa_inicial = parametros_anteriores["defesa"]
        mando_inicial = parametros_anteriores["mando"]
        mu_inicial = parametros_anteriores["mu"]
        rho_inicial = parametros_anteriores["rho"]

    x0 = np.concatenate([ataque_inicial, defesa_inicial, [mando_inicial], [mu_inicial], [rho_inicial]])

    # Hiperparâmetros de Shrinkage
    k = 20
    pesos_lambda = k / (k + n_i)

    if parametros_anteriores is None:
        MEDIA_4_PIORES_ATAQUE = -0.3
        MEDIA_4_PIORES_DEFESA = 0.3
    else:
        MEDIA_4_PIORES_ATAQUE = np.mean(sorted(parametros_anteriores["ataque"])[:4])
        MEDIA_4_PIORES_DEFESA = np.mean(sorted(parametros_anteriores["defesa"])[-4:])

    ataque_prior = np.zeros(N)
    defesa_prior = np.zeros(N)

    if parametros_anteriores is not None:
        ataque_antigo = parametros_anteriores["ataque"]
        defesa_antigo = parametros_anteriores["defesa"]
        for t, idx in mapa_times.items():
            if n_i[idx] > 0:
                ataque_prior[idx] = ataque_antigo[idx]
                defesa_prior[idx] = defesa_antigo[idx]
            else:
                ataque_prior[idx] = MEDIA_4_PIORES_ATAQUE
                defesa_prior[idx] = MEDIA_4_PIORES_DEFESA

    BETA = 50  # Trava de simetria

    # Mapeamento dos índices no vetor x
    IDX_MANDO = 2 * N
    IDX_MU = 2 * N + 1
    IDX_RHO = 2 * N + 2

    def funcao_e_gradiente(x):
        ataque = x[:N]
        defesa = x[N:2*N]
        mando = x[IDX_MANDO]
        mu = x[IDX_MU]
        rho = x[IDX_RHO]

        grad = np.zeros_like(x)
        log_likelihood = 0.0

        for idx_row, (_, jogo) in enumerate(df_janela.iterrows()):
            w_t = pesos_temporais[idx_row]
            i = mapa_times[jogo["mandante"]]
            j = mapa_times[jogo["visitante"]]

            gols_casa = int(jogo["mandante_Placar"])
            gols_fora = int(jogo["visitante_Placar"])

            lambda_casa = np.exp(mu + ataque[i] - defesa[j] + mando)
            lambda_fora = np.exp(mu + ataque[j] - defesa[i])

            # Correção de Dixon-Coles
            tau = 1.0
            dtau_dlambda_casa = 0.0
            dtau_dlambda_fora = 0.0
            dtau_drho = 0.0

            if gols_casa == 0 and gols_fora == 0:
                tau = 1.0 - lambda_casa * lambda_fora * rho
                dtau_dlambda_casa = -lambda_fora * rho
                dtau_dlambda_fora = -lambda_casa * rho
                dtau_drho         = -lambda_casa * lambda_fora

            elif gols_casa == 0 and gols_fora == 1:
                tau = 1.0 + lambda_casa * rho
                dtau_dlambda_casa = rho
                dtau_dlambda_fora = 0.0
                dtau_drho         = lambda_casa

            elif gols_casa == 1 and gols_fora == 0:
                tau = 1.0 + lambda_fora * rho
                dtau_dlambda_casa = 0.0
                dtau_dlambda_fora = rho
                dtau_drho         = lambda_fora
                
            elif gols_casa == 1 and gols_fora == 1:
                tau = 1.0 - rho
                dtau_dlambda_casa = 0.0
                dtau_dlambda_fora = 0.0
                dtau_drho         = -1.0

            tau = max(tau, 1e-10)

            ll_jogo = (poisson.logpmf(gols_casa, lambda_casa) + 
                       poisson.logpmf(gols_fora, lambda_fora) + 
                       np.log(tau))
            
            log_likelihood += w_t * ll_jogo

            # Resíduos com peso temporal
            erro_casa = gols_casa - lambda_casa
            erro_fora = gols_fora - lambda_fora

            adj_casa = (dtau_dlambda_casa * lambda_casa) / tau
            adj_fora = (dtau_dlambda_fora * lambda_fora) / tau

            grad_lambda_casa_total = w_t * (erro_casa + adj_casa)
            grad_lambda_fora_total = w_t * (erro_fora + adj_fora)

            # Acumulação direta no vetor 'grad' para normalização única
            grad[i]       += grad_lambda_casa_total
            grad[N + j]   -= grad_lambda_casa_total
            grad[j]       += grad_lambda_fora_total
            grad[N + i]   -= grad_lambda_fora_total

            grad[IDX_MANDO] += grad_lambda_casa_total
            grad[IDX_MU]    += (grad_lambda_casa_total + grad_lambda_fora_total)
            grad[IDX_RHO]   += w_t * (dtau_drho / tau)

        # Normalização global do Log-Likelihood e do Gradiente pelo peso total
        log_likelihood_norm = log_likelihood / soma_pesos
        grad /= soma_pesos  

        # Aplicação das penalidades de Shrinkage
        diff_atq = ataque - ataque_prior
        diff_def = defesa - defesa_prior
        penalizacao_shrinkage_norm = np.sum(pesos_lambda * diff_atq**2 + pesos_lambda * diff_def**2) / N
       
        grad[:N]     -= (2 * pesos_lambda * diff_atq) / N
        grad[N:2*N]  -= (2 * pesos_lambda * diff_def) / N

        # Trava de simetria (Média centrada em 0)
        mean_atq = np.mean(ataque)
        mean_def = np.mean(defesa)
        punicao_simetria = BETA * (mean_atq**2 + mean_def**2)
       
        grad[:N]     -= (2 * BETA / N) * mean_atq
        grad[N:2*N]  -= (2 * BETA / N) * mean_def

        # Inversão de sinal para minimização (L-BFGS-B)
        obj_final = -(log_likelihood_norm - penalizacao_shrinkage_norm - punicao_simetria)
        grad_final = -grad

        if not np.isfinite(obj_final):
            return 1e10, np.zeros_like(x)

        return obj_final, grad_final

    bounds = [(-4, 4)] * (2 * N)
    bounds.append((-1.5, 1.5))  # Mando Bounds
    bounds.append((-1.5, 1.5))  # Mu Bounds
    bounds.append((-0.9, 0.9))  # Rho Bounds

    resultado = minimize(
        funcao_e_gradiente, 
        x0, 
        method="L-BFGS-B", 
        jac=True, 
        bounds=bounds, 
        options={'maxiter': 400, 'ftol': 1e-5}
    )

    if not resultado.success and parametros_anteriores is not None:
        if "maxiter" not in resultado.message.lower():
            return parametros_anteriores

    x_otimo = resultado.x
    return {
        "times": TODOS_TIMES,
        "ataque": x_otimo[:N],
        "defesa": x_otimo[N:2*N],
        "mando": x_otimo[IDX_MANDO],
        "mu": x_otimo[IDX_MU],
        "rho": x_otimo[IDX_RHO]
    }

# =====================================================
# LOOP TEMPORAL CRONOLÓGICO
# =====================================================
PRIMEIRA_DATA_HISTORICO = df["data"].min()
DATA_MINIMA_PARA_PREVER = PRIMEIRA_DATA_HISTORICO + timedelta(days=JANELA_DIAS)

datas = sorted(df["data"].unique())
total_datas = len(datas)

previsoes = []
forcas_atuais = None
linhas_com_problema = []

for i, data_atual in enumerate(datas):

    if pd.isna(data_atual):
        linhas_com_problema.append({"passo": i + 1, "motivo": "Data Nula/NaN"})
        continue

    if data_atual < DATA_MINIMA_PARA_PREVER:
        continue

    data_formatada = pd.Timestamp(data_atual).strftime('%d/%m/%Y')
    print(f"Progresso: [{i+1}/{total_datas}] - Processando dia: {data_formatada}...", end="\r")

    # Filtra janela histórica
    inicio = data_atual - timedelta(days=JANELA_DIAS)
    df_janela = df[(df["data"] >= inicio) & (df["data"] < data_atual)]

    if len(df_janela) < 50:
        continue

    # Calibra forças enviando a data de referência para o peso temporal
    forcas_atuais = calcular_forcas(df_janela, data_referencia=data_atual, parametros_anteriores=forcas_atuais)

    ataques_modelo = forcas_atuais["ataque"]
    defesas_modelo = forcas_atuais["defesa"]
    mando_modelo = forcas_atuais["mando"]
    mu_modelo = forcas_atuais["mu"]
    rho_modelo = forcas_atuais["rho"]

    jogos = df[df["data"] == data_atual]

    for _, jogo in jogos.iterrows():
        mandante = jogo["mandante"]
        visitor = jogo["visitante"]

        idx_casa = MAPA_TIMES_GLOBAL[mandante]
        idx_vis = MAPA_TIMES_GLOBAL[visitor]

        atq_casa_puro = ataques_modelo[idx_casa]
        def_casa_puro = defesas_modelo[idx_casa]
        atq_fora_puro = ataques_modelo[idx_vis]
        def_fora_puro = defesas_modelo[idx_vis]

        log_lambda_casa = mu_modelo + atq_casa_puro - def_fora_puro + mando_modelo
        log_lambda_fora = mu_modelo + atq_fora_puro - def_casa_puro

        lambda_casa = np.exp(log_lambda_casa)
        lambda_fora = np.exp(log_lambda_fora)

        # Matriz Dixon-Coles aplicando o tau ajustado aos placares baixos
        matriz = np.zeros((MAX_GOLS + 1, MAX_GOLS + 1))
        for gc in range(MAX_GOLS + 1):
            for gv in range(MAX_GOLS + 1):
                prob_base = poisson.pmf(gc, lambda_casa) * poisson.pmf(gv, lambda_fora)
                
                tau = 1.0
                if gc == 0 and gv == 0:
                    tau = 1.0 - lambda_casa * lambda_fora * rho_modelo
                elif gc == 0 and gv == 1:
                    tau = 1.0 + lambda_casa * rho_modelo
                elif gc == 1 and gv == 0:
                    tau = 1.0 + lambda_fora * rho_modelo
                elif gc == 1 and gv == 1:
                    tau = 1.0 - rho_modelo
                
                tau = max(tau, 1e-10)
                matriz[gc, gv] = prob_base * tau

        soma_matriz = matriz.sum()
        if not np.isfinite(soma_matriz) or soma_matriz <= 0:
            continue
        matriz /= soma_matriz

        prob_casa = 0.0
        prob_empate = 0.0
        prob_fora = 0.0

        for gc in range(MAX_GOLS + 1):
            for gv in range(MAX_GOLS + 1):
                p = matriz[gc, gv]
                if gc > gv:
                    prob_casa += p
                elif gc == gv:
                    prob_empate += p
                else:
                    prob_fora += p

        previsoes.append({
            "data": data_atual,
            "time_casa": mandante,
            "time_vis": visitor,
            "prob_casa": round(prob_casa, 4),
            "prob_empate": round(prob_empate, 4),
            "prob_fora": round(prob_fora, 4),
            "lambda_casa": round(lambda_casa, 2),
            "lambda_fora": round(lambda_fora, 2),
            "atq_casa": round(atq_casa_puro, 3),
            "def_casa": round(def_casa_puro, 3),
            "atq_fora": round(atq_fora_puro, 3),
            "def_fora": round(def_fora_puro, 3),
            "fator_mando": round(mando_modelo, 3),
            "rho_dixon_coles": round(rho_modelo, 4),
            "gols_reais_casa": jogo["mandante_Placar"],
            "gols_reais_fora": jogo["visitante_Placar"]
        })

print("\nProcessamento concluído com sucesso!")

# =====================================================
# SALVAMENTO DO ARQUIVO FINAL
# =====================================================
if previsoes:
    df_out = pd.DataFrame(previsoes)
    df_out.to_csv("probabilidades_jogos_dixon_coles.csv", index=False)
    print("Sucesso - Planilha gerada com total de jogos calculados:", len(df_out))
else:
    print("Erro - Nenhuma previsão pôde ser gerada.") 

if linhas_com_problema:
    print(f"\n[AVISO] Encontrados {len(linhas_com_problema)} problemas durante a execução:")
    for erro in linhas_com_problema:
        print(f" - Passo {erro['passo']}: {erro['motivo']}")
else:
    print("0 erros de dados encontrados na base!")
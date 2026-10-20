import pandas as pd

import numpy as np

from scipy.stats import poisson

from datetime import timedelta

from scipy.optimize import minimize


# =====================================================

# CONFIGURAÇÕES INICIAIS

# =====================================================

JANELA_DIAS = 730  # Janela de 2 anos de histórico para calcular as forças

MAX_GOLS = 10      # Limite máximo de gols para a matriz de probabilidade


# =====================================================

# TRATAMENTO DE DADOS E MAPEAMENTO GLOBAL

# =====================================================

df = pd.read_csv("serie_A.csv")


# Remove qualquer linha com data ou placar nulo/vazio para evitar quebras catastróficas

df = df.dropna(subset=["data", "mandante_Placar", "visitante_Placar"])


# Padroniza o nome dos times (letras minúsculas e sem espaços extras)

df["mandante"] = df["mandante"].str.lower().str.strip()

df["visitante"] = df["visitante"].str.lower().str.strip()


# Converte e ordena os jogos por data cronológica

df["data"] = pd.to_datetime(df["data"], dayfirst=True)

df = df.sort_values("data").reset_index(drop=True)


# Mapeia todos os times únicos para garantir consistência de índices na otimização

TODOS_TIMES = sorted(set(df["mandante"]).union(set(df["visitante"])))

TOTAL_TIMES_BANCO = len(TODOS_TIMES)

MAPA_TIMES_GLOBAL = {t: i for i, t in enumerate(TODOS_TIMES)}

# =====================================================

# FUNÇÃO: FORÇA DOS TIMES (CÁLCULO VIA GRADIENTE ANALÍTICO)

# =====================================================

def calcular_forcas(df_janela, parametros_anteriores=None):

    N = TOTAL_TIMES_BANCO

    mapa_times = MAPA_TIMES_GLOBAL

    m = len(df_janela)


    # Conta quantos jogos cada time jogou na janela atual

    jogos_mandante = df_janela["mandante"].value_counts()

    jogos_visitante = df_janela["visitante"].value_counts()

   

    n_i = np.zeros(N)

    for t, idx in mapa_times.items():

        n_i[idx] = jogos_mandante.get(t, 0) + jogos_visitante.get(t, 0)


    # Define o chute inicial dos parâmetros (warm-start se houver parâmetro anterior)

    if parametros_anteriores is None:

        gols_totais = df_janela["mandante_Placar"].sum() + df_janela["visitante_Placar"].sum()

        media_gols = gols_totais / (2 * m) if m > 0 else 1.0

       

        mu_inicial = np.log(media_gols)

        ataque_inicial = np.zeros(N)

        defesa_inicial = np.zeros(N)

        mando_inicial = 0.2

    else:

        ataque_inicial = parametros_anteriores["ataque"]

        defesa_inicial = parametros_anteriores["defesa"]

        mando_inicial = parametros_anteriores["mando"]

        mu_inicial = parametros_anteriores["mu"]


    x0 = np.concatenate([ataque_inicial, defesa_inicial, [mando_inicial], [mu_inicial]])


    # Hiperparâmetros de Shrinkage Bayesiano (Suavização para times com poucos jogos)

    k = 30

    pesos_lambda = k / (k + n_i)


    # Prior dinâmico para times promovidos ou sem jogos recentes

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


    BETA = 50  # Peso da trava de simetria (Identificabilidade)


    # -----------------------------------------------------

    # FUNÇÃO OBJETIVO E GRADIENTE UNIFICADOS (L-BFGS-B SIMULTÂNEO)

    # -----------------------------------------------------

    def funcao_e_gradiente(x):

        ataque = x[:N]

        defesa = x[N:2*N]

        mando = x[2*N]

        mu = x[2*N+1]


        grad = np.zeros_like(x)

        grad_ataque = grad[:N]

        grad_defesa = grad[N:2*N]

        grad_mando = 0.0

        grad_mu = 0.0


        log_likelihood = 0.0


        # Loop de jogos calculando Verossimilhança + Derivadas Analíticas

        for _, jogo in df_janela.iterrows():

            i = mapa_times[jogo["mandante"]]

            j = mapa_times[jogo["visitante"]]


            gols_casa = jogo["mandante_Placar"]

            gols_fora = jogo["visitante_Placar"]


            lambda_casa = np.exp(mu + ataque[i] - defesa[j] + mando)

            lambda_fora = np.exp(mu + ataque[j] - defesa[i])


            log_likelihood += poisson.logpmf(gols_casa, lambda_casa) + poisson.logpmf(gols_fora, lambda_fora)


            # Resíduo: Gols Reais - Gols Esperados (Derivada exata de Poisson)

            erro_casa = gols_casa - lambda_casa

            erro_fora = gols_fora - lambda_fora


            grad_ataque[i] += erro_casa

            grad_defesa[j] -= erro_casa


            grad_ataque[j] += erro_fora

            grad_defesa[i] -= erro_fora


            grad_mando += erro_casa

            grad_mu += (erro_casa + erro_fora)


        grad[2*N] = grad_mando

        grad[2*N+1] = grad_mu


        # Normalização por número de jogos

        log_likelihood_norm = log_likelihood / m

        grad /= m  


        # Aplicação das penalidades de Shrinkage

        diff_atq = ataque - ataque_prior

        diff_def = defesa - defesa_prior

        penalizacao_shrinkage_norm = np.sum(pesos_lambda * diff_atq**2 + pesos_lambda * diff_def**2) / N

       

        grad_ataque -= (2 * pesos_lambda * diff_atq) / N

        grad_defesa -= (2 * pesos_lambda * diff_def) / N


        # Aplicação da punição de simetria (Média centrada em 0)

        mean_atq = np.mean(ataque)

        mean_def = np.mean(defesa)

        punicao_simetria = BETA * (mean_atq**2 + mean_def**2)

       

        grad_ataque -= (2 * BETA / N) * mean_atq

        grad_defesa -= (2 * BETA / N) * mean_def


        # Inversão para o otimizador (Minimizar o oposto da MaxVerossimilhança)

        obj_final = -(log_likelihood_norm - penalizacao_shrinkage_norm - punicao_simetria)

        grad_final = -grad


        if not np.isfinite(obj_final):

            return 1e10, np.zeros_like(x)


        return obj_final, grad_final


    bounds = [(-4, 4)] * (2 * N)

    bounds.append((-1.5, 1.5))  # Mando Bounds

    bounds.append((-1.5, 1.5))  # Mu Bounds


    resultado = minimize(funcao_e_gradiente, x0, method="L-BFGS-B", jac=True, bounds=bounds, options={'maxiter': 400, 'ftol': 1e-5})


    if not resultado.success and parametros_anteriores is not None:

        if "maxiter" not in resultado.message.lower():

            return parametros_anteriores


    x_otimo = resultado.x

    return {

        "times": TODOS_TIMES,

        "ataque": x_otimo[:N],

        "defesa": x_otimo[N:2*N],

        "mando": x_otimo[2*N],

        "mu": x_otimo[2*N+1]

    }


# =====================================================

# LOOP TEMPORAL CRONOLÓGICO (COM FILTRO REAL DE DATA)

# =====================================================

PRIMEIRA_DATA_HISTORICO = df["data"].min()

DATA_MINIMA_PARA_PREVER = PRIMEIRA_DATA_HISTORICO + timedelta(days=JANELA_DIAS)


datas = sorted(df["data"].unique())

total_datas = len(datas)


previsoes = []

forcas_atuais = None

linhas_com_problema = [] # Lista para registrar o que deu errado

for i, data_atual in enumerate(datas):

    # Se a data for nula, registra e pula
    if pd.isna(data_atual):
        linhas_com_problema.append({"passo": i + 1, "motivo": "Data Nula/NaN"})
        continue

    # TRAVA DE CRONOLOGIA: Impede previsões antes dos 730 dias
    if data_atual < DATA_MINIMA_PARA_PREVER:
        continue


    data_formatada = pd.Timestamp(data_atual).strftime('%d/%m/%Y')

    print(f"Progresso: [{i+1}/{total_datas}] - Processando dia: {data_formatada}...", end="\r")


    # Filtra os dados da janela móvel (somente o passado para calibrar o presente)

    inicio = data_atual - timedelta(days=JANELA_DIAS)

    df_janela = df[(df["data"] >= inicio) & (df["data"] < data_atual)]


    if len(df_janela) < 50:

        continue


    # Roda a calibração de forças para o dia de hoje

    forcas_atuais = calcular_forcas(df_janela, parametros_anteriores=forcas_atuais)


    ataques_modelo = forcas_atuais["ataque"]

    defesas_modelo = forcas_atuais["defesa"]

    mando_modelo = forcas_atuais["mando"]

    mu_modelo = forcas_atuais["mu"]


    # Coleta os jogos reais que vão acontecer HOJE para aplicar as probabilidades

    jogos = df[df["data"] == data_atual]


    for _, jogo in jogos.iterrows():

        mandante = jogo["mandante"]

        visitor = jogo["visitante"]


        idx_casa = MAPA_TIMES_GLOBAL[mandante]

        idx_vis = MAPA_TIMES_GLOBAL[visitor]


        # Extração de variáveis brutas

        atq_casa_puro = ataques_modelo[idx_casa]

        def_casa_puro = defesas_modelo[idx_casa]

        atq_fora_puro = ataques_modelo[idx_vis]

        def_fora_puro = defesas_modelo[idx_vis]


        log_lambda_casa = mu_modelo + atq_casa_puro - def_fora_puro + mando_modelo

        log_lambda_fora = mu_modelo + atq_fora_puro - def_casa_puro


        lambda_casa = np.clip(np.exp(log_lambda_casa), 0.05, 5)

        lambda_fora = np.clip(np.exp(log_lambda_fora), 0.05, 5)


        # Geração da matriz interna de distribuição Poisson

        matriz = np.zeros((MAX_GOLS + 1, MAX_GOLS + 1))

        for gc in range(MAX_GOLS + 1):

            for gv in range(MAX_GOLS + 1):

                matriz[gc, gv] = poisson.pmf(gc, lambda_casa) * poisson.pmf(gv, lambda_fora)


        soma_matriz = matriz.sum()

        if not np.isfinite(soma_matriz) or soma_matriz <= 0:

            continue

        matriz /= soma_matriz


        # Agrupamento da matriz nos mercados de 1X2 (Vitória, Empate, Derrota)

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


        # Aloca tudo em uma única linha ultra-informativa por partida

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

            "gols_reais_casa": jogo["mandante_Placar"],

            "gols_reais_fora": jogo["visitante_Placar"]

        })


print("\nProcessamento concluído com sucesso!")


# =====================================================

# SALVAMENTO DO ARQUIVO FINAL

# =====================================================

if previsoes:

    df_out = pd.DataFrame(previsoes)

    df_out.to_csv("probabilidades_jogos.csv", index=False)

    print("Sucesso - Planilha gerada com total de jogos calculados:", len(df_out))

else:

    print("Erro - Nenhuma previsão pôde ser gerada.") 

if linhas_com_problema:
    print(f"\n[AVISO] Encontrados {len(linhas_com_problema)} problemas durante a execução:")
    for erro in linhas_com_problema:
        print(f" - Passo {erro['passo']}: {erro['motivo']}")
else:
    print(" 0 erros de dados encontrados na base!")
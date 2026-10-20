import pandas as pd
import numpy as np
from sklearn.linear_model import PoissonRegressor
from scipy.stats import poisson

MAX_GOLS = 10

# 1. Leitura e tratamento inicial dos dados
df = pd.read_csv("serie_A.csv").dropna(subset=["data", "mandante_Placar", "visitante_Placar"])
df["mandante"] = df["mandante"].str.lower().str.strip()
df["visitante"] = df["visitante"].str.lower().str.strip()
df["data"] = pd.to_datetime(df["data"], dayfirst=True)
df = df[df["data"].dt.year == 2025].reset_index(drop=True)
df = df.sort_values("data").reset_index(drop=True)

TODOS_TIMES = sorted(set(df["mandante"]).union(set(df["visitante"])))
TAMANHO = len(TODOS_TIMES)
MAPA = {t: i for i, t in enumerate(TODOS_TIMES)}

df_n = df.head(340).copy()
df_f = df.tail(40).copy()

def preparar_dados_poisson(df_treino, todos_times):
    """
    Transforma cada partida em duas linhas para o PoissonRegressor do scikit-learn:
    1 para o time mandante, 1 para o time visitante.
    """
    X_rows = []
    y_gols = []
    
    N = len(todos_times)
    mapa = {t: i for i, t in enumerate(todos_times)}
    
    for _, row in df_treino.iterrows():
        m = row["mandante"]
        v = row["visitante"]
        gols_casa = row["mandante_Placar"]
        gols_fora = row["visitante_Placar"]
        
        idx_m = mapa[m]
        idx_v = mapa[v]
        
        # --- Linha para o Gols do Mandante ---
        # Features: [Ataque_Mandante, Defesa_Mandante, Ataque_Visitante, Defesa_Visitante, Mando]
        feat_casa = np.zeros(2 * N + 1)
        feat_casa[idx_m] = 1.0          # Ataque do mandante (+)
        feat_casa[N + idx_v] = -1.0     # Defesa do visitante (-)
        feat_casa[2 * N] = 1.0          # Fator mando
        
        X_rows.append(feat_casa)
        y_gols.append(gols_casa)
        
        # --- Linha para o Gols do Visitante ---
        feat_fora = np.zeros(2 * N + 1)
        feat_fora[idx_v] = 1.0          # Ataque do visitante (+)
        feat_fora[N + idx_m] = -1.0     # Defesa do mandante (-)
        feat_fora[2 * N] = 0.0          # Sem fator mando para o visitante
        
        X_rows.append(feat_fora)
        y_gols.append(gols_fora)
        
    return np.array(X_rows), np.array(y_gols)

def inferencia_sklearn_poisson(df_treino, todos_times):
    N = len(todos_times)
    X_treino, y_treino = preparar_dados_poisson(df_treino, todos_times)
    
    # Inicializa o regressor de Poisson do scikit-learn com regularização L2 (alpha)
    # alpha=0.01 substitui a penalização que antes era manual (-50/ -5)
    modelo_poisson = PoissonRegressor(alpha=0.01, max_iter=500)
    modelo_poisson.fit(X_treino, y_treino)
    
    coefs = modelo_poisson.coef_
    mu_intercepto = modelo_poisson.intercept_
    
    ataque = coefs[:N]
    defesa = coefs[N:2*N]
    mando = coefs[2*N]
    
    return {
        "times": todos_times,
        "ataque": ataque,
        "defesa": defesa,
        "mando": mando,
        "mu": mu_intercepto
    }

PRIMEIRA_DATA = df_f["data"].min()
datas = sorted(df_f["data"].unique())

previsoes = []
total_datas = len(datas)

print("Treinando modelo de Poisson com scikit-learn...")
forcas = inferencia_sklearn_poisson(df_n, TODOS_TIMES)

for i, data_atual in enumerate(datas):
    data_formatada = pd.Timestamp(data_atual).strftime('%d/%m/%Y')
    print(f"Progresso: [{i+1}/{total_datas}] - Processando dia: {data_formatada}...", end="\r")

    ataques_modelo = forcas["ataque"]
    defesas_modelo = forcas["defesa"]
    mando_modelo = forcas["mando"]
    mu_modelo = forcas["mu"]

    jogos = df[df["data"] == data_atual]

    for _, jogo in jogos.iterrows():
        mandante = jogo["mandante"]
        visitor = jogo["visitante"]

        idx_casa = MAPA[mandante]
        idx_vis = MAPA[visitor]

        atq_casa_puro = ataques_modelo[idx_casa]
        def_casa_puro = defesas_modelo[idx_casa]
        atq_fora_puro = ataques_modelo[idx_vis]
        def_fora_puro = defesas_modelo[idx_vis]

        # Como o PoissonRegressor usa a função de ligação log, a predição direta já considera o exponencial se ajustada, 
        # mas aqui calculamos de forma linear equivalente ao seu modelo original:
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

        # Agrupamento da matriz nos mercados de 1X2
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
            "gols_reais_casa": jogo["mandante_Placar"],
            "gols_reais_fora": jogo["visitante_Placar"]
        })

print("\nProcessamento concluído com sucesso!")

# =====================================================
# SALVAMENTO DO ARQUIVO FINAL
# =====================================================
if previsoes:
    df_out = pd.DataFrame(previsoes)
    df_out.to_csv("probabilidades_jogos_poisson_sklearn.csv", index=False)
    print("Sucesso - Planilha gerada com total de jogos calculados:", len(df_out))
else:
    print("Erro - Nenhuma previsão pôde ser gerada.")
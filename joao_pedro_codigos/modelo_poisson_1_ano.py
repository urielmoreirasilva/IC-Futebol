import pandas as pd
import numpy as np
from scipy.optimize import minimize
from scipy.stats import poisson
from datetime import timedelta 

MAX_GOLS=10

df=pd.read_csv("serie_A.csv").dropna(subset=["data", "mandante_Placar", "visitante_Placar"])
df["mandante"]=df["mandante"].str.lower().str.strip()
df["visitante"]=df["visitante"].str.lower().str.strip()
df["data"]=pd.to_datetime(df["data"], dayfirst=True)
df = df[df["data"].dt.year == 2025].reset_index(drop=True)
df=df.sort_values("data").reset_index(drop=True)

TODOS_TIMES=sorted(set(df["mandante"]).union(set(df["visitante"])))
TAMANHO=len(TODOS_TIMES)
MAPA={t:i for i, t in enumerate(TODOS_TIMES)}

df_n = df.head(340).copy()

df_f=df.tail(40).copy()

def inferencia(df_n):
    N=TAMANHO
    x0=np.zeros(2*N+2)

    def funcao_e_gradiente(x):
        ataque=x[:N]
        defesa=x[N:2*N]
        mando=x[2*N]
        mu=x[2*N+1]

        grad=np.zeros_like(x)
        grad_ataque=grad[:N]
        grad_defesa=grad[N:2*N]
        grad_mando=0.0
        grad_mu=0.0
        log_likelihood=0.0
        for _, jogo in df_n.iterrows():
            i = MAPA[jogo["mandante"]]
            j = MAPA[jogo["visitante"]]
            gols_casa=jogo["mandante_Placar"]
            gols_fora=jogo["visitante_Placar"]

            lambda_casa= np.exp(mu+ataque[i]-defesa[j]+mando)
            lambda_fora= np.exp(mu+ataque[j]-defesa[i])
            
            log_likelihood += poisson.logpmf(gols_casa,lambda_casa) + poisson.logpmf(gols_fora,lambda_fora)

            erro_casa= gols_casa - lambda_casa
            erro_fora= gols_fora - lambda_fora

            grad_ataque[i]+=erro_casa
            grad_defesa[j]-=erro_fora

            grad_defesa[j] -= erro_casa
            grad_defesa[i] -= erro_fora

            grad_mando += erro_casa
            grad_mu += (erro_casa+erro_fora)


        log_likelihood=log_likelihood-50*(np.mean(ataque)**2+np.mean(defesa)**2)
        grad_ataque -=5*(np.mean(ataque))
        grad_defesa -=5*(np.mean(defesa))

        grad[2*N], grad[2*N+1] = grad_mando, grad_mu
        log_likelihood=-log_likelihood
        grad=-grad
        return log_likelihood, grad


    bounds = [(-4, 4)] * (2 * N) + [(-1.5, 1.5), (-1.5, 1.5)]
    resultado = minimize(funcao_e_gradiente, x0, method="L-BFGS-B", jac=True, bounds=bounds, options={'maxiter': 400, 'ftol': 1e-5})


    x_otimo = resultado.x

    return {

        "times": TODOS_TIMES,

        "ataque": x_otimo[:N],

        "defesa": x_otimo[N:2*N],

        "mando": x_otimo[2*N],

        "mu": x_otimo[2*N+1]

    }

PRIMEIRA_DATA = df_f["data"].min()
datas=sorted(df_f["data"].unique())

previsoes=[]
    
total_datas=len(datas)


forcas=inferencia(df_n)
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

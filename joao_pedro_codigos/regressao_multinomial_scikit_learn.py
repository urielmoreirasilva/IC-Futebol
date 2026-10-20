import pandas as pd
import numpy as np
from sklearn.linear_model import LogisticRegression

# 1. Leitura e tratamento inicial dos dados
df_full = pd.read_csv("serie_A.csv").dropna(subset=["data", "mandante_Placar", "visitante_Placar"])
df_full["mandante"] = df_full["mandante"].str.lower().str.strip()
df_full["visitante"] = df_full["visitante"].str.lower().str.strip()
df_full["data"] = pd.to_datetime(df_full["data"], dayfirst=True)
df_full["ano"] = df_full["data"].dt.year

def criar_features_partidas(df_treino, todos_times):
    """
    Constrói a matriz de design (X) e o vetor de alvos (y) onde cada linha 
    representa uma partida e as colunas representam indicadores binários 
    para o ataque e a defesa de cada time, além do fator mandante.
    """
    X_rows = []
    y_target = []
    
    mapa_times = {t: i for i, t in enumerate(todos_times)}
    N = len(todos_times)
    
    for _, row in df_treino.iterrows():
        m = row["mandante"]
        v = row["visitante"]
        gc = row["mandante_Placar"]
        gv = row["visitante_Placar"]
        
        # Determina o resultado (0: Fora, 1: Empate, 2: Casa)
        if gc > gv:
            alvo = 2  # Vitória Casa
        elif gc == gv:
            alvo = 1  # Empate
        else:
            alvo = 0  # Vitória Fora
            
        # Vetor de features para a partida:
        # [Ataque_Mandante, Defesa_Mandante, Ataque_Visitante, Defesa_Visitante, Fator_Casa]
        feat = np.zeros(2 * N + 1)
        
        idx_m = mapa_times[m]
        idx_v = mapa_times[v]
        
        # Time mandante usa seu ataque e sua defesa
        feat[idx_m] = 1.0                # Ataque Mandante
        feat[N + idx_v] = -1.0           # Defesa Visitante (afeta negativamente o mandante)
        
        # Fator mando de campo (intercepto específico para o mandante)
        feat[2 * N] = 1.0                
        
        X_rows.append(feat)
        y_target.append(alvo)
        
    return np.array(X_rows), np.array(y_target)

# ==============================================================================
# 2. LOOP POR ANOS (2003 A 2025)
# ==============================================================================
todas_previsoes = []
anos_disponiveis = sorted([ano for ano in df_full["ano"].unique() if 2003 <= ano <= 2025])

for ano in anos_disponiveis:
    df_ano = df_full[df_full["ano"] == ano].sort_values("data").reset_index(drop=True)
    
    if len(df_ano) <= 40:
        continue

    todos_times = sorted(set(df_ano["mandante"]).union(set(df_ano["visitante"])))
    
    df_n = df_ano.iloc[:-40].copy()
    df_f = df_ano.iloc[-40:].copy()

    print(f"Treinando ano {ano} com scikit-learn (Treino: {len(df_n)} jogos | Teste: {len(df_f)} jogos)...")
    
    # Monta matrizes de treino
    X_treino, y_treino = criar_features_partidas(df_n, todos_times)
    
    # Inicializa a Regressão Logística Multinomial com penalização L2 (equivalente ao peso no seu código anterior)
    # C=0.02 corresponde aproximadamente a uma regularização forte equivalente ao peso 50.0
    modelo = LogisticRegression(multi_class='multinomial', solver='lbfgs', C=0.02, max_iter=500)
    modelo.fit(X_treino, y_treino)
    
    # Previsões no conjunto de teste
    mapa = {t: i for i, t in enumerate(todos_times)}
    N = len(todos_times)
    
    for _, jogo in df_f.iterrows():
        mandante = jogo["mandante"]
        visitor = jogo["visitante"]

        # Constrói o vetor de features para o jogo de teste
        feat_teste = np.zeros((1, 2 * N + 1))
        idx_m = mapa[mandante]
        idx_v = mapa[visitor]
        
        feat_teste[0, idx_m] = 1.0
        feat_teste[0, N + idx_v] = -1.0
        feat_teste[0, 2 * N] = 1.0
        
        # Obtém as probabilidades preditas [P(Fora), P(Empate), P(Casa)]
        probs = modelo.predict_proba(feat_teste)[0]
        
        prob_fora = probs[0]
        prob_empate = probs[1]
        prob_casa = probs[2]

        todas_previsoes.append({
            "ano": ano,
            "data": jogo["data"],
            "mandante": mandante,
            "visitante": visitor,
            "prob_casa": round(float(prob_casa), 4),
            "prob_empate": round(float(prob_empate), 4),
            "prob_fora": round(float(prob_fora), 4),
            "gols_reais_casa": jogo["mandante_Placar"],
            "gols_reais_fora": jogo["visitante_Placar"]
        })

# ==============================================================================
# 3. CONSOLIDAÇÃO E SALVAMENTO
# ==============================================================================
df_saida = pd.DataFrame(todas_previsoes)
df_saida.to_csv("probabilidades_multinomial_sklearn_2003_2025.csv", index=False)
print(f"\nSucesso: {len(df_saida)} partidas previstas entre 2003 e 2025 com scikit-learn!")
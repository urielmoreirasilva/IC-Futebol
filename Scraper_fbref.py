import os
import re
from io import StringIO
from bs4 import BeautifulSoup
import pandas as pd
from seleniumbase import SB

print("Escolha a liga")
print("1 - Brasileirão")
print("2 - Premier League")
print("3 - Ligue 1")
print("4 - La Liga")
print("5 - Bundesliga")
print("6 - Serie A")
print("7 - Liga AUF")

liga = int(input("Opção desejada: "))
ano = int(input("Ano desejado(2018-2026):"))
final = ano + 1

# Define a URL com base na liga selecionada
if liga == 1:
    url = f"https://fbref.com/en/comps/24/{ano}/schedule/{ano}-Serie-A-Scores-and-Fixtures"
    print(f"--- Acessando FBref: Brasileirão {ano} ---")
elif liga == 2:
    url = f"https://fbref.com/en/comps/9/{ano}-{final}/schedule/{ano}-{final}-Scores-and-Fixtures"
    print(f"--- Acessando FBref: Premier League {ano}/{final} ---")
elif liga == 3:
    url = f"https://fbref.com/en/comps/13/{ano}-{final}/schedule/{ano}-{final}-Ligue-1-Scores-and-Fixtures"
    print(f"--- Acessando FBref: Ligue 1 {ano}/{final} ---")
elif liga == 4:
    url = f"https://fbref.com/en/comps/12/{ano}-{final}/schedule/{ano}-{final}-La-Liga-Scores-and-Fixtures"
    print(f"--- Acessando FBref: La Liga {ano}/{final} ---")
elif liga == 5:
    url = f"https://fbref.com/en/comps/20/{ano}-{final}/schedule/{ano}-{final}-Bundesliga-Scores-and-Fixtures"
    print(f"--- Acessando FBref: Bundesliga {ano}/{final} ---")
elif liga == 6:
    url = f"https://fbref.com/en/comps/11/{ano}-{final}/schedule/{ano}-{final}-Serie-A-Scores-and-Fixtures"
    print(f"--- Acessando FBref: Serie A {ano}/{final} ---")
elif liga == 7:
    url = f"https://fbref.com/en/comps/45/{ano}/schedule/{ano}-Liga-AUF-Uruguaya-Scores-and-Fixtures"
    print(f"--- Acessando FBref: Liga AUF {ano} ---")

# Inicializa o SeleniumBase
with SB(uc=True) as sb:
    # 1. Abre a página
    sb.uc_open_with_reconnect(url, reconnect_time=5)
    sb.sleep(2)
    
    # 2. Se o Cloudflare travar na tela, tenta resolver o Turnstile
    try:
        print("Verificando se há desafio do Cloudflare...")
        sb.solve_captcha()
        sb.sleep(3)
    except Exception as e:
        print("Nenhum captcha detectado ou já resolvido.")

    # 3. Aguarda até a tabela aparecer (timeout de 35 segundos)
    try:
        sb.wait_for_element("table.stats_table", timeout=35)
        print("Tabela carregada com sucesso!")
    except Exception:
        # Tenta uma segunda resolução caso o Cloudflare tenha demorado
        print("Tentando resolver desafio de segurança novamente...")
        try:
            sb.solve_captcha()
            sb.sleep(4)
            sb.wait_for_element("table.stats_table", timeout=20)
        finally:
            pass
            

    # 4. Obtém o HTML após desbloqueio
    html = sb.get_page_source()

# --- Processamento do HTML com Pandas ---
soup = BeautifulSoup(html, "html.parser")
tabela = soup.find("table", class_="stats_table")

df = pd.read_html(StringIO(str(tabela)))[0]

# Limpeza das linhas extras do FBref
if "Wk" in df.columns:
    df = df[df["Wk"].notna()]
    df = df[df["Wk"].astype(str).str.isnumeric()]

# Seleciona as colunas úteis
colunas = [
    "Wk", "Date", "Time", "Home", "Score", "Away", 
    "xG", "xG.1", 
]
colunas_presentes = [c for c in colunas if c in df.columns]
df_final = df[colunas_presentes].copy()

df_final.rename(columns={
    "Wk": "Rodada",
    "Date": "Data",
    "Time": "Horario",
    "Home": "Mandante",
    "Score": "Placar",
    "Away": "Visitante",
    "xG": "xG_Mandante",
    "xG.1": "xG_Visitante",
}, inplace=True)

# Define o caminho e nome do arquivo CSV com base na liga selecionada
if liga == 1:
    caminho_csv = f"/home/ryan/Documents/brasileirao_{ano}_fbref.csv"
elif liga == 2:
    caminho_csv = f"/home/ryan/Documents/premier_league_{ano}_{final}_fbref.csv"
elif liga == 3:
    caminho_csv = f"/home/ryan/Documents/ligue1_{ano}_{final}_fbref.csv"
elif liga == 4:
    caminho_csv = f"/home/ryan/Documents/la_liga_{ano}_{final}_fbref.csv"
elif liga == 5:
    caminho_csv = f"/home/ryan/Documents/bundesliga_{ano}_{final}_fbref.csv"
elif liga == 6:
    caminho_csv = f"/home/ryan/Documents/serie_a_{ano}_{final}_fbref.csv"
elif liga == 7:
    caminho_csv = f"/home/ryan/Documents/liga_auf_{ano}_fbref.csv"

df_final.to_csv(caminho_csv, index=False, encoding="utf-8-sig")

print(f"\nConcluído com sucesso! {len(df_final)} partidas salvas em:")
print(caminho_csv)
print("\nExemplo das primeiras partidas:")
print(df_final[["Rodada", "Data", "Mandante", "Placar", "Visitante"]].head())
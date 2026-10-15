import os
import re
from io import StringIO
from bs4 import BeautifulSoup
import pandas as pd
from seleniumbase import SB

ano = "2025"
url = f"https://fbref.com/en/comps/24/{ano}/schedule/{ano}-Serie-A-Scores-and-Fixtures"

print(f"--- Acessando FBref: Brasileirão {ano} ---")

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
        except Exception:
            # Salva print da tela para você ver o que travou
            sb.save_screenshot("/home/ryan/Documents/erro_fbref.png")
            print("A tela travou. Foi salvo um print em 'erro_fbref.png'")
            raise

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

# Salva o arquivo CSV
caminho_csv = f"/home/ryan/Documents/brasileirao_{ano}_fbref.csv"
df_final.to_csv(caminho_csv, index=False, encoding="utf-8-sig")

print(f"\nConcluído com sucesso! {len(df_final)} partidas salvas em:")
print(caminho_csv)
print("\nExemplo das primeiras partidas:")
print(df_final[["Rodada", "Data", "Mandante", "Placar", "Visitante"]].head())
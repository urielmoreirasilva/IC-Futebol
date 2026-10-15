import os
import pandas as pd
from bs4 import BeautifulSoup
from curl_cffi import requests

URL = "https://fbref.com/en/comps/24/schedule/Serie-A-Scores-and-Fixtures"
ARQUIVO_CACHE = "brasileirao_fbref.csv"

if os.path.exists(ARQUIVO_CACHE):
    df = pd.read_csv(ARQUIVO_CACHE)
    print(f"Carregado do cache local ({ARQUIVO_CACHE}).")
else:
    session = requests.Session(impersonate="chrome120")
    
    headers = {
        "User-Agent": "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36",
        "Accept": "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8",
        "Accept-Language": "en-US,en;q=0.9,pt-BR;q=0.8,pt;q=0.7",
        "Sec-Ch-Ua": '"Not_A Brand";v="8", "Chromium";v="120", "Google Chrome";v="120"',
        "Sec-Ch-Ua-Mobile": "?0",
        "Sec-Ch-Ua-Platform": '"Linux"',
        "Sec-Fetch-Dest": "document",
        "Sec-Fetch-Mode": "navigate",
        "Sec-Fetch-Site": "none",
        "Sec-Fetch-User": "?1",
        "Upgrade-Insecure-Requests": "1",
    }

    response = session.get(URL, headers=headers)

    if response.status_code != 200:
        raise Exception(f"Erro ao acessar FBref: Status {response.status_code}")

    soup = BeautifulSoup(response.text, "html.parser")
    table = soup.find("table", {"id": lambda x: x and "sched" in x})

    jogos = []
    if table:
        tbody = table.find("tbody")
        for row in tbody.find_all("tr"):
            if row.get("class") and "thead" in row.get("class"):
                continue

            rodada_cell = row.find("th", {"data-stat": "gameweek"})
            data_cell = row.find("td", {"data-stat": "date"})
            home_cell = row.find("td", {"data-stat": "home_team"})
            away_cell = row.find("td", {"data-stat": "away_team"})
            score_cell = row.find("td", {"data-stat": "score"})

            if not home_cell or not away_cell:
                continue

            placar_raw = score_cell.text.strip() if score_cell else ""
            gols_home = None
            gols_away = None

            if "–" in placar_raw or "-" in placar_raw:
                sep = "–" if "–" in placar_raw else "-"
                partes = placar_raw.split(sep)
                try:
                    gols_home = int(partes[0].strip())
                    gols_away = int(partes[1].strip())
                except ValueError:
                    pass

            jogos.append({
                "rodada": rodada_cell.text.strip() if rodada_cell else None,
                "data": data_cell.text.strip() if data_cell else None,
                "mandante": home_cell.text.strip(),
                "gols_mandante": gols_home,
                "gols_visitante": gols_away,
                "visitante": away_cell.text.strip(),
                "placar": placar_raw if placar_raw else "A definir"
            })

    df = pd.DataFrame(jogos)
    if not df.empty:
        df.to_csv(ARQUIVO_CACHE, index=False)
        print(f"Sucesso! {len(df)} partidas salvas.")

print(df.head(10))

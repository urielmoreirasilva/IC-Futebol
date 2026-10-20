import pandas as pd
import time
import requests
from bs4 import BeautifulSoup
import re


# =====================================================
# FUNÇÃO PARA EXTRAIR UM JOGO
# =====================================================

def extrair_jogo(url):

    html = requests.get(url, timeout=10).text
    soup = BeautifulSoup(html, "html.parser")

    texto = soup.get_text(" ", strip=True)


    # =====================
    # Times e data
    # =====================

    titulo = soup.title.text

    jogo = titulo.split(" | ")[0]

    padrao = r"(.+) - (.+) (\d{4}-\d{2}-\d{2})"

    resultado = re.search(padrao, jogo)

    if resultado:
        mandante = resultado.group(1)
        visitante = resultado.group(2)
        data = resultado.group(3)

    else:
        mandante = None
        visitante = None
        data = None


    # =====================
    # Placar
    # =====================

    resultado_placar = re.search(
        r"The match ended with the result (\d+) : (\d+)",
        texto
    )

    if resultado_placar:

        gols_casa = int(resultado_placar.group(1))
        gols_fora = int(resultado_placar.group(2))

    else:

        gols_casa = None
        gols_fora = None



    # =====================
    # Best odds
    # =====================

    odd_casa = None
    odd_empate = None
    odd_fora = None


    for tabela in soup.find_all("table"):

        for linha in tabela.find_all("tr"):

            linha_texto = linha.get_text(" ", strip=True)

            if linha_texto.startswith("Best odds"):

                partes = linha_texto.split()

                if len(partes) >= 5:

                    odd_casa = float(partes[2])
                    odd_empate = float(partes[3])
                    odd_fora = float(partes[4])

                break
    return {

        "data": data,
        "mandante": mandante,
        "visitante": visitante,

        "gols_casa": gols_casa,
        "gols_fora": gols_fora,

        "odd_casa": odd_casa,
        "odd_empate": odd_empate,
        "odd_fora": odd_fora
    }



# =====================================================
# PEGAR LINKS DOS JOGOS DE 2009
# =====================================================

url_ano = "https://checkbestodds.com/football-odds/archive-brazil/2009"


html = requests.get(url_ano).text

soup = BeautifulSoup(html, "html.parser")


links = []


for link in soup.find_all("a"):

    href = link.get("href")

    if href and href.startswith("/football-odds/brazil/"):

        links.append(
            "https://checkbestodds.com" + href
        )


# remover duplicados
links = list(set(links))


print("Quantidade de links encontrados:", len(links))



# =====================================================
# TESTAR 10 JOGOS
# =====================================================

dados = []


# =====================================================
# EXTRAIR TODOS OS JOGOS
# =====================================================

dados = []


for i, link in enumerate(links):

    print(f"Extraindo {i+1}/{len(links)}")

    try:

        jogo = extrair_jogo(link)

        dados.append(jogo)


    except Exception as e:

        print("\nERRO NO JOGO:")
        print(link)
        print(type(e).__name__)
        print(e)


    # evita muitas requisições seguidas
    time.sleep(1)



# =====================================================
# SALVAR CSV
# =====================================================

df = pd.DataFrame(dados)


df.to_csv(
    "checkbestodds_brasileirao_2009.csv",
    index=False
)


print("\n====================")
print("Finalizado!")
print("Jogos extraídos:", len(df))
print("====================")


print(df.head())



# =====================================================
# TRANSFORMAR EM DATAFRAME
# =====================================================

df = pd.DataFrame(dados)

df.to_csv(
    "checkbestodds_brasileirao_2009.csv",
    index=False
)

print(df)
import pandas as pd

# Arquivo direto de resultados do Brasileirão da temporada
url_csv = "https://www.football-data.co.uk/new/BRA.csv"

df_bra = pd.read_csv(url_csv)

# Filtrando colunas essenciais
colunas = ['Date', 'Home', 'Away', 'HG', 'AG', 'Res', 'MaxCH', 'MaxCD','MaxCA']
df_jogos = df_bra[colunas].dropna()
print(df_jogos.tail(15))
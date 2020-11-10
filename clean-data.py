import pandas as pd


ccL = pd.read_csv("data/Countries-Continents.csv").values.tolist()

country_continent = { ccL[i][1].lower() : ccL[i][0] for i in range(len(ccL))}


csv2015 = pd.read_csv("data/2015.csv").values.tolist()
dict2015 = {entry[0].lower() : entry for entry in csv2015}

csv2016 = pd.read_csv("data/2016.csv").values.tolist()
dict2016 = {entry[0].lower() : entry for entry in csv2016}

csv2017 = pd.read_csv("data/2017.csv").values.tolist()
dict2017 = {entry[0].lower() : entry for entry in csv2017}

csv2018 = pd.read_csv("data/2018.csv").values.tolist()
dict2018 = {entry[1].lower() : entry for entry in csv2018}

csv2019 = pd.read_csv("data/2019.csv").values.tolist()
dict2019 = {entry[1].lower() : entry for entry in csv2019}

header = ["country", "region", "rank", "year", "happiness-score", "GDP", "social", "health", "freedom", "trust", "generosity"]

# The final dataframe
df = pd.DataFrame(columns=header)

# Using 2017 because it has the least amount of countries

for i in range(len(csv2017)):

    # Country
    c = csv2017[i][0].lower()

    #2015
    df.loc[5*i] = [c, country_continent[c], dict2015[c][2], "2015", dict2015[c][3], dict2015[c][5]] + dict2015[c][6:11]
    #2016
    df.loc[5*i+1] = [c, country_continent[c], dict2016[c][2], "2016", dict2016[c][3], dict2016[c][6]] + dict2016[c][7:12]
    #2017
    df.loc[5*i+2] = [c, country_continent[c], dict2017[c][1], "2017", dict2017[c][2], dict2017[c][5],
                    dict2017[c][6], dict2017[c][7], dict2017[c][8], dict2017[c][10], dict2017[c][9]]
    #2018
    df.loc[5*i+3] = [c, country_continent[c], dict2018[c][0], "2018"] + dict2018[c][2:7] + [dict2018[c][8] , dict2018[c][7]]
    #2019
    df.loc[5*i+4] = [c, country_continent[c], dict2019[c][0], "2019"] + dict2019[c][2:7] + [dict2019[c][8] , dict2019[c][7]]

print(df)
df.to_csv('final.csv', index=False)
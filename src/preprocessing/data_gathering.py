import os
import json
import datetime as dt

import requests
import pandas as pd


periodo_map = {
    2014: 0,
    # 2015: 4,
    # 2016: 8,
    # 2017: 12,
    # 2018: 16
}

base_url = "https://www.medellin.gov.co/mapas/rest/services/ServiciosMovilidad/Accidentes/MapServer/{periodo}/query?where=MES%3D'{mes}'&outFields=*&outSR=4326&f=json"


if __name__ == "__main__":

    data_list = []

    for year in periodo_map.keys():
        print(f"Requesting data for year {year}")

        for month in range(1, 3):
            mes = str(month).zfill(2)
            
            print(f"Requesting data for month {mes}")
            request = requests.get(
                url=base_url.format(periodo=periodo_map[year], mes=mes)
            )

            data = request.json()
            
            features = list(map(lambda x: x['attributes'], data['features']))
            data_list.append(pd.DataFrame(features))

    df = pd.concat(data_list)
    df.to_csv('../../data/raw/prueba.csv', index=False, encoding='utf-8')




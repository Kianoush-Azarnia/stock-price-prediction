import pandas as pd
from numpy import array
import os

pd.set_option('display.max_columns', None)
pd.set_option('display.max_rows', None)

dirname = os.path.dirname(__file__)
relative_path = os.path.join(dirname, 'Trades')

data_frames = []
data_columns = [
    'market_value', 'first_price', 'persian_symbol', 'trade_date', 'persian_name',
    'final_price', 'last_trade_price', 'number_of_trades', 'volume', 'value',
    'min_price', 'max_price', 'yesterday_price', 'change'
]

for filename in os.listdir('Trades'):
    lst = pd.read_html(relative_path + '\\' + filename, encoding='utf-8')
    # print(array(lst[0]).shape)
    df = pd.DataFrame(lst[0])
    df.columns = data_columns
    df['trade_date'] = df['trade_date'].fillna(filename[0:9])
    df['pd_trade_date'] = pd.to_datetime(df['trade_date'], format='%Y%m%d')
    data_frames.append(df)
    print(len(data_frames))

stock_dataset = pd.concat(data_frames, ignore_index=True)
stock_dataset.to_csv('trades_dataset.csv', encoding='utf-8-sig', index_label='trade_id')

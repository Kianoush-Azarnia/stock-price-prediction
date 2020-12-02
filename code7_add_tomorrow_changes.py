import numpy as np
import pandas as pd
import os
import random
import copy

# Calculation of Relative Strength Index (RSI)
pd.set_option('display.max_columns', None)
pd.set_option('display.max_rows', None)

dirname = os.path.abspath('')
data_path = os.path.join(dirname, 'Data')

selected_stocks_df = pd.read_csv(data_path + '\\' + 'selected_stocks_with_indicators.csv', encoding='utf-8')

stocks = selected_stocks_df['persian_symbol'].unique().tolist()
# selected_stocks_df['next_change'] = 0
selected_stocks_df['next_price'] = 0

for stock in stocks:
    # selected_stocks_df.loc[selected_stocks_df['persian_symbol'] == stock, 'next_change'] = \
    #     selected_stocks_df.loc[selected_stocks_df['persian_symbol'] == stock, 'change'].shift(-1)
    selected_stocks_df.loc[selected_stocks_df['persian_symbol'] == stock, 'next_price'] = \
        selected_stocks_df.loc[selected_stocks_df['persian_symbol'] == stock, 'final_price'].shift(-1)

# selected_stocks_df.to_csv(data_path + '\\' + 'stocks_with_next_changes.csv', encoding='utf-8-sig', index=False)
selected_stocks_df.to_csv(data_path + '\\' + 'sel_stocks_next_prices.csv', encoding='utf-8-sig', index=False)
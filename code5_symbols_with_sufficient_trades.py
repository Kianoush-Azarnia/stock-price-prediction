import pandas as pd
import os

pd.set_option('display.max_columns', None)
pd.set_option('display.max_rows', None)

dirname = os.path.dirname(__file__)
data_path = os.path.join(dirname, 'Data')

per_symbol_counts_df = pd.read_csv(data_path + '\\' + 'per_symbol_counts.csv')

# print(per_symbol_counts_df.head())

required_trades_number = 600
symbols_with_sufficient_trades = \
    per_symbol_counts_df[per_symbol_counts_df['counts'] >= required_trades_number]['persian_symbol']

print(symbols_with_sufficient_trades)

symbols_with_sufficient_trades.\
    to_csv(data_path + '\\' + 'symbols_with_sufficient_trades.csv', encoding='utf-8-sig', index=False)

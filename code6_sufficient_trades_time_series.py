import pandas as pd
import os

pd.set_option('display.max_columns', None)
pd.set_option('display.max_rows', None)

dirname = os.path.dirname(__file__)
data_path = os.path.join(dirname, 'Data')

symbols_sufficient_df = pd.read_csv(data_path + '\\' + 'symbols_with_sufficient_trades.csv')
trades_df = pd.read_csv(data_path + '\\' + 'trades_symbol_data_join_on_persian_symbol.csv', encoding='utf-8')

sufficient_trades_ts_df = trades_df[trades_df['persian_symbol'].isin(symbols_sufficient_df['persian_symbol'])]
sufficient_trades_ts_df = sufficient_trades_ts_df[[
    'persian_symbol', 'pd_trade_date', 'market_value', 'first_price', 'final_price', 'last_trade_price',
    'number_of_trades', 'volume', 'value', 'min_price', 'max_price', 'yesterday_price', 'change',
]]

print(sufficient_trades_ts_df.head())

sufficient_trades_ts_df.to_csv(
    data_path + '\\' + 'sufficient_trades_time_series.csv', encoding='utf-8-sig', index=False)

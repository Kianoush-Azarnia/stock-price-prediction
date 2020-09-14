import pandas as pd
import os

pd.set_option('display.max_columns', None)
pd.set_option('display.max_rows', None)

dirname = os.path.dirname(__file__)
data_path = os.path.join(dirname, 'Data')

symbol_df = pd.read_excel(data_path + '\\' + 'cleaned_symbol.xlsx')
persian_symbol_code_map = symbol_df[['symbol_code', 'persian_symbol']]
trades_df = pd.read_csv(data_path + '\\' + 'trades_dataset.csv', encoding='utf-8')

trades_df_with_symbol_code = pd.merge(trades_df, persian_symbol_code_map, on='persian_symbol', how='left')
# print(trades_df_with_symbol_code.tail)
# print(trades_df_with_symbol_code.isna().sum())

persian_symbols = trades_df['persian_symbol'].unique()
print("number of unique persian symbols: {}".format(len(persian_symbols)))

persian_symbols_without_symbol_code = trades_df_with_symbol_code[
    trades_df_with_symbol_code['symbol_code'].isnull()]['persian_symbol'].unique()
print("number of persian symbols with no symbol code: {}".format(persian_symbols_without_symbol_code.shape))

as_df = trades_df[(trades_df['persian_symbol'] == 'وپارس') | (trades_df['persian_symbol'] == 'وپارس2')]
print(as_df.shape)

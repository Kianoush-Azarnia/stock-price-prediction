import pandas as pd
import os

pd.set_option('display.max_columns', None)
pd.set_option('display.max_rows', None)

dirname = os.path.dirname(__file__)
data_path = os.path.join(dirname, 'Data')

symbol_df = pd.read_excel(data_path + '\\' + 'cleaned_symbol.xlsx')
symbol_df_useful_info = \
    symbol_df[['symbol_code', 'persian_symbol', 'group', 'industrial_group', 'sign', 'symbol', 'name']]
trades_df = pd.read_csv(data_path + '\\' + 'trades_dataset.csv', encoding='utf-8')

trades_df_with_symbol_code = pd.merge(trades_df, symbol_df_useful_info, on='persian_symbol', how='right')
# print(trades_df_with_symbol_code.tail)
print(trades_df_with_symbol_code.isna().sum())

persian_symbols = symbol_df['persian_symbol'].unique()
print("number of unique persian symbols: {}".format(len(persian_symbols)))

persians = symbol_df['persian_name'].unique()
print("number of persian names: {}".format(len(persians)))

persians_without_symbol_code = trades_df_with_symbol_code[
    trades_df_with_symbol_code['symbol_code'].isnull()]['persian_symbol'].unique()
print("number of persian names with no symbol code: {}".format(persians_without_symbol_code.shape))

cleaned_trades_df_with_symbol_code = trades_df_with_symbol_code[trades_df_with_symbol_code['trade_id'].notnull()]
print(cleaned_trades_df_with_symbol_code.isna().sum())

# cleaned_trades_df_with_symbol_code.to_csv(
#     data_path + '\\' + 'trades_symbol_data_join_on_persian_symbol.csv', encoding='utf-8-sig', index=False)

per_symbol_counts = cleaned_trades_df_with_symbol_code.groupby(['persian_symbol']).size().reset_index(name='counts')
per_symbol_counts.to_csv(data_path + '\\' + 'per_symbol_counts.csv', encoding='utf-8-sig', index=False)

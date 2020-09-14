import pandas as pd
from datetime import date, timedelta


def get_dates_from_start_date_to_today(start_date):
    end_date = date.today()
    idx = pd.date_range(start_date, end_date-timedelta(days=1), freq='d')

    dates_in_range = []
    for val in idx.values:
        day = pd.to_datetime(val).date().strftime('%Y%m%d')
        dates_in_range.append(day)

    return dates_in_range


if __name__ == '__main__':
    print(get_dates_from_start_date_to_today(date(2018, 1, 1)))

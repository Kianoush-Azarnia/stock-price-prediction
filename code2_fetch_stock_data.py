import os
import requests
from datetime import date
from code1_get_dates import get_dates_from_start_date_to_today

last_3_years = get_dates_from_start_date_to_today(date(2018, 1, 1))


def request_data(url, file_path):
    r = requests.get(url, stream=True)
    if r.ok:
        print("saving to", os.path.abspath(file_path))
        with open(file_path, 'wb') as f:
            for chunk in r.iter_content(chunk_size=1024 * 8):
                if chunk:
                    f.write(chunk)
                    f.flush()
                    os.fsync(f.fileno())
    else:  # HTTP status code 4XX/5XX
        print("Download failed: status code {}\n{}".format(r.status_code, r.text))


day_less_than_2KB = dict()


def download(url: str, folder_name, filename):
    dirname = os.path.dirname(__file__)
    dest_folder = os.path.join(dirname, folder_name)

    if not os.path.exists(dest_folder):
        os.makedirs(dest_folder)  # create folder if it does not exist

    file_path = os.path.join(dest_folder, filename)

    request_data(url, file_path)
    day_less_than_2KB[filename] = (os.path.getsize(file_path) < 1024 * 2)


for day in last_3_years:
    d_url = "http://www.tsetmc.com/tse/data/Export-xls.aspx?a=TradeOneDay&Date={}".format(day)
    f_name = "{}.aspx".format(day)
    download(d_url, 'Trades1', f_name)


for i in range(10):
    for day_file in day_less_than_2KB.keys():
        if day_less_than_2KB[day_file]:
            d_url = "http://www.tsetmc.com/tse/data/Export-xls.aspx?a=TradeOneDay&Date={}".format(day_file[0:9])
            download(d_url, 'Trades1', day_file)

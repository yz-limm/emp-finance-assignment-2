from pathlib import Path
from urllib.request import urlretrieve
from zipfile import ZipFile

BASE = "https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/"
FILES = [
    "F-F_Research_Data_Factors_CSV.zip",
    "Portfolios_Formed_on_ME_CSV.zip",
    "Portfolios_Formed_on_BE-ME_CSV.zip",
    "25_Portfolios_5x5_CSV.zip",
]
out = Path(__file__).resolve().parent / "latest_french"
out.mkdir(exist_ok=True)
for name in FILES:
    zpath = out / name
    print("Downloading", name)
    urlretrieve(BASE + name, zpath)
    with ZipFile(zpath) as z:
        z.extractall(out)
    zpath.unlink()
print("Saved current files to", out)

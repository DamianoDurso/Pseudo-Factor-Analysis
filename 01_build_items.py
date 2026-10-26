"""Step 1: build the item + key files read by every later script.

NEO:    NEO/Johnson IPIPitemkey.xls (Johnson's own file, items and keys)  ->  NEO/NEO_items_keys.csv
HEXACO: HEXACO/codebook.txt (items, from the HEXACO.zip data download)
        + the saved IPIP HEXACO key page (official + / - keys)          ->  HEXACO/HEXACO_items_keys.csv

Both CSVs keep the column names and item order the original embedding notebooks used.
"""
import html
import re
from pathlib import Path

import pandas as pd

ROOT = Path(__file__).resolve().parent
NEO = ROOT / 'NEO'
HEX = ROOT / 'HEXACO'
KEY_PAGE = HEX / 'IPIP HEXACO key page (ipip.ori.org, downloaded 2026-10-13).htm'

# NEO: straight conversion of Johnson's file.
neo = pd.read_excel(NEO / 'Johnson IPIPitemkey.xls')
neo['Short#'] = neo['Short#'].astype('Int64')
assert len(neo) == 300 and neo['Facet'].nunique() == 30
assert neo.groupby('Facet').size().eq(10).all()
assert neo['Sign'].str[0].isin(['+', '-']).all()
neo.to_csv(NEO / 'NEO_items_keys.csv', index=False)

# HEXACO items: lines such as "EFear8 I love dangerous situations." in codebook order.
items = []
for line in (HEX / 'codebook.txt').read_text().splitlines():
    m = re.match(r'^([HEXACO])([A-Za-z]{4})(\d+)\s+(.*)$', line.strip())
    if m:
        items.append((m[1], m[2], int(m[3]), m[4]))
hexaco = pd.DataFrame(items, columns=['Facet', 'key', 'num', 'content'])
assert len(hexaco) == 240 and hexaco.groupby('key').size().eq(10).all()

# HEXACO keys: each facet header "(H:Sinc)" is followed by "+ keyed" and "- keyed" item lists.
text = html.unescape(re.sub(r'<[^>]+>', ' ', KEY_PAGE.read_text(encoding='latin-1')))
text = re.sub(r'\s+', ' ', text.replace('\xa0', ' '))
text = text[:text.index(' Note. ')]  # drop the references footnote
parts = re.split(r'\(([HEXACO]) ?: ?([A-Za-z]{4})\)', text)
keys = []
for i in range(1, len(parts), 3):
    for sign, block in re.findall(r'([+-]) keyed(.*?)(?=[+-] keyed|$)', parts[i + 2]):
        for item in re.findall(r'[A-Z][^.]*\.', block):
            keys.append((parts[i], parts[i + 1], sign, item))
keys = pd.DataFrame(keys, columns=['Facet', 'key', 'Sign', 'ipip_text'])
assert len(keys) == 240


def norm(s):
    # IPIP lists items without the leading "I" and with quotation marks.
    return re.sub(r'^I ', '', s).replace('"', '').lower()


hexaco['match'] = hexaco['content'].map(norm)
keys['match'] = keys['ipip_text'].map(norm)
assert not keys.duplicated(['key', 'match']).any()
hexaco = hexaco.merge(keys[['Facet', 'key', 'match', 'Sign']], on=['Facet', 'key', 'match'],
                      how='left', validate='one_to_one').drop(columns='match')
assert hexaco['Sign'].notna().all()
hexaco.to_csv(HEX / 'HEXACO_items_keys.csv', index=False)

print(f'NEO: {len(neo)} items, {neo["Sign"].str[0].eq("-").sum()} negatively keyed')
print(f'HEXACO: {len(hexaco)} items, {hexaco["Sign"].eq("-").sum()} negatively keyed')

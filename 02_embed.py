"""Step 3: facet-by-facet cosine similarity matrices.

For each study, model and encoding, as in the original notebooks:
  atomic           embed each item, average the item embeddings within facet
  atomic reversed  as atomic, but negatively keyed items' embeddings are multiplied by -1
  macro            join the facet's items with no separator (''.join, as published), embed the string
Cosine similarities between facets, diagonal set to 1, in <study>/matrices/ with the original file
names: matrix_concatenated_item_<m>.csv, ..._item_rev_<m>.csv, matrix_concatenated_<m>.csv.

<study>/matrices/ holds the matrices saved by the original analysis (fitting them reproduces the published
loadings exactly), except HEXACO atomic reversed, which is rebuilt with the official keys. A matrix that is
already saved is checked against the fresh one (stop if any cell differs by 1e-5 or more), not overwritten;
a missing one is written.

Robustness check (Tables 7 and 8), as in the original robustness scripts: for NEO (macro encoding) the facet
embeddings, for HEXACO (atomic encoding) the item embeddings, of one factor at a time are replaced by Gaussian
noise (fixed seed per model); at each step the cosine matrices of the five transformers are averaged:
<study>/matrices/<neo|hexaco>_<step>_avg.csv.
"""
# TensorFlow must be imported before pandas/sklearn, and Grappler's meta-optimizer disabled,
# or loading the Universal Sentence Encoder hangs.
import tensorflow as tf
tf.config.optimizer.set_experimental_options({'disable_meta_optimizer': True})

from pathlib import Path

import numpy as np
import pandas as pd
from huggingface_hub import snapshot_download
from sentence_transformers import SentenceTransformer, util

ROOT = Path(__file__).resolve().parent

# Output name -> (Hugging Face model, pinned revision).
MODELS = {
    'use_dan': ('Dimitre/universal-sentence-encoder', '9b553ff4389970e39a39d15b1fa3bba76f57b356'),
    'distilroberta': ('sentence-transformers/nli-distilroberta-base-v2', 'cc35a0bfb6251228a6fb8c797bca5fef0ece3c1d'),
    'mpnet': ('sentence-transformers/all-mpnet-base-v2', 'e8c3b32edf5434bc2275fc9bab85f82640a19130'),
    't5': ('sentence-transformers/sentence-t5-base', 'fc5d4628481afbbaaacd7af6bb07cf9d3865f781'),
    'miniLM': ('microsoft/MiniLM-L12-H384-uncased', '44acabbec0ef496f6dbc93adadea57f376b7c0ec'),
    'psych': ('dwulff/mpnet-personality', 'da377389cdf92ed0e28ffbc608912bcbcd97d162'),
}

# Study -> (items file, facet column, text column).
STUDIES = {
    'NEO': ('NEO/NEO_items_keys.csv', 'Facet', 'Item'),
    'HEXACO': ('HEXACO/HEXACO_items_keys.csv', 'key', 'content'),
}

# Robustness check: noise seed per transformer; factors replaced cumulatively, in this order.
NOISE_SEEDS = {'distilroberta': 2027, 'mpnet': 2028, 't5': 2029, 'miniLM': 2031, 'psych': 2032}
PERTURB_ORDER = {'NEO': 'NEOAC', 'HEXACO': 'HEXACO'}


def load(name):
    repo, revision = MODELS[name]
    if name == 'use_dan':
        model = tf.saved_model.load(snapshot_download(repo, revision=revision))
        return lambda texts: model(tf.constant(texts)).numpy()
    model = SentenceTransformer(repo, revision=revision, device='cpu')
    model.float()  # sentence-t5-base loads in half precision under current library versions
    return lambda texts: np.stack([model.encode(t) for t in texts])  # one item at a time, as originally


def cosine(E):
    S = util.pytorch_cos_sim(E, E).numpy()
    np.fill_diagonal(S, 1)
    return S


def save_or_check(S, f):
    """Write a matrix, or, if it is already saved, check that it agrees to 1e-5."""
    if f.exists():
        saved = pd.read_csv(f)
        assert list(saved.columns) == list(S.columns), f
        diff = np.abs(saved.values - S.values).max()
        assert diff < 1e-5, f'{f.name}: differs from saved matrix by {diff:.2g}'
    else:
        S.to_csv(f, index=False)


for study, (path, facet_col, text_col) in STUDIES.items():
    items = pd.read_csv(ROOT / path)
    facets = items[facet_col].unique()
    sign = np.where(items['Sign'].str[0] == '-', -1.0, 1.0)[:, None]
    out = ROOT / study / 'matrices'
    out.mkdir(exist_ok=True)
    robust = {}  # embeddings perturbed in the robustness check: NEO macro, HEXACO item
    for name in MODELS:
        encode = load(name)
        E = encode(items[text_col].tolist())
        atomic = np.stack([E[items[facet_col] == f].mean(axis=0) for f in facets])
        reversed_ = np.stack([(E * sign)[items[facet_col] == f].mean(axis=0) for f in facets])
        macro = encode([''.join(items.loc[items[facet_col] == f, text_col]) for f in facets])
        for prefix, F in [('item_', atomic), ('item_rev_', reversed_), ('', macro)]:
            save_or_check(pd.DataFrame(cosine(F), columns=facets), out / f'matrix_concatenated_{prefix}{name}.csv')
        if name in NOISE_SEEDS:
            robust[name] = (macro if study == 'NEO' else E).astype(np.float32)
        print(study, name, 'done', flush=True)

    # Robustness check. Factor of each embedded row: NEO facets (first letter of Key), HEXACO items (Facet).
    factor = (np.array([items.loc[items[facet_col] == f, 'Key'].iloc[0][0] for f in facets]) if study == 'NEO'
              else items['Facet'].values)
    noise = {}
    for name, seed in NOISE_SEEDS.items():
        np.random.seed(seed)
        noise[name] = np.random.normal(0, 1, robust[name].shape).astype(np.float32)
    order = PERTURB_ORDER[study]
    for k in range(len(order) + 1):
        replaced = np.isin(factor, list(order[:k]))
        mats = []
        for name in NOISE_SEEDS:
            X = np.where(replaced[:, None], noise[name], robust[name])
            F = X if study == 'NEO' else np.stack([X[items[facet_col] == f].mean(axis=0) for f in facets])
            mats.append(cosine(F))
        avg = np.mean(mats, axis=0)
        np.fill_diagonal(avg, 1)
        step = 'baseline' if k == 0 else 'perturb_' + order[:k]
        save_or_check(pd.DataFrame(avg, columns=facets), out / f'{study.lower()}_{step}_avg.csv')
    print(study, 'robustness matrices done', flush=True)

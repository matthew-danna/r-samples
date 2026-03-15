# MSF 3D Character Network

Interactive 3D network map for Marvel Strike Force characters.

## What It Uses

- Nodes: all characters from `../characters.csv`
- Team links: curated memberships in `data/team_memberships.csv`
- Synergy links: pair connections in `data/synergy_pairs.csv`
- Inferred links: minion clusters inferred from prefixed IDs (`Aim_`, `Hydra_`, etc.)

## Run Locally

From `/Users/matthewdanna/Documents/GitHub/r-samples/msf`:

```bash
python -m http.server 8000
```

Then open:

- `http://localhost:8000/msf-network-3d/index.html`

## Editing Data

1. Add/adjust team memberships in `data/team_memberships.csv`
2. Add/adjust direct pair synergies in `data/synergy_pairs.csv`
3. Refresh browser

## Notes

The local `characters.csv` currently has very sparse built-in trait columns, so most meaningful links come from the curated team/synergy files above.

"""Export interactive trend charts + descriptive stats as static HTML for GitHub Pages.

Usage: python export_html.py <csv> <out.html>
Only aggregated means and summary stats are written — no respondent data.
"""
import sys

import pandas as pd
import plotly.express as px

csv, out = sys.argv[1], sys.argv[2]

df = pd.read_csv(csv, parse_dates=["tidspunkt"], low_memory=False)
num = [c for c in df.columns if pd.api.types.is_numeric_dtype(df[c])]

long = (
    df[["tidspunkt"] + num]
    .groupby("tidspunkt")
    .mean()
    .melt(ignore_index=False, var_name="variabel", value_name="gennemsnit")
    .reset_index()
)
fig = px.line(long, x="tidspunkt", y="gennemsnit", color="variabel", markers=True,
              title=f"Tendens over tid ({len(num)} variabler — klik i legenden for at vælge)")
fig.update_layout(
    yaxis=dict(range=[0, 1]),
    xaxis=dict(range=["2005-01-01", "2026-12-31"]),
    legend=dict(traceorder="normal"),
)

stats = (
    df[num]
    .agg(["count", "mean", "median", "std", "min", "max"])
    .T.rename(columns={"count": "N"})
    .round(3)
    .rename_axis("variabel")
    .reset_index()
)
stats_html = stats.to_html(index=False, border=0, classes="stats")

chart = fig.to_html(full_html=False, include_plotlyjs=False)
html = f"""<!DOCTYPE html>
<html lang="da">
<head>
<meta charset="utf-8">
<title>Tendenser over tid</title>
<script src="https://cdn.plot.ly/plotly-2.35.2.min.js"></script>
<style>
  body {{ font-family: sans-serif; margin: 2em auto; max-width: 1200px; padding: 0 1em; }}
  h2 {{ margin-top: 2.5em; }}
  table.stats {{ border-collapse: collapse; font-size: 0.85em; }}
  table.stats th, table.stats td {{ padding: 4px 10px; border-bottom: 1px solid #ddd; text-align: right; }}
  table.stats th {{ background: #f5f5f5; }}
  table.stats td:first-child, table.stats th:first-child {{ text-align: left; }}
</style>
</head>
<body>
{chart}
<h2>Beskrivende statistik (alle observationer)</h2>
{stats_html}
</body>
</html>"""
with open(out, "w") as f:
    f.write(html)
print(f"{out}: {len(num)} variables, {long['tidspunkt'].nunique()} time points, {len(stats)} stats rows")

"""Line chart of variable trends over time, with variable selection (Dash).

Usage: python linechart_app.py [csv_path]   (default: monadisk_fil.csv)
"""
import sys
import pandas as pd
import plotly.express as px
import plotly.graph_objects as go
from dash import Dash, dcc, html, Input, Output

CSV = sys.argv[1] if len(sys.argv) > 1 else "monadisk_fil.csv"

df = pd.read_csv(CSV, parse_dates=["tidspunkt"], low_memory=False)
NUMERIC_COLS = [c for c in df.columns if pd.api.types.is_numeric_dtype(df[c])]


def make_fig(cols):
    """Mean per time point for the selected columns."""
    if not cols:
        return go.Figure().update_layout(title="Vælg mindst én variabel")
    means = df[["tidspunkt"] + cols].groupby("tidspunkt").mean().reset_index()
    long = means.melt("tidspunkt", var_name="variabel", value_name="gennemsnit")
    return px.line(long, x="tidspunkt", y="gennemsnit", color="variabel",
                   markers=True, title="Tendens over tid (gennemsnit pr. tidspunkt)").update_layout(
        yaxis=dict(range=[0, 1]),
        xaxis=dict(range=["2005-01-01", "2026-12-31"]),
    )


app = Dash(__name__)
app.layout = html.Div([
    html.H4("Monadisk: tendenser over tid"),
    dcc.Dropdown(NUMERIC_COLS, ["tillid_pragmatisk", "tryghed_grundlaeggende"],
                 multi=True, id="vars",
                 placeholder="Vælg variabler at plotte..."),
    dcc.Graph(id="line"),
])


@app.callback(Output("line", "figure"), Input("vars", "value"))
def update(cols):
    return make_fig(cols or [])


if __name__ == "__main__":
    app.run(debug=True, port=8050)

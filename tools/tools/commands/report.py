import re
import os
import sys
from pathlib import Path

import click
import pandas as pd

# Line parsing rules
# pylint: disable=line-too-long
RULES = [
    r"^(?P<label>.+)\s*;\s*(?P<passes>\d+)\s*;\s*(?P<duration>\d+([.]\d+)?)\s*;\s*(?P<threads>\d+)$",
]

@click.command()
@click.option("-d", "--directory", default=os.getcwd(), help="Output files directory")
def report(directory):
    df = pd.DataFrame(
        columns=[
            "implementation",
            "solution",
            "label",
            "passes",
            "duration",
            "threads",
        ]
    )

    for file in Path(os.path.abspath(directory)).rglob("*.out"):
        if os.stat(file).st_size == 0:
            click.echo(f"File '{file}' appears to be empty. Skipping...")
            continue

        # Extracting additional information from the file name.
        name = os.path.splitext(os.path.basename(file))[0]
        metadata = name.split("-")
        found = 0

        click.echo(f"Processing {name}")
        with open(file, "r") as handle:
            for line in handle.readlines():
                for rule in RULES:
                    match = re.match(rule, line.strip())

                    if match:
                        data = match.groupdict()
                        df = df.append(
                            {
                                "implementation": metadata[0],
                                "solution": metadata[1],
                                "label": data.get("label", metadata[0]),
                                "passes": data.get("passes"),
                                "duration": data.get("duration"),
                                "threads": data.get("threads", 1),
                            },
                            ignore_index=True,
                        )
                        found += 1
                        break
        
        if found == 0:
            click.echo(f"No valid output: {file}")

    if df.empty:
        raise Exception("No data was found!")

    # Making sure the dataframe has the correct column types.
    # Unfortunately these cannot be specified during creation.
    df = df.astype(
        {
            "implementation": "string",
            "solution": "string",
            "label": "string",
            "passes": int,
            "duration": float,
            "threads": int,
        }
    )

    df["passes_per_second"] = df["passes"] / df["duration"] / df["threads"]

    # Generate single/multi threaded reports
    generate_reports(df.loc[df["threads"] == 1], "Software Drag Race (single-threaded)")
    generate_reports(df.loc[df["threads"] > 1], "Software Drag Race (multi-threaded)")

def generate_reports(df, title):
    """Generate report"""

    click.echo(f"Generating {title}")

    data = df.sort_values(by=["passes_per_second"], ascending=False)
    data.reset_index(drop=True, inplace=True)
    data.index += 1

    click.echo(data.to_string())
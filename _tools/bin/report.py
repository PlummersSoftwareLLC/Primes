#!/usr/bin/env python3

import argparse
import logging
import os
import re
import sys
from pathlib import Path

import pandas as pd

# Setup Python logging to print messages to stdout
log = logging.getLogger()
log.setLevel(logging.INFO)
handler = logging.StreamHandler(sys.stdout)
formatter = logging.Formatter("%(levelname)s: %(message)s")
handler.setFormatter(formatter)
log.addHandler(handler)

# Line parsing rules
# pylint: disable=line-too-long
RULES = [
    r"^(?P<label>.+)\s*;\s*(?P<passes>\d+)\s*;\s*(?P<duration>\d+([.]\d+)?)\s*;\s*(?P<threads>\d)$",
    r"^Passes:\s+(?P<passes>\d+),?\s+Time:\s+(?P<duration>\d+[.]\d+),?\s+Avg:\s+(\d+[.]\d+),?\s+Limit:\s+(\d+),?\s+Count1:\s+(\d+),?\s+Count2:\s+(\d+),?\s+Valid:\s+(true|false|1|0|Yes|No)$",
]


def generate_reports(df, title):
    """Generate report"""

    log.info("Generating %s", title)

    data = df.sort_values(by=["passes_per_second"], ascending=False)
    data.reset_index(drop=True, inplace=True)
    data.index += 1

    print(data.to_string())


def main(directory):
    """Main function"""

    df = pd.DataFrame(
        columns=[
            "implementation",
            "solution",
            "os",
            "label",
            "passes",
            "duration",
            "threads",
        ]
    )

    for file in Path(os.path.abspath(directory)).rglob("*.out"):
        if os.stat(file).st_size == 0:
            log.warning("File '%s' appears to be empty. Skipping...", file)
            continue

        # Extracting additional information from the file name.
        name = os.path.splitext(os.path.basename(file))[0]
        metadata = name.split("-", 2)

        log.info("Processing %s", name)
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
                                "os": metadata[2],
                                "label": data.get("label", metadata[0]),
                                "passes": data.get("passes"),
                                "duration": data.get("duration"),
                                "threads": data.get("threads", 1),
                            },
                            ignore_index=True,
                        )
                        break

    if df.empty:
        raise Exception("No data was found!")

    # Making sure the dataframe has the correct column types.
    # Unfortunately these cannot be specified during creation.
    df = df.astype(
        {
            "implementation": "string",
            "solution": "string",
            "os": "string",
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


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="")
    parser.add_argument(
        "-d",
        "--directory",
        type=str,
        default=os.getcwd(),
        help="Root directory",
        required=False,
    )
    args = parser.parse_args()

    try:
        main(**args.__dict__)
    # pylint: disable=broad-except
    except Exception as exc:
        log.error(exc)
        sys.exit(1)

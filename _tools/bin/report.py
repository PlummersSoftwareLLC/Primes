#!/usr/bin/env python3

import argparse
import glob
import logging
import os
import re
import sys
import time

import pandas as pd
import pandas_profiling as pp

# Setup Python logging to print messages to stdout
log = logging.getLogger()
log.setLevel(logging.INFO)
handler = logging.StreamHandler(sys.stdout)
formatter = logging.Formatter("%(levelname)s: %(message)s")
handler.setFormatter(formatter)
log.addHandler(handler)

# Line parsing rules
RULES = [
    r"^(?P<label>.+)\s*;\s*(?P<passes>\d+)\s*;\s*(?P<duration>\d+([.]\d+)?)\s*;\s*(?P<threads>\d)$",
    r"^Passes:\s+(?P<passes>\d+),?\s+Time:\s+(?P<duration>\d+[.]\d+),?\s+Avg:\s+(\d+[.]\d+),?\s+Limit:\s+(\d+),?\s+Count1:\s+(\d+),?\s+Count2:\s+(\d+),?\s+Valid:\s+(true|false|1|0|Yes|No)$",
]


def save_report(df, title, name):
    log.info(f"Generating: {title}")

    data = df.sort_values(by=["passes", "duration"], ascending=False)
    profile = pp.ProfileReport(data, title=title, explorative=True)
    profile.to_file(name)


def main(directory):
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

    directory = os.path.abspath(directory)
    files = glob.glob(f"{directory}/**/*.out")
    if not len(files):
        raise Exception("No output files found!")

    for file in files:
        if os.stat(file).st_size == 0:
            log.warning(f"File '{file}' appears to be empty. Skipping...")
            continue

        # Extracting additional information from the file name.
        name = os.path.splitext(os.path.basename(file))[0]
        metadata = name.split("-", 2)

        log.info(f"Processing {name} ...")
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

    # Generate reports
    now = time.time()
    save_report(
        df.loc[df["threads"] == 1],
        "Software Drag Race (single-threaded)",
        f"dr-st-{now}.html",
    )
    save_report(
        df.loc[df["threads"] > 1],
        "Software Drag Race (multi-threaded)",
        f"dr-mt-{now}.html",
    )


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
    except Exception as e:
        log.error(e)
        exit(1)

#!/usr/bin/env python3

import argparse
import os
import glob
import re
import time

import pandas as pd
import pandas_profiling as pp


def main(directory):
    directory = os.path.abspath(directory)

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

    for file in glob.glob(f"{directory}/**/*.out"):
        name = os.path.splitext(os.path.basename(file))[0]
        metadata = name.split("-", 2)

        with open(file, "r") as handle:
            for line in handle.readlines():
                match = re.match(
                    r"^\s*(?P<label>.+)\s*;\s*(?P<passes>\d+)\s*;\s*(?P<duration>\d+([.]\d+)?)\s*;\s*(?P<threads>\d)\s*$",
                    line,
                )

                if not match:
                    print("No matches found!")
                    continue

                df = df.append(
                    {
                        "implementation": metadata[0],
                        "solution": metadata[1],
                        "os": metadata[2],
                        "label": match.group("label"),
                        "passes": match.group("passes"),
                        "duration": match.group("duration"),
                        "threads": match.group("threads"),
                    },
                    ignore_index=True,
                )

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
    print(df.dtypes)

    df.sort_values(by=["passes", "duration"], ascending=False, inplace=True)
    print(df.to_string())

    profile = pp.ProfileReport(df, title="Software Drag Race", explorative=True)
    profile.to_file(f"dr-{time.time()}.html")


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
    main(**args.__dict__)

import sys

import click

from commands.report import report

@click.group()
def cli():
    pass

cli.add_command(report)

if __name__ == "__main__":
    try:
        cli()
    except Exception as exc:
        print(exc)
        sys.exit(1)
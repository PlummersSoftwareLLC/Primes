# CPython implementation of PrimeSieve

[![Open in Colab](https://colab.research.google.com/assets/colab-badge.svg)](https://colab.research.google.com/github/1mikegrn/pysieve/blob/master/jupyter/pysieve.ipynb)


Installs a native implementation (no libraries) of the PrimeSieve and provides a
console-script `pysieve` to run the main executable. (requires that the python interpreter has access to a C compiler so that the included C extension is compiled on install).

## Benchmarks

|Source|Passes|Time|Avg|Limit|Count|Valid|
|---|---|-----|---|---|---|---|
Lenovo x1-Carbon i7-9750H|1737|5.00086|0.002879|1000000|78498|True|
Google Colab|782|5.0015|0.006395|1000000|78498|True|

## installation

Easiest way to validate is to make a virtual env (venv), then install from this directory, and finally call the console script.

```bash
python -m venv venv
. venv/bin/activate
python -m pip install -e .
pysieve
```
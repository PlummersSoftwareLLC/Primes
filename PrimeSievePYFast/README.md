## CPython
CPython (or just regular Python that most people use) can benefit from greater performance by letting numba take care of huge nested loops. If a function with a ton of loops can be separated from the rest of the code, this can be really fast. Of course calls from that function also need to be passed to numba for it to be a benefit.

## PyPy
PyPy is also Python, and is very much compatible with CPython libraries, with a few exceptions. It also lags behind a few versions. But it's blazing fast. It's as fast as Go, Java and the like. It's a very good python implementation for people writing Python-only code.

## Setup

```sh
virtualenv -p /path/to/python venv
source venv/bin/activate
# Only if running CPython
pip install -r requirements.txt
python PrimePY.py
```

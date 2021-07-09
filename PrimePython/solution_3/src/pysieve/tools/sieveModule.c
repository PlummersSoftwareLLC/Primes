#include <Python.h>
#include <math.h>
#include <stdio.h>
#include <sys/time.h>

int count_primes(int* array, int n) {
    int sum = 0;
    for (int i=0; i<n; i++) {
        sum += array[i];
    }
    return sum;
}

int* _init_array(int n) {
    int limit = (n+1)/2;
    int* array = (int*)malloc(limit * sizeof(int*));
    for (int i=0; i<limit; i++) {
        array[i] = 1;
    }
    return array;
}

int _get_bit(int* array, int index) {
    if (index % 2 == 0) {
        return 0;
    } else {
        return array[index/2];
    }
}

void _clear_bit(int* array, int index) {
    array[index/2] = 0;
}

static PyObject* _run_sieve(PyObject *self, PyObject *args) {
    int upper_bound;

    // Ensure the first parameter is a callable (e.g. function)
    if(!PyArg_ParseTuple(args, "i", &upper_bound)) {
        return NULL;
    }
    
    int* primes = _init_array(upper_bound);

    int factor = 3;
    int q = sqrt(upper_bound);

    while (factor < q) {
        for (int num = factor; num < upper_bound; num++) {
            if (_get_bit(primes, num) == 1) {
                factor = num;
                break;
            }
        }

        for (int num = factor * 3; num < upper_bound; num += factor*2) {
            _clear_bit(primes, num);
        }

        factor += 2;
    }

    int result = count_primes(primes, upper_bound);
    free(primes);
    return Py_BuildValue("i", result);
}


static PyObject* _check_time(PyObject *self, PyObject *args) {
    struct timeval current_time;
    double start_time;
    double time_limit;

    if(!PyArg_ParseTuple(args, "dd", &start_time, &time_limit)) {
        return NULL;
    }

    gettimeofday(&current_time, NULL);
    double current_time_as_double = current_time.tv_sec+(current_time.tv_usec/1000000.0);

    if (current_time_as_double - start_time < time_limit) {
        return Py_BuildValue("i", 1);
    }
    return Py_BuildValue("i", 0);
}


static char runSieveDocs[] = 
    "determine which numbers are prime up to an upper bound";


static PyMethodDef sieveMethods[] = {
    {"_run_sieve", _run_sieve, METH_VARARGS, runSieveDocs},
    {"check_time", _check_time, METH_VARARGS, "check if time has elapsed"},
    {NULL, NULL, 0, NULL}
};


static struct PyModuleDef sieveModule = {
    PyModuleDef_HEAD_INIT,
    "sieveModule",
    NULL,
    -1,
    sieveMethods
};


PyMODINIT_FUNC PyInit_sieveModule(void) {
    return PyModule_Create(&sieveModule);
}

#include <Python.h>
#include <math.h>
#include <stdio.h>


int sum(int array[], int n) {
    int sum = 0;
    for (int i=0; i<n; i++) {
        sum += array[i];
    }
    return sum;
}

int* _init_array(int n) {
    int* array = (int*)malloc(n * sizeof(int*));
    array[0] = 0;
    array[1] = 0;
    array[2] = 1;
    for (int i=3; i<n; i++) {
        if (i%2 == 0) {
            array[i] = 0;
        }
        else {
            array[i] = 1;
        }
    }
    return array;
}


static PyObject* _run_sieve(PyObject *self, PyObject *args) {
    int upper_bound;

    // Ensure the first parameter is a callable (e.g. function)
    if(!PyArg_ParseTuple(args, "i", &upper_bound)) {
        return NULL;
    }
    
    int* primes = _init_array(upper_bound);
    for (int index = 3; index < sqrt(upper_bound); index += 2) {
        if (primes[index] == 1) {
            int flip_index = index + index;
            while (flip_index < upper_bound) {
                primes[flip_index] = 0;
                flip_index += index;
            }
        }
    }

    int result = sum(primes, upper_bound);
    free(primes);
    return Py_BuildValue("i", result);
}


static char runSieveDocs[] = 
    "determine which numbers are prime between 2 and an upper bound";


static PyMethodDef sieveMethods[] = {
    {"_run_sieve", _run_sieve, METH_VARARGS, runSieveDocs},
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

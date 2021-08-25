import os.path

from setuptools import setup, find_packages, Extension

HERE = os.path.dirname(os.path.abspath(__file__))

extensions = [
    Extension(
        "pysieve.tools.sieveModule",
        [os.path.join('src', 'pysieve', 'tools', 'sieveModule.c')]
    )
]

setup(
    ext_modules = extensions,
    include_package_data=True,
    name='pysieve',
    packages=find_packages(where="src"),
    package_dir={"": "src"},  
    zip_safe=False,
)

from setuptools import setup, find_packages

setup(
    name="mcanalysis",
    version="0.1.0",
    author="Kyra Delray",
    author_email="kyra.delray@wolfson.ox.ac.uk",
    description="Menstrual Cycle Analysis for health outcomes using GAM models",
    long_description=open("README.md").read() if __import__("os").path.exists("README.md") else "",
    long_description_content_type="text/markdown",
    url="https://github.com/kyradelray/mcanalysis",
    packages=find_packages(),
    classifiers=[
        "Development Status :: 3 - Alpha",
        "Intended Audience :: Science/Research",
        "License :: OSI Approved :: MIT License",
        "Programming Language :: Python :: 3",
        "Programming Language :: Python :: 3.8",
        "Programming Language :: Python :: 3.9",
        "Programming Language :: Python :: 3.10",
        "Programming Language :: Python :: 3.11",
        "Topic :: Scientific/Engineering :: Bio-Informatics",
    ],
    python_requires=">=3.8",
    install_requires=[
        "pandas>=1.3.0",
        "numpy>=1.20.0",
        "matplotlib>=3.4.0",
        "statsmodels>=0.13.0",
        "scipy>=1.7.0",
    ],
    extras_require={
        "dev": ["pytest", "pygam"],
    },
)

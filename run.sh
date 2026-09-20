#!/bin/env bash
OMP_NUM_THREADS=$(nproc) OMP_PROC_BIND=true ./raytracer && timg output.png

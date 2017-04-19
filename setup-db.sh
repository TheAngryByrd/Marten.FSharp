#!/usr/bin/env bash

kill $(lsof -ti :5432)
initdb -D data/
postgres -D data

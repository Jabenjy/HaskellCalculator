#!/usr/bin/env bash

CALCULATOR_DATA_DIR=${HOME}/.local/share/calculator

if [ ! -d ${CALCULATOR_DATA_DIR} ]; then
  mkdir -p ${CALCULATOR_DATA_DIR};
fi

sqlite3 ${CALCULATOR_DATA_DIR}/calculator.db "DROP TABLE IF EXISTS calculations;\
  CREATE TABLE calculations (id INTEGER PRIMARY KEY, lOperand INTEGER, rOperand INTEGER, operator CHAR, result FLOAT)"

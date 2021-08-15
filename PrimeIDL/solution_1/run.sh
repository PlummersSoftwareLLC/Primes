#!/bin/bash

if [[ -z "${IDL_DIR}" ]]; then
  PROG=gdl
else
  PROG=${IDL_DIR}/bin/idl
  unset IDL_STARTUP
fi

cd $(dirname "${BASH_SOURCE[0]}")

for file in primeidl_*.pro; do
   ${PROG} -quiet -e ${file%.pro} 2>>/dev/null;
done
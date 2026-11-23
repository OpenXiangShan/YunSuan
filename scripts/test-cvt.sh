#!/usr/bin/env bash
# Standalone numeric CVT regression; no Saturn/Rocket/HardFloat checkout required.
set -euo pipefail
cvt_repo="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$cvt_repo"
if [[ -n "${CVT_JAVA_HOME:-}" ]]; then
  export JAVA_HOME="$CVT_JAVA_HOME"
  export PATH="$JAVA_HOME/bin:$PATH"
fi
if [[ -n "${CVT_VERILATOR_HOME:-}" ]]; then
  export PATH="$CVT_VERILATOR_HOME/bin:$PATH"
  unset VERILATOR_ROOT
fi
if [[ -n "${LOWP_CORE_INJECT_FAULT:-}${BF16_CVT_INJECT_FAULT:-}${NV_PAYLOAD_INJECT_FAULT:-}" ]]; then
  echo 'Fault injection is set; run the targeted negative test explicitly.' >&2
  exit 2
fi
"${CVT_MILL:-mill}" -i -j 1 YunSuan.test.testOnly 'yunsuan.vector.cvt.*'

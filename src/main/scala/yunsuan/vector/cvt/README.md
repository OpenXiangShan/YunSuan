# Standalone conversion families

`yunsuan.vector.cvt` provides numeric conversion kernels without a host ISA,
vector-register file, scheduler, Saturn, Rocket Chip or HardFloat dependency.
The legacy `VectorConvert` numerical implementations remain the shared source.

| Entry | Operations | Core latency / II |
|---|---|---|
| GenericConvert | FP32 ↔ I8/U8/E4M3FN/E5M2/E2M1; floating formats optionally use UE8M0 | default2/1; raw decode-only1/1; pipeline=false1/1 |
| NativeBF16Convert / BF16Convert | FP32 ↔ BF16 | 1/1 |
| NativeFP32Convert | FP32 ↔ signed/unsigned32-bit integer | 2/1 |
| NvPayloadPipeline | prepared-input NV payload encode/reconstruct | encode-only4/1; decode or mixed6/1 |
| IntegerToFP32(width) | signed/unsigned integers width1..24 | combinational |

FP16/32/64 legacy conversion kernels are available as CVT16/CVT32/CVT64 and
VectorCvt. Their host adaptation and provider selection are separate work.
The new capability metadata advertises only modules actually provided here;
it does not advertise a host's FPConvBlock as a YunSuan implementation.

## Configuration and interface

`ConvertParams` selects formats, directions and scale modes at elaboration.
Only those families are instantiated. Runtime `format/decode/scaled/factor/rm`
must select an enabled operation on fire. Formats: I8=0,U8=1,E4M3=2,E5M2=3,E2M1=4.
The rm input supports RNE/RTZ/RDN/RUP/RMM(0..4). ISA legality is host-owned.

`fire` accepts at most one input per cycle. Result and fflags appear after
`GenericConvert.latency(params,pipeline)` captures; caller carries validity.
There is no output ready. Outputs hold after bubbles drain; reset cancels
pending transactions. Host must align tags/masks/flags/completion to actual
core latency and its own input register boundary. No input-dependent latency.
Low payloads occupy low bits of result. The core does not pack vector bytes,
filter inactive-lane flags, generate a block scale or manage source dependencies.

Host factories can read `GenericConvert.latency(params, pipeline)`,
`BF16Convert.latency`, `NativeBF16Convert.latency` and `NativeFP32Convert.latency`
before instantiating a module. Each companion also exposes `initiationInterval`;
each instance exposes its selected `latency` and `initiationInterval`.
`NativeConvertCapabilities` derives its timing from these entries. Latency counts
clock captures including the edge accepting `fire`: BF16 is available immediately
after that first edge; the default Generic and native FP32 results follow the
second edge. Any host input/output register adds to the host-visible latency.

Default encode/scaled paths register classification/normalization/exponent
preparation before shift/round/result selection. Single-stage is selectable;
choose against actual timing/area constraints. Public-library exploratory STA
is not target-technology closure. Generic source reuse does not imply a single
physically shared variable-format rounder: enabled candidate kernels are
selected behind muxes.

## Numerical rules

I8/U8 are saturated numeric casts, not affine quantizers. E4M3FN has finite
maximum448 and no infinity; E5M2 has infinity; E2M1 has maximum6 and no NaN/Inf.
Rounding and flags follow the independent reference tests. UE8M0 factor codes
represent 2^(factor-127), with255 as NaN. Encode divides by the represented
power of two, decode multiplies. Raw mode means scale1. BF16 is1/8/7 and differs
from FP16(1/5/10). NativeBF16 handles exact widening and rounded narrowing.

Legacy CVT_bf16/LowpConvert retain two-cycle compatibility. CVT_xx8/CVT_mxfp
retain two cycles with preparation inside arithmetic. Existing callers need
no latency change. `VCVT` selects `CVT_bf16` against CVT16/32/64 with a two-capture
selector; `VectorCvt` also delays its width/format packing controls twice.
Scalar `FPCVT` selects BF16 against CVT64 at the same second capture. Removing
the legacy BF16 output register would misalign these existing consumers.
New hosts consume `GenericConvert` and `NativeBF16Convert` directly and schedule
their actual depths; they do not inherit legacy alignment registers.

## Optional NV payload pipeline

This is a library module, not a public instruction or complete block quantizer.
Caller supplies inverse, stored E4M3 scale, alpha, zeroBlock, direction and tag.
Encode is `E2M1_RNE(RN32(x*inverse))`; caller's inverse is
`RN32(1/RN32(alpha*max(decodeE4(scale),2^-9)))` for the tested baseline contract.
Decode is `RN32(RN32(decodeE2(q)*decodeE4(scale))*alpha)`. Intermediate rounding
order is intentional. Uses existing YunSuan VectorFloatFMA multiply lanes.
All module lanes are active; host masks flags and provides output capacity.
Scale preparation/division/block statistics/packing/TR register access remain
outside. Encode-only instantiates one multiply bank; mixed/decode needs two.

## Standalone validation

From repository root with the repository toolchain and Verilator available:

```
bash scripts/test-cvt.sh
```

Tested with Mill 0.11.1, OpenJDK 21, Chisel 6.7.0, ChiselTest 6.0.0 and
Verilator 5.024. Select local installations using `CVT_JAVA_HOME`,
`CVT_VERILATOR_HOME` (installation roots) and optional `CVT_MILL` (executable).
Verilator 5.052 removed a `WData` alias used by this ChiselTest harness;
use a compatible Verilator version. No machine-specific path is required.

The tests are under `src/test/scala/vector/cvt`, package `yunsuan.vector.cvt`.
They use independent BigInt dyadic/rational references, exhaustive low codes,
midpoint neighbors, extreme scales, five RM modes, streaming/bubbles/reset,
parameter pruning, legacy compatibility and optional NV staged-rounding cases.
NativeFP32Convert has a direct two-cycle stream test. No backend checkout is
required. Host decode, lane packing, chaining and integration must be tested
in the integrating project and are not implied by these standalone results.

The BF16 change also fixes underflow detection at the subnormal boundary.
For FP32 `0x007f8000`, RNE produces BF16 `0x0080` with UF|NX (`0x03`),
although the stored result is normal: tininess is determined at BF16 precision
with an unbounded exponent before the final subnormal rounding. The old
result-exponent check reported only NX. `BF16TininessSpec` checks ten hand
anchors in both signs and all five rounding modes.

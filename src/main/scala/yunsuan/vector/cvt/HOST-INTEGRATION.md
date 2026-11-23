# Host integration and native conversion providers

The standalone API describes numeric lanes, not an instruction decoder or a
vector-register layout. Callers select providers by operation, keep one common
capability table in the host, and carry validity/metadata through the declared
latency. Width/format availability must reflect the installed provider.

| Operation family | Library entry | Recommended current EVU provider |
|---|---|---|
| FP32 ↔ I8/U8/E4M3FN/E5M2/E2M1 | GenericConvert, raw profile | new host adapter |
| FP32 ↔ scaled E4M3FN/E5M2/E2M1 | GenericConvert, UE8M0 profile | new host adapter |
| FP32 ↔ BF16 | NativeBF16Convert | dedicated short host adapter |
| FP32 ↔ I32/U32 | NativeFP32Convert/CVT32 | existing FPConv until per-form replacement validated |
| FP16 ↔ FP32 and other IEEE forms | legacy VectorCvt/CVT16/32/64 | existing FPConv/HardFloat |
| Prepared NV payload | NvPayloadPipeline | optional, separate context/packing contract |

BF16 (1/8/7) and FP16 (1/5/10) are distinct. Full upstream Saturn and the
production EVU subset also differ: the inspected EVU has no SEW64 and exposes
only signed I32↔FP32 plus FP16↔FP32 native conversions externally. The library
presence of CVT64 or a host factory's extra forms does not enable an ISA form.

The experimental integration previously exported a host descriptor named
`NativeConvertCapabilities.ieeeRemainder`. It is intentionally absent here.
Consumers of that experimental API must move this descriptor into their host
configuration, declaring only installed and validated operations. For example,
keep an EVU-local FP16/FP32 provider entry pointing to its FPConv implementation;
a full Saturn host may maintain a broader separate entry. Do not remove the
working native provider or advertise HardFloat as a YunSuan kernel.

To replace native providers later, adapt instruction controls to
`opType/input1H/output1H/rm`; verify source half selection, destination packing,
per-byte masks and active-lane flag reduction. Existing YunSuan `VectorCvt`
lives in `VectorConvert/Convert.scala`; it is not interface-compatible with
Saturn `FPConvBlock`. Preserve host entry-to-writeback timing or update scheduling
reservations, tags and masks together. Dual-provider numerical comparisons and
host RAW/WAR/WAW, tails, bubbles and flush tests gate any default change.

For packed low formats, derive physical source/destination offsets from the
operation's element widths. A 4-bit source needs nibble extraction even when the
command's logical SEW is 32. A given-scale operation must read the named scale
source with normal dependency tracking; the numerical lane does not own its
lifetime. Neither a format field nor a `Family` record creates arbitration for
a physically shared execution core.

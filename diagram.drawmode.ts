const d = new Diagram();

const backendStyle = { color: "backend" };

const javascriptSourceCode = d.addBox("JavaScript\nSource Code", { row: 0, col: 1, color: "frontend", icon: "globe" });
const quickjsngBytecodeParser = d.addBox("QuickJS-NG\nBytecode Parser", { row: 1, col: 1, color: "backend", icon: "server" });
const numericTierDetectionI32F64Array = d.addBox("Numeric Tier\nDetection\n(i32 / f64 / array)", { row: 2, col: 0, color: "ai" });
const sourcetosourceTransform = d.addBox("Source-to-Source\nTransform", { row: 2, col: 2, color: "ai" });
const llvmIrCodegenO2 = d.addBox("LLVM IR\nCodegen (O2)", { row: 3, col: 0, color: "backend", icon: "server" });
const workermjsSmartTrampolines = d.addBox("worker.mjs\n(Smart Trampolines)", { row: 3, col: 2, color: "frontend" });
const standaloneWasm = d.addBox("Standalone\n.wasm", { row: 4, col: 0, color: "database", icon: "storage" });
const v8TurbofanJitCompiler = d.addBox("V8 TurboFan\nJIT Compiler", { row: 4, col: 2, color: "orchestration", icon: "fire" });
const wasmInlinedIntoJsZeroBoundaryOverhead29xFaster = d.addBox("WASM Inlined into JS\nZero Boundary Overhead\n2-9x Faster", { row: 5, col: 1, color: "cache", icon: "check" });
const nodejs22 = d.addBox("Node.js 22+", { ...backendStyle, row: 6, col: 0 });
const deno = d.addBox("Deno", { ...backendStyle, row: 6, col: 1 });
const cloudflareWorkers = d.addBox("Cloudflare\nWorkers", { ...backendStyle, row: 6, col: 2 });

const edgeboxCompilerBuildTime = d.addGroup("EdgeBox Compiler (Build Time)", [quickjsngBytecodeParser, numericTierDetectionI32F64Array, sourcetosourceTransform, llvmIrCodegenO2, workermjsSmartTrampolines, standaloneWasm]);
const v8Runtime = d.addGroup("V8 Runtime", [v8TurbofanJitCompiler, wasmInlinedIntoJsZeroBoundaryOverhead29xFaster]);
const deployTargetsAnyV8Runtime = d.addGroup("Deploy Targets (Any V8 Runtime)", [nodejs22, deno, cloudflareWorkers]);

d.connect(javascriptSourceCode, quickjsngBytecodeParser, "parse");
d.connect(quickjsngBytecodeParser, numericTierDetectionI32F64Array, "bytecodes");
d.connect(quickjsngBytecodeParser, sourcetosourceTransform, "bytecodes");
d.connect(numericTierDetectionI32F64Array, llvmIrCodegenO2, "numeric\nfunctions");
d.connect(sourcetosourceTransform, workermjsSmartTrampolines, "rewrite bodies");
d.connect(llvmIrCodegenO2, standaloneWasm, "compile");
d.connect(standaloneWasm, v8TurbofanJitCompiler, "import");
d.connect(workermjsSmartTrampolines, v8TurbofanJitCompiler, "load");
d.connect(v8TurbofanJitCompiler, wasmInlinedIntoJsZeroBoundaryOverhead29xFaster, "TurboFan inlines\nWASM into JS");
d.connect(wasmInlinedIntoJsZeroBoundaryOverhead29xFaster, nodejs22);
d.connect(wasmInlinedIntoJsZeroBoundaryOverhead29xFaster, deno);
d.connect(wasmInlinedIntoJsZeroBoundaryOverhead29xFaster, cloudflareWorkers);

return d.render({ path: "/home/teamchong/Downloads/repos/edgebox/diagram" });
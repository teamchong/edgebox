var __assign = (this && this.__assign) || function () {
    __assign = Object.assign || function(t) {
        for (var s, i = 1, n = arguments.length; i < n; i++) {
            s = arguments[i];
            for (var p in s) if (Object.prototype.hasOwnProperty.call(s, p))
                t[p] = s[p];
        }
        return t;
    };
    return __assign.apply(this, arguments);
};
var d = new Diagram();
var backendStyle = { color: "backend" };
var javascriptSourceCode = d.addBox("JavaScript\nSource Code", { row: 0, col: 1, color: "frontend", icon: "globe" });
var quickjsngBytecodeParser = d.addBox("QuickJS-NG\nBytecode Parser", { row: 1, col: 1, color: "backend", icon: "server" });
var numericTierDetectionI32F64Array = d.addBox("Numeric Tier\nDetection\n(i32 / f64 / array)", { row: 2, col: 0, color: "ai" });
var sourcetosourceTransform = d.addBox("Source-to-Source\nTransform", { row: 2, col: 2, color: "ai" });
var llvmIrCodegenO2 = d.addBox("LLVM IR\nCodegen (O2)", { row: 3, col: 0, color: "backend", icon: "server" });
var workermjsSmartTrampolines = d.addBox("worker.mjs\n(Smart Trampolines)", { row: 3, col: 2, color: "frontend" });
var standaloneWasm = d.addBox("Standalone\n.wasm", { row: 4, col: 0, color: "database", icon: "storage" });
var v8TurbofanJitCompiler = d.addBox("V8 TurboFan\nJIT Compiler", { row: 4, col: 2, color: "orchestration", icon: "fire" });
var wasmInlinedIntoJsZeroBoundaryOverhead29xFaster = d.addBox("WASM Inlined into JS\nZero Boundary Overhead\n2-9x Faster", { row: 5, col: 1, color: "cache", icon: "check" });
var nodejs22 = d.addBox("Node.js 22+", __assign(__assign({}, backendStyle), { row: 6, col: 0 }));
var deno = d.addBox("Deno", __assign(__assign({}, backendStyle), { row: 6, col: 1 }));
var cloudflareWorkers = d.addBox("Cloudflare\nWorkers", __assign(__assign({}, backendStyle), { row: 6, col: 2 }));
var edgeboxCompilerBuildTime = d.addGroup("EdgeBox Compiler (Build Time)", [quickjsngBytecodeParser, numericTierDetectionI32F64Array, sourcetosourceTransform, llvmIrCodegenO2, workermjsSmartTrampolines, standaloneWasm]);
var v8Runtime = d.addGroup("V8 Runtime", [v8TurbofanJitCompiler, wasmInlinedIntoJsZeroBoundaryOverhead29xFaster]);
var deployTargetsAnyV8Runtime = d.addGroup("Deploy Targets (Any V8 Runtime)", [nodejs22, deno, cloudflareWorkers]);
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

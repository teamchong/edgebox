print("Testing native fetch...");

// This should trigger the native fetch path which calls http_dispatch
var result = _edgebox_fetch("https://example.com");
print("Fetch result: " + result);

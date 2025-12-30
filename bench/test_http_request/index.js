print("Testing HTTP request...");
var http = require("http");
print("HTTP module loaded");
print("createBlockingServer exists: " + (typeof http.createBlockingServer));
print("createNativeServer exists: " + (typeof http.createNativeServer));

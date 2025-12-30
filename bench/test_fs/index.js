print("Testing FS...");
var fs = require("fs");
print("FS module loaded");
var content = fs.readFileSync("/tmp/test_edgebox.txt", "utf8");
print("File content: " + content);

print("Testing blocking HTTP server...");
var http = require("http");
print("Creating blocking server on port 8892...");
http.createBlockingServer(8892, function(req, res) {
    res.writeHead(200, { 'Content-Type': 'text/plain' });
    res.end('Hello');
});
print("Server should be listening");

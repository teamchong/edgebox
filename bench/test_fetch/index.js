print("Testing fetch...");
fetch("https://httpbin.org/get").then(function(res) {
    print("Fetch result: " + res.status);
}).catch(function(err) {
    print("Fetch error: " + err);
});
print("Fetch initiated");

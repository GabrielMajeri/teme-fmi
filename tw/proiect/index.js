const http = require("http");

const server = http.createServer((req, res) => {
    const { method, url } = req;
    const ipAddress = req.connection.remoteAddress;
    console.log(`${method} ${url} - ${ipAddress}`);
    res.end("Hello world!");
});

const port = process.env.PORT || 8080;
console.log(`Starting Node.js HTTP server on port ${port}`);
server.listen(port);

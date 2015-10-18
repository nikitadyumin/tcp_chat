var net = require('net');
var client = new net.Socket();
client.connect(6667, '127.0.0.1', function() {
	console.log('connected');
	client.write('Hello1\n');
	client.write('Hello2');
	client.write('Hello3\r\n');
	client.write('hi1\r\nhi2\r\nhi3\nhi4\r\nhi5\nhi6\r\nhi7\r\nhi8\r\n');
	client.write('\\greetme\n');
});

client.on('data', function(data) {
	if (data.toString() === 'hi\r\n') {
		console.log('test passed');
	}
	client.destroy();
});

client.on('close', function() {
	console.log('closed');
});
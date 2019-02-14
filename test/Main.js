var cls = require('cls-hooked');
var ns = cls.getNamespace('zipkin') || cls.createNamespace('zipkin');

exports.startContext = function(sessionId) {
    return function(f) {
        return function() {
            ns.run(function() {
                ns.set("session-tracker-data", {"x-request-id": sessionId})
                f();
            });
        }
    }
}

var tempConsole = console.log;

console.log = function() {
    tempConsole(ns.get("session-tracker-data"), arguments[0]);
}
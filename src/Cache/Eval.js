exports["defineCommandJ"] = function (client, scriptName, numberOfKeys, script) {
  return function() {
    return client.defineCommand(scriptName, {
      numberOfKeys: numberOfKeys,
      lua: script
    });
  }
}

exports["runCommandJ"] = function (client, scriptName, keys, args) {
  var allArgs = keys;
  allArgs.push(args);
  return client[scriptName].apply(client, allArgs);
}
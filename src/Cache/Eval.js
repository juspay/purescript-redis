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
  allArgs = allArgs.concat(args);
  return client[scriptName].apply(client, allArgs);
}

exports["evalJ"] = function (client, script, keys, args) {
  var allArgs = [ script, keys.length ];
  allArgs = allArgs.concat(keys).concat(args);
  return client.eval(allArgs);
}
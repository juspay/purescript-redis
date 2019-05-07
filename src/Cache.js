/*
* Copyright (c) 2012-2017 "JUSPAY Technologies"
* JUSPAY Technologies Pvt. Ltd. [https://www.juspay.in]
*
* This file is part of JUSPAY Platform.
*
* JUSPAY Platform is free software: you can redistribute it and/or modify
* it for only educational purposes under the terms of the GNU Affero General
* Public License (GNU AGPL) as published by the Free Software Foundation,
* either version 3 of the License, or (at your option) any later version.
* For Enterprise/Commerical licenses, contact <info@juspay.in>.
*
* This program is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  The end user will
* be liable for all damages without limitation, which is caused by the
* ABUSE of the LICENSED SOFTWARE and shall INDEMNIFY JUSPAY for such
* damages, claims, cost, including reasonable attorney fee claimed on Juspay.
* The end user has NO right to claim any indemnification based on its use
* of Licensed Software. See the GNU Affero General Public License for more details.
*
* You should have received a copy of the GNU Affero General Public License
* along with this program. If not, see <https://www.gnu.org/licenses/agpl.html>.
*/

"use strict";

var Redis = require("ioredis");
var env = process.env.NODE_ENV || 'DEV';

function newClientPromise(client) {
  return new Promise(function (resolve, reject) {
    client.on("ready", function () {
      resolve(client);
    });

    client.on("error", function (err) {
      reject(err);
    });
  });
}

exports["_newCache"] = function (options) {
  var newClient = new Redis(options);

  newClient.on("error", errorHandler)

  return newClientPromise(newClient);
};

exports["_duplicateCache"] = function (client, options) {
  var dupClient = null;

  if (options === undefined) {
    dupClient = client.duplicate();
  } else {
    dupClient = client.duplicate(options);
  }

  dupClient.on("error", errorHandler)

  return newClientPromise(dupClient);
}

var errorHandler = function(err) {
  if (err) {
    console.log("Redis connection lost", err);
  }
};

exports["setJ"] = function(client, key, value, px, options) {
  var allArgs = [key, value];

  if (px != "") {
    allArgs.push("PX");
    allArgs.push(px);
  }

  if (options != "") {
    allArgs.push(options);
  }

  return client.set(allArgs)
}

exports["getJ"] = function(client, key) {
  return client.get(key);
};

exports["existsJ"] = function(client, key) {
  return client.exists(key);
};

exports["delJ"] = function(client, key) {
  return client.del(key);
};

exports["expireJ"] = function(client, key, ttl) {
  return client.expire(key, ttl);
}

exports["incrJ"] = function(client, key) {
  return client.incr(key);
}

exports["incrbyJ"] = function(client, key, by) {
  return client.incrby(key, by);
}

exports["publishJ"] = function(client, channel, message) {
  return client.publish(channel, message);
}

exports["subscribeJ"] = function(client, channels) {
  return client.subscribe(channels);
}

exports["setMessageHandlerJ"] = function(client, handler) {
  return function() {
    client.on("message", function (channelName, channelData) {
      handler(channelName)(channelData)()
    });
  };
}

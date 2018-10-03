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
* For Enterprise/Commercial licenses, contact <info@juspay.in>.
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
var bluebird = require("bluebird");

bluebird.promisifyAll(Redis.prototype);

var _newCache = function (options) {
  return function () {
    var newClient = new Redis(options);
    newClient.on("error", errorHandler)
    return newClient;
  };
};

var errorHandler = function(err) {
  if (err) {
    console.log("Redis connection lost", err);
  }
};

var setKeyJ = function(client) {
  return function(key) {
    return function(value) {
      return function() {
        return client.setAsync(key, value);
      };
    };
  };
}

var setexJ = function(client) {
  return function(key) {
    return function(value) {
      return function(ttl) {
        return function () {
          return client.setexAsync(key, ttl, value);
        };
      };
    };
  };
};

var getKeyJ = function(client) {
  return function(key) {
    return function () {
      return client.getAsync(key);
    };
  };
};

var delKeyJ = function(client) {
  return function(key) {
    return function () {
      return client.delAsync(key);
    };
  };
};

var expireJ = function(client) {
  return function(key) {
    return function(ttl) {
      return function () {
        return client.expire(key, ttl);
      };
    };
  };
};

var incrJ = function(client) {
  return function(key) {
    return function () {
      return client.incr(key);
    };
  };
};

var setHashJ = function(client) {
  return function(key) {
    return function(value) {
      return function () {
        return pubClient.hmset(key, value);
      };
    };
  };
};

var getHashKeyJ = function(client) {
  return function(key) {
    return function(field) {
      return function () {
        return client.hget(key, field);
      };
    };
  };
};

var publishToChannelJ = function(client) {
  return function(channel) {
    return function(message) {
      return function () {
        return client.publish(channel, message);
      };
    };
  }; 
};

var subscribeJ = function(client) {
  return function(channel) {
    return function () {
      return client.subscribe(channel);
    };
  };
};

var setMessageHandlerJ = function (client) {
  return function (handler) {
    return function () {
      return new Promise(function (resolve, reject) {
        try {
          client.on('message', function (channel, message) {
            handler(channel)(message)();
          })
          resolve();
        } catch (e) {
          reject(e);
        };
      });
    };
  };
};

exports._newCache = _newCache;
exports.setKeyJ = setKeyJ;
exports.getKeyJ = getKeyJ;
exports.setexJ = setexJ;
exports.delKeyJ = delKeyJ;
exports.expireJ = expireJ;
exports.incrJ = incrJ;
exports.setHashJ = setHashJ;
exports.getHashKeyJ = getHashKeyJ;
exports.publishToChannelJ = publishToChannelJ;
exports.subscribeJ = subscribeJ;
exports.setMessageHandlerJ = setMessageHandlerJ;

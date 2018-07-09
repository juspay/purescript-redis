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

// var Redis = require("ioredis");
var redis = require("redis");
var bluebird = require("bluebird");
var env = process.env.NODE_ENV || 'DEV';

// bluebird.promisifyAll(Redis.prototype);
bluebird.promisifyAll(redis.RedisClient.prototype);

var _newCache = function (options) {
  return function () {
    // var newClient = new Redis(options);
    var zipkinFlag = options.zipkinEnable
    var zipkinRedisFlag = options.zipkinRedis
    var newClient = null

    if (zipkinFlag === "true" && zipkinRedisFlag === "true") {
      var zipkin = require('zipkin')
      var logger = require('zipkin-transport-http')
      var CLSContext = require('zipkin-context-cls');
      var zipkinClient = require('zipkin-instrumentation-redis')

      var ctxImpl = new CLSContext()
      var endpoint = options.zipkinURL
      var serviceName = options.zipkinServiceName + '_redis'

      var recorder = new zipkin.BatchRecorder({
        logger: new logger.HttpLogger({
          endpoint: endpoint + '/api/v1/spans'
        })
      });
      var tracer = new zipkin.Tracer({localServiceName: serviceName, ctxImpl: ctxImpl, recorder: recorder})
      newClient = zipkinClient(tracer, redis, options)
    } else {
      newClient = redis.createClient(options)
    }
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
      return client.setAsync(key, value);
    };
  };
}

var setexJ = function(client) {
  return function(key) {
    return function(value) {
      return function(ttl) {
        return client.setexAsync(key, ttl, value);
      };
    };
  };
};

var getKeyJ = function(client) {
  return function(key) {
    return client.getAsync(key);
  };
};

var delKeyJ = function(client) {
  return function(key) {
    return client.delAsync(key);
  };
};

var expireJ = function(client) {
  return function(key) {
    return function(ttl) {
      return client.expire(key, ttl);
    }
  }
}

var incrJ = function(client) {
  return function(key) {
    return client.incr(key, callback);
  }
}

var callback = function(err, value) { return; }

var setHashJ = function(client) {
  return function(key) {
    return function(value) {
      return client.hmset(key, value);
    }
  }
}

var getHashKeyJ = function(client) {
  return function(key) {
    return function(field) {
      return client.hget(key, field, callback);
    }
  }
}

var publishToChannelJ = function(client) {
  return function(channel) {
    return function(message) {
      return client.publish(channel, message);
    }
  }; 
}

var subscribeJ = function(client) {
  return function(channel) {
    return client.subscribe(channel, callback);
  }
}

var getDefaultRetryStratergyJ = function() {
  return function(options) {
    return options.try_after * 1000;
  }
}

var setMessageHandlerJ = function(client) {
  return function(handler) {
    client.on("message", handler)
  }
}

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

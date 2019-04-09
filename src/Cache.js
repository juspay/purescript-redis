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

var clsBluebird = require('cls-bluebird');
var cls = require('cls-hooked');

var ns = cls.getNamespace('zipkin') || cls.createNamespace('zipkin');
clsBluebird(ns, bluebird);

bluebird.promisifyAll(redis)

exports["_newCache"] = function (options) {
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

exports["setKeyJ"] = function(client) {
  return function(key) {
    return function(value) {
      return client.setAsync(key, value);
    };
  };
}

exports["setexJ"] = function(client) {
  return function(key) {
    return function(value) {
      return function(ttl) {
        return client.setexAsync(key, ttl, value);
      };
    };
  };
};

exports["setJ"] = function(client) {
  return function(arr) {
    return client.setAsync(arr)
  }
}

exports["getKeyJ"] = function(client) {
  return function(key) {
    return client.getAsync(key);
  };
};

exports["existsJ"] = function(client) {
  return function(key) {
    return client.existsAsync(key);
  };
};

exports["delKeyJ"] = function(client) {
  return function(key) {
    return client.delAsync(key);
  };
};

exports["expireJ"] = function(client) {
  return function(key) {
    return function(ttl) {
      return client.expire(key, ttl);
    }
  }
}

exports["incrJ"] = function(client) {
  return function(key) {
    return client.incrAsync(key);
  }
}

var callback = function(err, value) { return; }

exports["setHashJ"] = function(client) {
  return function(key) {
      return function(field){
        return function(value) {
          return client.hsetAsync(key, field, value);
        }
      }
  }
}

exports["getHashKeyJ"] = function(client) {
  return function(key) {
    return function(field) {
      return client.hgetAsync(key, field);
    }
  }
}

exports["publishToChannelJ"] = function(client) {
  return function(channel) {
    return function(message) {
      return client.publish(channel, message);
    }
  }; 
}

exports["subscribeJ"] = function(client) {
  return function(channel) {
    return function() {
      return client.subscribe(channel, callback)
    }
  }
}

var getDefaultRetryStratergyJ = function() {
  return function(options) {
    return options.try_after * 1000;
  }
}

exports["setMessageHandlerJ"] = function(client) {
  return function(handler) {
    return function() {
      client.on("message", function (channelName, channelData) {
        handler(channelName)(channelData)()
      })}
  }
}

exports["rpopJ"] = function(client) {
  return function(listname) {
    return function(value) {
      return client.rpopAsync(listname, value);
    }
  }
}

exports["rpushJ"] = function(client) {
  return function(listname) {
    return function(value) {
      return client.rpushAsync(listname, value);
    }
  }
}

exports["lpopJ"] = function(client) {
  return function(listname) {
    return client.lpopAsync(listname);
  }
}

exports["lpushJ"] = function(client) {
  return function(listname) {
    return client.lpushAsync(listname);
  }
}

exports["lindexJ"] = function(client) {
  return function(listname) {
    return function(index) {
      return client.lindexAsync(listname, index);
    }
  }
}

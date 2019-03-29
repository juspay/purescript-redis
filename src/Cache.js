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

exports["_newMulti"] = function(client){
    return function(){
        return client.multi();
    }
}

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
////////////////////////////////////// Redis Streams ///////////////////////////////

exports["xackJ"] = function(client) { 
  return function(streamName) {  
    return function(groupName) {
      return function(uniqueMessageIdentifier) {
        return client.xackAsync(streamName, groupName, uniqueMessageIdentifier);
      }
    }
  }
}

exports["xaddJ"] = function(client) {
  return function(streamName) {
    return function(uniqueMessageIdentifier) {
      return function(field) {
        return function(value) {
          return client.xaddAsync(streamName, uniqueMessageIdentifier, field, value);
        }
      }
    }
  }
}

exports["xclaimJ"] = function(client) {
  return function(streamName) {
    return function(groupName) {
      return function(consumerName) {
        return function(minIdleTime) {
          return function(uniqueMessageIdentifier) {
            return client.xclaimAsync(streamName, groupName, consumerName, minIdleTime, uniqueMessageIdentifier);
          }
        }
      }
    }
  }
}

exports["xdelJ"] = function(client) {
  return function(streamName) {
    return function(uniqueMessageIdentifier) {
      return client.xdelAsync(streamName, uniqueMessageIdentifier);
    }
  }
}

// this has four subflows - create, destroy, delconsumer, setId, help
exports["xgroupJ"] = function(client) {
  return function(listname) {
    return function(index) {
      return client.xgroupAsync(listname, index);
    }
  }
}

exports["xlenJ"] = function(client) {
  return function(streamName) {
    return client.xlenAsync(streamName);
  }
}

exports["xpendingJ"] = function(client) {
  return function(listname) {
    return function(index) {
      return client.xpendingAsync(listname, index);
    }
  }
}

exports["xrangeJ"] = function(client) {
  return function(listname) {
    return function(index) {
      return client.xrangeAsync(listname, index);
    }
  }
}

exports["xreadJ"] = function(client) {
  return function(listname) {
    return function(index) {
      return client.xreadAsync(listname, index);
    }
  }
}

exports["xreadgroupJ"] = function(client) {
  return function(listname) {
    return function(index) {
      return client.xreadgroupAsync(listname, index);
    }
  }
}

exports["xrevrangeJ"] = function(client) {
  return function(listname) {
    return function(index) {
      return client.xrevrangeAsync(listname, index);
    }
  }
}



exports["xtrimJ"] = function(client) {
  return function(listname) {
    return function(index) {
      return client.xtrimAsync(listname, index);
    }
  }
}


////////////////////////////////////// Redis Streams ///////////////////////////////

exports["setMultiJ"] = function(arr){
    return function(multi){
       return multi.set(arr);
    }
}

exports["getKeyMultiJ"] = function(key){
    return function(multi){
       return multi.get(key);
    }
}

exports["setKeyMultiJ"] = function(key){
    return function(value){
        return function(multi){
            return multi.set(key,value);
        }
    }
}

exports["setexKeyMultiJ"] = function(key){
    return function(val){
        return function(ttl){
            return function(multi){
               return multi.setex(key, ttl, val);    
            }
        }
    }
}

exports["delKeyMultiJ"] = function(key){
    return function(multi){
        return multi.del(key);
    }
}

exports["expireMultiJ"] = function(key){
    return function(ttl){
        return function(multi){
           return multi.expire(key,ttl);
        }
    }
}

exports["incrMultiJ"] = function(key){
    return function(multi){
            return multi.incr(key);
    }
}

exports["setHashMultiJ"] = function(key){
    return function(field){
        return function(value){
            return function(multi){
               return multi.hset(key, field, value);
            }
        }
    }
}

exports["getHashMultiJ"] = function(key){
    return function(field){
        return function(multi){
            return multi.hget(key, field);
        } 
    }
}

exports["publishCMultiJ"] = function(channel){
    return function(message){
        return function(multi){
            return multi.publish(channel,message);
        }
    }
}

exports["subscribeMultiJ"] = function(channel){
    return function(multi){
        return multi.subscribe(channel);
    }
}

exports["rpopMultiJ"] = function(listName){
    return function(value){
        return function(multi){
            return multi.rpop(listName,value);
        }
    }
}

exports["rpushMultiJ"] = function(listName){
    return function(value){
        return function(multi){
            return multi.rpush(listName,value);
        }
    }
}

exports["lpopMultiJ"] = function(listName){
    return function(multi){
        return multi.lpop(listName);
    }
}

exports["lpushMultiJ"] = function(listName){
    return function(multi){
        return multi.lpush(listName);
    }
}

exports["lindexMultiJ"] = function(listName){
    return function(value){
        return function(multi){
            return multi.lindex(listName, value);
        }
    }
}

exports["execMulti"] = function(multi){
    return multi.execAsync();
}



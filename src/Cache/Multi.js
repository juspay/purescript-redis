/*
* Copyright (c) 2019 "JUSPAY Technologies"
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

exports["newMultiJ"] = function(client){
  return client.multi();
}

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

exports["execMultiJ"] = function(multi){
  return multi.execAsync();
}

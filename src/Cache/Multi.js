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

exports["newMultiJ"] = function (client) {
  return function () {
    return client.multi();
  }
}

exports["setMultiJ"] = function (key, value, px, options, multi) {
  return function () {
    var allArgs = [key, value];

    if (px != "") {
      allArgs.push("PX");
      allArgs.push(px);
    }

    if (options != "") {
      allArgs.push(options);
    }

    return multi.set(allArgs)
  }
}

exports["getMultiJ"] = function (key, multi) {
  return function () {
    return multi.get(key);
  }
}

exports["delMultiJ"] = function (key, multi) {
  return function () {
    return multi.del(key);
  }
}

exports["expireMultiJ"] = function (key, ttl, multi) {
  return function () {
    return multi.expire(key, ttl);
  }
}

exports["incrMultiJ"] = function (key, multi) {
  return function () {
    return multi.incr(key);
  }
}

exports["hsetMultiJ"] = function (key, field, value, multi) {
  return function () {
    return multi.hset(key, field, value);
  }
}

exports["hgetMultiJ"] = function (key, field, multi) {
  return function () {
    return multi.hget(key, field);
  }
}

exports["publishMultiJ"] = function (channel) {
  return function (message) {
    return function (multi) {
      return function () {
        return multi.publish(channel, message);
      }
    }
  }
}

exports["subscribeMultiJ"] = function (channel, multi) {
  return function () {
    return multi.subscribe(channel);
  }
}

exports["rpopMultiJ"] = function (listName, value, multi) {
  return function () {
    return multi.rpop(listName, value);
  }
}

exports["rpushMultiJ"] = function (listName, value, multi) {
  return function () {
    return multi.rpush(listName, value);
  }
}

exports["lpopMultiJ"] = function (listName) {
  return function (multi) {
    return function () {
      return multi.lpop(listName);
    }
  }
}

exports["lpushMultiJ"] = function (listName, multi) {
  return function () {
    return multi.lpush(listName);
  }
}

exports["lindexMultiJ"] = function (listName, value, multi) {
  return function () {
    return multi.lindex(listName, value);
  }
}

exports["execMultiJ"] = function (multi) {
  return multi.execAsync();
}

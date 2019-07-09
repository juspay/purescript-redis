/*
* Copyright (c) 2012-2019 "JUSPAY Technologies"
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

// Streams API

// Connection/Multi, function name as string, args as an array
function dispatch(object, func, args) {
  if (object.constructor.name === "Multi") {
    // The Multi object needs to provide an Eff with the actual call
    return function () {
      return object[func](args);
    };
  } else {
    // The Connection object needs to call the corresponding *Async() function
    // which will provide a Promise object
    return object[func + "Async"](args);
  }
}

// Not exposing MAXLEN, assuming that will be done with an explicit xtrim
exports["xaddJ"] = function (client, key, id, args) {
  var allArgs = [key, id].concat(args);
  return dispatch(client, "xadd", allArgs);
}

exports["xdelJ"] = function (client, key, id) {
  return dispatch(client, "xdel", [key, id]);
}

exports["xlenJ"] = function (client, key) {
  return dispatch(client, "xlen", key);
}

// Assumes no count if count == 0
exports["xrangeJ"] = function (client, key, start, end, count) {
  if (count > 0) {
    return dispatch(client, "xrange", [key, start, end, "COUNT", count]);
  } else {
    return dispatch(client, "xrange", [key, start, end]);
  }
}

// Assumes no count if count == 0
exports["xrevrangeJ"] = function (client, key, start, end, count) {
  if (count > 0) {
    return dispatch(client, "xrevrange", [key, start, end, "COUNT", count]);
  } else {
    return dispatch(client, "xrevrange", [key, start, end]);
  }
}

// Assumes no count if count == 0
exports["xreadJ"] = function (client, count, streams, ids) {
  var allArgs = [];

  if (count > 0) {
    allArgs.push("COUNT");
    allArgs.push(count);
  }

  allArgs.push("STREAMS");
  allArgs = allArgs.concat(streams).concat(ids);

  return dispatch(client, "xread", allArgs);
}

exports["xtrimJ"] = function (client, key, strategy, approx, len) {
  if (approx) {
    return dispatch(client, "xtrim", [key, strategy, "~", len]);
  } else {
    return dispatch(client, "xtrim", [key, strategy, len]);
  }
}

// Consumer group API

exports["xgroupCreateJ"] = function (client, key, groupName, fromId) {
  return dispatch(client, "xgroup", ["CREATE", key, groupName, fromId]);
}

exports["xgroupDestroyJ"] = function (client, key, groupName) {
  return dispatch(client, "xgroup", ["DESTROY", key, groupName]);
}

exports["xgroupDelConsumerJ"] = function (client, key, groupName, consumerName) {
  return dispatch(client, "xgroup", ["DELCONSUMER", key, groupName, consumerName]);
}

exports["xgroupSetIdJ"] = function (client, key, groupName, entryId) {
  return dispatch(client, "xgroup", ["SETID", key, groupName, entryId]);
}

// Assumes no count if count == 0
exports["xreadGroupJ"] = function (client, groupName, consumerName, count, noack, streams, ids) {
  var allArgs = ["GROUP", groupName, consumerName];

  if (count > 0) {
    allArgs.push("COUNT");
    allArgs.push(count);
  }

  if (noack) {
    allArgs.push("NOACK");
  }

  allArgs.push("STREAMS");
  allArgs = allArgs.concat(streams).concat(ids);

  return dispatch(client, "xreadgroup", allArgs);
}

exports["xackJ"] = function (client, key, groupName, ids) {
  var allArgs = [key, groupName].concat(ids);
  return dispatch(client, "xack", allArgs);
}

// Ignoring the additional optional arguments except FORCE as they do not seem
// immediately useful
exports["xclaimJ"] = function (client, key, groupName, consumerName, minIdleTime, ids, force) {
  var allArgs = [key, groupName, consumerName, minIdleTime].concat(ids);

  if (force) {
    allArgs.push("FORCE");
  }

  return dispatch(client, "xclaim", allArgs);
}

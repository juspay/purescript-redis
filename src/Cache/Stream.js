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

// Not exposing MAXLEN, assuming that will be done with an explicit xtrim
exports["xaddJ"] = function(client, key, id, args) {
  var allArgs = [key, id].concat(args);
  return client.xaddAsync.apply(client, allArgs);
}

exports["xdelJ"] = function(client, key, id) {
  return client.xdelAsync(key, id);
}

exports["xlenJ"] = function(client, key) {
  return client.xlenAsync(key);
}

// Assumes no count if count == 0
exports["xrangeJ"] = function(client, key, start, end, count) {
  if (count > 0) {
    return client.xrangeAsync(key, start, end, "COUNT", count);
  } else {
    return client.xrangeAsync(key, start, end);
  }
}

// Assumes no count if count == 0
exports["xrevrangeJ"] = function(client, key, start, end, count) {
  if (count > 0) {
    return client.xrevrangeAsync(key, start, end, "COUNT", count);
  } else {
    return client.xrevrangeAsync(key, start, end);
  }
}

// Assumes no count if count == 0
exports["xreadJ"] = function(client, count, streams, ids) {
  var allArgs = [];

  if (count > 0) {
    allArgs.push("COUNT").push(count);
  }

  allArgs.push("STREAMS");
  allArgs = allArgs.concat(streams).concat(ids);

  return client.xreadAsync(allArgs);
}

exports["xtrimJ"] = function(client, key, strategy, approx, len) {
  if (approx) {
    return client.xtrimAsync(key, strategy, "~", len);
  } else {
    return client.xtrimAsync(key, strategy, len);
  }
}

// Consumer group API

exports["xgroupCreateJ"] = function(client, key, groupName, fromId) {
  return client.xgroupAsync("CREATE", key, groupName, fromId);
}

exports["xgroupDestroyJ"] = function(client, key, groupName) {
  return client.xgroupAsync("DESTROY", key, groupName);
}

exports["xgroupDelConsumerJ"] = function(client, key, groupName, consumerName) {
  return client.xgroupAsync("DELCONSUMER", key, groupName, consumerName);
}

exports["xgroupSetIdJ"] = function(client, key, groupName, entryId) {
  return client.xgroupAsync("SETID", key, groupName, entryId);
}

// Assumes no count if count == 0
exports["xreadGroupJ"] = function(client, groupName, consumerName, count, noack, streams, ids) {
  var allArgs = ["GROUP", groupName, consumerName];

  if (count > 0) {
    allArgs.push("COUNT").push(count);
  }

  if (noack) {
    allArgs.push("NOACK");
  }

  allArgs.push("STREAMS");
  allArgs = allArgs.concat(streams).concat(ids);

  return client.xreadgroupAsync(allArgs);
}

exports["xackJ"] = function(client, key, groupName, ids) {
  var allArgs = [key, groupName].concat(ids);
  return client.xackAsync(allArgs);
}

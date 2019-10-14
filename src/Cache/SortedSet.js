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

exports["zaddJ"] = function (client, key, options, returnOptions, argsObj) {
  var allArgs = [key];

  if (options != "") {
    allArgs.push(options);
  }

  if (returnOptions != "") {
    allArgs.push(returnOptions);
  }

  Object.keys(argsObj).map(function(key) {
    allArgs.push(argsObj[key]); //score
    allArgs.push(key); //member
  });

  return client.zadd(allArgs);
}

exports["zrangeJ"] = function (client, key, start, stop) {
  return client.zrange(key, start, stop);
}

exports["zincrbyJ"] = function (client, key, increment, member) {
  return client.zincrby(key, increment, member);
}

exports["zremJ"] = function (client, key, memberArr) {
  return client.zrem(key, memberArr);
}

exports["zpopminJ"] = function (client, key, count) {
  return client.zpopmin(key, count);
}

exports["zpopmaxJ"] = function (client, key, count) {
  return client.zpopmax(key, count);
}

exports["zrangebyscoreJ"] = function (client, key, start, stop) {
  return client.zrangebyscore(key, start, stop);
}

exports["zremrangebyscoreJ"] = function (client, key, start, stop) {
  return client.zremrangebyscore(key, start, stop);
}

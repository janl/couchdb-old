// Licensed under the Apache License, Version 2.0 (the "License"); you may not
// use this file except in compliance with the License.  You may obtain a copy
// of the License at
//
//   http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
// WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the
// License for the specific language governing permissions and limitations under
// the License.

// A simple class to represent a database. Uses XMLHttpRequest to interface with
// the CouchDB server.

function CouchDB(name, httpHeaders) {
  this.name = name;
  this.uri = "/" + encodeURIComponent(name) + "/";
  
  // The XMLHttpRequest object from the most recent request. Callers can
  // use this to check result http status and headers.
  this.last_req = null;
  
  this.request = function(method, uri, requestOptions) {
      requestOptions = requestOptions || {}
      requestOptions.headers = combine(requestOptions.headers, httpHeaders)
      return CouchDB.request(method, uri, requestOptions);
    }

  // Creates the database on the server
  this.createDb = function() {
    this.last_req = this.request("PUT", this.uri);
    CouchDB.maybeThrowError(this.last_req);
    return JSON.parse(this.last_req.responseText);
  }

  // Deletes the database on the server
  this.deleteDb = function() {
    this.last_req = this.request("DELETE", this.uri);
    if (this.last_req.status == 404)
      return false;
    CouchDB.maybeThrowError(this.last_req);
    return JSON.parse(this.last_req.responseText);
  }

  // Save a document to the database
  this.save = function(doc, options) {
    if (doc._id == undefined)
      doc._id = CouchDB.newUuids(1)[0];

    this.last_req = this.request("PUT", this.uri  + 
        encodeURIComponent(doc._id) + encodeOptions(options),
        {body: JSON.stringify(doc)});
    CouchDB.maybeThrowError(this.last_req);
    var result = JSON.parse(this.last_req.responseText);
    doc._rev = result.rev;
    return result;
  }

  // Open a document from the database
  this.open = function(docId, options) {
    this.last_req = this.request("GET", this.uri + encodeURIComponent(docId) + encodeOptions(options));
    if (this.last_req.status == 404)
      return null;
    CouchDB.maybeThrowError(this.last_req);
    return JSON.parse(this.last_req.responseText);
  }

  // Deletes a document from the database
  this.deleteDoc = function(doc) {
    this.last_req = this.request("DELETE", this.uri + encodeURIComponent(doc._id) + "?rev=" + doc._rev);
    CouchDB.maybeThrowError(this.last_req);
    var result = JSON.parse(this.last_req.responseText);
    doc._rev = result.rev; //record rev in input document
    doc._deleted = true;
    return result;
  }

  // Deletes an attachment from a document
  this.deleteDocAttachment = function(doc, attachment_name) {
    this.last_req = this.request("DELETE", this.uri + encodeURIComponent(doc._id) + "/" + attachment_name + "?rev=" + doc._rev);
    CouchDB.maybeThrowError(this.last_req);
    var result = JSON.parse(this.last_req.responseText);
    doc._rev = result.rev; //record rev in input document
    return result;
  }
  
  this.bulkSave = function(docs, options) {
    // first prepoulate the UUIDs for new documents
    var newCount = 0
    for (var i=0; i<docs.length; i++) {
      if (docs[i]._id == undefined)
        newCount++;
    }
    var newUuids = CouchDB.newUuids(docs.length);
    var newCount = 0
    for (var i=0; i<docs.length; i++) {
      if (docs[i]._id == undefined)
        docs[i]._id = newUuids.pop();
    }
    var json = {"docs": docs};
    // put any options in the json
    for (var option in options) {
      json[option] = options[option];
    }
    this.last_req = this.request("POST", this.uri + "_bulk_docs", {
      body: JSON.stringify(json)
    });
    if (this.last_req.status == 417) {
      return {errors: JSON.parse(this.last_req.responseText)};
    }
    else {
      CouchDB.maybeThrowError(this.last_req);
      var results = JSON.parse(this.last_req.responseText);
      for (var i = 0; i < docs.length; i++) {
        if(results[i].rev)
          docs[i]._rev = results[i].rev;
      }
      return results;
    }
  }
  
  this.ensureFullCommit = function() {
    this.last_req = this.request("POST", this.uri + "_ensure_full_commit");
    CouchDB.maybeThrowError(this.last_req);
    return JSON.parse(this.last_req.responseText);
  }

  // Applies the map function to the contents of database and returns the results.
  this.query = function(mapFun, reduceFun, options, keys) {
    var options = options || {};
    this.last_req = this.request("GET", 
      this.uri + "_all_docs_by_seq" + encodeOptions({include_docs: true}));
    var docs = JSON.parse(this.last_req.responseText).rows;

    var tmp_db = [];
    for(var idx in docs) {
      tmp_db.push(docs[idx].doc);
    }

    // utility functions
    // TODO: share with view.js
    var map_result = [];
    emit = function(key, value) {
      map_result.push([key, value]);
    }

    sum = function(values) {
      var rv = 0;
      for (var i in values) {
        rv += values[i];
      }
      return rv;
    }

    for(var idx in tmp_db) {
      var doc = tmp_db[idx];
      if(doc) {
        mapFun(doc);
      }
    }

    // sort map results by key
    map_result.sort(function(a, b) {
      if(a[0] === b[0]) {
        return 0;
      } else {
        return (a[0] > b[0]) ? 1 : -1;
      }
    });

    var apply_options = function(map, opts) {
      var opts = opts || {};
      var startkey = opts.startkey || 0;
      var endkey = opts.endkey || 10000000000000000000;
      var result = [];
      for(var idx in map) {
        var row = map[idx];
        if(row[0] >= startkey && row[0] <= endkey) {
          result.push(row);
        }
      }
      return result;
    };

    map_result = apply_options(map_result, options);

    var map_result_groups = [];
    var group_map_result function(map_result) {
      for(var idx in map_result) {
        var last_row = map_result_groups[map_result_groups.length - 1] || null;
        var row = map_result[idx];
        if(last_row && last_row.key == row[0]) {
          var map_result_groups[last_row.key].push(row);
        } else {
          var map_result_groups[last_row.key] = [row];
        }
      }
      return map_result_groups;
    };
    map_result_groups = group_map_result(map_result);

    // run reduce on map
    var keys = new Array(map_result.length);
    var values = new Array(map_result.length);
    for(var i = 0; i < map_result.length; i++) {
        keys[i] = map_result[i][0];
        values[i] = map_result[i][1];
    }

    // TODO
    //  - reduce groups
    //  - treat group = false as special case?

    if(reduceFun) {
      var reduce_result = reduceFun(keys, values, false);
      return {rows: [{key:null, value: reduce_result}]};
    } else {
      var rows = map_result.map(function(kv) {
        return {"total_rows": map_result.length, "offset": options.startkey,key:kv[0], value:kv[1]};
      });
      return {"total_rows": map_result.length, "offset": options.startkey, "rows": rows};
    }
    // var body = {language: "javascript"};
    // if(keys) {
    //   body.keys = keys ;      
    // }
    // if (typeof(mapFun) != "string")
    //   mapFun = mapFun.toSource ? mapFun.toSource() : "(" + mapFun.toString() + ")";
    // body.map = mapFun;
    // if (reduceFun != null) {
    //   if (typeof(reduceFun) != "string")
    //     reduceFun = reduceFun.toSource ? reduceFun.toSource() : "(" + reduceFun.toString() + ")";
    //   body.reduce = reduceFun;
    // }
    // if (options && options.options != undefined) {
    //     body.options = options.options;
    //     delete options.options;
    // }
    // this.last_req = this.request("POST", this.uri + "_temp_view" + encodeOptions(options), {
    //   headers: {"Content-Type": "application/json"},
    //   body: JSON.stringify(body)
    // });
    // CouchDB.maybeThrowError(this.last_req);
    // return JSON.parse(this.last_req.responseText);
  }

  this.view = function(viewname, options, keys) {
    var viewParts = viewname.split('/');
    var viewPath = this.uri + "_design/" + viewParts[0] + "/_view/" 
        + viewParts[1] + encodeOptions(options);
    if(!keys) {
      this.last_req = this.request("GET", viewPath);      
    } else {
      this.last_req = this.request("POST", viewPath, {
        headers: {"Content-Type": "application/json"},
        body: JSON.stringify({keys:keys})
      });      
    }
    if (this.last_req.status == 404)
      return null;
    CouchDB.maybeThrowError(this.last_req);
    return JSON.parse(this.last_req.responseText);
  }

  // gets information about the database
  this.info = function() {
    this.last_req = this.request("GET", this.uri);
    CouchDB.maybeThrowError(this.last_req);
    return JSON.parse(this.last_req.responseText);
  }

  this.allDocs = function(options,keys) {
    if(!keys) {
      this.last_req = this.request("GET", this.uri + "_all_docs" + encodeOptions(options));      
    } else {
      this.last_req = this.request("POST", this.uri + "_all_docs" + encodeOptions(options), {
        headers: {"Content-Type": "application/json"},
        body: JSON.stringify({keys:keys})
      });      
    }
    CouchDB.maybeThrowError(this.last_req);
    return JSON.parse(this.last_req.responseText);
  }
  
  this.designDocs = function() {
    return this.allDocs({startkey:"_design", endkey:"_design0"});
  };

  this.allDocsBySeq = function(options,keys) {
    var req = null;
    if(!keys) {
      req = this.request("GET", this.uri + "_all_docs_by_seq" + encodeOptions(options));      
    } else {
      req = this.request("POST", this.uri + "_all_docs_by_seq" + encodeOptions(options), {
        headers: {"Content-Type": "application/json"},
        body: JSON.stringify({keys:keys})
      });      
    }
    CouchDB.maybeThrowError(req);
    return JSON.parse(req.responseText);
  }

  this.compact = function() {
    this.last_req = this.request("POST", this.uri + "_compact");
    CouchDB.maybeThrowError(this.last_req);
    return JSON.parse(this.last_req.responseText);
  }
  
  this.setDbProperty = function(propId, propValue) {
    this.last_req = this.request("PUT", this.uri + propId,{
      body:JSON.stringify(propValue)
    });
    CouchDB.maybeThrowError(this.last_req);
    return JSON.parse(this.last_req.responseText);
  }
  
  this.getDbProperty = function(propId) {
    this.last_req = this.request("GET", this.uri + propId);
    CouchDB.maybeThrowError(this.last_req);
    return JSON.parse(this.last_req.responseText);
  }
  
  this.setAdmins = function(adminsArray) {
    this.last_req = this.request("PUT", this.uri + "_admins",{
      body:JSON.stringify(adminsArray)
    });
    CouchDB.maybeThrowError(this.last_req);
    return JSON.parse(this.last_req.responseText);
  }
  
  this.getAdmins = function() {
    this.last_req = this.request("GET", this.uri + "_admins");
    CouchDB.maybeThrowError(this.last_req);
    return JSON.parse(this.last_req.responseText);
  }
  
  // Convert a options object to an url query string.
  // ex: {key:'value',key2:'value2'} becomes '?key="value"&key2="value2"'
  function encodeOptions(options) {
    var buf = []
    if (typeof(options) == "object" && options !== null) {
      for (var name in options) {
        if (!options.hasOwnProperty(name)) continue;
        var value = options[name];
        if (name == "key" || name == "startkey" || name == "endkey") {
          value = toJSON(value);
        }
        buf.push(encodeURIComponent(name) + "=" + encodeURIComponent(value));
      }
    }
    if (!buf.length) {
      return "";
    }
    return "?" + buf.join("&");
  }

  function toJSON(obj) {
    return obj !== null ? JSON.stringify(obj) : null;
  }
  
  function combine(object1, object2) {
    if (!object2)
      return object1;
    if (!object1)
      return object2;
      
    for (var name in object2)
      object1[name] = object2[name];
      
    return object1;
  }
  
  
}

// this is the XMLHttpRequest object from last request made by the following
// CouchDB.* functions (except for calls to request itself).
// Use this from callers to check HTTP status or header values of requests.
CouchDB.last_req = null; 


CouchDB.allDbs = function() {
  CouchDB.last_req = CouchDB.request("GET", "/_all_dbs");
    CouchDB.maybeThrowError(CouchDB.last_req);
  return JSON.parse(CouchDB.last_req.responseText);
}

CouchDB.allDesignDocs = function() {
  var ddocs = {}, dbs = CouchDB.allDbs();
  for (var i=0; i < dbs.length; i++) {
    var db = new CouchDB(dbs[i]);
    ddocs[dbs[i]] = db.designDocs();
  };
  return ddocs;
};

CouchDB.getVersion = function() {
  CouchDB.last_req = CouchDB.request("GET", "/");
  CouchDB.maybeThrowError(CouchDB.last_req);
  return JSON.parse(CouchDB.last_req.responseText).version;
}

CouchDB.replicate = function(source, target, rep_options) {
  rep_options = rep_options || {};
  var headers = rep_options.headers || {};
  CouchDB.last_req = CouchDB.request("POST", "/_replicate", {
    headers: headers,
    body: JSON.stringify({source: source, target: target})
  });
  CouchDB.maybeThrowError(CouchDB.last_req);
  return JSON.parse(CouchDB.last_req.responseText);
}

CouchDB.newXhr = function() {
  if (typeof(XMLHttpRequest) != "undefined") {
    return new XMLHttpRequest();
  } else if (typeof(ActiveXObject) != "undefined") {
    return new ActiveXObject("Microsoft.XMLHTTP");
  } else {
    throw new Error("No XMLHTTPRequest support detected");
  }
}

CouchDB.request = function(method, uri, options) {
  options = options || {};
  var req = CouchDB.newXhr();
  req.open(method, uri, false);
  if (options.headers) {
    var headers = options.headers;
    for (var headerName in headers) {
      if (!headers.hasOwnProperty(headerName)) continue;
      req.setRequestHeader(headerName, headers[headerName]);
    }
  }
  req.send(options.body || "");
  return req;
}

CouchDB.requestStats = function(module, key, test) {
  var query_arg = "";
  if(test !== null) {
    query_arg = "?flush=true";
  }

  var stat = CouchDB.request("GET", "/_stats/" + module + "/" + key + query_arg).responseText;
  return JSON.parse(stat)[module][key];
}

CouchDB.uuids_cache = [];

CouchDB.newUuids = function(n) {
  if (CouchDB.uuids_cache.length >= n) {
    var uuids = CouchDB.uuids_cache.slice(CouchDB.uuids_cache.length - n);
    if(CouchDB.uuids_cache.length - n == 0) {
      CouchDB.uuids_cache = [];
    } else {
      CouchDB.uuids_cache =
          CouchDB.uuids_cache.slice(0, CouchDB.uuids_cache.length - n);
    }
    return uuids;
  } else {
    CouchDB.last_req = CouchDB.request("GET", "/_uuids?count=" + (100 + n));
    CouchDB.maybeThrowError(CouchDB.last_req);
    var result = JSON.parse(CouchDB.last_req.responseText);
    CouchDB.uuids_cache =
        CouchDB.uuids_cache.concat(result.uuids.slice(0, 100));
    return result.uuids.slice(100);
  }
}

CouchDB.maybeThrowError = function(req) {
  if (req.status >= 400) {
    try {
      var result = JSON.parse(req.responseText);
    } catch (ParseError) {
      var result = {error:"unknown", reason:req.responseText};
    }
    throw result;
  }
}

CouchDB.params = function(options) {
  options = options || {};
  var returnArray = [];
  for(var key in options) {
    var value = options[key];
    returnArray.push(key + "=" + value);
  }
  return returnArray.join("&");
}
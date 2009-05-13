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

couchTests.filtered_map= function(debug) {
  var db = new CouchDB("test_suite_db");
  db.deleteDb();
  db.createDb();
  if (debug) debugger;

  var designDoc = {
    _id:"_design/test", // turn off couch.js id escaping?
    language: "javascript",
    views: {
      all_docs: {
        map: "function(doc) { emit(doc.integer, null); }"
      }
    }
  };
  
  var designDocFoo = {
    _id:"_design/test_foo", // turn off couch.js id escaping?
    language: "javascript",
    options: {
      filter: [{type: "foo"}]
    },
    views: {
      all_docs: {
        map: "function(doc) { emit(doc.integer, null); }"
      }
    }
  };

  var designDocBar = {
    _id:"_design/test_bar", // turn off couch.js id escaping?
    language: "javascript",
    options: {
      filter: [{type: "bar"}]
    },
    views: {
      all_docs: {
        map: "function(doc) { emit(doc.integer, null); }"
      }
    }
  };

  var designDocFooBar = {
    _id:"_design/test_foobar", // turn off couch.js id escaping?
    language: "javascript",
    options: {
      filter: [{type: "foo"}, {type: "bar"}]
    },
    views: {
      all_docs: {
        map: "function(doc) { emit(doc.integer, null); }"
      }
    }
  };

  var designDocFooBaz = {
    _id:"_design/test_foobarbaz", // turn off couch.js id escaping?
    language: "javascript",
    options: {
      filter: [{type: "foo"}, {"class": "bar"}]
    },
    views: {
      all_docs: {
        map: "function(doc) { emit(doc.integer, null); }"
      }
    }
  };

  db.save(designDoc);
  db.save(designDocFoo);
  db.save(designDocBar);
  db.save(designDocFooBar);
  db.save(designDocFooBaz);

  db.save({type:"foo", integer:1});
  db.save({type:"bar", integer:2});
  db.save({"class":"bar", integer:3});

  var res = db.view("test/all_docs");
  var foo_res = db.view("test_foo/all_docs");
  var bar_res = db.view("test_bar/all_docs");
  var foobar_res = db.view("test_foobar/all_docs");
  var foobarbaz_res = db.view("test_foobarbaz/all_docs");

  TEquals(3, res.total_rows, "number of docs in the regular view");
  TEquals(1, foo_res.total_rows, "number of docs in the foo view");
  TEquals(1, bar_res.total_rows, "number of docs in the bar view");
  TEquals(2, foobar_res.total_rows, "number of docs in the foobar view");
  TEquals(2, foobarbaz_res.total_rows, "number of docs in the foobarbaz view");

  TEquals(1, res.rows[0].key, "regular view result row 0");
  TEquals(2, res.rows[1].key, "regular view result row 1");

  TEquals(1, foo_res.rows[0].key, "foo view result");
  TEquals(2, bar_res.rows[0].key, "bar view result");

  TEquals(1, foobar_res.rows[0].key, "foobar view result row 0");
  TEquals(2, foobar_res.rows[1].key, "foobar view result row 1");

  TEquals(1, foobarbaz_res.rows[0].key, "mixed view result row 1");
  TEquals(3, foobarbaz_res.rows[1].key, "mixed view result row 2");
}

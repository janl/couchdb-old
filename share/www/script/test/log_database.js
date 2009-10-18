couchTests.log_database = function(debug) {
  var db = new CouchDB("test_suite_db", {"X-Couch-Full-Commit":"false"});
  db.deleteDb();
  db.createDb();

  if (debug) debugger;

  var log_db = new CouchDB("log", {"X-Couch-Full-Commit":"false"});
  log_db.deleteDb();

  var xhr = CouchDB.request("PUT", "/_config/log/database",{
    body : JSON.stringify("log"),
    headers: {"X-Couch-Persist": "false"}
  });

  TEquals(200, xhr.status, "should enable database logging");
  var log_count = log_db.info().doc_count;
  var req = CouchDB.request("GET", "/_sleep?time=500");
  var log_count2 = log_db.info().doc_count;
  T(log_count2 > log_count, "log database should grow");
};

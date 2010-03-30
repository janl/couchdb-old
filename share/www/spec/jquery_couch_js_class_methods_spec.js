// Specs for jquery_couch.js lines 48-156 and 415-448

describe 'jQuery couchdb'
  before
    if(typeof(old_alert) == 'undefined'){
      old_alert = alert;
    }
    alert = function(msg){
      // console.log('alert: ', msg)
    };
  end
  
  after
    alert = old_alert;
  end
  
  describe 'activeTasks'
    before_each
      db = $.couch.db("spec_db");
      db.create();
      db.saveDoc({'type':'battlestar', 'name':'galactica'});
    end
    
    after_each
      db.drop();
    end
    
    it 'should return an empty array when there are no active tasks'
      $.couch.activeTasks({
        success: function(resp){
          resp.should.eql []
        }
      });
    end
    
    it 'should return an active task'
      db.compact({
        ajaxStart: function(resp){
          $.couch.activeTasks({
            success: function(resp2){
              resp2[0].type.should.eql "Database Compaction"
              resp2[0].task.should.eql "spec_db"
              resp2[0].status.should.eql "Starting"
              resp2[0].should.include "pid"
            }
          });
        }
      });
    end
  end
  
  describe 'allDbs'
    it 'should return an array that includes a created database'
      temp_db = $.couch.db("temp_spec_db");
      temp_db.create();
      $.couch.allDbs({
        success: function(resp){
          resp.should.include "temp_spec_db"
        }
      });
      // note to self: why drop and not deleteDb like in couch.js?
      temp_db.drop();
    end
    
    it 'should return an array that does not include a database that does not exist'
      $.couch.allDbs({
        success: function(resp){
          resp.should.not.include("not_existing_temp_spec_db");
        }
      });
    end
  end
  
  describe 'config'
    it 'should get the config settings'
      $.couch.config({
        success: function(resp){
          console.log(resp)
          resp.httpd.port.should.eql window.location.port
          resp.stats.samples.should.match /\[.*\]/
          resp.native_query_servers.should.have_prop "erlang"
        }
      });
    end
    
    it 'should get a specific config setting'
      
    end
    
    it 'should delete a config setting'

    end
    
    it 'should update a config setting'
      
    end
  end
  
  describe 'session'
    it 'should return information about the session'
      $.couch.session({
        success: function(resp){
          resp.info.should.have_prop 'authentication_db'
          resp.userCtx.should.include 'name'
          resp.userCtx.roles.should.be_an Array
        }
      });
    end
  end
  
  describe 'userDb'
    
  end
  
  describe 'signup'
    
  end
  
  describe 'login'
    
  end
  
  describe 'logout'
    
  end
  
  describe 'encodeDocId'
    it 'should return the encoded docID when it is not a design document'
      $.couch.encodeDocId("viper").should.eql(encodeURIComponent("viper"))
    end
    
    it 'should encode only the name of the design document'
      $.couch.encodeDocId("_design/raptor").should.eql("_design/" + encodeURIComponent("raptor"))
    end
    
    it 'should also work when the name of the des'
      $.couch.encodeDocId("_design/battlestar/_view/crew").should.eql("_design/" + encodeURIComponent("battlestar/_view/crew"))
    end
  end
    
  describe 'info'
    it 'should return the CouchDB version'
      $.couch.info({
        success: function(resp){
          resp.couchdb.should.eql "Welcome"
          resp.version.should_match /^\d\d?\.\d\d?\.\d\d?.*/
        }
      });
    end
  end
  
  describe 'replicate'
    before_each
      db = $.couch.db("spec_db");
      db.create();
      db2 = $.couch.db("spec_db_2");
      db2.create();
      host = window.location.protocol + "//" + window.location.host ;
    end
    
    after_each
      db.drop();
      db2.drop();
    end
  
    it 'should return no_changes true when there are no changes between the dbs'
      $.couch.replicate(host + db.uri, host + db2.uri, {
        success: function(resp){
          resp.ok.should.be_true
          resp.no_changes.should.be_true
        }
      });
    end
    
    it 'should return the session ID'
      db.saveDoc({'type':'battlestar', 'name':'galactica'});
      $.couch.replicate(host + db.uri, host + db2.uri, {
        success: function(resp){
          resp.session_id.length.should.be_at_least 30
        }
      });
    end
    
    it 'should return source_last_seq'
      db.saveDoc({'type':'battlestar', 'name':'galactica'});
      db.saveDoc({'type':'battlestar', 'name':'pegasus'});
      
      $.couch.replicate(host + db.uri, host + db2.uri, {
        success: function(resp){
          resp.source_last_seq.should.eql 2
        }
      });
    end
    
    it 'should return the replication history'
      db.saveDoc({'type':'battlestar', 'name':'galactica'});
      db.saveDoc({'type':'battlestar', 'name':'pegasus'});
      
      $.couch.replicate(host + db.uri, host + db2.uri, {
        success: function(resp){
          resp.history[0].docs_written.should.eql 2
          resp.history[0].start_last_seq.should.eql 0
        }
      });
    end
    
    it 'should pass through replication options'
      db.saveDoc({'type':'battlestar', 'name':'galactica'});
      db2.drop();
      $.couch.replicate(host + db.uri, host + db2.uri, {
        error: function(status, error, reason){
          status.should.eql 500
          reason.should.match /db_not_found/
        }
      });
      
      // this is failing because the ajax call is ignoring the options. 
      $.couch.replicate(host + db.uri, host + db2.uri, {
        body : {"create_target":true},
        success: function(resp){
          resp.ok.should.eql true
          resp.history[0].docs_written.should.eql 1
        },
        error: function(status, error, reason){
          console.log('in error')
          console.log('status', status)
          console.log('error', error)
          console.log('reason', reason)
        }
      });
      
      db2.info({
        success: function(resp){
          resp.db_name.should.eql "spec_db_2"
        }
      });
    end
  end
  
  describe 'newUUID'
    
  end
end
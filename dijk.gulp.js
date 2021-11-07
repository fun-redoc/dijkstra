const gulp = require('gulp');
const fs = require('fs');
const zowe = require('@zowe/cli');
const https = require('https');
const path = require('path');

const { ZosmfRestClient, JobsConstants } = require('@zowe/cli');

const profile = {
 host: process.env.MF_HOST,
 port: process.env.MF_PORT,
 user: process.env.MF_PWD,
 password: process.env.MF_PWD,
 rejectUnauthorized: false,
 srcPath: "./src",
 jclPath: "./src/dijk2.jcl",
 compilestep: "DIJKCBL",
 runstep: "RUN",
}

var sess = null;

exports.default = function() {
//var sess = zowe.ZosmfSession.createSessCfgFromArgs(profile); ???
  sess = zowe.ZosmfSession.createBasicZosmfSession(profile);

  let cobolWatch = gulp.watch(profile.srcPath + '/*.cbl');
  cobolWatch.on('change',
    function(path, stats) {
      uploadFileAnTest(profile.user + ".CBL", sess, path);
    }
  );

  let jclWatch = gulp.watch(profile.srcPath + '/*.jcl');
  jclWatch.on('change',
    function(path, stats) {
      uploadFileAnTest(profile.user + ".JCL", sess, path);
    }
  );

};

/*
************************* helper fnctions *************************
*/

function uploadFileAnTest(pdsname, sess, path){
  console.log("CHANGED: " + path);
  let pathSplit = path.split("/"); //path.sep);
  let fileName = pathSplit[pathSplit.length-1];
  let ddName = fileName.split(".")[0];
  let dsname = pdsname + "(" + ddName  + ")";
  zowe.Upload.fileToDataset(sess, path, dsname)
    .then(
        res => {
          execJcl(sess, profile.jclPath);
        }
      , err=>{ 
          console.log("ERR while uploading " + path);
          console.log("ERR: " + err);
        }
    )
}

function execJcl(session, path) {
  fs.readFile(path, 
    (err, content) => {
      if( err ) {
        console.log("ERR loading jcl File")
      }
      else {
        zowe.SubmitJobs.submitJclNotify(session, content.toString())
          .then(job => {
            if( job.retcode === "CC 0000" ) {
              console.log("SUCCESS");
              showSpoolOutput(session, job)
            } else {
              zowe.GetJobs.getSpoolFilesForJob(session, job)
                .then(
                    jobFiles => {
                      jobFiles.forEach(
                        (value, idx, arr) => {
                          try{
                            checkStep(session, value);
                          } catch(e) {
                            console.log("ERR1: " + e);
                          }
                        }
                      )
                    },
                    err => {
                      console.log("ERR while getting job Files");
                    }
                )
            }
          }, 
          err=> {
            console.log("ERR while submitting jcl");
          }
        )
      }
    }
  )
}

function showSpoolOutput(session, job) {
  zowe.GetJobs.getSpoolFilesForJob(session, job)
    .then(
        jobFiles => {
          jobFiles.forEach(
            (value, idx, arr) => {
              // only care for stepname "RUN"
              if(value.stepname === profile.runstep) {
                loadSpool(session, value, function(s) { console.log(s) })
              }
            }
          )
        },
        err => {
          console.log("ERR while getting job Files");
        }
    )
}

function loadSpool(session, params, parserCallback) {
  let uri = JobsConstants.RESOURCE 
          + "/" + params.jobname 
          + "/" + params.jobid
          + JobsConstants.RESOURCE_SPOOL_FILES
          + "/" + params.id
          + "/records";
          //zowe jobs view sfbi JOB09288 --help
  ZosmfRestClient.getExpectString(sess,uri)
   .then(
     content => {
        parserCallback(content);
     },
     err => {
       console.log("ERR" + err)
     }
   )
}

function parseCompilerStatus(spoolString) {
  let lines = spoolString.split("\n");
  let n = lines.length;
  let retcodeLineNum = n - 2;
  let retcodeLine = lines[retcodeLineNum];
  let retcodeLineSplit = retcodeLine.split(" ");
  let retcodeLineRetCodePos = retcodeLineSplit.length - 1;
  return parseInt(retcodeLineSplit[retcodeLineRetCodePos]);
}

function parseLinkerStatus(spoolString) {

  let re = /RETURN CODE = (.*)./;
  let status = re.exec(spoolString)[1].trim();
  return parseInt(status);
}

function checkStepCobol(session, params) {
  if(params.procstep === profile.compilestep) 
  {
    try {
      loadSpool(sess, params, 
                s => {let status = parseCompilerStatus(s);
                      if(status == 0) {
                        console.log(params.stepname + ":"  + params.procstep + " COMPILATION SUCCESS");
                      } else {
                        console.log(params.stepname + ":"  + params.procstep + " COMPILATION FAILES with RC = " + status);
                      }
                      });
    } catch(e) {
      console.log("ERR2: " + e);
    }
  }
}

function checkStepLink(session, params) {
  if(params.procstep === "LKED") 
  {
    try {
      loadSpool(sess, params, 
                s => {let status = parseLinkerStatus(s);
                      if(status == 0) {
                        console.log(params.stepname + ":"  + params.procstep + " Linker SUCCESS");
                      } else {
                        console.log(params.stepname + ":"  + params.procstep + " Linker FAILES with RC = " + status);
                      }
                      });
    } catch(e) {
      console.log("ERR3: " + e);
    }
  }
}

function checkSysout(session, params) {
  // TODO check this sysout
}

function checkStep(session, params) {
  switch (params.stepname) {
    case profile.compilestep:
      loadSpool(session, params, function(s) { console.log(s)})
      break;
    default:
      //console.log("ERR: unknown step type " + JSON.stringify(params));
  }
}

function checkStep_doesntwork(session, params) {
  if(params.ddname === "SYSOUT") {
    checkSysout(session, params);
  } else {
    switch (params.procstep) {
      case "COBOL":
          checkStepCobol(session,params);
        break;
      case "LKED":
          checkStepLink(session,params);
        break;
      case profile.compilestep:
          checkStepLink(session,params);
        break;
      default:
        console.log("ERR: unknown step type " + JSON.stringify(params));
    }
  }
}

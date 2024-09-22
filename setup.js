"use strict";
// Some functions which are probably specific to my application
// Depends on papaparse.js (or better yet, papaparse.min.js).


// Returns true if we are in debug mode. Debug mode is invoked if: (1)
// the document is a local file (i.e. protocol == "file:", or (2) the
// URL has a query parameter "debug=T"
function IsDebugging() {
    return GetUrlParam("debug") == "T" || window.location.protocol == "file:";
}

// Chooses (and returns) a logger. Normally it will be a Firebase
// logger, but when debugging, it will be a console logger.  
//
// If running in debug mode, the "#watermark" style.display is cleared
function ChooseLogger(debug, firestore) {
    // Get the data logger
    var logger = null;
    if (debug) {
        // This logger just writes to the console
        logger = new ConsoleDataLogger(GetUserId());
        // Display debug symbol
        var ele = document.getElementById("watermark");
        if (ele)
            ele.style.display = "";
    } else {
        // This logger writes to a Firebase database
        //InitFirebase();
        logger = new FirebaseLogger(GetUserId(), firestore);
    }
    return logger;
}

/** Creates a set of spans within the "progress" element. */
function CreateProgressIndicators(numPhotos) {
    const prog = document.getElementById("progress");
    for (let i = 0; i < numPhotos; i++) {
        const span = document.createElement("span");
        span.style.width = Math.floor(100 * (1 / (numPhotos + 2))) + "%";
        prog.appendChild(span);
    }
}

// Downloads the photo list and starts the trial.
// Assumes that papaparse is loaded.
function PrepareAndStartTrial(logger, photosPerTrial, photosCsvUrl, photoEleId, escapeTimeout, animationDuration) {

    // Invoked once we have the list of photos, obtained by reading the CSV file
    function prepare(candidatePhotos) {
        // Prepare the photo list from the candidate photos
        var photos = new PhotoSeq(candidatePhotos, photosPerTrial);

        // Start the trial
        new Trial(logger, photos, photoEleId, escapeTimeout, animationDuration, new Sounds(ENABLE_SOUNDS), revealTime).prepare();
    }

    ReadPhotosCSV(photosCsvUrl, prepare);
}

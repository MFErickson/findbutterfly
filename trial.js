"use strict";
/* Trial logic HTML assumptions: 
The img element:
  id = configurable
  class = configurable
  While an image is loading, it (temporarily) has class "loading". 
  When the image has been scored, the score name (e.g. "ant") is added to the class list.

The (optional) scoring buttons:
  class = "score"
  id = score value (e.g. "ant")

When the trial is finished, the URL is set to "finish.html".
*/

// =======================
// General functions

// General function to insert a DOM node after an existing node
function insertAfter(newNode, referenceNode) {
    referenceNode.parentNode.insertBefore(newNode, referenceNode.nextSibling);
}

// General function to remove a DOM node
function removeElement(element) {
    var par = element.parentNode;
    if (par != null)
        par.removeChild(element);
}

function GetUrlParam(name) {
    return new URLSearchParams(window.location.search).get(name);
}

// =========================

// A class which controls the logic of a trial
class Trial {
    // Creates a new Trial instance.
    // @constructor
    // @param {Logger} logger Data logger.
    // @param {PhotoSeq} photos the photo pool.
    // @param (string) photoEleId ID of the DOM IMG element that displays photos.
    // @param {number} escapeTimeout Duration (milliseconds) before animals escape. If <= 0, animals don't escape.
    // @param {number} animationDuration the animation duration
    //                 (milliseconds) so we can use setTimeout rather than
    //                 transitionend events.
    // @param {Sounds} soundEffects Sound effects for user feedback.
    // @param {number} Duration of revealed image in milliseconds.
    constructor(logger, photos, photoEleId, escapeTimeout, animationDuration, soundEffects, revealTime) {
        this.logger = logger;
        this.photos = photos;
        this.photoEleId = photoEleId;
        this.escapeTimeout = escapeTimeout;
        this.allowEscape = escapeTimeout > 0;
        this.animationDuration = animationDuration;
        this.soundEffects = soundEffects;
        this.revealTime = revealTime;
        
        this.startTime = 0;
        this.timeoutId = null;
        this.timerId = null;
        this.mistakes = [];
        this.totalScored = 0;
        this.buttonsDisabled = false;
    }

    // Gets the game ready to run. Displays the first photo, sets up
    // score button handlers and keyboard shortcuts, and starts the
    // timer used to trigger escapes and record user decision times.
    prepare(shortcutKeys) {

        // Setup click event handlers on buttons
        var scoreBtns = document.querySelectorAll(".score");
        var self = this;
        scoreBtns.forEach(function(elem) {
            elem.addEventListener("click", (e) => {
                self.userScore(e.currentTarget.id);
            })
        });

        if (shortcutKeys) {
            // Setup keyboard shortcut keys
            function handleKey(e) {
                var score = shortcutKeys[e.key];
                if (typeof(score) !== typeof undefined) {
                    self.userScore(score);
                }
            }
            document.addEventListener("keydown", handleKey);
        }

        this.loadCurrentPhoto();
    }


    // =======================
    // Game functions

    // "Disable" user input
    // It isn't really disabled, but scoring doesn't function while disabled
    disableButtons() {
        //console.log("DISABLE");
        this.buttonsDisabled = true;
    }

    // "Enable" the "Ant" and "Non ant" buttons.
    enableButtons() {
        this.buttonsDisabled = false;
    }

    /** Draws the image to the canvas. */
    drawImage(cnv, photos, back, butt) {
        const ctx = cnv.getContext('2d');
        // Draw the background
        ctx.drawImage(back, 0, 0, cnv.width, cnv.height);

        // Draw the butterfly
        const width = Math.round(butt.naturalWidth * buttScale);
        const w2 = width / 2
        const height = Math.round(butt.naturalHeight * buttScale);
        const h2 = height / 2;
        const size = Math.max(width, height) * 2;
        
        // Origin is 0 - 1, and is used as the centre of the image;
        // scale up to available canvas size. Leave a buffer of image
        // size/2 around the outside
        const cx = Math.round(photos.currentX * (cnv.width - size) + size / 2);
        const cy = Math.round(photos.currentY * (cnv.height - size) + size / 2);
        ctx.translate(cx, cy);
        ctx.rotate(photos.currentRot);
        ctx.drawImage(butt, -w2, -h2, width, height);

        // Save butterfly position for logging
        this.pos = {cx: cx - w2, cy: cy - h2, width: width, height: height, rot: photos.currentRot};
        
        // Prepare the ROI for hit testing
        const b = 5;   // Buffer to ease clicking on small butterflies
        const roi = new ROI([[-w2 - b, -h2 - b], [-w2 - b, h2 + b], [w2 + b, h2 + b], [w2 + b, -h2 - b], [-w2 - b, -h2 - b]]);

        roi.stroke(ctx);
    }

    /** Handle a successful hit by the user. */
    handleClickHit() {
        this.soundEffects.playHit();
        this.userScore("hit");
    }

    /** Handle an unsuccessful hit by the user. */
    handleClickMiss() {
        this.soundEffects.playMiss();
        // Record the miss
        var nMilliSecs = Date.now() - this.startTime;
        this.logger.logImageScore(this.photos.currentButterfly.URL, this.photos.currentBackground.URL,
                                  this.pos, "miss", nMilliSecs);
    }

    /** Processes a click event on the canvas. */
    handleClick(e) {
        if (this.buttonsDisabled) {
            console.log("Ignoring click, input disabled");
            return false;
        }

        const ctx = e.target.getContext('2d');
        const hit = ROI.hitTest(ctx, e.offsetX, e.offsetY);
        if (hit)
            this.handleClickHit();
        else
            this.handleClickMiss();
    }

    /** Loads a new photo. A new image element is created, its src
        attribute is set to the specified url of the current butterfly
        photo, and its class is set to this.photoEleId and
        "loading". Once the image has been loaded, the element is
        added to the document, its ID set to "photo", "loading" is
        removed from its class, and the countdown timer is started. */
    async loadCurrentPhoto() {
        const [butt, back] = await this.photos.loadCurrent();

        // We want the new element to be briefly visible at the same time as the old element
        var old = document.getElementById(this.photoEleId);
        var oldPar = old.parentElement;

        // Create a new canvas element
        const cnv = document.createElement("canvas");
        // Set the canvas drawing area == element size or else it gets scaled!
        cnv.width = oldPar.offsetWidth;
        cnv.height = oldPar.offsetHeight;
        // cnv.width = old.width;
        // cnv.height = old.height;
        cnv.className = this.photoEleId + " loading";
        // Clear ID on old element, and...
        old.id = null;
        // set it on the new one
        cnv.id = this.photoEleId;
        
        // Setup click handler
        cnv.addEventListener('click', e => { this.handleClick(e); });
        
        // Add the new element after the existing element
        insertAfter(cnv, old);

        // Draw the image to the canvas
        this.drawImage(cnv, this.photos, back, butt);

        // Calling setTimeout is an ugly hack to get the display to animate. It can fail
        setTimeout(function() {cnv.classList.remove("loading"); }, 100);

        // Another ugly thing - CSS animations are not run when the
        // tab is inactive, so use a timeout rather than transitionend
        // event to ensure the buttons are always enabled. Hard-wire
        // the animation duration
        setTimeout(e => {
            //img.removeEventListener("transitionend", photoDisplayed, false);
            this.enableButtons();
            // New image is now displayed, get rid of the old one
            removeElement(old);
        }, this.animationDuration);

        // Start the user's timer
        this.startTiming();
    }

    showTimeInSecs(secs) {
        const bar = document.getElementById("bar");
        const pc = 100 * secs / (this.escapeTimeout / 1000);
        bar.style.width = Math.min(100, pc) + "%";
    }

    showTotalProgress() {
        const spans = document.querySelectorAll('#progress span');
        for (let i = 0; i < this.photos.currentIndex; i++) {
            spans[i].classList.add('done');
        }
    }

    revealAnswer(cnv) {
        const ctx = cnv.getContext('2d')
        ctx.strokeStyle = "red";
        ctx.lineWidth = 4;
        ctx.stroke();

        // Give them a chance to see it
        return new Promise(resolve => setTimeout(resolve, this.revealTime));
    }

    // Starts the timer, and sets a timeout to score the current image as
    // "escape" once the timeout has elapsed.
    startTiming() {
        this.startTime = Date.now();
    
        // Update timer regularly
        var self = this;
        this.timerId = setInterval(function() {
            var nMilliSecs = Date.now() - self.startTime;
            self.showTimeInSecs(nMilliSecs / 1000);
        }, 20);

        function timeoutFired() {
            self.timeoutId = null;
            if (self.allowEscape) {
                self.soundEffects.playEscape();
                self.userScore("escape");
            }
        }
        this.timeoutId = setTimeout(timeoutFired, this.escapeTimeout);
    }

    // Stops the timer
    stopTiming() {
        var nMilliSecs = Date.now() - this.startTime;
        clearInterval(this.timerId);
        return nMilliSecs;
    }

    // Called when there are no more images to be scored. Saves the total
    // time in a cookie, and displays the finish page
    trialFinished() {
        setCookie("totalTime", this.logger.totalElapsed);
        setCookie("totalScored", this.totalScored);
        setCookie("errors", JSON.stringify(this.mistakes));
        // Save the session ID so we can optionally report user's results if we decide that's a good idea
        setCookie("sessionId", this.logger.sessionId);
        // Browse to the finish page
        window.location = "finish.html"; 
    }
    
    // Records the user's classification for the current image.
    // Animates the current image away (by setting its class to the
    // value of score - the actual animation should happen in CSS),
    // and loads the next image.
    async userScore(score) {
        // Find the photo element
        var ie = document.getElementById(this.photoEleId);
        
        // Once the butterfly has "escaped", the user can no longer interact with the butterfly
        if (this.buttonsDisabled) {
            //console.log("Ignoring score, buttons disabled");
            return false;
        }
        
        // Cancel the timeout since the image has been scored
        if (this.timeoutId) {
            clearTimeout(this.timeoutId);
            this.timeoutId = null;
        }
        
        if (!this.photos.hasCurrentButterfly) {
            //console.log("Skipping score - no current photo (" + this.photos.index + " of " + this.photos.numToShow + ")");
            return false;
        }
        
        this.disableButtons();

        var nMilliSecs = this.stopTiming();
        this.logger.logImageScore(this.photos.currentButterfly.URL, this.photos.currentBackground.URL,
                                  this.pos, score, nMilliSecs);
        
        this.totalScored++;
        // Record mistakes. Assume "hit" is always correct
        if (score != "hit") {
            this.mistakes.push(this.photos.currentButterfly.URL);

            // Show the user the correct answer
            await this.revealAnswer(ie);
        }
        
        // Move on to the next image
        var morePhotos = this.photos.moveToNext;
        ie.classList.add(score);
        var self = this;
        function onTransEnd(e) {
            // Hide it, but don't remove it because it's a place holder for the next image
            ie.style.display = "none";
            if (!morePhotos)
                self.trialFinished();
        }
        // Don't use transitionend since it doesn't fire if tab is inactive
        //ie.addEventListener("transitionend", onTransEnd, false);
        setTimeout(onTransEnd, this.animationDuration);
        
        // Load next image
        if (morePhotos) {
            this.loadCurrentPhoto();
        }
        
        // Show progress through list of photos
        this.showTotalProgress();

        return true;
    }
}

// ================================================================================
// Sound effects

class Sounds {
    constructor (enableSounds) {
        this.enableSounds = enableSounds;
        this.hit = new Audio('hit.wav');
        this.miss = new Audio('miss.wav');
        this.escape = new Audio('escape.wav');
    }
    
    playHit() {
        if (this.enableSounds)
            this.hit.play();
    }
    
    playMiss() {
        if (this.enableSounds)
            this.miss.play();
    }
    
    playEscape() {
        if (this.enableSounds)
            this.escape.play();
    }
}


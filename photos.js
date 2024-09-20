"use strict";

// The PhotoSeq class represents a random sequence of butterfly photos
// and randomly assigned backgrounds which make up a single trial,
// with a current photo.
class PhotoSeq {
    /** 
     * Constructs a PhotoSeq instance. Randomly selects numToShow
     * butterflies and backgrounds from candidatePhotos.
     *
     * @param {array} candidatePhotos Array of photos to select from. 
     */
    constructor(candidatePhotos, numToShow) {
        
        this.index = 0;
        this.butterflies = this.selectRandom(candidatePhotos.filter(x => x.What == "Butt"), numToShow);
        this.backgrounds = this.selectRandom(candidatePhotos.filter(x => x.What == "Back"), numToShow);
        // Butterfly image origin, random from 0 to 1
        this.x = Array.from({length: numToShow}, () => Math.random());
        this.y = Array.from({length: numToShow}, () => Math.random());
        // Butterfly image rotation, random from 0 to 2 pi
        this.rot = Array.from({length: numToShow}, () => Math.random() * 2 * Math.PI);
    }

    // Progresses the current photo to the next photo in the trial
    get moveToNext() {
        return ++this.index < this.butterflies.length;
    }
    get currentIndex() { return this.index; }
    get currentButterfly() { return this.butterflies[this.index]; }
    get currentX() { return this.x[this.index]; }
    get currentY() { return this.y[this.index]; }
    get currentRot() { return this.rot[this.index]; }
    get currentBackground() { return this.backgrounds[this.index]; }
    get hasCurrentButterfly() { return this.index < this.butterflies.length; }
    get percentComplete() { return 100 * (this.index + 1) / this.butterflies.length; }

    // Used internally. Selects the photos to be used in the trial
    // from the list of candidate photos
    selectRandom(photos, numToShow) {
        // Shuffle then just take the first n photos
        shuffleArray(photos);
        return photos.slice(0, numPhotos);
    }

    loadCurrent() {
        const buttP = LoadImage(this.butterflies[this.index].URL);
        const backP = LoadImage(this.backgrounds[this.index].URL);
        return Promise.all([buttP, backP]);
    }
};

function shuffleArray(a) {

    for (let i = a.length - 1; i > 0; i--) {
        let j = Math.floor(Math.random() * (i + 1)); // Random index from 0 to i
        [a[i], a[j]] = [a[j], a[i]]                  // Swap elements
    }
}

function ReadPhotosCSV(photosCsvUrl, callback) {
    Papa.parse(photosCsvUrl, {
	download: true,
        header: true,
        skipEmptyLines: true,
        dynamicTyping: true,
	complete: function(results) {
            if (results.errors.length > 0) {
                alert("Unable to read the list of photos to be displayed: " + results.errors[0].message + ", row " + results.errors[0].row);
            } else {
                // Call the callback
                callback(results.data);
            }
	}
    });
}



const LoadImage = url => {
    return new Promise((resolve, reject) => {
        var image = new Image();
        image.addEventListener('load', () => resolve(image));
        image.addEventListener('error', reject);
        image.src = url;
    });
}
    

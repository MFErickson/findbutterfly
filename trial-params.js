// Photo timeout (i.e. time until "escape") in milliseconds
const escapeTimeout = 10000;

// Time to show "revealed" image after user failure to detect (milliseconds)
const revealTime = 1000;

// Number of photos to show in a single trial
const numPhotos = 30;

// Amount to scale butterflies by - before use, it is multipled by the
// background element width to account for differences between browsers
const buttScale = 0.03 / 500;

// Where do we get photo info from?
const PHOTOS_INFO_URL = "photo_info.csv";

// Should we play sounds?
const ENABLE_SOUNDS = true;

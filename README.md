# Find the Butterfly

This project contains a game-like web application which requires
participants to view a series of photos of butterflies, and locate the
insect against the background. It is used to assess how conspicuous different butterflies are.

This project is hosted on GitHub pages. The game can be accessed at
https://XXXXXXXXX.github.io/findbutterfly/

For local development, you can use the free Mongoose server from
https://mongoose.ws/binary/. Copy the executable into this directory,
then just click it and it serves up the current directory on port
8080. It is useful for locally testing Javascript, cookies and reading
files. Once it is running, there is an icon in the system tray that
can be used to stop it.

The Folder `R` is not part of the web application. It contains scripts to build the photo_info.csv file and to download the game data from Firestore.

## TODO
- Optionally replace the sounds effects files, "hit.wav", "miss.wav" and "escape.wav". For now, I just copied some crappy Windows sounds.
- Edit (or scrap) the start pages: index.html, consent.html, start.html. Current sequence of pages is:
  - index.html - the first page that people will see, introduces the game. Clicking the "Play now" button leads to...
  - consent.html - depends on the ethics approval as to whether you need this page or not. Clicking on "I agree" leads to...
  - start.html - allows user to specify whether or not they have played before. Clicking either button leads to...
  - trial.html - the game. Once finished, leads to...
  - finish.html - summarises user performance. Possibilities are a leaderboard, allow user to enter email address to be informed about the research outcome.
- Edit the finish page: finish.html.

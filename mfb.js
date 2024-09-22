/*
  This command updated the database:

  curl -X PUT -d '{ "test": { "field 1": 1, "field 2": 2} }' "https://conspicuous-butterflies-default-rtdb.asia-southeast1.firebasedatabase.app/butt-scores.json" -i

*/
// Firestore realtime database
    const firebaseConfig = {
        apiKey: "AIzaSyCgVh3CPVMWYEkP1GNw_52_P_j_63K0Ens",
        authDomain: "conspicuous-butterflies.firebaseapp.com",
        databaseURL: "https://conspicuous-butterflies-default-rtdb.asia-southeast1.firebasedatabase.app",
        projectId: "conspicuous-butterflies",
        storageBucket: "conspicuous-butterflies.appspot.com",
        messagingSenderId: "925729905337",
        appId: "1:925729905337:web:9990488d59bc9ea6769968"
    };


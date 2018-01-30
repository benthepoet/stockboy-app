// Import modules
var config = require('./config'),
    Elm = require('./elm/Main.elm');
    
// Import SASS
require('./sass/style.scss');

// Set flags
var flags = {
    token: getItem(config.STORAGE.TOKEN)
};

// Run the application
var app = Elm.Main.fullscreen(flags);

// Set subscriptions
app.ports.syncToken.subscribe(setItem.bind(null, config.STORAGE.TOKEN));

function getItem(key) {
    return sessionStorage.getItem(key);
}

function setItem(key, value) {
    if (!value) {
        sessionStorage.removeItem(key);
    } else {
        sessionStorage.setItem(key, value);
    }
}
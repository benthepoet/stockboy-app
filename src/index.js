require('./sass/style.scss');
const Elm = require('./elm/Main.elm');

const TOKEN_KEY = 'token';

const app = Elm.Main.fullscreen({ token: getItem(TOKEN_KEY) });

app.ports.syncToken.subscribe(setItem.bind(null, TOKEN_KEY));

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
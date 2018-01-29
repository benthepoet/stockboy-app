require('./sass/style.scss');
const Elm = require('./elm/Main.elm');

const { ports } = Elm.Main.fullscreen({ token: getItem('token') });
ports.syncToken.subscribe(setItem.bind(null, 'token'));

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
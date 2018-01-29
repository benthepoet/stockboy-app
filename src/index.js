require('./sass/style.scss');
const Elm = require('./elm/Main.elm');

const FLAGS = {
    token: getItem('token')
};

const app = Elm.Main.fullscreen(FLAGS);
app.ports.syncToken.subscribe(setItem.bind(null, 'token'));

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
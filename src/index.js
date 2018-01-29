require('./sass/style.scss');
const Elm = require('./elm/Main.elm');

const TOKEN_KEY = 'token';

const node = document.querySelector('#main');
const app = Elm.Main.embed(node, { token: getToken() });

app.ports.syncToken.subscribe(setToken);

function getToken() {
    return sessionStorage.getItem(TOKEN_KEY);
}

function setToken(value) {
    if (!value) {
        sessionStorage.removeItem(TOKEN_KEY);
    } else {
        sessionStorage.setItem(TOKEN_KEY, value);
    }
}
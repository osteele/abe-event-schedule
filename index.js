import './src/Main.scss';
import Elm from './src/Main.elm';
const app = Elm.Main.embed(document.getElementById('app'), {
    apiServer: process.env.API_SERVER || 'http://localhost:3000'
});

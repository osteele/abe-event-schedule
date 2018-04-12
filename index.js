import './src/Main.scss';
import Elm from './src/Main.elm';

Elm.Main.embed(document.getElementById('app'), {
  apiServer: process.env.API_SERVER || 'http://localhost:3000',
});

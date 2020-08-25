import { Elm } from './App/Main.elm';
import registerServiceWorker from './registerServiceWorker';

const basePath = new URL(document.baseURI).pathname;

Elm.App.Main.init({
  node: document.getElementById('root'),
  flags : { basePath }
});

registerServiceWorker();

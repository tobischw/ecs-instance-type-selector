import { Elm } from './App/Main.elm';
import registerServiceWorker from './registerServiceWorker';

Elm.App.Main.init({
  node: document.getElementById('root')
});

registerServiceWorker();

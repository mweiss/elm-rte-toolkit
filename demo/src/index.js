import './css/reset.css';
import './css/main.css';
import './css/demo.css';
import './css/rte.css';
import './css/modal.css';

import '@webcomponents/webcomponentsjs/webcomponents-bundle.js'
import { Elm } from './Main.elm';
import '../../js/elmEditor';

Elm.Main.init({
  node: document.getElementById('root')
});

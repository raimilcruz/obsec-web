import React from 'react';
import ReactDOM from 'react-dom';
import injectTapEventPlugin from 'react-tap-event-plugin';

import AppRoutes from './AppRoutes';


require('./../www/main.css');
//require('highlight.js/styles/github.css');
//Needed for onTouchTap 
//Can go away when react 1.0 release
//Check this repo:
//https://github.com/zilverline/react-tap-event-plugin
injectTapEventPlugin();



// Render the main app react component into the app div.
// For more details see: https://facebook.github.io/react/docs/top-level-api.html#react.render
//ReactDOM.render((AppRoutes), document.getElementById('app')); 
ReactDOM.render((AppRoutes), document.getElementById('app')); 

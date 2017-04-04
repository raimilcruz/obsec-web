
import React from 'react';
import { render } from 'react-dom';
import { Router, Route, Link, browserHistory, useRouterHistory, IndexRoute } from 'react-router';
import {createHashHistory} from 'history';
import Index from './Index';
import Main from './Main';

const AppRoutes = (
	<Router history={useRouterHistory(createHashHistory)({queryKey: false})}>
        <Route path="/" component={Main}>
          <IndexRoute component={Index} />
        </Route>
	</Router>
);

export default AppRoutes;
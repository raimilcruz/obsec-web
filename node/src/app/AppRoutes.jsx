
import React from 'react';
import { render } from 'react-dom';
import { Router, Route, Link, browserHistory, useRouterHistory, IndexRoute } from 'react-router';
import {createHashHistory} from 'history';
import LanguagePad from './LanguagePad';
import Main from './Main';


class GLanguagePad  extends React.Component{
    render() {
        return <LanguagePad urlExamples={"gexamples"}
                            urlTypeCheck={"gtypecheck"}
                            urlExecute={"greduce"}
                            language={"gobsec"}
        />
    }
}


const AppRoutes = (
	<Router history={useRouterHistory(createHashHistory)({queryKey: false})}>
        <Route path="/" component={Main}>
          <IndexRoute component={LanguagePad} />
          <Route path={"/generic"} component={GLanguagePad}/>
        </Route>
	</Router>
);

export default AppRoutes;

import React from 'react';
import { render } from 'react-dom';
import { Router, Route, Link, browserHistory, useRouterHistory, IndexRoute } from 'react-router';
import {createHashHistory} from 'history';
import LanguagePad from './LanguagePad';
import EqJudgmentManager from './components/EqJudgmentManager';
import TypingJudgmentManager from './components/TypingJudgmentManager';

import Main from './Main';


class GLanguagePad  extends React.Component{
    render() {
        return (<LanguagePad urlExamples={"gexamples"}
                                urlTypeCheck={"gtypecheck"}
                                urlExecute={"greduce"}
                                language={"gobsec"}
            />)
    }
}


const AppRoutes = (
	<Router history={useRouterHistory(createHashHistory)({queryKey: false})}>
        <Route path="/" component={Main}>
          <IndexRoute component={GLanguagePad} />
          <Route path={"/generic"} component={GLanguagePad}/>
            <Route path={"/obsec"} component={LanguagePad}/>
            <Route path={"/eqjudgment"} component={EqJudgmentManager}/>
            <Route path={"/typingjudgment"} component={TypingJudgmentManager}/>
        </Route>
	</Router>
);

export default AppRoutes;
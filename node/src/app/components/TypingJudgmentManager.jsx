import React from "react";
import request from "superagent";
import TextField from 'material-ui/TextField';
import DerivationTree from "./DerivationTree";

class DerivationTreeManager extends React.Component{
    state = {
        rootJudgment: null,
        adquiringJudgment: false,
        expr:0,
    };
    urlGetJudgment = "getTypingJudgment";

    constructor(props) {
        super(props);
    }

    handleClick_ShowJudgment = ()=>{
        this.setState(
            {
                adquiringJudgment:true
            },
            () => {
                request.post(this.urlGetJudgment)
                    .send(
                        {
                            expr: this.state.expr,
                            context:"",
                        })
                    .set('Accept', 'application/json')
                    .end((err, res) => {
                        if (err || !res.ok) {
                            alert('Oh no! Configuration Error');
                        } else {
                            if (res.body.status === "OK") {
                                res.body.premise.stepId = res.body.premise.key;
                                this.setState({
                                    adquiringJudgment: false,
                                    rootJudgment: res.body.premise
                                });
                            }
                            else {
                                alert('Oh no! error ' + res.body.error);
                            }
                        }
                    });
            });
    };
    handleOnChange_Term=(event)=>{
        this.setState({
            expr: event.target.value
        });
    };

    render() {
        return (
            <div >
                <TextField id="lambdaCalculusTerm"  onChange={this.handleOnChange_Term}/>
                <button onClick={this.handleClick_ShowJudgment}>Show Judgment</button>
                {
                    this.state.adquiringJudgment?
                        <div>Adquiring judgment</div>:
                        <div>
                            <div>{this.state.rootJudgment!=null
                                ?<div>
                                    <DerivationTree rootDerivation={this.state.rootJudgment}/>
                                </div>
                                :null
                                }
                        </div>
                        </div>
                }
            </div>
        );
    }
}
export default DerivationTreeManager;
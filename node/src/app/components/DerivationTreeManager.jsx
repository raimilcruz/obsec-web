import React from "react";
import request from "superagent";
import TextField from 'material-ui/TextField';
import DerivationTree from "./DerivationTree";

class DerivationTreeManager extends React.Component{
    state = {
        rootJudgment: null,
        adquiringJudgment: false,
        firstNumber:0,
        secondNumber:1
    };
    urlGetJudgment = "getjudgment";

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
                            n1: this.state.firstNumber,
                            n2: this.state.secondNumber
                        })
                    .set('Accept', 'application/json')
                    .end((err, res) => {
                        if (err || !res.ok) {
                            alert('Oh no! Configuration Error');
                        } else {
                            if (res.body.status === "OK") {
                                this.setState({
                                    adquiringJudgment: false,
                                    rootJudgment: {
                                        context: res.body.premise.context,
                                        goal: res.body.premise.goal,
                                        premises:[],
                                        key : res.body.premise.key,
                                        stepId:res.body.premise.key
                                    }
                                }, () => {
                                    //TODO: extra things here

                                })
                            }
                            else {
                                alert('Oh no! error ' + res.body.error);
                            }
                        }
                    });
            });
    };
    handleOnChange_FirstNumber=(event)=>{
        this.setState({
            firstNumber: event.target.value
        });
    };
    handleOnChange_SecondNumber=(event)=>{
        this.setState({
            secondNumber: event.target.value
        });
    };

    render() {
        return (
            <div >
                <span>Insert two number to show the judgment step to derive a less than b </span>
                <TextField id="firstNumber" type="number" onChange={this.handleOnChange_FirstNumber}/>
                <TextField id="secondNumber" type="number" onChange={this.handleOnChange_SecondNumber} />
                <button onClick={this.handleClick_ShowJudgment}>Show Judgment</button>
                {
                    this.state.adquiringJudgment?
                        <div>Adquiring judgment</div>:
                        <div>
                            <div>{this.state.rootJudgment!=null
                                ?<div>
                                    <DerivationTree rootDerivation={this.state.rootJudgment}/>
                                </div>
                                :<div></div>
                                }
                        </div>
                        </div>
                }
            </div>
        );
    }
}
export default DerivationTreeManager;
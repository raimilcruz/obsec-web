import React from "react";
import request from "superagent";
const update = require('react-addons-update');
import TextField from 'material-ui/TextField';


class DerivationTree extends React.Component{
    state={
        stepNoVisible : {},
        adquiringStep:false,
        rootDerivation:null
    };
    stepJudgment = "stepjudgment";
    judgments = {};
    judgmentParents = {};
    counter = 0;

    constructor(props) {
        super(props);
        //receive props.rootDerivation
    }
    componentDidMount(){
        console.log("component did mount");
        this.judgments[this.props.rootDerivation.stepId]=this.props.rootDerivation;
    }

    handleClick_StepJudgment = (stepId)=>{
        console.log("handle click " + stepId);
        this.setState({
            adquiringStep:true,
            stepNoVisible :update(this.state.stepNoVisible,{[stepId]: {$set: true}})
        },()=> {
            request.post(this.stepJudgment)
                .send(this.judgments[stepId])
                .set('Accept', 'application/json')
                .end((err, res) => {
                    if (err || !res.ok) {
                        alert('Oh no! Configuration Error');
                    } else {
                        if (res.body.status === "OK") {
                            this.modifyJudgment(this.judgments[stepId],res.body.step,stepId);
                            this.forceUpdate();
                            this.setState({
                                adquiringStep: false,
                            }, () => {
                            })
                        }
                        else {
                            alert('Oh no! error ' + res.body.error);
                        }
                    }
                });
        });
    };

    inferRuleClassName =(judgment)=>{
        return (judgment.status)
        ?(judgment.status === 1)
                ?"infer-step-done"
                : (judgment.kind === -1)
                    ?"infer-step-failed"
                    : "infer-step-pending"
        :"infer-step-pending";
    };

    buildJudgmentView = (judgment)=>{
        const premises  =  judgment.premises == null?null: (judgment.premises.map(x=> this.buildJudgmentView(x)));
        const self =this;
        return (
            <div className={"infer-step " + this.inferRuleClassName(judgment)} key={judgment.stepId}>
                <div>{premises}</div>
                <div>--------------------------------------------------------</div>
                <div>{judgment.goal.rep}</div>
                {
                    !this.state.stepNoVisible[judgment.stepId]
                        ? <button onClick={function(){self.handleClick_StepJudgment(judgment.stepId)}}>Step</button>
                        :null
                }

            </div>
        );
    };

    render() {
        console.log("calling render");
        const view = this.buildJudgmentView(this.props.rootDerivation);
        return (
            <div>
                {view}
            </div>
        );
    }
    //helper function to modify the derivation tree
    modifyJudgment =(judgment,step)=>{
        if(step.premises){
            step.premises.forEach((x,i)=> {
                x.stepId = judgment.stepId + "-"+ this.counter;
                this.judgmentParents[x.stepId] = judgment;
                this.counter++;
                this.judgments[x.stepId] = x;
            });
        }
        judgment.kind = step.kind;
        judgment.premises = step.premises;

        judgment.status = this.inferRuleKindToStatus(step.kind);
        if(judgment.status !== 0){
            this.updateDoneStatus(this.judgmentParents[judgment.stepId]);
        }
    };

    inferRuleKindToStatus = (inferRuleStatus)=>{
        if(inferRuleStatus === "case-base-rule" ) {
            return 1;
        }
        else if(inferRuleStatus === "no-rule" ) {
            return -1;
        }
        return 0;
    };

    updateDoneStatus = (judgment) => {
        if(!judgment){
            return;
        }
        console.log("updating parent with id:"+ judgment.stepId);
       if(judgment.premises.every((x)=> x.status === 1)) {
           judgment.status = 1;
           this.updateDoneStatus(this.judgmentParents[judgment.stepId]);
       }
       else if(judgment.premises.filter((x)=> x.status === -1).length > 0) {
           judgment.status = -1;
           this.updateDoneStatus(this.judgmentParents[judgment.stepId]);
       }
    }
}
export default DerivationTree;
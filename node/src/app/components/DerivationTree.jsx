import React from "react";
import request from "superagent";
import update from 'immutability-helper';
import IconButton from 'material-ui/IconButton';


class DerivationTree extends React.Component{
    state={
        stepNoVisible : {},
        adquiringStep:false,
        renderingStep:false,
        rootDerivation:null,
    };
    stepJudgment = "stepjudgment";
    resumeJudgment = "resumejudgment";
    judgments = {};
    judgmentParents = {};
    counter = 0;
    //a "ref" to the div that contains the judgment visualization
    judgmentWrapper = null;

    constructor(props) {
        super(props);
        //receive props.rootDerivation
    }
    componentDidMount(){
        this.judgments[this.props.rootDerivation.stepId]=this.props.rootDerivation;
        //request MathJax to render the formulas
        MathJax.Hub.Queue(["Typeset",MathJax.Hub, "derivationTree"]);
    }

    handleClick_StepJudgment = (stepId)=>{
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
                                renderingStep:true,
                            },()=>{
                                this.queuePremiseForMathJax(stepId);
                            });
                        }
                        else {
                            alert('Oh no! error ' + res.body.error);
                        }
                    }
                });
        });
    };
    updateJudgmentArguments = (stepId)=>{
        this.setState({
            adquiringStep:true,
        },()=> {
            request.post(this.resumeJudgment)
                .send(this.judgments[stepId])
                .set('Accept', 'application/json')
                .end((err, res) => {
                    if (err || !res.ok) {
                        alert('Oh no! Configuration Error');
                    } else {
                        if (res.body.status === "OK") {
                            this.forceUpdate();
                            this.setState({
                                adquiringStep: false,
                                renderingStep:true,
                            },()=>{
                                this.queuePremiseForMathJax(stepId);
                            });
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
                : (judgment.status === -1)
                    ?"infer-step-failed"
                    : "infer-step-pending"
        :"infer-step-pending";
    };

    queuePremiseForMathJax = (stepId) => {
        if(this.state.renderingStep){
            MathJax.Hub.Queue(["Typeset",MathJax.Hub, "derivationTree"], () => this.setState({renderingStep: false}));
        }
    };

    buildSideCondition = (sideCondition)=>{
        return (
            <div className={"infer infer-step"}  key={sideCondition.key}>
            {"$$" + sideCondition.rep +"$$"}
            </div>
            );
    };

    buildJudgmentView = (judgment)=>{
        const premises  =  judgment.premises == null?null: (judgment.premises.map(x=> this.buildJudgmentView(x)));
        const sideConditions = judgment.sideConditions==null?null:(judgment.sideConditions.map(x=> this.buildSideCondition(x)));
        const self =this;
        return (
            <div className={"infer infer-step " + this.inferRuleClassName(judgment)} key={judgment.stepId}>
                <div>{premises} {sideConditions}</div>
                {
                    !this.state.stepNoVisible[judgment.stepId]
                        ? <button onClick={function(){self.handleClick_StepJudgment(judgment.stepId)}}>S
                        </button>
                        :null
                }
                <hr className={this.inferRuleClassName(judgment)}/>
                <div>{"$$"+judgment.rep+"$$"}</div>
            </div>
        );
    };

    render() {
        console.log("calling render");
        const view = this.buildJudgmentView(this.props.rootDerivation);
        return (
            <div ref={divElem=> this.judgmentWrapper = divElem} id="derivationTree">
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
        judgment.output = step.output;
        judgment.kind = step.kind;
        judgment.rep = step.rep;
        judgment.sideConditions = step.sideConditions;
        judgment.premises = step.premises;

        judgment.status = this.inferRuleKindToStatus(step.kind);
        if(judgment.status !== 0){
            if(judgment.status === 1 && judgment.output){
                this.updateJudgmentDependencies(this.judgmentParents[judgment.stepId],judgment);
            }
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
           //this.updateJudgmentArguments();
           this.updateDoneStatus(this.judgmentParents[judgment.stepId]);
           if(judgment.output){
               this.updateJudgmentDependencies(this.judgmentParents[judgment.stepId],judgment);
           }
       }
       else if(judgment.premises.filter((x)=> x.status === -1).length > 0) {
           judgment.status = -1;
           this.updateDoneStatus(this.judgmentParents[judgment.stepId]);
       }
    };

    updateJudgmentDependencies = (judgmentParent,doneChildJudgment)=>{
        if(!judgmentParent || !doneChildJudgment){
            return;
        }
        var childIndex = judgmentParent.premises.findIndex(x=> x.stepId === doneChildJudgment.stepId);
        console.log("index " + childIndex);
        var self =this;
        judgmentParent.sideConditions.forEach(x=> {
            x.rep = self.replaceAll(x.rep,"$"+(childIndex+1),doneChildJudgment.output);
            console.log(x.rep);
        });
        judgmentParent.output = self.replaceAll(judgmentParent.output,"$"+(childIndex+1),doneChildJudgment.output);
        judgmentParent.rep = self.replaceAll(judgmentParent.rep,"$"+(childIndex+1),doneChildJudgment.output);

    };

    replaceAll = (str,what,to)=>{
        what = what.replace(/[-\/\\^$*+?.()|[\]{}]/g, '\\$&');
        var re = new RegExp(what, 'g');
        return str.replace(re,to);
    }
}
export default DerivationTree;
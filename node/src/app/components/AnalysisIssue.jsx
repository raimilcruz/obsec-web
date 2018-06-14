import React from "react";


const issueLabelBase = {
    borderRadius: 3,
    color: "#222",
    padding: "0 5px"
}
const issueSecError = {
    backgroundColor: "red"
}
const issueDartError = {
    backgroundColor: "#f7977a"
}
const issueWarning = {
    backgroundColor: "#F5D04C"
}
const issueStyle = {
    backgroundColor:"#444",
    borderRadius: 3,
    cursor: "pointer",
    lineHeight: 2,
    marginRight: 4,
    marginBottom: 2,
}

 class AnalysisIssue extends React.Component{
    constructor(props) {
        super(props);
        this.onClick = this.onClick.bind(this);
    }
    getErrorStyle (errorKind){
        var style = {};
        if(errorKind === "secerror"){
            style= issueSecError;
        }
        else if(errorKind === "warning"){
            style= issueWarning;
        }
        else{
            style= issueDartError;
        }
        return Object.assign(
            {},
            issueLabelBase,
            style);
    }
    onClick(event) {
        this.props.handleClick(this.props.issue)
    }
    render() {
        const {
            issue,
        } = this.props;
        return (
            <div className='issue' onClick={this.onClick}>
                <span className={"issuelabel "+issue.kind}>{issue.kind}</span>
                <span className='message'>{issue.message}</span>
            </div>
        );
    }
}
export default AnalysisIssue;
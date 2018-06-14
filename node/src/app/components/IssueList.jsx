import React from "react";
import AnalysisIssue from "./AnalysisIssue"

class IssueList extends React.Component{
    constructor(props) {
        super(props);
    }

    render() {
        const {
            handleClick,
            issues,
        } = this.props;
        var id = 1;
        let issueNodes = issues.map((issue) => {
            issue.id = id;
            id = id+1;
            return (
                <AnalysisIssue
                    key = {issue.id}
                    issue={issue}
                    handleClick={handleClick}
                />
            )
        });
        return (
            <div>
                { issueNodes }
            </div>
        );
    }
}
export default IssueList;

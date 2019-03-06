import React from 'react';
import Paper from 'material-ui/Paper';
import RaisedButton from 'material-ui/RaisedButton';
import request from 'superagent';
import Subheader from 'material-ui/Subheader';

import SelectField from 'material-ui/SelectField';
import MenuItem from 'material-ui/MenuItem';
import SvgIcon from 'material-ui/SvgIcon';
import IconButton from 'material-ui/IconButton';
import Dialog from 'material-ui/Dialog';
import FlatButton from 'material-ui/FlatButton';
import AceEditor from 'react-ace';

import Markdown from 'react-markdown';
import 'brace/mode/java';
import 'brace/theme/github';

const Highlight = require('react-highlight');
import AnalysisIssue from './components/AnalysisIssue';
import Syntax from './components/Syntax';
import GObSecSyntax from './components/GObSecSyntax';
import ObSecSyntax from './components/ObSecSyntax';

const HelpIcon = (props) => (
    <SvgIcon {...props}>
        <path d="M0 0h24v24H0z" fill="none"/>
        <path
            d="M12 2C6.48 2 2 6.48 2 12s4.48 10 10 10 10-4.48 10-10S17.52 2 12 2zm1 17h-2v-2h2v2zm2.07-7.75l-.9.92C13.45 12.9 13 13.5 13 15h-2v-.5c0-1.1.45-2.1 1.17-2.83l1.24-1.26c.37-.36.59-.86.59-1.41 0-1.1-.9-2-2-2s-2 .9-2 2H8c0-2.21 1.79-4 4-4s4 1.79 4 4c0 .88-.36 1.68-.93 2.25z"/>
    </SvgIcon>
);
const errorStyle={
    color:'red'
};


export default class LanguagePad extends React.Component {
    state = {
        formula: "bar",
        program: "",
        desc: "" ,
        error: "",
        analysisIssue:null,
        executionState: 0,
        executionResult: "",
        typingState: 0,
        defaultProgram: null,
        expressionType: "",
        syntaxOpen: false,
        markers:[],
        loadingExamples: true,
        loadingSyntax: true,
        examples: [],
        syntax: {}
    };



    urlExamples = "examples";
    urlSyntax = "syntax";
    urlTypeCheck = "typecheck";
    urlExecute = "reduce";
    language = "gobsec";


    constructor(props) {
        super(props);
        if(this.props.urlExamples)
            this.urlExamples = this.props.urlExamples;
        if(this.props.urlSyntax)
            this.urlSyntax= this.props.urlSyntax;
        if(this.props.urlTypeCheck)
            this.urlTypeCheck = this.props.urlTypeCheck;
        if(this.props.urlExecute)
            this.urlExecute = this.props.urlExecute;
        if(this.props.language)
            this.language = this.props.language;

    }

    componentDidMount() {
        console.log("component did mount:"+ this.urlExamples);
        this.loadExamples();
        this.loadSyntax();
    }


    onChangeProgram = (v) => {
        this.setState({
            program: v,//v.replace("\\", "λ").replace("&", "∧").replace("|", "∨"),
            typingState: 0,
            markers:[]
        });
    };

    findProgramByValue(v) {
        return this.state.examples.filter(e => e.id == v)[0]
    }
    loadExamples() {
        this.setState({loadingExamples: true}, () => {
            request.post(this.urlExamples)
                .send()
                .set('Accept', 'application/json')
                .end((err, res) => {
                    if (err || !res.ok) {
                        alert('Oh no! Configuration Error');
                    } else {
                        if (res.body.status === "OK") {
                            this.setState({
                                loadingExamples: false,
                                examples: res.body.examples,
                                defaultProgram: (res.body.examples.length>0?res.body.examples[0].id:null),
                                program: (res.body.examples.length>0?res.body.examples[0].program:null),
                                desc:(res.body.examples.length>0?res.body.examples[0].desc:null)
                            }, () => {
                            })
                        } else {
                            alert('Oh no! error ' + res.body.error);
                        }
                    }
                });
        });
    };
    loadSyntax() {
        this.setState({loadingSyntax: true}, () => {
            request.post(this.urlSyntax)
                .send()
                .set('Accept', 'application/json')
                .end((err, res) => {
                    if (err || !res.ok) {
                        alert('Oh no! Configuration Error');
                    } else {
                        if (res.body.status === "OK") {
                            this.setState({
                                loadingSyntax: false,
                                syntax: res.body.syntax,
                            }, () => {
                            })
                        } else {
                            alert('Oh no! error ' + res.body.error);
                        }
                    }
                });
        });
    }

    examplesItems(){
        return this.state.examples.map((e) => (
            <MenuItem key={e.id} value={e.id} primaryText={e.title}/>
        ));
    };

    changeDefaultProgram = (event, index, programValue) => {
        var p = this.findProgramByValue(programValue);
        this.setState({program: p.program, desc: p.desc,typingState:0,markers:[]})
    };

    typecheck = () => {
        this.setState({error: "", executionState: 0, typingState: 0}, () => {
            request.post(this.urlTypeCheck)
                .send({program: this.state.program})
                .set('Accept', 'application/json')
                .end((err, res) => {
                    if (err || !res.ok) {
                        alert('Oh no! error');
                    } else {
                        if (res.body.status === "OK") {
                            this.setState({
                                expressionType: res.body.expressionType,
                                typingState: 1
                            }, () => {
                                //TODO: Do extra things here if needed
                            })
                        }

                        else if(res.body.status === "AnalysisKO"){
                            this.setState({
                                error: res.body.issue.message,
                                analysisIssue: res.body.issue,
                                typingState: -1
                            }, () => {
                            })
                        }
                        else {
                            this.setState({error: res.body.error, typingState: -2});
                        }
                    }
                });
        });
    };

    reduce = () => {
        this.setState({
            loadingReduce: true,
            executionState: 0,
            executionError: null
        }, () => {
            request.post(this.urlExecute)
                .send({program: this.state.program})
                .set('Accept', 'application/json')
                .end((err, res) => {
                    if (err || !res.ok) {
                        alert('Oh no! Configuration Error');
                    } else {
                        if (res.body.status === "OK") {
                            this.setState({
                                error: res.body.error,
                                executionResult: res.body.result,
                                executionState: 1
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

    syntaxHandleOpen = () => {
        this.setState({syntaxOpen: true});
    };

    syntaxHandleClose = () => {
        this.setState({syntaxOpen: false});
    };

    handleIssueClick = (issue) => {
        let position = issue.position;
        this.setState({
            markers: [{ startRow: position.line, startCol: position.columnStart,
                endRow: position.lineEnd, endCol: position.columnEnd, className: 'errorHighlight', type: 'text' }]
        });
    };

    render() {

        return (
            <Paper style={{padding: '8px'}}>
                <Dialog
                    title="Syntax"
                    actions={[<FlatButton
                        label="Close"
                        primary={true}
                        onTouchTap={this.syntaxHandleClose}
                    />]}
                    modal={false}
                    open={this.state.syntaxOpen}
                    onRequestClose={this.syntaxHandleClose}
                >
                    <Syntax syntaxDefinition={this.state.syntax.productions}/>
                    <br/>
                </Dialog>

                <div className="form" style={{position: "relative"}}>
                    {
                        this.state.loadingExamples?
                            <div>Loading programs</div>
                            :
                            <SelectField value={this.state.defaultProgram}
                                         onChange={this.changeDefaultProgram}
                                         floatingLabelText="Preloaded examples"
                                         autoWidth={true} fullWidth={true}>
                                {this.examplesItems()}
                            </SelectField>
                    }

                    <div >
                        <Markdown
                            source={this.state.desc}
                        />
                    </div>
                   {/* <div dangerouslySetInnerHTML={{ __html: marked(this.lineBreakToBreak(this.state.desc)) }} />*/}
                   {/* <div>
                        {this.getMarkdownText()}
                    </div>*/}

                    <br/>
                    <div style={{position: 'relative'}}>

                           <AceEditor
                            name="formula"
                            value={this.state.program}
                            mode="java"
                            theme="github"
                            width = "70%"
                            tabSize = {2}
                            onChange={this.onChangeProgram}
                            minLines={5}
                            maxLines={30}
                            onKeyDown={this.onKeyDown}
                            editorProps={{$blockScrolling: true}}
                            markers={this.state.markers}
                        />

                        <IconButton tooltip="See syntax" onClick={this.syntaxHandleOpen}>
                            <HelpIcon />
                        </IconButton>
                        <RaisedButton label="Typecheck!" secondary={true} onClick={this.typecheck}
                                      style={{position: "absolute", bottom: "12px"}}/>
                    </div>
                </div>
                {
                    this.state.typingState !== 0 ?
                        <div>
                            {
                                this.state.typingState === 1 ?
                                    <div>
                                        <Subheader>Expression Type</Subheader>
                                        <div className='p_wrap'> {this.state.expressionType}</div>
                                        <div style={{marginTop: '16px'}}>
                                            <RaisedButton label="Execute!" primary={true} onClick={this.reduce}/>
                                        </div>
                                        {
                                            this.state.executionState !== 0 ?
                                                <div>
                                                    {
                                                        this.state.executionState === 1 ?
                                                            <div>
                                                                <Subheader>Result</Subheader>
                                                                <div> {this.state.executionResult}</div>
                                                            </div>
                                                            :
                                                            <div>
                                                                <Subheader>Runtime error</Subheader>
                                                                <div style={errorStyle}>{this.state.error}</div>
                                                            </div>
                                                    }
                                                </div> : null
                                        }
                                    </div>
                                    : (this.state.typingState === -1 ?
                                    <div>
                                        <div className="analysisResultTitle">Analysis Error</div>
                                        <div className="analysisResultBody">
                                            <AnalysisIssue
                                                key = {1}
                                                issue={this.state.analysisIssue}
                                                handleClick={this.handleIssueClick}
                                            />
                                        </div>
                                    </div> :
                                    (this.state.typingState === -2 ?
                                            <div>
                                                Implementation error
                                                <div className='issue' >
                                                    <span className="issuelabel error">Implementation</span>
                                                    <span className='message'>{this.state.error}</span>
                                                </div>
                                            </div>
                                            : <div>Invalid application state</div>
                                    ))
                            }
                        </div> : null
                }


            </Paper>);
    }

}
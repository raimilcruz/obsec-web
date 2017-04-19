import React from 'react';
import ReactDOM from 'react-dom';
import $ from 'jquery';
import Paper from 'material-ui/Paper';
import TextField from 'material-ui/TextField';
import RaisedButton from 'material-ui/RaisedButton';
import request from 'superagent';
import Subheader from 'material-ui/Subheader';
import Popover from 'material-ui/Popover';
import LoadableContent from './components/LoadableContent';

import ArrowDownward from 'material-ui/svg-icons/navigation/arrow-downward';
import Toggle from 'material-ui/Toggle';

import RErrorIcon from 'material-ui/svg-icons/social/sentiment-very-dissatisfied.js';

import {red500} from 'material-ui/styles/colors';

import SelectField from 'material-ui/SelectField';
import MenuItem from 'material-ui/MenuItem';
import SvgIcon from 'material-ui/SvgIcon';
import IconButton from 'material-ui/IconButton';
import Dialog from 'material-ui/Dialog';
import FlatButton from 'material-ui/FlatButton';
import AceEditor from 'react-ace';
import 'brace/mode/java';
import 'brace/theme/github';

var Highlight = require('react-highlight');

class MyTextField extends React.Component {
    oldIdx = 0
    oldLength = 0
    componentWillUpdate = (nextProps, nextState) => {
        const node = $('textarea[name=formula]')[0]
        this.oldLength = node.value.length;
        this.oldIdx = node.selectionStart;
    }
    componentDidUpdate = (prevProps) => {
        //var node = ReactDOM.findDOMNode(this);
        const node = $('textarea[name=formula]')[0]
        node.value = this.props.value;
        var newIdx = Math.max(0, node.value.length - this.oldLength + this.oldIdx);
        node.selectionStart = node.selectionEnd = newIdx;
    }

    render() {
        return (<TextField {...this.props} />);
    }
}
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

function replaceCurlyBracket(str){
    return str.replace(/@;/g, "{").replace(/#;/g,"}");
}

const examples = [
    {
        value: 1,
        text: "1.1: Introducing type-based declassification policies: Password policy",
        program:   "let{\n  type StringEq = [{== : String -> Bool}]\n  deftype AuthServer {\n    {login: String<StringEq String -> Int}\n  }\n  val auth =  new {z : AuthServer<L => \n                def login password guess = if password.==(guess) then 1 else 0\n              }\n}\nin \nauth.login(\"qwe123\",\"qwe123\")",
        desc: "This is the first example of the paper. The login method receives two arguments: the 'secret' password " +
        "and the user guess. The 'password' argument has a declassification policy that allows to release the result " +
        "of the == comparison. The body of the 'login' method adheres to that policy, so the resulting integer is public"
    },
    {
        value: 2,
        text: "1.2: Method implementation does not respect the password policy",
        program:   "let{\n  type StringEq = [{== : String -> Bool}]\n  deftype AuthServer {\n    {login: String<StringEq String -> Int}\n  }\n  val auth =  new {z : AuthServer<L => \n                def login password guess = if password.hash().==(guess.hash()) then 1 else 0\n              }\n}\nin \nauth.login(\"qwe123\",\"qwe123\")",
        desc: "Now the 'login' method does not adhere to the password policy and it uses the hash method, which is not the public type. " +
        "Note that the conditional expression become high, and hence the result of the if expression is also high"
    },
    {
        value: 3,
        text: "1.3: Password policy is full secret",
        program:   "let{\n  deftype AuthServer {\n    {login: String<H String -> Int}\n  }\n  val auth =  new {z : AuthServer<L => \n                def login password guess = if password.==(guess) then 1 else 0\n              }\n}\nin \nauth.login(\"qwe123\",\"qwe123\")",
        desc: "This example differs from the first one in that the first argument of the login method (i.e. the real password) is full secret." +
        " In this case, the implementation of the login does not adhere to the policy of password because it is using the == method that is" +
        " not in the public facet, so the resulting type of the login method body is a secret integer." +
        " Hence the method implementation result type is not subtype of the method signature return type, deriving in a type error." +
        " Feel free to change the login method signature return type to Int<H and then the program will be well-typed, meaning the result is secret."
    },
    {
        value: 4,
        text: "2: Password policy with hash and eq",
        program: "let {\n  type StringHashEq = [{hash : -> Int<[{== : Int -> Bool}]}]\n  type AuthServer = [{login : String<StringHashEq String -> Int<L}]\n  val auth =  new {z : AuthServer<L => \n                def login password guess = if password.hash().==(guess.hash()) then 1 else 0\n              }\n} in\nauth.login(\"qwe123\",\"qwe123\")",
        desc: "The password policy now indicates that information about the password can be done public by calling 1) the hash method over the password, " +
        " and then 2) to compare the result with a public integer. The program adheres to the policy, so it is well-typed." +
        " Any variation to the implementation of the login method that does not respect the policy will be ill-typed. " +
        "For instance, change the conditional expression to password.hash().+(1).==(guess.hash().+(1))"
    },
    {
        value : 5,
        text:"3: Recursive declassification over list",
        program: "let{\n  type StringEq = [{== : String -> Bool}]\n  deftype StrEqList{\n    {isEmpty: -> Bool<L}\n    {head: -> String<StringEq }\n    {tail: -> StrList<StrEqList}\n  }\n  val listHelper =  new {z : [{contains : StrList<StrEqList -> Bool<L}]<L  =>\n                      def contains myList  =\n                        if myList.isEmpty()\n                        then false\n                        else\n                          if myList.head().==(\"a\")\n                          then true\n                          else z.contains(myList.tail())\n                    }\n}\nin\nlistHelper.contains(mklist(\"b\",\"c\",\"a\"))",
        desc: "Recursive declassification policies are desirable to express interesting declassification of "+
        "either inductive data structures or object interfaces (whose essence are recursive types). Consider for instance a secret list" +
        " of strings, for which we want to allow traversal of the "+
        "structure and comparison of its elements with a given string. " +
        "Note that the head method returns a String that only has the == operation public." +
        " Any method invocation over the head of the list (different than ==) renders the program ill-typed. " +
        "For instance, changing the inner if condition to myList.head().hash().==(“a”.hash()): the type checker reports “Both branches of an if expression must have the same type”. This is because the else branch of the outer if has type Bool<H while the then branch has type Bool<L. "
    }
]
const examplesItems = examples.map((e) => {
    return <MenuItem key={e.value} value={e.value} primaryText={e.text}/>
});


export default class Main extends React.Component {
    state = {
        formula: "bar",
        program: this.findProgramByValue(1).program, //"if true_H::Bool_?  then ref 10_L else ref 20_L",//"(λx: Ref Int.\n    (λy: Unit. !x) (x := 10)\n) (ref 4)",
        desc: this.findProgramByValue(1).desc,
        error: "",
        executionState: 0,
        executionResult: "",
        typingState: 0,
        defaultProgram: 1,
        expressionType: "",
        syntaxOpen: false,
        typeDefinitionsOpen:null,
        anchorEl:null
    }

    componentDidMount() {
        //this.math = MathJax.Hub.getAllJax("toLatex")[0]
    }


    onChange = (e, v) => {
        if (!this.math)
            this.math = MathJax.Hub.getAllJax("toLatex")[0]
        this.setState({formula: v}, () => {
            //MathJax.Hub.Queue(["Typeset",MathJax.Hub, "toLatex"]);
            MathJax.Hub.Queue(["Text", this.math, this.state.formula])
        });
    }

    guessPlusSymbol = (c) => {
        v.split("")
    }
    onChangeProgram = (v) => {
        //this.guessPlusSymbol(v);
        this.setState({
            program: v,//v.replace("\\", "λ").replace("&", "∧").replace("|", "∨"),
            typingState: 0
        })
    }

    findProgramByValue(v) {
        return examples.filter(e => e.value == v)[0]
    }

    changeDefaultProgram = (event, index, defaultProgram) => {
        let p = this.findProgramByValue(defaultProgram);
        this.setState({defaultProgram, program: p.program, desc: p.desc,typingState:0})
    };

    typecheck = () => {
        this.setState({error: "", executionState: 0, typingState: 0}, () => {
            request.post('typecheck')
                .send({program: this.state.program})
                .set('Accept', 'application/json')
                .end((err, res) => {
                    if (err || !res.ok) {
                        alert('Oh no! error');
                    } else {
                        if (res.body.status == "OK") {
                            this.setState({
                                expressionType: res.body.expressionType,
                                typingState: 1
                            }, () => {
                                //TODO: Do extra things here if needed
                            })
                        }
                        else {
                            this.setState({error: res.body.error, typingState: -1});
                        }
                    }
                });
        });
    }

    reduce = () => {
        this.setState({loadingReduce: true, executionState: 0, executionError: null}, () => {
            request.post('reduce')
                .send({program: this.state.program})
                .set('Accept', 'application/json')
                .end((err, res) => {
                    if (err || !res.ok) {
                        alert('Oh no! Configuration Error');
                    } else {
                        if (res.body.status == "OK") {
                            this.setState({
                                error: res.body.error,
                                executionResult: res.body.result,
                                executionState: 1
                            }, () => {
                                //TODO: extra things here

                            })
                        } else {
                            alert('Oh no! error ' + res.body.error);
                        }
                    }
                });
        });
    }


    onToggle = (e, v) => {
        this.setState({substitutionMode: v ? 0 : 1}, () => {
            if (this.state.tree) this.reduce();
        });

    }

    syntaxHandleOpen = () => {
        this.setState({syntaxOpen: true});
    };

    syntaxHandleClose = () => {
        this.setState({syntaxOpen: false});
    };
    handleTouchTap = (event,obSecType) => {
        // This prevents ghost click.
        event.preventDefault();

        this.setState({
            typeDefinitionsOpen: obSecType,
            anchorEl: event.currentTarget,
        });
    };
    handleRequestClose = () => {
        this.setState({
            typeDefinitionsOpen: null,
        });
    };

    lcb() {
        return '{'
    };

    rcb() {
        return '}'
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
                    <table>
                        <tbody>
                        <tr>
                            <td>S</td>
                            <td>::=</td>
                            <td>T &lt; T</td>
                            <td>(Security types)</td>
                        </tr>
                        <tr>
                            <td>T</td>
                            <td>::=</td>
                            <td>
                                <RaisedButton
                                    onTouchTap={(event)=> this.handleTouchTap(event,"Bool")}
                                    label="Bool"
                                /> |
                                <RaisedButton
                                    onTouchTap={(event)=> this.handleTouchTap(event,"Int")}
                                    label="Int"
                                /> |
                                <RaisedButton
                                    onTouchTap={(event)=> this.handleTouchTap(event,"String")}
                                    label="String"
                                /> |
                                <RaisedButton
                                    onTouchTap={(event)=> this.handleTouchTap(event,"StrList")}
                                    label="StrList"
                                /> | X | OT</td>
                            <td>(Types)</td>
                        </tr>
                        <tr>
                            <td>OT</td>
                            <td>::=</td>
                            <td>[X M*] | [M*]</td>
                            <td>(Object Type)</td>
                        </tr>
                        <tr>
                            <td>M</td>
                            <td>::=</td>
                            <td>{this.lcb()} name : S* -> S {this.rcb()} </td>
                            <td>(Method signature)</td>
                        </tr>
                        <tr>
                            <td>t</td>
                            <td>::=</td>
                            <td> o | x | t.m(t) | b | n | s |
                                <strong>if</strong> t <strong>then</strong> t <strong>else</strong> t |
                                <strong>mkList</strong>(t*) | <strong>let</strong> {"{"} TD* TA* VD* {"}"} <strong>in</strong> t
                            </td>
                            <td>(Terms)</td>
                        </tr>
                        <tr>
                            <td>o</td>
                            <td>::=</td>
                            <td> <strong>new</strong> {this.lcb()} x : S => (<strong>def</strong> name x* = t)* {this.rcb()}
                            </td>
                            <td>(Object)</td>
                        </tr>
                        <tr>
                            <td>TD</td>
                            <td>::=</td>
                            <td> <strong>deftype</strong>{this.lcb()} M*  {this.rcb()}
                            </td>
                            <td>(Type Declaration)</td>
                        </tr>
                        <tr>
                            <td>TA</td>
                            <td>::=</td>
                            <td> <strong>type</strong> X = OT
                            </td>
                            <td>(Type Alias)</td>
                        </tr>
                        <tr>
                            <td>VD</td>
                            <td>::=</td>
                            <td> <strong>val</strong> x = t
                            </td>
                            <td>(Value declaration)</td>
                        </tr>
                        <tr>
                            <td>b</td>
                            <td>::=</td>
                            <td>true | false</td>
                            <td>(Booleans)</td>
                        </tr>
                        <tr>
                            <td>n</td>
                            <td>::=</td>
                            <td>natural numbers</td>
                            <td>(Natural numbers)</td>
                        </tr>
                        <tr>
                            <td>s</td>
                            <td>::=</td>
                            <td>string literals</td>
                            <td>(String literals)</td>
                        </tr>
                        <tr>
                            <td>X</td>
                            <td>::=</td>
                            <td>identifier</td>
                            <td>(Type Variable)</td>
                        </tr>
                        </tbody>
                    </table>
                    <br/>
                </Dialog>

                <Popover
                    open={this.state.typeDefinitionsOpen!=null}
                    anchorEl={this.state.anchorEl}
                    anchorOrigin={{horizontal: 'left', vertical: 'bottom'}}
                    targetOrigin={{horizontal: 'left', vertical: 'top'}}
                    onRequestClose={this.handleRequestClose}
                >
                   <Highlight className="java">
                       {
                           this.state.typeDefinitionsOpen=="Int"?
                           "[" +
                           "{+ : Int<Int -> Int<Int}\n" +
                           "{- : Int<Int -> Int<Int}\n"+
                           "{== : Int<Int -> Bool<Bool}]"
                           : (this.state.typeDefinitionsOpen=="Bool")?
                           "[{if : T<T T<T -> T<T}]"
                           : (this.state.typeDefinitionsOpen=="String")?
                           "[" +
                           "{== : String<String -> Bool<Bool}\n" +
                           "{hash : -> Int<Int}\n"+
                           "{length : -> Int<Int}]"
                           : (this.state.typeDefinitionsOpen=="StrList")?
                           "[l \n" +
                           "{isEmpty : -> Bool<Bool}\n" +
                           "{head : -> String<String}\n"+
                           "{tail : -> l<l}]"
                           :
                            null
                       }
                   </Highlight>
                </Popover>



                <div className="form" style={{position: "relative"}}>
                    <SelectField value={this.state.defaultProgram} onChange={this.changeDefaultProgram}
                                 floatingLabelText="Preloaded examples" autoWidth={true} fullWidth={true}>
                        {examplesItems}
                    </SelectField>
                    <div style={{color: "rgba(0, 0, 0, 0.541176)"}}>
                        {this.state.desc}
                    </div>

                    <div>
                        Program
                    </div>
                    <div style={{position: 'relative'}}>
                        {
                            /*<MyTextField floatingLabelText="Program"
                                     name="formula"
                                     value={this.state.program}
                                     multiLine={true}
                                     onChange={this.onChangeProgram}
                                     rows="5"
                                     style={{width: '70%', marginRight: '16px'}}
                                     errorText={this.state.error}
                                     onKeyDown={this.onKeyDown}
                                     ref={(c) => this._program = c}
                        />*/
                        }

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
                                        <div> {this.state.expressionType}</div>
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
                                    :
                                    <div>
                                        <Subheader>Type checking error</Subheader>
                                        <div  style={errorStyle}>{this.state.error.split('\n').map((item, key) => {
                                            return <span key={key}>{item}<br/></span>
                                        })}</div>
                                    </div>
                            }
                        </div> : null
                }


            </Paper>);
    }

}
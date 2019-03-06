import React from "react";
import Popover from 'material-ui/Popover';
import RaisedButton from 'material-ui/RaisedButton';
var Highlight = require('react-highlight');

export default class Syntax extends React.Component{
    state = {
        typeDefinitionsOpen:null,
        anchorEl:null,
    };
    syntaxDefinition = [];
    //sample definition of productions rules
    /*syntaxDefinition = [{
            name : "S",
            items:[{
                name: "Int",
                expanded: "true"
            },{
                name:"B"
            }],
            category:"Security types"
        },{
            name : "T",
        items:[{
            name: "C"
        },{
            name:"D"
        }],
            category:"Types"
        },{
            name:"Int",
            expanded:true,
            expansion: "[" +
            "{+ [l >: Int] : Int<l -> Int<l}\n" +
            "{- [l >: Int] : Int<l -> Int<l}\n"+
            "{== [l >: Int] : Int<l -> Bool<(Bool,l)}]"
        }
    ];*/


    constructor(props) {
        super(props);
        if(this.props.syntaxDefinition)
            this.syntaxDefinition = this.props.syntaxDefinition;
    }

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

    renderSyntaxDefinition = ()=>{
        return (this.syntaxDefinition)
                ?this.syntaxDefinition.filter(x=> !x.expanded).map(this.renderSyntaxRule)
                : "Syntax not provided";
    };
    renderSyntaxRule = (ruleDef)=>{
        const items = ruleDef.items.map((e,i)=> this.renderItem(e,i,ruleDef.items.length-1 !== i));
        return (
          <tr key={ruleDef.name}>
              <td>{ruleDef.name}</td>
              <td>::=</td>
              <td>{items}</td>
              <td>({ruleDef.category})</td>
          </tr>
        )
    };
    renderItem = (ruleItem,index,renderSeparator)=>{
        return (
            <span key={"item"+ index}>
                <span>
                    {(ruleItem.expanded)
                        ?
                        <RaisedButton
                            onTouchTap={(event)=> this.handleTouchTap(event,ruleItem.name)}
                            label={ruleItem.name}
                        />
                        : ruleItem.name}
                </span>
                {
                    renderSeparator
                        ? <span> | </span>
                        : null
                }

            </span>
        )
    };
    renderExpandedItem = (ruleName)=>{
        //TODO: search the definition in syntaxDefinition
        const ruleExpanded = this.syntaxDefinition.filter(x=> x.name === ruleName)[0];
        if(ruleExpanded)
            return (ruleExpanded.expansion);
        return null;
    };

    render(){
        return (
            <div>
                <table>
                    <tbody>
                    {this.renderSyntaxDefinition()}
                    </tbody>
                </table>
                <Popover
                    open={this.state.typeDefinitionsOpen!=null}
                    anchorEl={this.state.anchorEl}
                    anchorOrigin={{horizontal: 'left', vertical: 'bottom'}}
                    targetOrigin={{horizontal: 'left', vertical: 'top'}}
                    onRequestClose={this.handleRequestClose}
                >
                    <Highlight className="java">
                        { this.renderExpandedItem(this.state.typeDefinitionsOpen)}
                    </Highlight>
                </Popover>
            </div>
        )
    }
}
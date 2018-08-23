import React from "react";
import Popover from 'material-ui/Popover';
import RaisedButton from 'material-ui/RaisedButton';
import Syntax from './Syntax';
var Highlight = require('react-highlight');

export default class ObSecSyntax extends Syntax{
    render(){
        return (
        <div>
            <table>
                <tbody>
                <tr>
                    <td>S</td>
                    <td>::=</td>
                    <td>T &lt; U</td>
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
                    <td>name : S* -> S </td>
                    <td>(Method signature)</td>
                </tr>
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
                   <div>Working on it!</div>
                </Highlight>
            </Popover>
        </div>
        )
    }
}
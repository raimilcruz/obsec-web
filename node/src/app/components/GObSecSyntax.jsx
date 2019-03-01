import React from "react";
import Popover from 'material-ui/Popover';
import RaisedButton from 'material-ui/RaisedButton';
import Syntax from './Syntax';
var Highlight = require('react-highlight');

export default class GObSecSyntax extends Syntax{
   /* render(){
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
                    <td>U</td>
                    <td>::=</td>
                    <td>T | L</td>
                    <td>(Labels)</td>
                </tr>
                <tr>
                    <td>OT</td>
                    <td>::=</td>
                    <td>[X M*] | [M*]</td>
                    <td>(Object Type)</td>
                </tr>
                <tr>
                    <td>    M</td>
                    <td>::=</td>
                    <td>{this.lcb()} name [LD*]: S* -> S {this.rcb()} </td>
                    <td>(Method signature)</td>
                </tr>
                <tr>
                    <td>LD</td>
                    <td>::=</td>
                    <td>LV <strong>super</strong> T | LV <strong>extends</strong> T | LV : T .. T</td>
                    <td>(Label bound)</td>
                </tr>
                <tr>
                    <td>LV</td>
                    <td>::=</td>
                    <td>L | <strong>low</strong>  L</td>
                    <td>(Label variables)</td>
                </tr>
                <tr>
                    <td>t</td>
                    <td>::=</td>
                    <td> o | x | t[T*].m(t*) | b | n | s |
                        <strong> if</strong> t <strong>then</strong> t <strong>else</strong> t
                    </td>
                    <td>(Terms)</td>
                </tr>
                <tr>
                    <td colSpan={2}></td>
                    <td>
                        <strong>mkList</strong>(t*) | <strong>let</strong> {"{"} TD* TA* VD* {"}"} <strong>in</strong> t
                    </td>
                    <td></td>
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
                <tr>
                    <td>L</td>
                    <td>::=</td>
                    <td>identifier</td>
                    <td>(Label Variable)</td>
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
                    {
                        this.state.typeDefinitionsOpen=="Int"?
                        "[" +
                        "{+ [l >: Int] : Int<l -> Int<l}\n" +
                        "{- [l >: Int] : Int<l -> Int<l}\n"+
                        "{== [l >: Int] : Int<l -> Bool<(Bool,l)}]"
                        : (this.state.typeDefinitionsOpen=="Bool")?
                            "[{if : T<T T<T -> T<T}]"
                            : (this.state.typeDefinitionsOpen=="String")?
                                "[" +
                                "{== [l >: String]: String<l -> Bool<(Bool,l)}\n" +
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
        </div>
        )
    }*/
}
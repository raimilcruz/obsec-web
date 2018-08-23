import React from "react";
import Popover from 'material-ui/Popover';
import RaisedButton from 'material-ui/RaisedButton';
var Highlight = require('react-highlight');

export default class Syntax extends React.Component{
    state = {
        typeDefinitionsOpen:null,
        anchorEl:null,
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


    render(){
        return (
        <div>
           This component does not provide a direct render. 
        </div>
        )
    }
}
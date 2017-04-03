
/*Bug in the original file, I have to fix it by commenting a line */
import React from 'react';
import keycode from 'keycode';
import Formsy from 'formsy-react';
import TextField from 'material-ui/TextField';
import {_setMuiComponentAndMaybeFocus} from 'formsy-material-ui/lib/utils';

let FormsyText = React.createClass({
  mixins: [ Formsy.Mixin ],

  propTypes: {
    name: React.PropTypes.string.isRequired,
    value: React.PropTypes.any,
    onFocus: React.PropTypes.func,
    onBlur: React.PropTypes.func
  },

  componentDidMount: function(){
    if(this.refs.textfield.input.value)
  	 this.setValue(this.refs.textfield.input.value);
  },

  handleBlur: function handleBlur(event) {
    this.setValue(event.currentTarget.value);
    if (this.props.onBlur) this.props.onBlur(event);
  },

  handleKeyDown: function handleKeyDown(event) {
    if (keycode(event) === 'enter') this.handleEnterKeyDown(event);
    if (this.props.onKeyDown) this.props.onKeyDown(event, event.currentTarget.value);
  },

  _setMuiComponentAndMaybeFocus: _setMuiComponentAndMaybeFocus,

  render: function () {
    return (
      <TextField
        {...this.props}
        ref={"textfield"}
        onBlur={this.handleBlur}
        onFocus={this.props.onFocus}
        onKeyDown={this.handleKeyDown}
        errorText={this.getErrorMessage()}
      />
    );
  }
});

export default FormsyText;
import React from 'react';
import $ from 'jquery';


import FormsyText from './FormsyText';
//import FormsyText from 'formsy-material-ui/lib/FormsyText';
import CustomValidations from '../forms/CustomValidations';
import Select from './Select';
import DatePicker from 'material-ui/DatePicker';
const update = require('react-addons-update');
const dateFormat = require('dateformat');

import areIntlLocalesSupported from 'intl-locales-supported';

let DateTimeFormat = null;
if (areIntlLocalesSupported(['es'])) {
    DateTimeFormat = global.Intl.DateTimeFormat;
} else {
  const IntlPolyfill = require('intl');
  DateTimeFormat = IntlPolyfill.DateTimeFormat;
  require('intl/locale-data/jsonp/es');
}

export default class FormCard extends React.Component{
	  obj: String

	  changeSelectField = (name) => (value) => this.setState({[this.obj]: update(this.state[this.obj], {[name]: {$set: value.value}})});
    changeFormField = (name, transform) => (e,v) =>{
      if(e) e.stopPropagation(); //BUG: https://github.com/callemall/material-ui/issues/2189
      this.setState({[this.obj]: update(this.state[this.obj], {[name]: {$set: transform?transform(v):v}})});
    }
    onEdit = () =>{
        this.setState({editing: true, [this.obj]: $.extend(true, {}, this.props[this.obj])})
    }
    onCancel = () => {
        this.setState({editing: false, [this.obj]: this.props[this.obj]});
    }
    getMeta = (name) =>{
      return this.props.options.actions.POST[name];
    }

    renderSelectField = (obj, name, options, readOnly, hintText, validations, validationsErrors) => {
      return (<div>
                  <Select
                    name={name}
                    for={name}
                    floatingLabelText={this.getMeta(name).label}
                    options={options}
                    name={name}
                    readOnly={readOnly}
                    value={obj[name]}
                    onChange={this.changeSelectField(name)}
                  />                                
              </div>)
    }

    renderTextField = (obj, name, readOnly, hintText, validations, validationErrors, props) => {
      return (<div>
                  <FormsyText name={name} 
                  validations={validations?validations:CustomValidations.mapV(this.props.options, name)} 
                  validationErrors={validationErrors?validationErrors:CustomValidations.mapM(this.props.options, name)}  
                  hintText={hintText}
                  floatingLabelText={this.getMeta(name).label}
                  value={obj[name]?obj[name]:""}
                  readOnly={readOnly}
                  ref={name}
                  onChange={this.changeFormField(name)}
                  {...props}
                  />
              </div>);
    }

    renderDateField = (obj, name, readOnly) => {
    	return (
				<DatePicker container="inline"
		              mode="landscape"
		              locale="es"
		              DateTimeFormat={DateTimeFormat}
		              floatingLabelText={this.getMeta(name).label}
		              value={obj[name]?new Date(obj[name]+"T12:00:00Z"):null}
		              onChange={this.changeFormField(name, (d) => {return dateFormat(d, "yyyy-mm-dd")})}
		              disabled={readOnly}
		              name={name}
		              ref={name}
	  			/>);
    }

    enableButton = () => {
      this.setState({
        canSubmit: true,
      });
    }

    disableButton = () => {
      this.setState({
        canSubmit: false,
      });
    }

    handleRequestCloseSnackbar = () => {
    	this.setState({snackbar: false});
    }
}
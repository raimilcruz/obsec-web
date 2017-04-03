import React from 'react';


import DatePicker from 'material-ui/DatePicker';

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


export default class DateField extends React.Component{
	customChange = (e, val) =>{
		if(this.props.onChange){
			this.props.onChange(e,dateFormat(val, "yyyy-mm-dd"));
		}
	}
	render(){
		return (
				<DatePicker {...this.props}
					  container="inline"
		              mode="landscape"
		              locale="es"
		              DateTimeFormat={DateTimeFormat}
		              value={this.props.value?new Date(this.props.value+"T12:00:00Z"):null}
		              onChange={this.customChange}
	  			/>
		);
	}

}
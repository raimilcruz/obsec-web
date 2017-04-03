import React from 'react';
import ReactDOM from 'react-dom';
import InputHeader from './InputHeader';
import ReactSelect from 'react-select';
import 'react-select/dist/react-select.css'; 
import TextField from 'material-ui/TextField';

const styles = {
	icon: {
		display: 'inline-block',
		fill: 'rgb(224, 224, 224)',
		height: '24px', width: '24px',
		transition: 'all 450ms cubic-bezier(0.23, 1, 0.32, 1) 0ms',
		position: 'absolute',
		right: '0px',
	    top: '14px',
	    WebkitUserSelect: 'none'
	}
}
export default class Select extends React.Component{
	getSelected = (key) => {
        const cs = this.props.options.filter(c => c.value==key);
        if(cs.length<=0)
            return "";
        else
            return cs[0].label;
    }
    onChange = () => {
        console.log('change!')
    }
    render(){
        return (
            <div>
                {this.props.readOnly?
                    <TextField floatingLabelText={this.props.floatingLabelText} defaultValue={this.getSelected(this.props.value)} readOnly={true} onInputChange={this.onChange}/>
                    :
                    <div><InputHeader>{this.props.floatingLabelText}</InputHeader>
                    <ReactSelect {...this.props} /></div>
                }
            </div>
        )
    }
}
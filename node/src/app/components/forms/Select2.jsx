import React from 'react';
import ReactDOM from 'react-dom';
import TextField from 'material-ui/TextField';
import Paper from 'material-ui/Paper';
import InputHeader from './InputHeader';
import $ from 'jquery';

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
export default class Select2 extends React.Component{
	state = {
		value: this.props.defaultValue,
		open: false
	}
	

	constructor(props, context) {
        super(props, context);
        //document.addEventListener('click', this.handleClick, false);
    }
    componentWillMount = () => {
        document.addEventListener('click', this.handleClick, false);
    }

    componentWillUnmount = () => {
        document.removeEventListener('click', this.handleClick, false);
    }


    handleClick = (e) => {
    	const dom = ReactDOM.findDOMNode(this.refs.select)
        if (dom.contains(e.target)) {
        	return;
        } else{
        	this.setState({open:false})
        }
    }

    scroll = () => {
    	if(this.state.open){
			console.log('scrolling');
			$('.Select2-items ul').animate({scrollTop: $('.Select2-item.CL').position().top + 'px'});
		}
    }
	toggle = (v) =>{
		
		this.setState({open: !this.state.open}, s => {
			if(this.state.open){
				const ul = $(ReactDOM.findDOMNode(this.refs.ul));
				const stop = ul.scrollTop();
				const offset = (ul.find('.Select2-item.'+this.state.value).position().top+stop)-92;

				ul.animate({scrollTop:  + offset+'px'})
			}
		});

	}
	onSelect = (v) =>{
		this.setState({value: v, open: false});

	}
	getSelected = (key) => {
        const cs = this.props.options.filter(c => c[0]==key);
        if(cs.length<=0)
            return "ERROR";
        else
            return cs[0][1];
    }
    render(){
    	//<TextField floatingLabelText="País" defaultValue={this.props.defaultValue} />
    	const options = this.props.options.map(c =>
            <li val={c[0]} key={c[0]} onClick={this.onSelect.bind(this, c[0])} className={"Select2-item "+c[0]+" "+(this.state.value==c[0]?'Select2-active':'')}>{c[1]}</li>
        )
        return (
            <div className="Select2" ref="select">
            	<InputHeader>País</InputHeader>
            	<div className="Select2-visible" onClick={this.toggle}>
	                <div className="Select2-input">{this.getSelected(this.state.value)}</div>
	                <svg viewBox="0 0 24 24" style={styles.icon}><path d="M7 10l5 5 5-5z"></path></svg>
                	<hr />
                </div>
                <input type="hidden" name={this.props.name} value={this.state.value} />
                <Paper className="Select2-items" style={{width: '300px', display: this.state.open?'':'none'}} ref="items">
                	<ul ref="ul">{options}</ul>
                </Paper>
            </div>
        )
    }
}